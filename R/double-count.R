# nolint start
#' Reconcile potential double counts between stations
#'
#' This is a pure, deterministic implementation of the operational phase-two
#' reconciliation. Input order is not used to break ties.
#'
#' @param data Converter output (or its `data` member).
#' @param rules A `brc_rules` profile.
#' @param id_start Optional first identifier minus one. Existing IDs are
#'   retained; missing IDs are assigned from this value.
#' @return A `brc_double_count` list with `data`, `summary`, and `provenance`.
#' @export
brc_reconcile_double_counts <- function(
  data,
  rules = brc_rules("operational_v1"),
  id_start = 0L
) {
  if (inherits(data, "brc_conversion")) {
    data <- data$data
  }
  required <- c(
    "datetime",
    "telpost",
    "speciesname",
    "count",
    "age",
    "sex",
    "plumage",
    "location",
    "counttype",
    "year",
    "id",
    "source_key"
  )
  if (!is.data.frame(data) || length(setdiff(required, names(data)))) {
    stop(
      "Double-count input is missing required converter columns.",
      call. = FALSE
    )
  }
  if (!inherits(rules, "brc_rules")) {
    stop("`rules` must be a brc_rules object.", call. = FALSE)
  }
  needed_tables <- c(
    "comparable_groups",
    "speeds",
    "protocol_windows"
  )
  if (length(setdiff(needed_tables, names(rules$tables)))) {
    stop("`rules` lacks required double-count tables.", call. = FALSE)
  }
  out <- data
  n <- nrow(out)
  count_numeric <- suppressWarnings(as.numeric(as.character(out$count)))
  if (
    anyNA(count_numeric) ||
      any(!is.finite(count_numeric)) ||
      any(count_numeric < 0) ||
      any(count_numeric != floor(count_numeric)) ||
      any(count_numeric > .Machine$integer.max)
  ) {
    stop("`count` must contain non-negative whole numbers.", call. = FALSE)
  }
  raw <- as.integer(count_numeric)
  times <- if ("time2" %in% names(out) && inherits(out$time2, "POSIXt")) {
    out$time2
  } else {
    as.POSIXct(
      out$datetime,
      format = "%Y-%m-%d %H:%M:%S",
      tz = "Etc/GMT-4"
    )
  }
  if (anyNA(times)) {
    stop("`datetime`/`time2` contains invalid timestamps.", call. = FALSE)
  }
  source_keys <- as.character(out$source_key)
  if (
    anyNA(source_keys) ||
      anyDuplicated(source_keys) ||
      any(!grepl("^[0-9a-f]{64}$", source_keys))
  ) {
    stop("`source_key` values must be unique lowercase SHA-256.", call. = FALSE)
  }
  stable_input_id <- suppressWarnings(as.integer(as.character(out$id)))
  if (anyNA(stable_input_id) || anyDuplicated(stable_input_id)) {
    stop("Input `id` must be a unique integer stable key.", call. = FALSE)
  }
  order_index <- order(
    as.numeric(times),
    stable_input_id,
    source_keys,
    method = "radix"
  )
  out <- out[order_index, , drop = FALSE]
  raw <- raw[order_index]
  times <- times[order_index]
  source_keys <- source_keys[order_index]
  times_num <- as.numeric(times)
  if (
    length(id_start) != 1L ||
      is.na(id_start) ||
      id_start != floor(id_start) ||
      id_start < 0 ||
      id_start + n > .Machine$integer.max
  ) {
    stop("`id_start` must permit an integer ID for every row.", call. = FALSE)
  }
  ids <- as.integer(id_start) + seq_len(n)
  out$id <- ids

  species <- as.character(out$speciesname)
  years <- as.integer(out$year)
  telpost <- as.integer(out$telpost)
  location <- as.character(out$location)
  age <- as.character(out$age)
  sex <- as.character(out$sex)
  plumage <- as.character(out$plumage)
  counttype <- tolower(as.character(out$counttype))
  context <- dc_build_context(rules, years, species)

  remaining <- raw
  dcdel <- integer(n)
  dckept <- integer(n)
  remarks <- rep("", n)
  reconciled_events <- 0L
  configured_left <- context$configured_left
  phase_one <- dc_phase_one(species, telpost, years, times_num, rules)
  eligible <- !phase_one &
    !(counttype %in% c("sc", "single"))
  station_one <- which(
    telpost == 1L & eligible & species %in% configured_left
  )
  station_one <- station_one[order(times[station_one], ids[station_one])]
  station_two <- which(telpost == 2L & eligible)
  station_two <- station_two[order(times[station_two], ids[station_two])]
  station_two_time <- times_num[station_two]

  for (left in station_one) {
    seconds <- context$speed[[left]]
    center <- times_num[[left]]
    first <- findInterval(
      center - seconds,
      station_two_time,
      left.open = TRUE
    ) +
      1L
    last <- findInterval(center + seconds, station_two_time)
    candidates <- if (first <= last) {
      station_two[seq.int(first, last)]
    } else {
      integer()
    }
    candidates <- candidates[
      remaining[candidates] > 0L & location[candidates] != "E3"
    ]
    if (!length(candidates)) {
      next
    }
    comparable_species <- dc_comparable_values(
      species[[left]],
      years[[left]],
      context
    )
    if (!length(comparable_species)) {
      next
    }
    candidates <- candidates[
      species[candidates] %in%
        comparable_species &
        dc_location_mask(
          location[[left]],
          location[candidates],
          years[[left]],
          seconds == 900L
        )
    ]
    candidates <- candidates[order(
      abs(times_num[candidates] - times_num[[left]]),
      ids[candidates]
    )]
    for (right in candidates) {
      if (remaining[left] <= 0L) {
        break
      }
      keep <- dc_keep_row(
        species[[left]],
        species[[right]],
        age[[left]],
        age[[right]],
        sex[[left]],
        sex[[right]],
        plumage[[left]],
        plumage[[right]],
        location[[left]],
        location[[right]],
        left,
        right
      )
      if (!keep) {
        next
      }
      left_capacity <- raw[left] - (dckept[left] - dcdel[left])
      right_capacity <- raw[right] - (dckept[right] - dcdel[right])
      amount <- min(
        remaining[left],
        remaining[right],
        left_capacity,
        right_capacity
      )
      if (amount <= 0L) {
        next
      }
      deleted <- if (keep == left) right else left
      kept <- if (keep == left) left else right
      remaining[deleted] <- remaining[deleted] - amount
      dcdel[deleted] <- dcdel[deleted] - amount
      dckept[kept] <- dckept[kept] + amount
      remarks[kept] <- paste0(
        remarks[kept],
        "ID",
        ids[deleted],
        ":",
        -amount,
        " "
      )
      remarks[deleted] <- paste0(
        remarks[deleted],
        "ID",
        ids[kept],
        ":",
        amount,
        " "
      )
      reconciled_events <- reconciled_events + 1L
    }
  }
  number <- raw + dcdel
  if (
    any(number < 0L) || sum(dckept + dcdel) != 0L || any(number != raw + dcdel)
  ) {
    stop("Internal double-count balance invariant failed.", call. = FALSE)
  }
  output_ctype <- as.character(out$counttype)
  output_ctype[phase_one] <- "sc"
  north <- suppressWarnings(as.integer(as.character(out$countback)))
  north[is.na(north)] <- 0L
  output_data <- data.frame(
    id = ids,
    date = format(times, "%Y-%m-%d", tz = "Etc/GMT-4"),
    time = format(times, "%H:%M:%S", tz = "Etc/GMT-4"),
    species = as.character(out$speciesname),
    number = number,
    raw = raw,
    north = north,
    station = as.integer(out$telpost),
    location = as.character(out$location),
    age = as.character(out$age),
    sex = as.character(out$sex),
    morph = as.character(out$plumage),
    ctype = output_ctype,
    mtype = as.character(out$migtype),
    remark = as.character(out$remark),
    dckept = dckept,
    dcdel = dcdel,
    dcremark = remarks,
    stringsAsFactors = FALSE
  )
  structure(
    list(
      data = output_data,
      summary = list(
        input_rows = n,
        reconciled_events = reconciled_events,
        input_count = sum(raw),
        output_count = sum(number),
        transferred_count = sum(dckept)
      ),
      provenance = list(
        rule_version = rules$version,
        algorithm = "operational-double-count-v1-deterministic"
      )
    ),
    class = "brc_double_count"
  )
}

dc_build_context <- function(rules, years, species) {
  year_keys <- as.character(sort(unique(years)))
  comparable_groups <- lapply(year_keys, function(year_key) {
    year <- as.integer(year_key)
    x <- rules$tables$comparable_groups
    x <- x[x$effective_from <= year & x$effective_to >= year, , drop = FALSE]
    split(as.character(x$species), as.character(x$group))
  })
  names(comparable_groups) <- year_keys

  speed <- integer(length(species))
  speed_table <- rules$tables$speeds
  for (year_key in year_keys) {
    year <- as.integer(year_key)
    active <- speed_table[
      speed_table$effective_from <= year & speed_table$effective_to >= year,
      ,
      drop = FALSE
    ]
    defaults <- active$seconds[active$species == "*"]
    default_speed <- if (length(defaults)) as.integer(defaults[[1L]]) else 900L
    hits <- years == year
    speed[hits] <- default_speed
    matched <- match(species[hits], as.character(active$species))
    matched_speed <- active$seconds[matched]
    keep <- !is.na(matched_speed)
    hit_index <- which(hits)
    speed[hit_index[keep]] <- as.integer(matched_speed[keep])
  }

  list(
    comparable = comparable_groups,
    speed = speed,
    configured_left = unique(as.character(rules$tables$comparable_groups$group))
  )
}

dc_comparable_values <- function(left, year, context) {
  year_map <- context$comparable[[as.character(year)]]
  if (is.null(year_map)) {
    return(character())
  }
  values <- year_map[[left]]
  if (is.null(values)) character() else values
}

dc_comparable <- function(left, right, year, rules) {
  context <- dc_build_context(rules, year, left)
  right %in% dc_comparable_values(left, year, context)
}

dc_location_mask <- function(left, right, year, soaring) {
  if (year < 2018L) {
    return(rep(FALSE, length(right)))
  }
  ordinary <- switch(
    as.character(left),
    E2 = right %in% c("W3", "W2", ""),
    E3 = right %in% c("W2", "W3", "W1", ""),
    ` ` = right %in% c("W2", "W3"),
    E1 = right == "W3",
    FALSE
  )
  if (identical(as.character(left), "")) {
    ordinary <- right %in% c("W2", "W3")
  }
  if (!soaring) {
    return(ordinary)
  }
  soaring_overlap <- (left == "E3" & right %in% c("O", "E1", "E2")) |
    (left == "O" & right == "W3") |
    (left == "W1" & right == "W3") |
    (left == "W2" & right == "W3")
  ordinary | soaring_overlap
}

dc_location <- function(left, right, year, soaring, rules = NULL) {
  mask <- dc_location_mask(left, as.character(right), year, soaring)
  if (length(mask) == 1L) mask[[1L]] else mask
}

dc_phase_one <- function(species, telpost, years, times_num, rules) {
  window <- rules$tables$protocol_windows
  window <- window[
    window$window_id == "honey_buzzard_phase_one",
    ,
    drop = FALSE
  ]
  result <- rep(FALSE, length(species))
  if (!nrow(window)) {
    return(result)
  }
  candidates <- which(species == "HB" & telpost == 1L)
  if (!length(candidates)) {
    return(result)
  }
  for (year in unique(years[candidates])) {
    active <- window$effective_from <= year & window$effective_to >= year
    if (!any(active)) {
      next
    }
    w <- window[which(active)[1L], , drop = FALSE]
    start <- as.numeric(as.POSIXct(
      sprintf("%d-%s 00:00:00", year, w$start_month_day[[1L]]),
      tz = "Etc/GMT-4"
    )) +
      as.integer(w$start_offset_seconds[[1L]])
    end <- as.numeric(as.POSIXct(
      sprintf("%d-%s 00:00:00", year, w$end_month_day[[1L]]),
      tz = "Etc/GMT-4"
    )) +
      as.integer(w$end_offset_seconds[[1L]])
    hits <- candidates[years[candidates] == year]
    result[hits] <- times_num[hits] > start & times_num[hits] < end
  }
  result
}

dc_precision <- function(value, broad, kind) {
  value <- ifelse(is.na(value), "", as.character(value))
  broad <- ifelse(is.na(broad), "", as.character(broad))
  if (identical(value, broad)) {
    return(0L)
  }
  if (!nzchar(value)) {
    return(-1L)
  }
  if (!nzchar(broad)) {
    return(1L)
  }
  if (kind == "age" && value %in% c("ad", "imm") && broad == "nonjuv") {
    return(1L)
  }
  if (kind == "age" && broad %in% c("ad", "imm") && value == "nonjuv") {
    return(-1L)
  }
  if (kind == "sex" && value == "f" && broad == "fc") {
    return(1L)
  }
  if (kind == "sex" && broad == "f" && value == "fc") {
    return(-1L)
  }
  NA_integer_
}

dc_keep_row <- function(
  left_species,
  right_species,
  left_age,
  right_age,
  left_sex,
  right_sex,
  left_plumage,
  right_plumage,
  left_location_value,
  right_location_value,
  left,
  right
) {
  equal_species <- identical(
    left_species,
    right_species
  )
  # Character columns have no factor levels, so cross-species rank never
  # is zero for both operands, so cross-species rank never decides the keeper.
  left_species <- FALSE
  right_species <- FALSE
  precision <- function(a, b, kind) {
    score <- dc_precision(a, b, kind)
    c(
      left = identical(score, 1L),
      right = identical(score, -1L),
      equal = identical(as.character(a), as.character(b))
    )
  }
  age <- precision(left_age, right_age, "age")
  sex <- precision(left_sex, right_sex, "sex")
  morph <- precision(left_plumage, right_plumage, "plumage")
  left_location <- !identical(left_location_value, "E3")
  right_location <- !left_location
  keep_left <-
    ((equal_species || left_species) &&
      age["left"] &&
      (sex["left"] || sex["equal"]) &&
      (morph["left"] || morph["equal"])) ||
    ((equal_species || left_species) &&
      age["equal"] &&
      sex["left"] &&
      (morph["left"] || morph["equal"])) ||
    ((equal_species || left_species) &&
      age["equal"] &&
      sex["equal"] &&
      morph["left"]) ||
    (left_species && age["equal"] && sex["equal"] && morph["equal"]) ||
    (equal_species &&
      age["equal"] &&
      sex["equal"] &&
      morph["equal"] &&
      left_location)
  keep_right <-
    ((equal_species || right_species) &&
      age["right"] &&
      (sex["right"] || sex["equal"]) &&
      (morph["right"] || morph["equal"])) ||
    ((equal_species || right_species) &&
      age["equal"] &&
      sex["right"] &&
      (morph["right"] || morph["equal"])) ||
    (right_species && age["equal"] && sex["equal"] && morph["right"]) ||
    (equal_species &&
      age["equal"] &&
      sex["equal"] &&
      morph["equal"] &&
      right_location)
  if (keep_left) {
    left
  } else if (keep_right) {
    right
  } else {
    0L
  }
}
# nolint end
