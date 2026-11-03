#' Assign protocol filters
#'
#' This stage assigns the operational protocol filters from versioned rule
#' tables, calculating the astronomical times needed by those rules internally.
#'
#' @param data Reconciled observations containing `date`, `time`, `species`,
#'   `number`, `location`, and `age`.
#' @param rules A validated `brc_rules` object.
#' @return A `brc_protocol_filter` result with canonical data, summary, rule
#'   diagnostics, and solar-calculation provenance.
#' @export
brc_assign_protocol_filter <- function(
  data,
  rules = brc_rules("operational_v1")
) {
  required <- c("date", "time", "species", "number", "location", "age")
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop(
      "`data` is missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (!inherits(rules, "brc_rules")) {
    stop("`rules` must be a validated brc_rules object.", call. = FALSE)
  }
  dates <- as.Date(as.character(data$date), format = "%Y-%m-%d")
  if (anyNA(dates)) {
    stop("`data$date` must contain valid YYYY-MM-DD dates.", call. = FALSE)
  }
  times <- as.character(data$time)
  if (any(!grepl("^([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$", times))) {
    stop(
      "`data$time` must contain valid HH:MM:SS wall-clock times.",
      call. = FALSE
    )
  }
  years <- as.integer(format(dates, "%Y"))
  yday <- normalized_filter_yday(dates)
  seconds <- filter_clock_seconds(times)
  solar <- match_filter_solar(dates, solar_times_for_dates(dates))
  sunrise <- filter_clock_seconds(solar$sunrise)
  sunset <- filter_clock_seconds(solar$sunset)

  out <- data
  out$species <- as.character(out$species)
  out$species[out$species == "HB_AD"] <- "HB_NONJUV"
  out$species[out$species == "SB_AD"] <- "SB_NONJUV"
  out$filter <- 0L
  windows <- rules$tables$protocol_windows
  standard <- windows[windows$window_id == "standard", , drop = FALSE]
  if (nrow(standard) != 1L) {
    stop(
      "Rule table must contain exactly one `standard` protocol window.",
      call. = FALSE
    )
  }
  standard_active <- filter_window_active(
    years,
    yday,
    standard,
    normalized = TRUE
  )
  out$filter[
    standard_active &
      seconds >= sunrise + standard$start_offset_seconds[[1L]] &
      seconds <= sunset + standard$end_offset_seconds[[1L]]
  ] <- as.integer(standard$filter[[1L]])

  harrier <- windows[windows$window_id == "harrier_early", , drop = FALSE]
  if (nrow(harrier) != 1L) {
    stop(
      "Rule table must contain exactly one `harrier_early` window.",
      call. = FALSE
    )
  }
  harriers <- c("Mar", "Mon", "Pal", "Hen", "MonPalHen", "Harrier_SPEC")
  harrier_active <- filter_window_active(
    years,
    yday,
    harrier,
    normalized = TRUE
  )
  out$filter[
    harrier_active &
      out$species %in% harriers &
      seconds >= sunrise + harrier$start_offset_seconds[[1L]] &
      seconds < sunrise + harrier$end_offset_seconds[[1L]]
  ] <- as.integer(harrier$filter[[1L]])
  window_filter <- out$filter

  exclusions <- rules$tables$exclusions
  exclusion_hits <- lapply(seq_len(nrow(exclusions)), function(i) {
    filter_exclusion_hits(out, years, exclusions[i, , drop = FALSE])
  })
  effective <- exclusions$action == "filter_zero"
  zero_hits <- if (any(effective)) {
    Reduce(`|`, exclusion_hits[effective], init = rep(FALSE, nrow(out)))
  } else {
    rep(FALSE, nrow(out))
  }
  out$filter[zero_hits] <- 0L

  diagnostics <- filter_rule_diagnostics(
    out,
    window_filter,
    exclusion_hits,
    exclusions
  )
  out$year <- as.integer(format(as.Date(out$date), "%Y"))
  out$yday <- normalized_filter_yday(as.Date(out$date))
  out <- canonical_filter_columns(out)
  rownames(out) <- NULL
  structure(
    list(
      data = out,
      diagnostics = diagnostics,
      summary = list(
        input_rows = nrow(data),
        output_rows = nrow(out),
        filter_counts = table(factor(out$filter, levels = 0:2)),
        filter_zero_by_exclusion = vapply(
          exclusion_hits[effective],
          sum,
          integer(1)
        )
      ),
      provenance = list(
        rule_version = rules$version,
        timezone_name_or_fixed_offset = attr(solar, "solar_metadata")$timezone,
        solar_library_version = attr(
          solar,
          "solar_metadata"
        )$solar_library_version,
        latitude = attr(solar, "solar_metadata")$latitude,
        longitude = attr(solar, "solar_metadata")$longitude,
        yday_policy = "calendar-yday-minus-leap-day-after-february"
      )
    ),
    class = "brc_protocol_filter"
  )
}

filter_rule_diagnostics <- function(
  data,
  window_filter,
  exclusion_hits,
  exclusions
) {
  baseline <- data.frame(
    id = as.integer(data$id),
    filter = as.integer(window_filter),
    rule_id = ifelse(
      window_filter == 1L,
      "protocol.window.standard",
      ifelse(
        window_filter == 2L,
        "protocol.window.harrier_early",
        "protocol.default_outside_window"
      )
    ),
    reason = ifelse(
      window_filter == 1L,
      "inside standard protocol window",
      ifelse(
        window_filter == 2L,
        "inside harrier early protocol window",
        "outside all applicable protocol windows"
      )
    ),
    stringsAsFactors = FALSE
  )
  effective <- which(exclusions$action == "filter_zero")
  causes <- lapply(effective, function(rule_row) {
    rows <- which(exclusion_hits[[rule_row]])
    if (!length(rows)) {
      return(NULL)
    }
    data.frame(
      id = as.integer(data$id[rows]),
      filter = 0L,
      rule_id = as.character(exclusions$rule_id[[rule_row]]),
      reason = paste0(
        "filter-zero exclusion: ",
        as.character(exclusions$condition[[rule_row]])
      ),
      stringsAsFactors = FALSE
    )
  })
  causes <- Filter(Negate(is.null), causes)
  result <- if (length(causes)) {
    do.call(rbind, c(list(baseline), causes))
  } else {
    baseline
  }
  rownames(result) <- NULL
  result
}


normalized_filter_yday <- function(dates) {
  years <- as.integer(format(dates, "%Y"))
  leap <- years %% 4L == 0L & (years %% 100L != 0L | years %% 400L == 0L)
  raw <- as.integer(format(dates, "%j"))
  after_february <- format(dates, "%m-%d") >= "03-01"
  raw - as.integer(leap & after_february)
}

filter_clock_seconds <- function(x) {
  pieces <- strsplit(as.character(x), ":", fixed = TRUE)
  as.integer(vapply(
    pieces,
    function(z) {
      as.integer(z[[1L]]) *
        3600L +
        as.integer(z[[2L]]) * 60L +
        as.integer(z[[3L]])
    },
    integer(1)
  ))
}

match_filter_solar <- function(dates, solar) {
  date_text <- format(dates, "%Y-%m-%d")
  if (anyDuplicated(solar$date)) {
    stop("Solar-times dates must be unique.", call. = FALSE)
  }
  index <- match(date_text, solar$date)
  if (anyNA(index)) {
    stop(
      "Solar times have no row for: ",
      paste(unique(date_text[is.na(index)]), collapse = ", "),
      call. = FALSE
    )
  }
  matched <- solar[index, c("sunrise", "sunset"), drop = FALSE]
  attr(matched, "solar_metadata") <- attr(solar, "solar_metadata")
  matched
}

filter_window_active <- function(year, yday, window, normalized = TRUE) {
  start <- normalized_filter_yday(as.Date(paste0(
    "2023-",
    window$start_month_day[[1L]]
  )))
  end <- normalized_filter_yday(as.Date(paste0(
    "2023-",
    window$end_month_day[[1L]]
  )))
  year >= window$effective_from[[1L]] &
    year <= window$effective_to[[1L]] &
    yday >= start &
    yday <= end
}

filter_exclusion_hits <- function(data, years, rule) {
  active <- years >= rule$effective_from[[1L]] &
    years <= rule$effective_to[[1L]]
  values <- strsplit(rule$values[[1L]], "|", fixed = TRUE)[[1L]]
  values <- values[nzchar(values)]
  field <- rule$field[[1L]]
  value_hit <- if (!length(values)) {
    rep(TRUE, nrow(data))
  } else if (field %in% names(data)) {
    as.character(data[[field]]) %in% values
  } else if (identical(field, "year")) {
    years %in% suppressWarnings(as.integer(values))
  } else {
    stop("Unsupported exclusion field: ", field, call. = FALSE)
  }
  condition <- rule$condition[[1L]]
  condition_hit <- switch(
    condition,
    "always" = rep(TRUE, nrow(data)),
    "location in E2|E3|W2|W3" = data$location %in% c("E2", "E3", "W2", "W3"),
    stop("Unsupported exclusion condition: ", condition, call. = FALSE)
  )
  active & value_hit & condition_hit
}

canonical_filter_columns <- function(data) {
  aliases <- c(raw = "number", mtype = "health")
  for (from in names(aliases)) {
    to <- aliases[[from]]
    if (!to %in% names(data) && from %in% names(data)) {
      data[[to]] <- data[[from]]
    }
  }
  wanted <- c(
    "id",
    "date",
    "time",
    "species",
    "number",
    "north",
    "station",
    "location",
    "age",
    "sex",
    "morph",
    "health",
    "remark",
    "dcremark",
    "filter",
    "year",
    "yday"
  )
  for (name in setdiff(wanted, names(data))) {
    data[[name]] <- if (
      name %in%
        c(
          "id",
          "number",
          "north",
          "station",
          "filter",
          "year",
          "yday"
        )
    ) {
      NA_integer_
    } else {
      ""
    }
  }
  data[wanted]
}
