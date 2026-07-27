#' Convert and normalize raw BRC observations
#'
#' `brc_convert()` is a pure in-memory conversion of reviewed observations.
#' It accepts only the reviewed 13-column raw schema and never reads or writes
#' files. Stable source keys follow `source-key-v1` in the processing contract.
#'
#' @param data A data frame with the exact raw observation schema.
#' @param rules A validated `brc_rules` profile.
#' @param source_artifact_sha256 Lowercase SHA-256 of the unmodified source
#'   artifact from which `data` was read.
#' @param strict_mappings If `TRUE`, an unreviewed species mapping is an error.
#'   Set to `FALSE` to reject unknown species mappings instead of emitting
#'   observations with unresolved species.
#' @return A `brc_conversion` list containing normalized data, side outputs,
#'   rejected rows, summary, and provenance.
#' @export
brc_convert <- function(
  data,
  rules = brc_rules("operational_v1"), # nolint
  source_artifact_sha256 = NULL,
  strict_mappings = TRUE
) {
  raw_columns <- c(
    "datetime",
    "telpost",
    "speciesname",
    "count",
    "countback",
    "local",
    "age",
    "sex",
    "plumage",
    "remark",
    "location",
    "migtype",
    "counttype"
  )
  validate_converter_input(data, raw_columns, source_artifact_sha256)
  if (!inherits(rules, "brc_rules")) {
    stop("`rules` must be a validated brc_rules object.", call. = FALSE)
  }
  validate_brc_rules(rules$tables, rules$version) # nolint

  normalized <- data
  blank_record <- converter_blank_record(data)
  source_rows <- seq_len(nrow(data))
  normalized$source_key <- vapply(
    source_rows,
    function(row) {
      digest::digest(
        paste(
          "source-key-v1",
          source_artifact_sha256,
          row,
          sep = "\u001f"
        ),
        algo = "sha256",
        serialize = FALSE
      )
    },
    character(1)
  )

  normalized <- normalize_converter_text(normalized)
  if (is.numeric(data$countback)) {
    normalized$countback[is.na(data$countback)] <- NA_character_
  }
  if (is.numeric(data$local)) {
    normalized$local[is.na(data$local)] <- NA_character_
  }
  parsed <- parse_converter_datetime(normalized$datetime, blank_record)
  normalized$year <- parsed$year
  normalized$protocol <- 0L
  normalized$time2 <- parsed$time2
  normalized$yday <- parsed$yday
  normalized$count <- strict_numeric_count(normalized$count, blank_record)
  normalized$telpost <- normalize_station(normalized$telpost, blank_record)

  normalized$speciesname <- map_converter_aliases(
    normalized$speciesname,
    normalized$year,
    rules
  )
  normalized <- stage_converter_species(normalized)
  normalized <- normalize_age_sex_morph(normalized)
  known_species <- validate_converter_species(
    normalized$speciesname,
    normalized$year,
    rules,
    blank_record,
    strict_mappings
  )

  metadata_events <- c("BREAK", "END", "RESUME", "START")
  metadata_index <- normalized$speciesname %in% metadata_events
  shot_index <- normalized$speciesname %in%
    "SHOT" |
    (!is.na(normalized$migtype) & normalized$migtype != "")
  observation_index <- known_species

  output_columns <- c(
    raw_columns,
    "year",
    "protocol",
    "time2",
    "yday",
    "id",
    "source_key"
  )
  normalized$id <- NA_integer_
  normalized$id[observation_index] <- seq_len(sum(observation_index))
  normalized <- normalized[output_columns]

  rejected_index <- !observation_index
  rejected <- normalized[rejected_index, , drop = FALSE]
  rejected$reason <- ifelse(
    blank_record[rejected_index],
    "blank_record",
    ifelse(
      metadata_index[rejected_index],
      "metadata_event",
      "unknown_mapping"
    )
  )
  result <- list(
    data = normalized[observation_index, , drop = FALSE],
    metadata = normalized[metadata_index, , drop = FALSE],
    shots = normalized[shot_index, , drop = FALSE],
    rejected = rejected,
    summary = list(
      input_rows = nrow(data),
      output_rows = sum(observation_index),
      metadata_rows = sum(metadata_index),
      shot_rows = sum(shot_index),
      rejected_rows = sum(rejected_index),
      input_count = sum(normalized$count, na.rm = TRUE),
      output_count = sum(normalized$count[observation_index], na.rm = TRUE)
    ),
    provenance = list(
      rule_version = rules$version,
      source_artifact_sha256 = source_artifact_sha256,
      source_key_algorithm = "source-key-v1"
    )
  )
  rownames(result$data) <- NULL
  rownames(result$metadata) <- NULL
  rownames(result$shots) <- NULL
  rownames(result$rejected) <- NULL
  structure(result, class = "brc_conversion")
}

converter_blank_record <- function(data) {
  is_blank <- lapply(data, function(column) {
    is.na(column) | trimws(as.character(column)) == ""
  })
  Reduce(`&`, is_blank, init = rep(TRUE, nrow(data)))
}

validate_converter_input <- function(data, raw_columns, source_hash) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  missing <- setdiff(raw_columns, names(data))
  extra <- setdiff(names(data), raw_columns)
  if (
    length(missing) || length(extra) || !identical(names(data), raw_columns)
  ) {
    detail <- c(
      if (length(missing)) paste("missing:", paste(missing, collapse = ", ")),
      if (length(extra)) paste("unexpected:", paste(extra, collapse = ", ")),
      if (!length(missing) && !length(extra)) "columns are out of order"
    )
    stop(
      "Input does not match exact raw schema (",
      paste(detail, collapse = "; "),
      ").",
      call. = FALSE
    )
  }
  if (
    !is.character(source_hash) ||
      length(source_hash) != 1L ||
      is.na(source_hash) ||
      !grepl("^[0-9a-f]{64}$", source_hash)
  ) {
    stop(
      "`source_artifact_sha256` must be one lowercase 64-character SHA-256.",
      call. = FALSE
    )
  }
  invisible(data)
}

normalize_converter_text <- function(data) {
  text_columns <- setdiff(names(data), "source_key")
  for (column in text_columns) {
    data[[column]] <- as.character(data[[column]])
  }
  blank_compatible <- c(
    "countback",
    "local",
    "age",
    "sex",
    "plumage",
    "remark",
    "migtype",
    "counttype"
  )
  for (column in blank_compatible) {
    data[[column]][is.na(data[[column]])] <- ""
  }
  data$remark <- gsub("[,;']", " ", data$remark)
  species <- gsub("'", " ", data$speciesname, fixed = TRUE)
  species <- gsub("[-/ ]", "_", species)
  data$speciesname <- species
  data$location[data$location == "w1"] <- "W1"
  data$age <- gsub("[ -]", "", tolower(data$age))
  data$age[data$age == "novalue"] <- ""
  data$age[data$age == "j"] <- "juv"
  data$age[data$age == "i"] <- "imm"
  data$age[data$age == "a"] <- "ad"
  data$age[data$age == "adult"] <- "ad"
  data$age[data$age == "m"] <- "nonjuv"

  data$sex <- gsub(" ", "", tolower(data$sex), fixed = TRUE)
  data$sex[data$sex %in% c("novalue", "nonjuv")] <- ""
  data$sex[data$sex == "female"] <- "f"
  data$sex[data$sex == "male"] <- "m"

  data$plumage <- tolower(data$plumage)
  morph_map <- c(
    d = "dark",
    l = "light",
    fulvescens = "ful",
    `fox red` = "",
    melanistic = "mel",
    leucistic = "leu"
  )
  replace <- data$plumage %in% names(morph_map)
  data$plumage[replace] <- unname(morph_map[data$plumage[replace]])

  data$counttype <- tolower(data$counttype)
  data$counttype[data$counttype == "s"] <- "sc"
  data$counttype[data$counttype == "d"] <- "dc"
  data$migtype <- tolower(data$migtype)
  data$migtype[data$migtype %in% c("injured", "i")] <- "inj"
  data$migtype[data$migtype %in% c("killed", "k")] <- "kil"
  data
}

parse_converter_datetime <- function(value, allow_missing) {
  valid_shape <- !is.na(value) &
    grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$", value)
  parsed <- as.POSIXct(
    value,
    format = "%Y-%m-%d %H:%M:%S",
    tz = "Etc/GMT-4"
  )
  roundtrip <- format(parsed, "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-4")
  invalid <- which(
    !allow_missing & (!valid_shape | is.na(parsed) | roundtrip != value)
  )
  if (length(invalid)) {
    stop(
      "invalid datetime at row ",
      invalid[[1L]],
      ": ",
      as.character(value[invalid[[1L]]]),
      call. = FALSE
    )
  }
  dates <- as.Date(substr(value, 1L, 10L))
  list(
    time2 = parsed,
    year = as.integer(substr(value, 1L, 4L)),
    yday = as.integer(format(dates, "%j"))
  )
}

strict_numeric_count <- function(value, allow_missing) {
  text <- as.character(value)
  parsed <- suppressWarnings(as.numeric(text))
  invalid <- which(
    !allow_missing & (is.na(parsed) | !is.finite(parsed) | parsed < 0)
  )
  if (length(invalid)) {
    stop(
      "invalid count at row ",
      invalid[[1L]],
      ": ",
      text[invalid[[1L]]],
      call. = FALSE
    )
  }
  parsed
}

normalize_station <- function(value, allow_missing) {
  value <- trimws(as.character(value))
  value[value %in% c("1047", "1. Sakhalvasho")] <- "1"
  value[value %in% c("1048", "2. Shuamta")] <- "2"
  invalid <- which(!allow_missing & !value %in% c("1", "2", "3"))
  if (length(invalid)) {
    stop(
      "unknown station at row ",
      invalid[[1L]],
      ": ",
      value[invalid[[1L]]],
      call. = FALSE
    )
  }
  value
}

map_converter_aliases <- function(value, year, rules) {
  aliases <- rules$tables$aliases
  mapped <- value
  contextual <- aliases$alias %in%
    c("Pal", "Mon", "Hen") &
    aliases$species == "MonPalHen"
  aliases <- aliases[!contextual, , drop = FALSE]
  alias_maps <- converter_alias_maps(aliases, year)
  for (year_value in names(alias_maps)) {
    hits <- which(year == as.integer(year_value))
    if (!length(hits)) {
      next
    }
    replacements <- unname(alias_maps[[year_value]][value[hits]])
    keep <- !is.na(replacements)
    mapped[hits[keep]] <- replacements[keep]
  }
  mapped
}

converter_alias_maps <- function(aliases, years) {
  lapply(
    stats::setNames(as.list(sort(unique(years))), sort(unique(years))),
    function(year_value) {
      active <- aliases[
        aliases$effective_from <= year_value &
          aliases$effective_to >= year_value,
        ,
        drop = FALSE
      ]
      if (!nrow(active)) {
        return(stats::setNames(character(), character()))
      }
      dedup <- !duplicated(active$alias)
      stats::setNames(
        as.character(active$species[dedup]),
        as.character(active$alias[dedup])
      )
    }
  )
}

stage_converter_species <- function(data) {
  pal_mon <- data$speciesname %in% c("Pal", "Mon")
  data$age[pal_mon & data$age == "" & data$sex %in% c("f", "m")] <- "nonjuv"
  family <- data$speciesname %in% c("Pal", "Mon", "Hen", "Mar")
  data$sex[family & data$age == "juv"] <- ""

  blank_pal <- !is.na(data$speciesname) &
    data$speciesname == "Pal" &
    data$sex == "" &
    data$age == ""
  data$remark[blank_pal] <- paste(
    "previously Pal without age and sex -",
    data$remark[blank_pal]
  )
  data$speciesname[blank_pal] <- "MonPalHen"

  blank_family <- !is.na(data$speciesname) &
    data$speciesname %in% c("Pal", "Mon", "Hen") &
    data$sex == "" &
    data$age != "juv"
  data$remark[blank_family] <- paste(
    "previously",
    data$speciesname[blank_family],
    "without age and sex -",
    data$remark[blank_family]
  )
  data$speciesname[blank_family] <- "MonPalHen"

  female_type <- !is.na(data$speciesname) &
    data$speciesname %in% c("Pal", "Mon") &
    data$sex == "fc"
  data$speciesname[female_type] <- "MonPalHen"
  data$age[
    data$speciesname == "MonPalHen" & data$age == "ad"
  ] <- "nonjuv"
  data
}

validate_converter_species <- function(
  value,
  year,
  rules,
  allow_missing,
  strict_mappings
) {
  species <- rules$tables$species
  events <- c("BREAK", "END", "RESUME", "START")
  known <- rep(FALSE, length(value))
  target <- !allow_missing
  for (year_value in sort(unique(year))) {
    hits <- which(target & year == as.integer(year_value))
    if (!length(hits)) {
      next
    }
    active <- species[
      species$effective_from <= year_value &
        species$effective_to >= year_value,
      ,
      drop = FALSE
    ]
    known[hits] <- value[hits] %in% active$species
  }
  if (strict_mappings) {
    unknown <- which(target & !known & !value %in% events)
    if (length(unknown)) {
      stop(
        "unknown required species mapping at row ",
        unknown[[1L]],
        ": ",
        value[[unknown[[1L]]]],
        call. = FALSE
      )
    }
  }
  known
}

normalize_age_sex_morph <- function(data) {
  data$sex[data$age %in% c("ad", "nonjuv") & data$sex == "fc"] <- "f"
  data$sex[data$age == "juv" & data$sex == "fc"] <- ""
  data$age[data$sex %in% c("f", "m") & data$age == ""] <- "nonjuv"
  data$age[
    data$speciesname %in%
      c("HB_AD", "HB_JUV", "BK_NONJUV", "BK_JUV")
  ] <- ""
  data$sex[data$plumage == "fc"] <- "fc"
  data$plumage[data$plumage == "fc"] <- ""
  data
}
