#' Preprocess RAW Trektellen CSV to the standardised BRC form
#'
#' @param csv_path Path to .csv file with RAW Trektellen counts
#' @param date_str Date string in YYYY-MM-DD format to filter data from RAW counts.
#'   `NULL` keeps all dates without retrieving count-period metadata.
#'
#' @returns a pre-processed dataframe which follows BRC data formatting
#' @export
#'
preprocess_raw_trektellen_data <- function(csv_path, date_str = NULL) {
  # Load csv file
  data <- readr::read_csv(
    csv_path,
    col_types = readr::cols(
      date = readr::col_date(format = ""),
      timestamp = readr::col_time(format = ""),
      countid = readr::col_double(),
      telpost = readr::col_double(),
      speciesname = readr::col_character(),
      speciesid = readr::col_double(),
      count = readr::col_double(),
      countback = readr::col_double(),
      local = readr::col_double(),
      age = readr::col_character(),
      sex = readr::col_character(),
      plumage = readr::col_character(),
      remark = readr::col_character(),
      location = readr::col_character(),
      migtype = readr::col_character(),
      counttype = readr::col_character(),
      year = readr::col_double(),
      yday = readr::col_double()
    )
  )

  # Filter for a specific date
  if (!is.null(date_str)) {
    date_Date <- as.Date(date_str, format = "%Y-%m-%d")

    on_date <- data$date == date_Date
    data <- data[on_date, ]

    if (nrow(data) == 0) {
      stop("Date selection resulted in 0 rows")
    }

    for (station in c(1047, 1048)) {
      period <- extract_count_period(station, date_str)
      if (is.null(period)) {
        if (!any(data$telpost == station, na.rm = TRUE)) {
          next
        }
        warning(
          "Count times for station ", station,
          " are unavailable; using placeholder START/END times.",
          call. = FALSE
        )
        period <- list(start = hms::as_hms(0), end = hms::as_hms(86399))
      }
      times <- data.frame(
        date = rep(date_Date, 2),
        timestamp = hms::as_hms(c(period$start, period$end)),
        telpost = station,
        speciesname = c("START", "END"),
        location = "O",
        count = 1
      )
      data <- dplyr::bind_rows(data, times)
      if (!is.null(period$breakresumes)) {
        data <- add_breakresume(data, date_Date, period$breakresumes, station)
      }
    }
  }

  # Change timestamp to 00:00:00 if timestamp was missing
  timestamp_missing <- is.na(data$timestamp)
  data$timestamp[timestamp_missing] <- hms::as_hms(0)

  # Make datetime column
  data$datetime <- lubridate::ymd(data$date) + lubridate::hms(data$timestamp)

  # Turn counttype NAs into ""
  data$counttype[is.na(data$counttype)] <- ""

  # Remove unused columns, including the date and timestamp columns, which we can regenerate later on
  drop_cols <- c("date", "timestamp", "countid", "speciesid", "year", "yday")
  data <- data[, -which(names(data) %in% drop_cols)]

  # Reorder columns
  column_order <- c(
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
  data <- data[, column_order]

  # Replace station IDs with names
  data$telpost[data$telpost == 1047] <- "1. Sakhalvasho"
  data$telpost[data$telpost == 1048] <- "2. Shuamta"

  # Replace species names
  repl <- list(
    HB_AD = "HB_NONJUV",
    `large FALCON` = "Large FALCON",
    `Raptor-SPEC` = "Raptor_SPEC",
    `Stork-SPEC` = "Stork_SPEC",
    `Buzzard-SPEC` = "Buzzard_SPEC",
    `dove (Columba) sp.` = "Dove_SPEC",
    `Harrier-SPEC` = "Harrier_SPEC",
    `Oriental Turtle-Dove` = "OrientalTD",
    WhitePel = "WhiteP",
    DalPel = "DalmatianP"
  )
  for (k in names(repl)) {
    data$speciesname[data$speciesname == k] <- repl[[k]]
  }

  # Sort by datetime, telpost
  data <- data[order(data$datetime, data$telpost), ]

  data
}

#' Check formatted Trektellen data
#'
#' @param data Dataframe with BRC formatted data
#'
#' @returns Dataframe with checked Trektellen data. Rows are flagged for
#' several common data issues in the 'check' column.
#' @export
check_trektellen_data <- function(data) {
  # Season parameters
  season_params <- get_season_params(unique(lubridate::year(data$datetime)))

  # Check doublecounts
  dc_idx <- which(data$counttype == "D")
  nr_doublecounts <- length(dc_idx)

  suspect_dc_records <- integer(0)

  j <- 1
  while (j < nr_doublecounts) {
    cur_idx <- dc_idx[j]
    next_idx <- dc_idx[j + 1]

    cur_r <- data[cur_idx, ]
    next_r <- data[next_idx, ]

    suspect <- FALSE

    # 10-minute window
    minutes_diff <- difftime(next_r$datetime, cur_r$datetime, units = "mins")
    minutes_diff <- as.numeric(minutes_diff)
    if (minutes_diff > 10) {
      suspect <- TRUE
    }

    # Are the species the same?
    if (cur_r$speciesname != next_r$speciesname) {
      suspect <- TRUE
    }

    # Age the same?
    if (!is.na(cur_r$age) & !is.na(next_r$age)) {
      if (cur_r$age != next_r$age) {
        suspect <- TRUE
      }
    }

    # Sex the same?
    if (!is.na(cur_r$sex) & !is.na(next_r$sex)) {
      if (cur_r$sex != next_r$sex) {
        suspect <- TRUE
      }
    }

    # Count the same?
    if (cur_r$count != next_r$count) {
      suspect <- TRUE
    }

    # Compare distance codes
    # Consecutive doublecount records cannot be from the same station
    if (cur_r$telpost == next_r$telpost) {
      suspect <- TRUE
    }

    overlap <- overlapping_zones[[cur_r$telpost]][[cur_r$location]]
    if (!next_r$location %in% overlap) {
      suspect <- TRUE
    }

    # Store results
    if (suspect) {
      suspect_dc_records <- c(suspect_dc_records, cur_idx)
      j <- j + 1
    } else {
      # Pair is valid, skip next
      j <- j + 2
    }
  }

  # Check if records contain protocol species or protocol codes
  codes <- c("START", "END", "SHOT", "BREAK", "RESUME")
  nonprotocol_species <- !(data$speciesname %in% names(expected_combinations)) &
    !(data$speciesname %in% codes)
  nonprotocol_species_records <- which(nonprotocol_species)

  # Migtype records in groups > 1
  many_migtype <- !is.na(data$migtype) & (data$count > 1)
  suspect_migtype_records <- which(many_migtype)

  # Obligatory columns have info
  oblig_cols <- c("datetime", "telpost", "speciesname", "count", "location")
  gap_records <- which(apply(is.na(data[, oblig_cols, drop = FALSE]), 1, any))

  # Location == >E3
  suspect_location_records <- which(data$location == ">E3")

  # Morphs other than for BootedE / EleonoraF
  nonstandard_morph <- !(data$speciesname %in% c("BootedE", "EleonoraF")) &
    (data$plumage %in% c("D", "L"))
  suspect_morphs <- which(nonstandard_morph)

  # Missing timestamps (were set to 00:00:00)
  timestamps <- format(data$datetime, "%H:%M:%S")
  missing_timestamps <- which(
    timestamps == "00:00:00" | timestamps == "23:59:59"
  )

  # HB aged vs total checks (within time window, by station/location)
  HB_codes <- c("HB", "HB_NONJUV", "HB_JUV")
  HBs <- data[data$speciesname %in% HB_codes, , drop = FALSE]
  count_age_mismatch_records_hb <- integer(0)

  if (nrow(HBs) > 0) {
    for (i in seq_len(nrow(HBs))) {
      row <- HBs[i, ]
      if (row$speciesname == "HB") {
        next
      }

      # Shuamta focus window exception
      if (
        row$telpost == "2. Shuamta" &&
          !row$counttype == "S" &&
          row$datetime >= season_params["hb_focus_start"] &&
          row$datetime <= season_params["hb_focus_end"]
      ) {
        next
      }

      window_start <- row$datetime - lubridate::dminutes(window_minutes)
      window_end <- row$datetime + lubridate::dminutes(window_minutes)

      in_window <- (HBs$datetime >= window_start) &
        (HBs$datetime <= window_end) &
        (HBs$location == row$location) &
        (HBs$telpost == row$telpost)
      HBs_window <- HBs[in_window, , drop = FALSE]

      total_HB <- sum(
        HBs_window$count[HBs_window$speciesname == "HB"],
        na.rm = TRUE
      )
      total_HB_NONJUV <- sum(
        HBs_window$count[HBs_window$speciesname == "HB_NONJUV"],
        na.rm = TRUE
      )
      total_HB_JUV <- sum(
        HBs_window$count[HBs_window$speciesname == "HB_JUV"],
        na.rm = TRUE
      )

      if (total_HB_NONJUV + total_HB_JUV > total_HB) {
        # record index in original data
        orig_idx <- which(data$speciesname %in% HB_codes)[i]
        count_age_mismatch_records_hb <- c(
          count_age_mismatch_records_hb,
          orig_idx
        )
      }
    }
  }

  # BK aged vs total checks
  BK_codes <- c("BK", "BK_NONJUV", "BK_JUV")
  BKs <- data[data$speciesname %in% BK_codes, , drop = FALSE]
  count_age_mismatch_records_bk <- integer(0)

  if (nrow(BKs) > 0) {
    for (i in seq_len(nrow(BKs))) {
      row <- BKs[i, ]
      if (row$speciesname == "BK") {
        next
      }

      window_start <- row$datetime - lubridate::dminutes(window_minutes)
      window_end <- row$datetime + lubridate::dminutes(window_minutes)

      in_window <- (BKs$datetime >= window_start) &
        (BKs$datetime <= window_end) &
        (BKs$location == row$location) &
        (BKs$telpost == row$telpost)
      BKs_window <- BKs[in_window, , drop = FALSE]

      total_BK <- sum(
        BKs_window$count[BKs_window$speciesname == "BK"],
        na.rm = TRUE
      )
      total_BK_NONJUV <- sum(
        BKs_window$count[BKs_window$speciesname == "BK_NONJUV"],
        na.rm = TRUE
      )
      total_BK_JUV <- sum(
        BKs_window$count[BKs_window$speciesname == "BK_JUV"],
        na.rm = TRUE
      )

      if (total_BK_NONJUV + total_BK_JUV > total_BK) {
        orig_idx <- which(data$speciesname %in% BK_codes)[i]
        count_age_mismatch_records_bk <- c(
          count_age_mismatch_records_bk,
          orig_idx
        )
      }
    }
  }

  # Non-singlecounted HB at station 2 within HB focus period
  non_singlecount_hb <- which(
    data$speciesname == "HB" &
      data$counttype != "S" &
      data$datetime >= season_params["hb_focus_start"] &
      data$datetime <= season_params["hb_focus_end"] &
      data$telpost == "2. Shuamta"
  )

  # Ageing outside permitted distances
  ageing_outside_permitted_distances <- which(
    data$speciesname %in%
      c("HB_JUV", "HB_NONJUV", "BK_JUV", "BK_NONJUV") &
      data$location %in% c("W3", "W2", "E2", "E3", ">E3")
  )

  # -- Expected age/sex combinations -------------------------------------------
  unexpected_age_records <- integer(0)
  unexpected_sex_records <- integer(0)
  unexpected_combination_records <- integer(0)

  paired_species <- c("MonPalHen", "Mon", "Pal", "Hen", "Marsh", "Pernis_SPEC")

  for (sp in names(expected_combinations)) {
    details <- expected_combinations[[sp]]
    species_records_idx <- which(data$speciesname == sp)
    if (length(species_records_idx) == 0) {
      next
    }
    species_records <- data[species_records_idx, , drop = FALSE]

    if (sp %in% paired_species) {
      expected_pairs <- details
      idx_ok <- integer(0)
      for (pair in expected_pairs) {
        age_ok <- if (is.na(pair[[1]])) {
          is.na(species_records$age)
        } else {
          species_records$age == pair[[1]]
        }
        sex_ok <- if (is.na(pair[[2]])) {
          is.na(species_records$sex)
        } else {
          species_records$sex == pair[[2]]
        }
        ok <- which(age_ok & sex_ok)
        idx_ok <- union(idx_ok, ok)
      }
      idx_all <- seq_len(nrow(species_records))
      unexpected_local <- setdiff(idx_all, idx_ok)
      if (length(unexpected_local) > 0) {
        unexpected_combination_records <- c(
          unexpected_combination_records,
          species_records_idx[unexpected_local]
        )
      }
      next
    }

    # Age rules
    if (is.null(details$age)) {
      unexpected_age <- !is.na(species_records$age)
    } else {
      allowed_age <- details$age
      unexpected_age <- !(species_records$age %in_na_ok% allowed_age)
    }
    unexpected_age_records <- c(
      unexpected_age_records,
      species_records_idx[which(unexpected_age)]
    )

    # Sex rules
    if (is.null(details$sex)) {
      unexpected_sex <- !is.na(species_records$sex)
    } else {
      allowed_sex <- details$sex
      unexpected_sex <- !(species_records$sex %in_na_ok% allowed_sex)
    }
    unexpected_sex_records <- c(
      unexpected_sex_records,
      species_records_idx[which(unexpected_sex)]
    )
  }

  # Unreliable ageing (complex mask)
  unreliable_ageing <- (data$location %in% c("W3", "E3", ">E3")) &
    !is.na(data$age) &
    !(data$speciesname %in%
      c("Mon", "Pal", "Hen", "Marsh", "MonPalHen") &
      data$age == "Non-Juv" &
      data$sex %in% c("M", "F")) &
    !((data$speciesname == "MonPalHen") & data$age == "J") &
    !(data$speciesname %in%
      c("Large EAGLE", "LesserSE", "GreaterSE", "SteppeE") &
      data$age %in% c("J", "Non-Juv"))
  unreliable_ageing_records <- which(unreliable_ageing)

  # Female Pallid Harrier with detailed age ---------------------------------
  unreliable_female_pallid <- (data$speciesname == "Pal") &
    (data$age %in% c("I", "A")) &
    (data$sex == "F")
  unreliable_female_pallid_records <- which(unreliable_female_pallid)

  # Add 'check' column
  data$check <- ""

  add_flag <- function(idx, msg) {
    if (length(idx) > 0) {
      data$check[idx] <<- paste0(data$check[idx], msg, ", ")
    }
  }

  add_flag(unexpected_age_records, "unexpected age")
  add_flag(unexpected_sex_records, "unexpected sex")
  add_flag(unexpected_combination_records, "unexpected species+age+sex combination")
  add_flag(ageing_outside_permitted_distances, "ageing distance")
  add_flag(non_singlecount_hb, "singlecount missing? (leave as is)")
  add_flag(
    count_age_mismatch_records_hb,
    "mismatch number of counted and aged birds"
  )
  add_flag(
    count_age_mismatch_records_bk,
    "mismatch number of counted and aged birds"
  )
  add_flag(suspect_morphs, "unexpected morph")
  add_flag(missing_timestamps, "incorrect timestamp")
  add_flag(suspect_location_records, "unusual location")
  add_flag(gap_records, "gaps in essential columns")
  add_flag(suspect_dc_records, "erroneous doublecount (leave as is)")
  add_flag(suspect_migtype_records, "unusual nr of killed/injured birds")
  add_flag(unreliable_ageing_records, "doubtful ageing")
  add_flag(unreliable_female_pallid_records, "doubtful ageing")

  # Override flag in case of non-protocol species
  if (length(nonprotocol_species_records) > 0) {
    data$check[nonprotocol_species_records] <- "non-protocol species, "
  }

  # trim trailing ", "
  data$check <- sub(",\\s*$", "", data$check)

  data
}

`%in_na_ok%` <- function(x, allowed) {
  # returns TRUE if x is allowed (including NA handling), FALSE otherwise
  out <- rep(FALSE, length(x))
  allow_na <- any(is.na(allowed))
  out[!is.na(x)] <- x[!is.na(x)] %in% allowed
  out[is.na(x)] <- allow_na
  out
}

#' Add BREAK/RESUME records for multi-count days
#'
#' @param data Data from trektellen
#' @param date_Date Date object
#' @param timestamps hms timestamps for BREAK/RESUME records
#' @param telpost Telpost, either 1047 or 1048
#'
#' @returns Data with added BREAK/RESUME records
add_breakresume <- function(data, date_Date, timestamps, telpost) {
  times_br <- data.frame(
    date = rep(date_Date, length(timestamps)),
    timestamp = hms::as_hms(unlist(timestamps)),
    telpost = rep(telpost, length(timestamps)),
    speciesname = rep(c("BREAK", "RESUME"), length(timestamps) / 2),
    location = rep("O", length(timestamps)),
    count = 1
  )
  data <- dplyr::bind_rows(data, times_br)
  return(data)
}
