#' Extract count period from the Trektellen website
#'
#' A thin wrapper over [extract_count_header()] - they read the same count page,
#' so this reuses it and returns just the counting period, with the times parsed
#' to [hms::hms()] objects as the preprocessing pipeline expects.
#'
#' @param station Station ID (1047/1048) or display name ("1. Sakhalvasho" or
#'   "2. Shuamta").
#' @param date_str Date string ("YYYY-MM-DD").
#'
#' @returns NULL if the page/period could not be read, otherwise a list with
#'   `start` and `end` (hms) and `breakresumes` (a list of hms, or NULL).
#' @export
extract_count_period <- function(station, date_str) {
  header <- extract_count_header(station, date_str)
  cp <- if (is.null(header)) NULL else header$counting_period
  if (is.null(cp) || is.na(cp$start) || is.na(cp$end)) {
    message("Fetching count times from Trektellen failed, returning NULL")
    return(NULL)
  }
  list(
    start = hms::parse_hm(cp$start),
    end = hms::parse_hm(cp$end),
    breakresumes = if (length(cp$breakresumes)) {
      lapply(cp$breakresumes, hms::parse_hm)
    } else {
      NULL
    }
  )
}

#' Resolve a station argument to its Trektellen station ID
#'
#' @param station Station ID (1047 or 1048) or display name ("1. Sakhalvasho" or
#'   "2. Shuamta").
#'
#' @returns Integer Trektellen station ID.
#' @keywords internal
station_to_id <- function(station) {
  ids <- c("1. Sakhalvasho" = 1047L, "2. Shuamta" = 1048L)
  if (is.numeric(station)) {
    if (!station %in% c(1047, 1048)) stop("Station ID should be 1047 or 1048")
    return(as.integer(station))
  }
  if (is.character(station) && station %in% names(ids)) {
    return(unname(ids[[station]]))
  }
  stop("station must be an ID (1047/1048) or a name ",
       "(\"1. Sakhalvasho\" or \"2. Shuamta\")")
}

#' Extract count header metadata from the Trektellen count page
#'
#' Scrapes the per-count header block shown on a Trektellen `count/view` page
#' (the same page [extract_count_period()] reads for count times) and returns
#' it as count *metadata*: the counting period, count type, weather, the
#' observers present (the BRC "counters", coordinators among them), an optional
#' remark, and the page's daily totals (individuals, species, hours counted).
#' This is not part of the checked record data set — it is context about the
#' count, useful for count metadata and for seeing who was on station.
#'
#' Every field renders on the page as a `"Label: value"` line, so parsing is a
#' simple lookup over the page text - deliberately minimal and easy to eyeball.
#'
#' @param station Station ID (1047 or 1048) or display name ("1. Sakhalvasho" or
#'   "2. Shuamta").
#' @param date_str Count date as "YYYY-MM-DD" (dashes are stripped for the URL).
#'
#' @returns `NULL` if the page cannot be fetched, otherwise a named list:
#'   `station_id`, `date`, `counting_period` (`start`/`end` "HH:MM" plus
#'   `breakresumes`), `count_type`, `weather`, `observers` (character vector),
#'   `remark`, and `totals` (`individuals`, `species`, `hours`). Absent fields
#'   are `NA` (or an empty vector).
#' @export
#'
#' @examples
#' \dontrun{
#' extract_count_header(1047, "2025-09-07")
#' }
extract_count_header <- function(station, date_str) {
  station_id <- station_to_id(station)
  tt_url <- sprintf(
    "https://trektellen.org/count/view/%d/%s?sc=0",
    station_id,
    gsub("-", "", date_str)
  )

  page <- tryCatch(rvest::read_html(tt_url), error = function(e) NULL)
  if (is.null(page)) {
    message("Fetching count header from Trektellen failed, returning NULL")
    return(NULL)
  }

  # The header renders each field on its own line as "Label: value".
  lines <- trimws(strsplit(rvest::html_text2(page), "\n")[[1]])
  field <- function(label) {
    hit <- lines[startsWith(lines, paste0(label, ":"))]
    if (length(hit) == 0) return(NA_character_)
    trimws(sub(sprintf("^%s:\\s*", label), "", hit[[1]]))
  }
  # Pull the first capture group of `pattern` out of `x` (NA-safe).
  grab <- function(x, pattern) {
    if (is.na(x)) return(NA_character_)
    m <- regmatches(x, regexec(pattern, x))[[1]]
    if (length(m) >= 2) m[[2]] else NA_character_
  }

  # "Counting period" is "06:35 - 19:01", or "&"-joined pairs when the count
  # paused; split on both separators, keeping break/resume times in the middle.
  period_raw <- field("Counting period")
  times <- if (is.na(period_raw)) {
    character(0)
  } else {
    trimws(strsplit(period_raw, "[-&]")[[1]])
  }
  times <- times[nzchar(times)]
  n <- length(times)
  counting_period <- list(
    start = if (n >= 1) times[[1]] else NA_character_,
    end = if (n >= 1) times[[n]] else NA_character_,
    breakresumes = if (n > 2) times[2:(n - 1)] else character(0)
  )

  observers_raw <- field("Observers")
  observers <- if (is.na(observers_raw)) {
    character(0)
  } else {
    trimws(strsplit(observers_raw, ",")[[1]])
  }

  totals_raw <- field("Totals")  # "7259 individuals, 15 species, 12:26 hours"

  # The remark is free text at the bottom of the header cell, after the species
  # table (labelled "Comments:" on the page, and often multi-line). Grab the
  # header cell's text nodes that follow that table so the whole remark is kept.
  td <- rvest::html_element(page, "#left_content table td")
  remark <- NA_character_
  if (!is.na(td)) {
    xp <- "./text()[preceding-sibling::table]"
    parts <- trimws(rvest::html_text(rvest::html_elements(td, xpath = xp)))
    parts <- parts[nzchar(parts)]
    if (length(parts)) {
      remark <- trimws(sub("^(Comments|Remarks?|Notes?):\\s*", "",
                           paste(parts, collapse = "\n")))
    }
  }
  if (!is.na(remark) && !nzchar(remark)) remark <- NA_character_

  list(
    station_id = station_id,
    date = date_str,
    counting_period = counting_period,
    count_type = field("Count type"),
    weather = field("Weather"),
    observers = observers,
    remark = remark,
    totals = list(
      individuals = as.integer(grab(totals_raw, "([0-9]+) individuals")),
      species = as.integer(grab(totals_raw, "([0-9]+) species")),
      hours = grab(totals_raw, "([0-9:]+) hours")
    )
  )
}
