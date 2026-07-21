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

#' Read a Trektellen count page's own count date
#'
#' The `count/view` page shows the date it belongs to in its `<h1>` heading
#' (e.g. "Batumi - Shuamta\nTuesday 21 October 2025"). Parsing it lets a caller
#' verify the page it got back is actually for the date it asked for - Trektellen
#' redirects a date with NO count to the station's most recent count, so a naive
#' scrape of `.../20260721` would otherwise return the header of a stale, unrelated
#' count. Month names are mapped explicitly so this does not depend on the R
#' session locale.
#'
#' @param page An `xml_document` parsed from a count-view page.
#'
#' @returns The page's count date as "YYYY-MM-DD", or `NA_character_` if no date
#'   could be read from the heading.
#' @keywords internal
count_page_date <- function(page) {
  h1 <- rvest::html_element(page, "h1.left")
  if (length(h1) == 0 || is.na(h1)) return(NA_character_)
  txt <- rvest::html_text2(h1)
  m <- regmatches(txt, regexec("([0-9]{1,2})\\s+([A-Za-z]+)\\s+([0-9]{4})", txt))[[1]]
  if (length(m) < 4) return(NA_character_)
  months <- c(january = 1L, february = 2L, march = 3L, april = 4L, may = 5L,
              june = 6L, july = 7L, august = 8L, september = 9L,
              october = 10L, november = 11L, december = 12L)
  mo <- months[[tolower(m[[3]])]]
  if (is.null(mo)) return(NA_character_)
  sprintf("%04d-%02d-%02d", as.integer(m[[4]]), mo, as.integer(m[[2]]))
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

  # Trektellen redirects a date with NO count to the station's most recent count
  # (e.g. .../1048/20260721 -> .../1048/20251021), serving that stale page with a
  # 200. Guard against scraping it as if it were `date_str`: the page names its own
  # count date in the heading, so require that to match. A mismatch (or an
  # unreadable heading) means there is no count on this date - an ABSENCE, which the
  # caller must not mistake for a zero count.
  page_date <- count_page_date(page)
  if (is.na(page_date) || !identical(page_date, date_str)) {
    message("Trektellen has no count for ", date_str,
            " (page is ", if (is.na(page_date)) "undated" else page_date,
            "), returning NULL")
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
