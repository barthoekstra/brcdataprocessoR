solar_times_for_dates <- function(
  dates,
  lat = 41.690259,
  lon = 41.729722,
  timezone = "Etc/GMT-4"
) {
  if (!requireNamespace("suncalc", quietly = TRUE)) {
    stop(
      "Package `suncalc` is required to calculate solar times.",
      call. = FALSE
    )
  }
  dates <- as.Date(dates)
  dates <- dates[!is.na(dates)]
  if (!length(dates)) {
    stop("`dates` must contain at least one valid date.", call. = FALSE)
  }
  if (
    length(lat) != 1L ||
      is.na(lat) ||
      !is.finite(lat) ||
      lat < -90 ||
      lat > 90 ||
      length(lon) != 1L ||
      is.na(lon) ||
      !is.finite(lon) ||
      lon < -180 ||
      lon > 180
  ) {
    stop(
      "`lat` and `lon` must be valid decimal-degree coordinates.",
      call. = FALSE
    )
  }
  if (
    !is.character(timezone) ||
      length(timezone) != 1L ||
      is.na(timezone) ||
      !nzchar(timezone)
  ) {
    stop("`timezone` must be a non-empty string.", call. = FALSE)
  }
  dates <- sort(unique(dates))
  sun <- suncalc::getSunlightTimes(
    date = dates,
    lat = lat,
    lon = lon,
    keep = c("sunrise", "sunset"),
    tz = timezone
  )
  structure(
    data.frame(
      date = format(dates, "%Y-%m-%d"),
      sunrise = format(sun$sunrise, "%H:%M:%S", tz = timezone),
      sunset = format(sun$sunset, "%H:%M:%S", tz = timezone),
      stringsAsFactors = FALSE
    ),
    class = c("brc_solar_times", "data.frame"),
    solar_metadata = list(
      timezone = timezone,
      latitude = lat,
      longitude = lon,
      solar_library_version = as.character(utils::packageVersion("suncalc"))
    )
  )
}

#' Render protocol sunrise and sunset times as a LaTeX table
#'
#' Calculates sunrise and sunset with `suncalc` for every date in the count
#' period returned by [get_season_params()], rounds the times to the nearest
#' minute, and arranges them in the two-panel layout used by the protocol.
#'
#' @param year A single whole-number protocol year.
#' @param lat Latitude in decimal degrees. Defaults to the operational Batumi
#'   count-area coordinate.
#' @param lon Longitude in decimal degrees. Defaults to the operational Batumi
#'   count-area coordinate.
#' @param timezone Timezone used for the displayed wall-clock times.
#' @param caption Plain-text table caption. `NULL` uses a year-specific caption;
#'   LaTeX-special characters are escaped.
#' @param label LaTeX label without surrounding braces. May contain letters,
#'   numbers, colons, periods, underscores, and hyphens.
#' @return A length-one character vector containing a complete LaTeX `table`
#'   environment.
#' @examples
#' cat(brc_sunrise_sunset_latex(2025))
#' @export
brc_sunrise_sunset_latex <- function(
  year,
  lat = 41.690259,
  lon = 41.729722,
  timezone = "Etc/GMT-4",
  caption = NULL,
  label = "tab:sunrise-sunset"
) {
  if (!is.numeric(year) || length(year) != 1L || is.na(year)) {
    stop("`year` must be a single year.", call. = FALSE)
  }
  if (!is.finite(year) || year != floor(year)) {
    stop("`year` must be a whole-number year.", call. = FALSE)
  }
  if (year < 1000 || year > 9999) {
    stop("`year` must be between 1000 and 9999.", call. = FALSE)
  }
  year <- as.integer(year)
  if (is.null(caption)) {
    caption <- paste0(
      "Sunrise and sunset times during the count period in ",
      year,
      "."
    )
  }
  validate_latex_text(caption, "caption")
  validate_latex_label(label)
  caption <- escape_latex_text(caption)

  season <- get_season_params(year)
  dates <- seq(
    as.Date(season[["season_start"]]),
    as.Date(season[["season_end"]]),
    by = "day"
  )
  solar <- solar_times_for_dates(
    dates,
    lat = lat,
    lon = lon,
    timezone = timezone
  )
  if (anyNA(solar[c("sunrise", "sunset")])) {
    stop(
      "Sunrise or sunset is unavailable for at least one count date.",
      call. = FALSE
    )
  }
  display <- data.frame(
    date = format(as.Date(solar$date), "%d/%m/%Y"),
    sunrise = solar_time_to_minute(solar$sunrise),
    sunset = solar_time_to_minute(solar$sunset),
    stringsAsFactors = FALSE
  )

  right_count <- ceiling(nrow(display) / 2)
  left_count <- nrow(display) - right_count
  left <- display[seq_len(left_count), , drop = FALSE]
  right <- display[left_count + seq_len(right_count), , drop = FALSE]
  if (left_count < right_count) {
    left <- rbind(
      data.frame(date = "", sunrise = "", sunset = ""),
      left
    )
  }
  rows <- vapply(
    seq_len(right_count),
    function(index) {
      paste0(
        left$date[[index]], " & ",
        left$sunrise[[index]], " & ",
        left$sunset[[index]], " &  & ",
        right$date[[index]], " & ",
        right$sunrise[[index]], " & ",
        right$sunset[[index]], " \\\\"
      )
    },
    character(1L)
  )

  paste(
    c(
      "\\begin{table}[H]",
      paste0("\\caption{", caption, "}"),
      paste0("\\label{", label, "}"),
      "\\begin{tabularx}{\\linewidth}{",
      "  >{\\raggedright\\arraybackslash}p{0.10\\linewidth}",
      "  >{\\raggedleft\\arraybackslash}p{0.10\\linewidth}",
      "  >{\\raggedleft\\arraybackslash}p{0.10\\linewidth}",
      "  >{\\raggedright\\arraybackslash}X",
      "  >{\\raggedright\\arraybackslash}p{0.10\\linewidth}",
      "  >{\\raggedleft\\arraybackslash}p{0.10\\linewidth}",
      "  >{\\raggedleft\\arraybackslash}p{0.10\\linewidth}",
      "}",
      "\\rowcolor{white}",
      "\\toprule",
      paste0(
        "\\textbf{Date} & \\textbf{Sunrise} & \\textbf{Sunset} & & ",
        "\\textbf{Date} & \\textbf{Sunrise} & \\textbf{Sunset} \\\\"
      ),
      "\\midrule",
      rows,
      "\\bottomrule",
      "\\end{tabularx}",
      "\\end{table}"
    ),
    collapse = "\n"
  )
}

validate_latex_text <- function(value, argument) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(value)
  ) {
    stop("`", argument, "` must be a non-empty string.", call. = FALSE)
  }
  invisible(value)
}

validate_latex_label <- function(label) {
  validate_latex_text(label, "label")
  if (!grepl("^[A-Za-z0-9:._-]+$", label)) {
    stop(
      "`label` must be a valid LaTeX label containing only letters, ",
      "numbers, colons, periods, underscores, and hyphens.",
      call. = FALSE
    )
  }
  invisible(label)
}

escape_latex_text <- function(value) {
  characters <- strsplit(value, "", fixed = TRUE)[[1L]]
  escaped <- vapply(
    characters,
    function(character) {
      switch(
        character,
        "\\" = "\\textbackslash{}",
        "{" = "\\{",
        "}" = "\\}",
        "$" = "\\$",
        "&" = "\\&",
        "#" = "\\#",
        "%" = "\\%",
        "_" = "\\_",
        "~" = "\\textasciitilde{}",
        "^" = "\\textasciicircum{}",
        character
      )
    },
    character(1L)
  )
  paste0(escaped, collapse = "")
}

solar_time_to_minute <- function(time) {
  fields <- strsplit(time, ":", fixed = TRUE)
  seconds <- vapply(
    fields,
    function(value) {
      as.integer(value[[1L]]) * 3600L +
        as.integer(value[[2L]]) * 60L +
        as.integer(value[[3L]])
    },
    integer(1L)
  )
  rounded_minutes <- floor((seconds + 30) / 60)
  paste0(
    rounded_minutes %/% 60 %% 24,
    ":",
    sprintf("%02d", rounded_minutes %% 60)
  )
}
