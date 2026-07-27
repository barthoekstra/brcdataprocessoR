#' Build the operational solar-time policy
#'
#' Calculates date-specific sunrise and sunset times using the same coordinates,
#' timezone, and `suncalc` call as the established BRC processing pipeline.
#'
#' @param dates Dates or values coercible to `Date`.
#' @param lat Latitude in decimal degrees. Defaults to the operational Batumi
#'   count-area coordinate.
#' @param lon Longitude in decimal degrees. Defaults to the operational Batumi
#'   count-area coordinate.
#' @param timezone Timezone used for the returned wall-clock times.
#' @param id Identifier for the resulting time policy.
#' @return A validated time-policy list suitable for `brc_process()` or
#'   `brc_assign_protocol_filter()`.
#' @export
brc_solar_time_policy <- function(
  dates,
  lat = 41.690259,
  lon = 41.729722,
  timezone = "Etc/GMT-4",
  id = "operational-solar-suncalc-v1"
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
    list(
      id = id,
      timezone = timezone,
      latitude = lat,
      longitude = lon,
      solar_library_version = as.character(utils::packageVersion("suncalc")),
      solar_times = data.frame(
        date = format(dates, "%Y-%m-%d"),
        sunrise = format(sun$sunrise, "%H:%M:%S", tz = timezone),
        sunset = format(sun$sunset, "%H:%M:%S", tz = timezone),
        stringsAsFactors = FALSE
      )
    ),
    class = "brc_time_policy"
  )
}
