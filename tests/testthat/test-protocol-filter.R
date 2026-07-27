protocol_row <- function(
  date,
  time,
  species = "HB",
  location = "E1",
  age = "",
  id = 1L,
  number = 1L
) {
  data.frame(
    id = id,
    date = date,
    time = time,
    species = species,
    number = number,
    north = 0L,
    station = 1L,
    location = location,
    age = age,
    sex = "",
    morph = "",
    health = "",
    remark = "",
    dcremark = "",
    stringsAsFactors = FALSE
  )
}

solar_clock <- function(date, event = "sunrise", offset = 0) {
  solar <- brcdataprocessoR:::solar_times_for_dates(date)
  value <- as.POSIXct(
    paste(date, solar[[event]]),
    format = "%Y-%m-%d %H:%M:%S",
    tz = "Etc/GMT-4"
  )
  format(value + offset, "%H:%M:%S", tz = "Etc/GMT-4")
}

synthetic_protocol_rules <- function(version = "operational_v1") {
  structure(
    list(
      version = version,
      tables = list(
        protocol_windows = data.frame(
          window_id = c("standard", "harrier_early"),
          start_month_day = c("08-17", "08-27"),
          end_month_day = c("10-16", "09-27"),
          start_offset_seconds = c(3600L, 0L),
          end_offset_seconds = c(-7200L, 3600L),
          filter = c(1L, 2L),
          effective_from = c(2018L, 2018L),
          effective_to = c(2025L, 2025L)
        ),
        exclusions = data.frame(
          rule_id = c(
            "nonregular_raptors_since_2011",
            "outer_east",
            "age_protocol_outer"
          ),
          field = c(
            "species",
            "location",
            "species"
          ),
          values = c(
            paste(
              c(
                "SparrowH_SPEC",
                "Falcon_SPEC",
                "Goshawk",
                "Kestrel_SPEC",
                "CommonKes",
                "LesserKes",
                "Merlin",
                "SPH_Goshawk",
                "EurasianSH",
                "Hobby",
                "Hobby_RedFF",
                "LevantSH",
                "RedFF"
              ),
              collapse = "|"
            ),
            "E4|>E3",
            "BK_JUV|BK_NONJUV|HB_JUV|HB_NONJUV|SB_NONJUV|SB_JUV"
          ),
          condition = c(
            "always",
            "always",
            "location in E2|E3|W2|W3"
          ),
          effective_from = c(
            2018L,
            2018L,
            2018L
          ),
          effective_to = c(
            2025L,
            2025L,
            2025L
          ),
          action = rep("filter_zero", 3L),
          stringsAsFactors = FALSE
        )
      ),
      provenance = list(source = "synthetic-package-safe")
    ),
    class = "brc_rules"
  )
}

test_that("standard window and normalized leap yday boundaries are inclusive", {
  d1 <- "2023-08-17"
  d2 <- "2023-10-16"
  d3 <- "2024-08-17"
  x <- rbind(
    protocol_row(d1, solar_clock(d1, offset = 3600), id = 1L),
    protocol_row(
      d1,
      solar_clock(d1, event = "sunset", offset = -7200),
      id = 2L
    ),
    protocol_row(d1, solar_clock(d1, offset = 3599), id = 3L),
    protocol_row(
      d2,
      solar_clock(d2, event = "sunset", offset = -7200),
      id = 4L
    ),
    protocol_row(d3, solar_clock(d3, offset = 3600), id = 5L)
  )
  result <- brc_assign_protocol_filter(x, synthetic_protocol_rules())
  expect_equal(result$data$filter, c(1L, 1L, 0L, 1L, 1L))
  expect_equal(result$data$yday, c(229L, 229L, 229L, 289L, 229L))
})

test_that("harrier filter two uses sunrise through but not sunrise plus hour", {
  d <- "2024-08-27"
  end <- "2024-09-27"
  x <- rbind(
    protocol_row(d, solar_clock(d), "Harrier_SPEC", id = 1L),
    protocol_row(d, solar_clock(d, offset = 3600), "Mon", id = 2L),
    protocol_row(end, solar_clock(end, offset = 3600), "Pal", id = 3L),
    protocol_row(d, solar_clock(d, offset = 3601), "Hen", id = 4L),
    protocol_row(d, solar_clock(d, offset = 1800), "HB", id = 5L)
  )
  result <- brc_assign_protocol_filter(x, synthetic_protocol_rules())
  expect_equal(result$data$filter, c(2L, 1L, 1L, 1L, 0L))
})

test_that("protocol filtering uses the operational suncalc calculation", {
  skip_if_not_installed("suncalc")
  dates <- as.Date(c("2024-08-27", "2024-09-27"))
  solar <- brcdataprocessoR:::solar_times_for_dates(dates)
  expected <- suncalc::getSunlightTimes(
    date = dates,
    lat = 41.690259,
    lon = 41.729722,
    keep = c("sunrise", "sunset"),
    tz = "Etc/GMT-4"
  )
  expect_equal(solar$date, format(sort(dates), "%Y-%m-%d"))
  expect_equal(
    solar$sunrise,
    format(expected$sunrise, "%H:%M:%S", tz = "Etc/GMT-4")
  )
  expect_equal(
    solar$sunset,
    format(expected$sunset, "%H:%M:%S", tz = "Etc/GMT-4")
  )
  metadata <- attr(solar, "solar_metadata")
  expect_equal(metadata$latitude, 41.690259)
  expect_equal(metadata$longitude, 41.729722)
  expect_equal(
    metadata$solar_library_version,
    as.character(utils::packageVersion("suncalc"))
  )
})

test_that("location, age, and species exclusions come from operational rules", {
  x <- rbind(
    protocol_row("2023-08-17", "12:00:00", "HB_JUV", "E2", id = 1L),
    protocol_row("2023-08-17", "12:00:00", "HB", "E4", id = 2L),
    protocol_row("2023-08-17", "12:00:00", "StepBuz", "E1", id = 3L),
    protocol_row("2023-08-17", "12:00:00", "Goshawk", "E1", id = 4L)
  )
  result <- brc_assign_protocol_filter(x, synthetic_protocol_rules())
  expect_equal(result$data$filter, c(0L, 0L, 1L, 0L))
  expect_gte(sum(result$summary$filter_zero_by_exclusion), 3L)
})

test_that("filter diagnostics preserve exact rules and multiple zero causes", {
  x <- rbind(
    protocol_row("2023-08-17", "05:00:00", "HB", "E1", id = 1L),
    protocol_row("2023-08-17", "12:00:00", "HB", "E1", id = 2L),
    protocol_row("2023-08-17", "12:00:00", "Goshawk", "E4", id = 3L),
    protocol_row("2011-08-17", "12:00:00", "StepBuz", "E1", id = 4L),
    protocol_row("2023-08-17", "12:00:00", "HB_JUV", "E3", id = 5L)
  )
  result <- brc_assign_protocol_filter(x, synthetic_protocol_rules())
  diagnostics <- result$diagnostics
  expect_true(any(
    diagnostics$id == 1L &
      diagnostics$rule_id == "protocol.default_outside_window"
  ))
  expect_true(any(
    diagnostics$id == 2L &
      diagnostics$rule_id == "protocol.window.standard"
  ))
  row_three <- diagnostics$rule_id[diagnostics$id == 3L]
  expect_true(all(
    c(
      "outer_east",
      "nonregular_raptors_since_2011"
    ) %in%
      row_three
  ))
  expect_true(any(
    diagnostics$id == 5L &
      diagnostics$rule_id == "age_protocol_outer"
  ))
})

test_that("solar calculation is automatic and session-pure", {
  x <- protocol_row("2023-08-17", "12:00:00")
  old_tz <- Sys.getenv("TZ", unset = NA_character_)
  old_options <- options()
  brc_assign_protocol_filter(x, synthetic_protocol_rules())
  expect_identical(Sys.getenv("TZ", unset = NA_character_), old_tz)
  expect_identical(options(), old_options)
})
