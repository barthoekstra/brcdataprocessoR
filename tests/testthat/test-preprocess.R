raw_count_csv <- function(path) {
  raw <- data.frame(
    date = c("2025-09-07", "2025-09-07", "2025-09-08"),
    timestamp = "12:00:00", countid = 1:3, telpost = c(1047, 1048, 1047),
    speciesname = "HB", speciesid = 1, count = 1, countback = 0, local = 0,
    age = NA_character_, sex = NA_character_, plumage = NA_character_,
    remark = NA_character_, location = "E1", migtype = NA_character_,
    counttype = "", year = 2025, yday = c(250, 250, 251)
  )
  utils::write.csv(raw, path, row.names = FALSE, na = "")
}

test_that("undated preprocessing preserves all observations without fetching periods", {
  path <- withr::local_tempfile(fileext = ".csv")
  raw_count_csv(path)
  local_mocked_bindings(extract_count_period = function(...) stop("unexpected fetch"))

  data <- preprocess_raw_trektellen_data(path)

  expect_equal(nrow(data), 3L)
  expect_equal(as.character(as.Date(data$datetime)), c("2025-09-07", "2025-09-07", "2025-09-08"))
  expect_equal(data$speciesname, rep("HB", 3L))
})

test_that("missing station periods do not replace the other station's hours", {
  path <- withr::local_tempfile(fileext = ".csv")
  raw_count_csv(path)
  for (missing_station in c(1047, 1048)) {
    local_mocked_bindings(extract_count_period = function(station, date_str) {
      if (station == missing_station) {
        return(NULL)
      }
      list(
        start = hms::parse_hm("07:00"), end = hms::parse_hm("18:00"),
        breakresumes = list(hms::parse_hm("12:30"), hms::parse_hm("13:00"))
      )
    })
    expect_warning(
      data <- preprocess_raw_trektellen_data(path, "2025-09-07"),
      paste0("", missing_station, ".*unavailable")
    )
    known_name <- if (missing_station == 1047) "2. Shuamta" else "1. Sakhalvasho"
    known <- data[data$telpost == known_name & data$speciesname != "HB", ]
    expect_equal(known$speciesname, c("START", "BREAK", "RESUME", "END"))
    expect_equal(format(known$datetime, "%H:%M:%S"), c("07:00:00", "12:30:00", "13:00:00", "18:00:00"))
    missing <- data[data$telpost != known_name & data$speciesname != "HB", ]
    expect_equal(format(missing$datetime, "%H:%M:%S"), c("00:00:00", "23:59:59"))
  }
})

test_that("an absent station does not acquire artificial count records", {
  path <- withr::local_tempfile(fileext = ".csv")
  raw_count_csv(path)
  local_mocked_bindings(extract_count_period = function(station, date_str) {
    if (station == 1048) {
      return(NULL)
    }
    list(start = hms::parse_hm("07:00"), end = hms::parse_hm("18:00"), breakresumes = NULL)
  })

  data <- preprocess_raw_trektellen_data(path, "2025-09-08")

  expect_equal(data$speciesname, c("START", "HB", "END"))
  expect_equal(unique(data$telpost), "1. Sakhalvasho")
})
