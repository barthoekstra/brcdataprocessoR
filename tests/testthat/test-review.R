review_row <- function(datetime = "2025-10-01 12:00:00", station = "1. Sakhalvasho",
                       species = "HB", counttype = "D", location = "E3") {
  data.frame(
    datetime = as.POSIXct(datetime, tz = "Etc/GMT-4"), telpost = station,
    speciesname = species, count = 1, countback = 0, local = 0,
    age = NA_character_, sex = NA_character_, plumage = NA_character_,
    remark = NA_character_, location = location, migtype = NA_character_,
    counttype = counttype
  )
}

test_that("focus rules include all of the last day", {
  rows <- rbind(
    review_row("2025-09-08 12:00:00", "2. Shuamta", counttype = "", location = "E1"),
    review_row("2025-09-09 12:00:00", "2. Shuamta", counttype = "", location = "E1"),
    review_row("2025-09-10 12:00:00", "2. Shuamta", counttype = "", location = "E1")
  )
  flags <- check_trektellen_data(rows)$check
  expect_match(flags[1:2], "singlecount missing", all = TRUE)
  expect_identical(flags[3], "")

  rows$speciesname <- "HB_NONJUV"
  flags <- check_trektellen_data(rows)$check
  expect_identical(flags[1:2], c("", ""))
  expect_match(flags[3], "mismatch number")
})

test_that("incomplete records are retained and flagged without crashing comparisons", {
  pair <- rbind(review_row(), review_row(station = "2. Shuamta", location = "W3"))
  for (column in c("datetime", "telpost", "speciesname", "count", "location")) {
    rows <- pair
    rows[[column]][1] <- NA
    result <- check_trektellen_data(rows)
    expect_equal(nrow(result), 2L)
    expect_match(result$check[1L], "gaps in essential columns", info = column)
  }
  pair$datetime[] <- NA
  expect_match(check_trektellen_data(pair)$check, "gaps in essential columns", all = TRUE)
})

test_that("unmatched double counts are flagged but complete pairs pass", {
  single <- review_row()
  expect_match(check_trektellen_data(single)$check, "erroneous doublecount")
  pair <- rbind(single, review_row(station = "2. Shuamta", location = "W3"))
  expect_identical(check_trektellen_data(pair)$check, c("", ""))
  flags <- check_trektellen_data(rbind(pair, single))$check
  expect_identical(flags[1:2], c("", ""))
  expect_match(flags[3], "erroneous doublecount")
})
