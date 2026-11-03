header_page <- function(period, metadata) {
  xml2::read_html(paste0(
    "<h1 class='left'>Batumi<br>Saturday 10 October 2026</h1>",
    "<div id='left_content'><table><tr><td>Counting period: ", period,
    "<br>Count type: Storks and raptors<br>", metadata,
    "<table><tr><td>Totals: 2420 individuals, 50 species, 20:42 hours</td></tr></table>",
    "Comments: World Migratory Bird Day</td></tr></table></div>"
  ))
}

parse_header <- function(period, metadata) {
  parse_count_header(header_page(period, metadata), 1047L, "2026-10-10")
}

test_that("multiple uploads retain documented first-period metadata", {
  h <- parse_header(
    "07:04 - 17:02 & 07:11 - 17:55",
    paste0("Weather (first period): Sunny<br>",
           "Observers (first period): Mitra, Valentin<br>",
           "Weather (second period): <br>Observers (second period): <br>")
  )
  expect_equal(h$weather, "Sunny")
  expect_equal(h$observers, c("Mitra", "Valentin"))
  expect_equal(h$counting_period$start, "07:04")
  expect_equal(h$counting_period$end, "17:55")
  expect_equal(h$remark, "World Migratory Bird Day")
})

test_that("metadata from both periods is retained", {
  h <- parse_header(
    "07:04 - 17:02 & 07:11 - 17:55",
    paste0("Weather (first period): Sunny<br>Weather (second period): Cloudy<br>",
           "Observers (first period): Mitra<br>Observers (second period): Valentin<br>")
  )
  expect_equal(h$weather, "Sunny, Cloudy")
  expect_equal(h$observers, c("Mitra", "Valentin"))
})

test_that("ordinary headers and missing metadata keep their existing meaning", {
  h <- parse_header("07:04 - 17:02", "Weather: Sunny<br>Observers: Mitra<br>")
  expect_equal(h$weather, "Sunny")
  expect_equal(h$observers, "Mitra")
  h <- parse_header("07:04 - 17:02 & 07:11 - 17:55", "")
  expect_true(is.na(h$weather))
  expect_length(h$observers, 0)
})

test_that("overlapping uploads are merged before deriving breaks", {
  h <- parse_header("07:04 - 17:02 & 07:11 - 17:55", "")
  expect_equal(h$counting_period, list(start = "07:04", end = "17:55", breakresumes = character()))
  h <- parse_header("10:00 - 11:00 & 07:00 - 12:00 & 13:00 - 18:00", "")
  expect_equal(h$counting_period, list(start = "07:00", end = "18:00", breakresumes = c("12:00", "13:00")))
  h <- parse_header("07:00 - 12:00 & 12:00 - 18:00", "")
  expect_length(h$counting_period$breakresumes, 0L)
})

test_that("malformed counting periods cannot become effort records", {
  for (period in c("07:00", "07:00 - 12:00 & 13:00", "18:00 - 07:00", "25:00 - 26:00", "unknown")) {
    h <- parse_header(period, "")
    expect_true(is.na(h$counting_period$start), info = period)
    expect_true(is.na(h$counting_period$end), info = period)
    expect_length(h$counting_period$breakresumes, 0L)
  }
})
