# Regression tests for the count-page date guard. Trektellen redirects a date with
# NO count to the station's most recent count and serves it with a 200, so the
# scraper must read the page's OWN date (from its <h1> heading) and reject a page
# whose date does not match the one requested.

page_with_heading <- function(heading) {
  xml2::read_html(sprintf(
    "<html><body><h1 class='left'><a href='/site/info/1048'>Batumi - Shuamta</a><br/>%s</h1></body></html>",
    heading
  ))
}

test_that("count_page_date reads the count date from the page heading", {
  expect_equal(
    count_page_date(page_with_heading("Tuesday 21 October 2025")),
    "2025-10-21"
  )
})

test_that("count_page_date zero-pads single-digit days and months", {
  expect_equal(
    count_page_date(page_with_heading("Sunday 7 September 2025")),
    "2025-09-07"
  )
})

test_that("count_page_date does not depend on the session locale", {
  withr::with_locale(c(LC_TIME = "C"), {
    expect_equal(
      count_page_date(page_with_heading("Tuesday 21 October 2025")),
      "2025-10-21"
    )
  })
})

test_that("count_page_date returns NA when the heading carries no date", {
  expect_true(is.na(count_page_date(page_with_heading("Batumi - Shuamta"))))
})

test_that("count_page_date returns NA when there is no heading at all", {
  page <- xml2::read_html("<html><body><p>no heading here</p></body></html>")
  expect_true(is.na(count_page_date(page)))
})
