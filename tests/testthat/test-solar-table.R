test_that("solar display rounds seconds and wraps midnight", {
  expect_identical(
    solar_time_to_minute(c("06:20:29", "06:20:30", "23:59:30", "00:00:00")),
    c("6:20", "6:21", "0:00", "0:00")
  )
  expect_identical(solar_time_to_minute(character()), character())
})

test_that("protocol solar table reproduces the requested LaTeX layout", {
  table <- brc_sunrise_sunset_latex(2025)

  expect_length(table, 1L)
  expect_match(
    table,
    "\\\\caption\\{Sunrise and sunset times during the count period in 2025\\.\\}",
    perl = TRUE
  )
  expect_match(table, "\\\\label\\{tab:sunrise-sunset\\}", perl = TRUE)
  expect_match(
    table,
    paste0(
      " &  &  &  & 16/09/2025 & 6:55 & 19:23 ",
      "\\\\"
    ),
    fixed = TRUE
  )
  expect_match(
    table,
    paste0(
      "12/08/2025 & 6:20 & 20:19 &  & 17/09/2025 & 6:56 & 19:22 ",
      "\\\\"
    ),
    fixed = TRUE
  )
  expect_match(
    table,
    paste0(
      "15/09/2025 & 6:54 & 19:25 &  & 21/10/2025 & 7:33 & 18:25 ",
      "\\\\"
    ),
    fixed = TRUE
  )
})

test_that("protocol solar table follows package season dates", {
  table <- brc_sunrise_sunset_latex(2018)

  expect_match(table, "17/08/2018", fixed = TRUE)
  expect_match(table, "16/10/2018", fixed = TRUE)
  expect_false(grepl("12/08/2018", table, fixed = TRUE))
})

test_that("protocol solar table validates scalar years", {
  expect_error(brc_sunrise_sunset_latex(integer()), "single year")
  expect_error(brc_sunrise_sunset_latex(c(2025, 2026)), "single year")
  expect_error(brc_sunrise_sunset_latex(2025.5), "whole-number year")
})

test_that("protocol solar table safely handles LaTeX metadata", {
  table <- brc_sunrise_sunset_latex(
    2025,
    caption = "Sunrise & sunset: 100%_checked"
  )

  expect_match(
    table,
    "\\caption{Sunrise \\& sunset: 100\\%\\_checked}",
    fixed = TRUE
  )
  expect_error(
    brc_sunrise_sunset_latex(2025, label = "tab:solar table"),
    "LaTeX label"
  )
})
