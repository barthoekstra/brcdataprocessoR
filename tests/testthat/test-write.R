write_input <- function() {
  data.frame(
    datetime = as.POSIXct(c("2025-09-07 12:00:00", "2025-09-07 12:10:00"),
      tz = "Etc/GMT-4"
    ),
    telpost = c("1. Sakhalvasho", "2. Shuamta"),
    speciesname = c("HB", "BK"), count = c(2, 3)
  )
}

xlsx_strings <- function(path) {
  document <- xml2::xml_ns_strip(
    xml2::read_xml(unz(path, "xl/sharedStrings.xml"))
  )
  xml2::xml_text(xml2::xml_find_all(document, "//si"))
}

test_that("writing splits stations and preserves wall-clock timestamps", {
  folder <- withr::local_tempdir()
  input <- write_input()
  original <- input
  write_data(input, folder)

  expect_identical(input, original)
  expect_equal(list.files(folder), c("20250907_S1.xlsx", "20250907_S2.xlsx"))
  for (station in 1:2) {
    path <- file.path(folder, paste0("20250907_S", station, ".xlsx"))
    strings <- xlsx_strings(path)
    expect_true(input$telpost[station] %in% strings)
    expect_false(input$telpost[3L - station] %in% strings)
    expect_true(format(input$datetime[station], "%Y-%m-%d %H:%M:%S") %in% strings)
    sheet <- xml2::xml_ns_strip(
      xml2::read_xml(unz(path, "xl/worksheets/sheet1.xml"))
    )
    expect_equal(
      xml2::xml_text(xml2::xml_find_all(sheet, "//c[@r='D2']/v")),
      as.character(input$count[station])
    )
  }
})

test_that("combined output contains both stations", {
  folder <- withr::local_tempdir()
  input <- write_input()
  write_data(input, folder, split_by_station = FALSE)

  expect_equal(list.files(folder), "20250907.xlsx")
  expect_true(all(input$telpost %in% xlsx_strings(file.path(folder, "20250907.xlsx"))))
})

test_that("existing output is protected unless overwrite is requested", {
  for (split in c(TRUE, FALSE)) {
    folder <- withr::local_tempdir()
    input <- write_input()
    write_data(input, folder, split_by_station = split)
    files <- list.files(folder, full.names = TRUE)
    before <- tools::md5sum(files)
    expect_error(write_data(input, folder, split_by_station = split), "already.*exists?")
    expect_identical(tools::md5sum(files), before)

    input$speciesname <- "Osprey"
    write_data(input, folder, split_by_station = split, overwrite = TRUE)
    expect_true(all(tools::md5sum(files) != before))
    expect_true(all(vapply(files, function(path) "Osprey" %in% xlsx_strings(path), logical(1))))
  }
})

test_that("multiple dates are rejected before creating any files", {
  folder <- withr::local_tempdir()
  input <- write_input()
  input$datetime[2] <- input$datetime[2] + 86400
  expect_error(write_data(input, folder), "multiple dates")
  expect_length(list.files(folder), 0L)
})

test_that("a second workbook failure leaves no partial or overwritten output", {
  original_write <- writexl::write_xlsx
  for (overwrite in c(FALSE, TRUE)) {
    folder <- withr::local_tempdir()
    input <- write_input()
    if (overwrite) write_data(input, folder)
    files <- list.files(folder, full.names = TRUE)
    before <- tools::md5sum(files)
    calls <- 0L
    local_mocked_bindings(
      write_xlsx = function(...) {
        calls <<- calls + 1L
        if (calls == 2L) stop("second workbook failed")
        original_write(...)
      },
      .package = "writexl"
    )
    input$speciesname <- "Osprey"
    expect_error(write_data(input, folder, overwrite = overwrite), "second workbook failed")
    expect_identical(list.files(folder, full.names = TRUE, all.files = FALSE), files)
    expect_identical(tools::md5sum(files), before)
  }
})
