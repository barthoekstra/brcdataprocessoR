check_protocol_tuple <- function(species, age = NA_character_, sex = NA_character_) {
  row <- data.frame(
    datetime = as.POSIXct("2025-09-10 12:00:00", tz = "Etc/GMT-4"),
    telpost = "1. Sakhalvasho", speciesname = species,
    count = 1, countback = 0, local = 0, age = age, sex = sex,
    plumage = NA_character_, remark = NA_character_, location = "E1",
    migtype = NA_character_, counttype = "", stringsAsFactors = FALSE
  )
  check_trektellen_data(row)$check
}

test_that("new protocol species retain age and sex validation", {
  for (sp in c("CommonBuz", "RoughLB", "BeardedV", "BWKite")) {
    for (age in c("J", "I", "A", "Non-Juv", NA_character_)) {
      expect_identical(check_protocol_tuple(sp, age), "", info = sp)
    }
    expect_match(check_protocol_tuple(sp, "J", "M"), "unexpected sex")
    expect_match(check_protocol_tuple(sp, "X"), "unexpected age")
  }
})

test_that("Pernis may only be sexed when adult", {
  for (age in "A") {
    for (sex in c("M", "F", NA_character_)) {
      expect_identical(check_protocol_tuple("Pernis_SPEC", age, sex), "")
    }
  }
  expect_identical(check_protocol_tuple("Pernis_SPEC", "J"), "")
  expect_identical(check_protocol_tuple("Pernis_SPEC"), "")
  for (age in c("J", NA_character_)) {
    for (sex in c("M", "F")) {
      expect_match(check_protocol_tuple("Pernis_SPEC", age, sex),
                   "unexpected species\\+age\\+sex combination")
    }
  }
  for (age in c("I", "Non-Juv")) {
    expect_match(check_protocol_tuple("Pernis_SPEC", age), "unexpected species")
  }
  expect_match(check_protocol_tuple("Pernis_SPEC", "J", "FC"), "unexpected species")
  expect_match(check_protocol_tuple("UnknownSpecies"), "non-protocol species")
})
