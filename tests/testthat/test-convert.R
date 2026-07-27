converter_rules <- function() {
  brc_rules("operational_v1")
}

converter_input <- function() {
  utils::read.csv(
    testthat::test_path("fixtures", "converter-synthetic.csv"),
    stringsAsFactors = FALSE,
    na.strings = "<NA>",
    check.names = FALSE
  )
}

test_that("operational aliases cover the modern raw vocabulary only", {
  rules <- brc_rules()
  aliases <- rules$tables$aliases
  expect_equal(nrow(aliases), 26L)
  expect_false(any(aliases$alias %in% c("HoneyBuz", "Accspec", "ShortEO")))
  expect_equal(
    brcdataprocessoR:::map_converter_aliases(
      c("Buz_HonBuz", "Large_FALCON", "PernisSpec", "StepEagle"),
      rep(2025L, 4L),
      rules
    ),
    c("Buzzard_SPEC", "LargeFALCON", "Pernis_SPEC", "SteppeE")
  )
})

synthetic_hash <- paste(rep("a", 64L), collapse = "")

test_that("converter normalizes observations and retains stable provenance", {
  input <- converter_input()
  original <- input
  before <- list(wd = getwd(), tz = Sys.getenv("TZ"), options = options())

  result <- brc_convert(input, converter_rules(), synthetic_hash)

  expect_s3_class(result, "brc_conversion")
  expect_identical(input, original)
  expect_identical(getwd(), before$wd)
  expect_identical(Sys.getenv("TZ"), before$tz)
  expect_identical(options(), before$options)
  expect_named(
    result,
    c("data", "metadata", "shots", "rejected", "summary", "provenance")
  )
  expect_equal(result$data$speciesname, c("HB", "HB", "HB_JUV", "SHOT"))
  expect_equal(result$data$age, c("juv", "nonjuv", "", ""))
  expect_equal(result$data$sex, c("m", "f", "", ""))
  expect_equal(result$data$plumage, c("dark", "", "light", ""))
  expect_equal(result$data$migtype, c("inj", "", "", ""))
  expect_equal(result$data$counttype, c("sc", "dc", "", ""))
  expect_equal(result$data$telpost, c("1", "2", "1", "2"))
  expect_equal(result$data$location, c("W1", "E2", "O", "E1"))
  expect_equal(result$data$remark[1], "synthetic remark")
  expect_equal(result$data$year, rep(2025L, 4L))
  expect_equal(result$data$yday, c(224L, 225L, 226L, 228L))
  expect_equal(result$data$id, 1:4)
  expect_match(result$data$source_key, "^[0-9a-f]{64}$")
  expect_length(unique(result$data$source_key), 4L)
  expect_identical(
    result$data$source_key[1],
    digest::digest(
      paste("source-key-v1", synthetic_hash, 1L, sep = "\u001f"),
      algo = "sha256",
      serialize = FALSE
    )
  )
})

test_that("converter returns side outputs and stage counts", {
  result <- brc_convert(converter_input(), converter_rules(), synthetic_hash)

  expect_equal(result$metadata$speciesname, "START")
  expect_equal(result$shots$speciesname, c("HB", "SHOT"))
  expect_equal(result$rejected$speciesname, "START")
  expect_equal(result$rejected$reason, "metadata_event")
  expect_equal(result$summary$input_rows, 5L)
  expect_equal(result$summary$output_rows, 4L)
  expect_equal(result$summary$metadata_rows, 1L)
  expect_equal(result$summary$shot_rows, 2L)
  expect_equal(result$summary$rejected_rows, 1L)
})

test_that("converter enforces exact schema, provenance, mappings, and values", {
  input <- converter_input()

  expect_error(
    brc_convert(input[-1L], converter_rules(), synthetic_hash),
    "exact raw schema.*missing.*datetime"
  )
  extra <- input
  extra$unexpected <- 1
  expect_error(
    brc_convert(extra, converter_rules(), synthetic_hash),
    "exact raw schema.*unexpected"
  )
  expect_error(
    brc_convert(input, converter_rules()),
    "source_artifact_sha256"
  )
  unknown <- input
  unknown$speciesname[1L] <- "Synthetic mystery bird"
  expect_error(
    brc_convert(unknown, converter_rules(), synthetic_hash),
    "unknown required species mapping.*row 1.*Synthetic_mystery_bird"
  )
  obsolete_event <- input
  obsolete_event$speciesname[1L] <- "NIS"
  expect_error(
    brc_convert(obsolete_event, converter_rules(), synthetic_hash),
    "unknown required species mapping.*row 1.*NIS"
  )
  invalid_time <- input
  invalid_time$datetime[1L] <- "2025-02-30 12:00:00"
  expect_error(
    brc_convert(invalid_time, converter_rules(), synthetic_hash),
    "invalid datetime.*row 1"
  )
  invalid_count <- input
  invalid_count$count[1L] <- "one"
  expect_error(
    brc_convert(invalid_count, converter_rules(), synthetic_hash),
    "invalid count.*row 1"
  )
  invalid_station <- input
  invalid_station$telpost[1L] <- "mystery"
  expect_error(
    brc_convert(invalid_station, converter_rules(), synthetic_hash),
    "unknown station.*row 1"
  )
})

test_that("operational harrier staging preserves current conventions", {
  staged <- data.frame(
    speciesname = c("Pal", "Mon", "Hen", "Mar", "ImperialE"),
    age = c("", "", "juv", "juv", "5cy"),
    sex = c("", "m", "f", "f", ""),
    plumage = "",
    remark = c("two  spaces", "", "", "", ""),
    year = 2025L,
    stringsAsFactors = FALSE
  )

  result <- brcdataprocessoR:::stage_converter_species(staged)
  result <- brcdataprocessoR:::normalize_age_sex_morph(result)

  expect_equal(
    result$speciesname,
    c("MonPalHen", "Mon", "Hen", "Mar", "ImperialE")
  )
  expect_equal(result$age, c("", "nonjuv", "juv", "juv", "5cy"))
  expect_equal(result$sex, c("", "m", "", "", ""))
  expect_equal(
    result$remark[1],
    "previously Pal without age and sex - two  spaces"
  )
})
