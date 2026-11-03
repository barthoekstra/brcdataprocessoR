test_that("processing retains rejected records and solar provenance", {
  input <- utils::read.csv(
    test_path("fixtures", "converter-synthetic.csv"),
    na.strings = "<NA>",
    check.names = FALSE
  )
  input$speciesname[1L] <- "Unknown species"
  hash <- strrep("a", 64L)

  result <- brc_process(
    input,
    source_artifact_sha256 = hash,
    strict_mappings = FALSE
  )

  expect_equal(result$rejected$speciesname, c("Unknown_species", "START"))
  expect_equal(result$rejected$reason, c("unknown_mapping", "metadata_event"))
  expect_equal(
    result$rejected$source_key[1L],
    digest::digest(paste("source-key-v1", hash, 1L, sep = "\u001f"),
                   algo = "sha256", serialize = FALSE)
  )
  expect_equal(nrow(result$data), 3L)
  expect_length(result$provenance$source_keys, nrow(result$data))
  expect_equal(result$provenance$timezone_name_or_fixed_offset, "Etc/GMT-4")
  expect_equal(result$provenance$solar_latitude, 41.690259)
  expect_equal(result$provenance$solar_longitude, 41.729722)
  expect_equal(
    result$provenance$solar_library_version,
    as.character(utils::packageVersion("suncalc"))
  )
})
