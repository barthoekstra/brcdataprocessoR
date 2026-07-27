# nolint start
dc_row <- function(
  time,
  station,
  species = "HB",
  count = 1L,
  location = if (station == 1L) "E3" else "W3",
  age = "",
  sex = "",
  plumage = "",
  counttype = "",
  id = station,
  source_key = sprintf("%064x", id)
) {
  data.frame(
    datetime = time,
    telpost = station,
    speciesname = species,
    count = count,
    countback = "0",
    local = "0",
    age = age,
    sex = sex,
    plumage = plumage,
    remark = "",
    location = location,
    migtype = "",
    counttype = counttype,
    year = as.integer(substr(time, 1, 4)),
    protocol = 0L,
    time2 = as.POSIXct(time, tz = "Etc/GMT-4"),
    yday = as.integer(format(as.Date(substr(time, 1, 10)), "%j")),
    id = id,
    source_key = source_key,
    stringsAsFactors = FALSE
  )
}

synthetic_dc_rules <- function(non_soaring = FALSE) {
  groups <- data.frame(
    group = c("HB", "HB", rep("Buzzard_SPEC", 4)),
    species = c("HB", "Buzzard_SPEC", "HB", "BK", "SB", "Buzzard_SPEC"),
    member_type = rep("species", 6),
    rank = c(2L, 1L, 1:4),
    effective_from = 2010L,
    effective_to = 2025L
  )
  attr(groups, "reviewed_group_nodes") <- "Buzzard_SPEC"
  structure(
    list(
      version = "synthetic",
      tables = list(
        comparable_groups = groups,
        speeds = data.frame(
          species = c("*", if (non_soaring) "HB" else character()),
          speed_class = c(
            "soaring",
            if (non_soaring) "non_soaring" else character()
          ),
          seconds = c(900L, if (non_soaring) 600L else integer()),
          effective_from = 2010L,
          effective_to = 2025L
        ),
        protocol_windows = data.frame(
          window_id = "honey_buzzard_phase_one",
          start_month_day = "08-21",
          end_month_day = "09-10",
          start_offset_seconds = 1L,
          end_offset_seconds = 1L,
          filter = 0L,
          effective_from = 2010L,
          effective_to = 2025L
        )
      ),
      provenance = list(source = "synthetic")
    ),
    class = "brc_rules"
  )
}

test_that("inclusive speed boundaries reconcile and outside does not", {
  for (seconds in c(600L, 900L)) {
    rules <- synthetic_dc_rules(seconds == 600L)
    at <- rbind(
      dc_row("2025-10-01 12:00:00", 1L, count = 3L, id = 1L),
      dc_row(
        sprintf("2025-10-01 12:%02d:00", seconds / 60),
        2L,
        count = 2L,
        id = 2L
      )
    )
    result <- brc_reconcile_double_counts(at, rules, id_start = 100L)
    expect_equal(result$data$number, c(1L, 2L))
    expect_equal(sum(result$data$dckept + result$data$dcdel), 0)

    outside <- at
    outside$time2[2] <- outside$time2[1] + seconds + 1
    outside$datetime[2] <- format(outside$time2[2], "%Y-%m-%d %H:%M:%S")
    expect_equal(
      brc_reconcile_double_counts(outside, rules)$data$number,
      c(3L, 2L)
    )
  }
})

test_that("precision, stable ties, partial flocks and hierarchy are deterministic", {
  rules <- synthetic_dc_rules()
  x <- rbind(
    dc_row("2025-10-01 12:00:00", 1L, "HB", 5L, age = "ad", id = 9L),
    dc_row("2025-10-01 12:00:00", 2L, "Buzzard_SPEC", 2L, id = 8L),
    dc_row("2025-10-01 12:00:00", 2L, "Buzzard_SPEC", 4L, id = 7L)
  )
  a <- brc_reconcile_double_counts(x, rules)
  b <- brc_reconcile_double_counts(x[c(3, 1, 2), ], rules)
  expect_equal(a$data$number, c(4L, 2L, 5L))
  expect_identical(a$data, b$data)

  same <- rbind(
    dc_row("2025-10-01 12:00:00", 1L, "HB", 5L, age = "ad", id = 1L),
    dc_row("2025-10-01 12:00:00", 2L, "HB", 2L, id = 2L),
    dc_row("2025-10-01 12:00:01", 2L, "HB", 4L, id = 3L)
  )
  partial <- brc_reconcile_double_counts(same, rules)
  expect_equal(partial$data$number, c(5L, 0L, 1L))
  expect_true(all(a$data$number >= 0))
  expect_equal(partial$summary$reconciled_events, 2L)
})

test_that("single counts and HB phase one are excluded", {
  rules <- synthetic_dc_rules()
  x <- rbind(
    dc_row("2025-08-25 12:00:00", 1L, id = 1L),
    dc_row("2025-08-25 12:00:00", 2L, id = 2L)
  )
  expect_equal(brc_reconcile_double_counts(x, rules)$data$number, c(1L, 1L))
  x$datetime <- "2025-10-01 12:00:00"
  x$time2 <- as.POSIXct(x$datetime, tz = "Etc/GMT-4")
  x$counttype[1] <- "sc"
  expect_equal(brc_reconcile_double_counts(x, rules)$data$number, c(1L, 1L))
})

test_that("phase-one offsets are strict and nonconfigured species are ignored", {
  rules <- synthetic_dc_rules()
  pair_at <- function(time, species = "HB") {
    rbind(
      dc_row(time, 1L, species = species, id = 1L),
      dc_row(time, 2L, species = species, id = 2L)
    )
  }
  at_start <- brc_reconcile_double_counts(
    pair_at("2025-08-21 00:00:01"),
    rules
  )
  inside <- brc_reconcile_double_counts(
    pair_at("2025-08-21 00:00:02"),
    rules
  )
  at_end <- brc_reconcile_double_counts(
    pair_at("2025-09-10 00:00:01"),
    rules
  )
  shot <- brc_reconcile_double_counts(
    pair_at("2025-10-01 12:00:00", "SHOT"),
    rules
  )
  expect_equal(at_start$data$number, c(0L, 1L))
  expect_equal(inside$data$number, c(1L, 1L))
  expect_equal(at_end$data$number, c(0L, 1L))
  expect_equal(shot$data$number, c(1L, 1L))
})

test_that("legacy location overlap is directed and soaring-specific", {
  rules <- synthetic_dc_rules()
  overlap <- brcdatapreprocessoR:::dc_location
  expect_true(overlap("E2", "W2", 2025L, FALSE, rules))
  expect_false(overlap("W2", "E2", 2025L, FALSE, rules))
  expect_true(overlap("", "W3", 2025L, FALSE, rules))
  expect_false(overlap("W3", "", 2025L, FALSE, rules))
  expect_true(overlap("W2", "W3", 2025L, TRUE, rules))
  expect_false(overlap("W2", "W3", 2025L, FALSE, rules))
  expect_true(overlap("E3", "E2", 2025L, TRUE, rules))
  expect_false(overlap("E3", "E2", 2025L, FALSE, rules))
})

test_that("station two may be a comparable leaf member", {
  rules <- synthetic_dc_rules()
  x <- rbind(
    dc_row("2025-10-01 12:00:00", 1L, "HB", id = 1L),
    dc_row(
      "2025-10-01 12:00:00",
      2L,
      "Buzzard_SPEC",
      location = "W3",
      id = 2L
    )
  )
  expect_true(brcdatapreprocessoR:::dc_comparable(
    "HB",
    "Buzzard_SPEC",
    2025L,
    rules
  ))
  result <- brc_reconcile_double_counts(x, rules)
  expect_equal(result$data$number, c(1L, 1L))
})

test_that("comparability is literal membership, not an implicit self-match", {
  rules <- synthetic_dc_rules()
  extra <- data.frame(
    group = "ShortTE",
    species = "Raptor_SPEC",
    member_type = "species",
    rank = 1L,
    effective_from = 2010L,
    effective_to = 2025L
  )
  rules$tables$comparable_groups <- rbind(
    rules$tables$comparable_groups,
    extra
  )
  expect_false(brcdatapreprocessoR:::dc_comparable(
    "ShortTE",
    "ShortTE",
    2025L,
    rules
  ))
  expect_true(brcdatapreprocessoR:::dc_comparable(
    "ShortTE",
    "Raptor_SPEC",
    2025L,
    rules
  ))
})

test_that("input is validated without mutation", {
  rules <- synthetic_dc_rules()
  x <- dc_row("2025-10-01 12:00:00", 1L)
  original <- x
  expect_error(brc_reconcile_double_counts(x[-1], rules), "required")
  expect_identical(x, original)
  fractional <- x
  fractional$count <- 1.5
  expect_error(brc_reconcile_double_counts(fractional, rules), "whole numbers")
  overflow <- x
  overflow$count <- .Machine$integer.max + 1
  expect_error(brc_reconcile_double_counts(overflow, rules), "whole numbers")
})
# nolint end
