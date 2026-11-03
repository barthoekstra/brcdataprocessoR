# brcdataprocessoR <a href="https://www.batumiraptorcount.org"><img src="https://static1.squarespace.com/static/5b33912fb27e39bd89996b9d/t/5b33ac53352f535c7e8effcb/1560539069142/?format=120w" alt="BRC logo" align="right" width="175" style="max-width: 175px;"></a>

The package processes reviewed raw [Batumi Raptor Count](https://www.batumiraptorcount.org)
observations from [Trektellen](https://www.trektellen.org). It performs
deterministic preprocessing, conversion, cross-station double-count
removal, and protocol flagging. Records are flagged rather than deleted.

Author: Bart Hoekstra | Mail: [bart.hoekstra@batumiraptorcount.org](mailto:bart.hoekstra@batumiraptorcount.org)

`brcdataprocessoR` supports the regular Batumi Raptor Count data workflow:
raw-data preprocessing, a manual review round, and canonical processing. The
workflow preserves records. Data are flagged or returned as side outputs; the
package does not silently delete observations.

## Regular workflow

### 1. Preprocess raw Trektellen data

Start with a raw Trektellen CSV. `preprocess_raw_trektellen_data()` selects a
count date, retrieves count-period metadata, adds `START`, `END`, `BREAK`, and
`RESUME` records, standardizes the fields, and returns the 13-column schema
used by the processing pipeline.

`process_day()` orchestrates preprocessing and writes the result to `raw/`,
the review copy to `inprogress/`, and a backup to `inprogress-backup/`.
`process_today()` is the equivalent wrapper for the current date. `write_data()`
writes one-date outputs as `.xlsx` files, split by station by default.

```r
process_day(
  day_str = "2026-08-17",
  tt_file = "trektellen.csv",
  output_folder = "data-repo"
)
```

### 2. Manually review the preprocessed data

Review the generated files in `inprogress/` using the normal coordinator or
data-technician review process. `check_trektellen_data()` adds a `check`
column with warnings for issues such as possible double counts, missing fields
or timestamps, unusual morphs or locations, injured or killed birds, age/count
mismatches, unexpected age/sex combinations, ageing-distance problems, and
non-protocol species.

These are review prompts, not automatic deletion rules. In particular,
double-count and missing-single-count warnings explicitly remain available for
human assessment. The reviewed CSV is the input to the final processing step.

### 3. Process the reviewed data

`brc_process()` accepts the reviewed 13-column schema and runs the canonical
pipeline for one count date or multiple dates:

1. `brc_convert()` normalizes stations, species aliases, age/sex/morph fields,
   timestamps, counts, and metadata records.
2. `brc_reconcile_double_counts()` reconciles potential cross-station double
   counts using the operational rules.
3. `brc_assign_protocol_filter()` assigns protocol flags and applies operational
   filter-zero exclusions.

```r
raw_path <- "reviewed-count.csv"
reviewed <- read.csv(raw_path, stringsAsFactors = FALSE, check.names = FALSE)

result <- brc_process(
  reviewed,
  source_artifact_sha256 = digest::digest(
    file = raw_path,
    algo = "sha256",
    serialize = FALSE
  )
)

result$data
result$rejected
result$summary
result$provenance
```

The result contains canonical observations, metadata rows, shot rows, stage
summaries, stable source keys, and processing provenance.
With `strict_mappings = FALSE`, unknown species are returned in `rejected`
with their source keys and rejection reasons.

## Canonical input

The reviewed input has these columns:

```text
datetime, telpost, speciesname, count, countback, local, age, sex,
plumage, remark, location, migtype, counttype
```

## Protocol flags

The `filter` column is a classification, not a deletion instruction:

- `1`: standard protocol window, 17 August through 16 October, from sunrise + 1
  hour through sunset - 2 hours;
- `2`: early-morning harrier window, 27 August through 27 September, from
  sunrise through but not including sunrise + 1 hour; and
- `0`: outside the applicable window or overridden by an operational exclusion.

All rows remain available in the returned data. Operational exclusions cover
non-regular species, outer-east locations, and age-protocol observations in
excluded locations.

## Scientific rules

The package exposes the current `operational_v1` rule profile:

```r
rules <- brc_rules("operational_v1")
validate_brc_rules(rules$tables, rules$version)
```

The profile contains the active species vocabulary, modern aliases,
comparable-species groups, reconciliation speeds, protocol windows, and
exclusions. Obsolete pre-2018 compatibility rules are not part of the current
processing profile.

## Trektellen metadata helpers

The preprocessing workflow can retrieve count metadata directly:

```r
period <- extract_count_period(1047, "2026-08-17")
header <- extract_count_header(1047, "2026-08-17")
```

## Development

Run the package tests with:

```sh
Rscript --vanilla -e 'testthat::test_local(".", reporter = "summary")'
```

Format R code and tests with Air:

```sh
air format R tests
air format --check R tests
```

The package is maintained by Bart Hoekstra for the Batumi Raptor Count.
