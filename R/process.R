#' Process reviewed BRC count data
#'
#' Runs conversion, double-count reconciliation, and protocol assignment without
#' reading files or changing session state.
#'
#' @param data The exact reviewed raw observation data frame.
#' @param rules A validated scientific rule profile.
#' @param source_artifact_sha256 SHA-256 of the caller-read source artifact.
#' @param time_policy Explicit solar/time policy for protocol assignment. If
#'   omitted, it is generated with `brc_solar_time_policy()` from the input
#'   dates.
#' @param id_start First generated identifier minus one.
#' @param strict_mappings Whether unknown species mappings are errors.
#' @return A `brc_result` with canonical data, side outputs, stage summaries,
#'   and provenance.
#' @export
brc_process <- function(
  data,
  rules = brc_rules("operational_v1"), # nolint
  source_artifact_sha256,
  time_policy = NULL,
  id_start = 0L,
  strict_mappings = TRUE
) {
  if (is.null(time_policy)) {
    time_policy <- brc_solar_time_policy(substr(data$datetime, 1L, 10L))
  }
  converted <- brc_convert(
    data,
    rules,
    source_artifact_sha256,
    strict_mappings
  )
  reconciled <- brc_reconcile_double_counts(
    converted$data,
    rules,
    id_start
  )
  filtered <- brc_assign_protocol_filter(
    reconciled$data,
    rules,
    time_policy
  )

  ordered <- order(
    as.numeric(converted$data$time2),
    converted$data$id,
    converted$data$source_key,
    method = "radix"
  )
  source_keys <- converted$data$source_key[ordered]
  source_ids <- reconciled$data$id
  keep <- match(filtered$data$id, source_ids)
  if (anyNA(keep)) {
    stop("Internal source-key propagation failed.", call. = FALSE)
  }
  source_keys <- source_keys[keep]
  attr(filtered$data, "source_keys") <- unname(source_keys)

  structure(
    list(
      data = filtered$data,
      metadata = converted$metadata,
      shots = converted$shots,
      summary = list(
        convert = converted$summary,
        double_count = reconciled$summary,
        filter = filtered$summary
      ),
      provenance = list(
        rule_version = rules$version,
        package_version = as.character(utils::packageVersion(
          "brcdataprocessoR"
        )),
        source_artifact_sha256 = source_artifact_sha256,
        source_key_algorithm = converted$provenance$source_key_algorithm,
        source_keys = source_keys,
        time_policy_id = filtered$provenance$time_policy_id,
        timezone_name_or_fixed_offset = filtered$provenance$timezone_name_or_fixed_offset,
        solar_library_version = filtered$provenance$solar_library_version,
        solar_latitude = filtered$provenance$latitude,
        solar_longitude = filtered$provenance$longitude
      )
    ),
    class = "brc_result"
  )
}
