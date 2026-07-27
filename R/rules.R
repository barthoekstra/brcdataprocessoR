#' Load the BRC operational scientific rule profile
#'
#' @param version Rule profile version. Only `operational_v1` is supported.
#' @return A validated `brc_rules` object.
#' @export
brc_rules <- function(version = "operational_v1") {
  version <- canonical_rule_version(version)
  if (!identical(version, "operational_v1")) {
    stop("Unknown rule version: ", version, call. = FALSE)
  }
  tables <- embedded_profile_tables()
  provenance <- embedded_rule_provenance(tables)
  validate_brc_rules(tables, version)
  structure(
    list(
      version = version,
      tables = tables,
      provenance = list(
        files = provenance
      )
    ),
    class = "brc_rules"
  )
}

canonical_rule_version <- function(version) {
  version
}

#' Validate scientific rule tables
#'
#' @param tables Named list of rule data frames.
#' @param version Rule version used in error messages.
#' @return `tables`, invisibly.
#' @export
validate_brc_rules <- function(tables, version = "unknown") {
  schemas <- rule_schemas()
  missing_tables <- setdiff(names(schemas), names(tables))
  if (length(missing_tables)) {
    rule_error(version, "profile", "tables", missing_tables, "missing")
  }
  for (name in names(schemas)) {
    validate_table_schema(tables[[name]], name, schemas[[name]], version)
  }

  species <- tables$species$species
  unknown_types <- which(!tables$aliases$target_type %in% c("species", "event"))
  if (length(unknown_types)) {
    detail <- paste0(
      tables$aliases$alias[unknown_types],
      " -> ",
      tables$aliases$species[unknown_types],
      " (",
      tables$aliases$target_type[unknown_types],
      ")"
    )
    rule_error(
      version,
      "aliases",
      "target_type",
      detail,
      "unclassified_mapping"
    )
  }
  validate_mapping(tables$aliases, "aliases", species, version)
  validate_mapping(
    tables$speeds[tables$speeds$species != "*", , drop = FALSE],
    "speeds",
    species,
    version
  )
  comparable_species <- tables$comparable_groups[
    tables$comparable_groups$member_type == "species",
    ,
    drop = FALSE
  ]
  validate_mapping(comparable_species, "comparable_groups", species, version)
  assert_domain(
    tables$comparable_groups$member_type,
    c("species", "group"),
    "comparable_groups",
    "member_type",
    version
  )
  reviewed_nodes <- attr(
    tables$comparable_groups,
    "reviewed_group_nodes",
    exact = TRUE
  )
  group_members <- unique(tables$comparable_groups$species[
    tables$comparable_groups$member_type == "group"
  ])
  undeclared_nodes <- setdiff(group_members, reviewed_nodes %||% character())
  if (length(undeclared_nodes)) {
    rule_error(
      version,
      "comparable_groups",
      "species[group]",
      undeclared_nodes,
      "undeclared_reviewed_group_node"
    )
  }

  assert_domain(
    tables$aliases$target_type,
    c("species", "event"),
    "aliases",
    "target_type",
    version
  )
  events <- c("BREAK", "END", "RESUME", "START")
  event_targets <- tables$aliases$species[tables$aliases$target_type == "event"]
  assert_domain(event_targets, events, "aliases", "species[event]", version)
  assert_domain(
    tables$speeds$speed_class,
    c("soaring", "non_soaring"),
    "speeds",
    "speed_class",
    version
  )
  assert_domain(
    tables$exclusions$action,
    c("filter_zero", "observe_only"),
    "exclusions",
    "action",
    version
  )
  assert_domain(
    tables$exclusions$action,
    c("filter_zero"),
    "exclusions",
    "action",
    version
  )
  if (any(tables$speeds$seconds <= 0L)) {
    rule_error(version, "speeds", "seconds", tables$speeds$seconds, "domain")
  }
  if (any(!tables$protocol_windows$filter %in% 0:2)) {
    rule_error(
      version,
      "protocol_windows",
      "filter",
      tables$protocol_windows$filter,
      "domain"
    )
  }
  date_fields <- c("start_month_day", "end_month_day")
  for (field in date_fields) {
    valid <- grepl(
      "^(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])$",
      tables$protocol_windows[[field]]
    )
    if (any(!valid)) {
      rule_error(
        version,
        "protocol_windows",
        field,
        tables$protocol_windows[[field]][!valid],
        "date_domain"
      )
    }
  }
  invisible(tables)
}

rule_schemas <- function() {
  list(
    species = list(
      columns = c(
        "species",
        "effective_from",
        "effective_to"
      ),
      key = "species",
      integer = c("effective_from", "effective_to")
    ),
    aliases = list(
      columns = c(
        "alias",
        "species",
        "target_type",
        "effective_from",
        "effective_to"
      ),
      key = "alias",
      integer = c("effective_from", "effective_to")
    ),
    comparable_groups = list(
      columns = c(
        "group",
        "species",
        "member_type",
        "rank",
        "effective_from",
        "effective_to"
      ),
      key = c("group", "species", "rank"),
      integer = c("rank", "effective_from", "effective_to")
    ),
    speeds = list(
      columns = c(
        "species",
        "speed_class",
        "seconds",
        "effective_from",
        "effective_to"
      ),
      key = "species",
      integer = c("seconds", "effective_from", "effective_to")
    ),
    protocol_windows = list(
      columns = c(
        "window_id",
        "start_month_day",
        "end_month_day",
        "start_offset_seconds",
        "end_offset_seconds",
        "filter",
        "effective_from",
        "effective_to"
      ),
      key = "window_id",
      integer = c(
        "start_offset_seconds",
        "end_offset_seconds",
        "filter",
        "effective_from",
        "effective_to"
      )
    ),
    exclusions = list(
      columns = c(
        "rule_id",
        "field",
        "values",
        "condition",
        "effective_from",
        "effective_to",
        "action"
      ),
      key = "rule_id",
      integer = c("effective_from", "effective_to")
    )
  )
}

validate_table_schema <- function(table, name, schema, version) {
  if (!is.data.frame(table)) {
    rule_error(version, name, "table", class(table), "not_data_frame")
  }
  missing <- setdiff(schema$columns, names(table))
  if (length(missing)) {
    rule_error(version, name, "columns", missing, "missing")
  }
  optional_empty <- c("values", "condition")
  for (column in setdiff(schema$columns, optional_empty)) {
    if (any(is.na(table[[column]]) | !nzchar(as.character(table[[column]])))) {
      rule_error(version, name, column, "<empty>", "required")
    }
  }
  key <- do.call(paste, c(table[schema$key], sep = "\r"))
  if (anyDuplicated(key)) {
    duplicate <- unique(key[duplicated(key)])
    rule_error(
      version,
      name,
      paste(schema$key, collapse = "+"),
      duplicate,
      "duplicate"
    )
  }
  for (column in schema$integer) {
    if (!is.integer(table[[column]]) || any(is.na(table[[column]]))) {
      rule_error(version, name, column, class(table[[column]]), "integer")
    }
  }
  for (column in schema$logical) {
    if (!is.logical(table[[column]]) || any(is.na(table[[column]]))) {
      rule_error(version, name, column, class(table[[column]]), "boolean")
    }
  }
  if (any(table$effective_from > table$effective_to)) {
    rule_error(
      version,
      name,
      "effective_from/effective_to",
      "from > to",
      "date_range"
    )
  }
}

validate_mapping <- function(table, name, species, version) {
  required <- if ("target_type" %in% names(table)) {
    table$target_type != "event"
  } else {
    rep(TRUE, nrow(table))
  }
  unknown <- unique(table$species[required & !table$species %in% species])
  if (length(unknown)) {
    rule_error(version, name, "species", unknown, "unknown_required_mapping")
  }
}

`%||%` <- function(left, right) {
  if (is.null(left)) right else left
}

assert_domain <- function(values, allowed, table, field, version) {
  invalid <- unique(values[!values %in% allowed])
  if (length(invalid)) {
    rule_error(version, table, field, invalid, "domain")
  }
}

rule_error <- function(version, table, field, values, problem) {
  stop(
    "Rule profile ",
    version,
    ": table ",
    table,
    " field ",
    field,
    " ",
    problem,
    ": ",
    paste(unique(values), collapse = ", "),
    call. = FALSE
  )
}

embedded_rule_provenance <- function(tables) {
  data.frame(
    path = paste0("embedded:operational_v1/", names(tables)),
    sha256 = vapply(
      tables,
      function(table) {
        digest::digest(table, algo = "sha256")
      },
      character(1)
    ),
    stringsAsFactors = FALSE
  )
}
