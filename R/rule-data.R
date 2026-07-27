# Readable, versioned rule data used by the built-in operational_v1 profile.
# Domain values are named or grouped where possible; constructors add shared metadata.

rule_period <- function(data, from = 2018L, to = 9999L) {
  data$effective_from <- as.integer(from)
  data$effective_to <- as.integer(to)
  data
}

embedded_species_rules <- function() {
  species <- c(
    "HB",
    "HB_JUV",
    "HB_NONJUV",
    "StepBuz",
    "CommonBuz",
    "CrestedHB",
    "RoughLB",
    "LongLB",
    "Pernis_SPEC",
    "Buzzard_SPEC",
    "BlackKite",
    "BK_JUV",
    "BK_NONJUV",
    "RedKite",
    "Kite_SPEC",
    "LesserSE",
    "GreaterSE",
    "SteppeE",
    "ImperialE",
    "GoldenE",
    "BootedE",
    "ShortTE",
    "Osprey",
    "WhiteTE",
    "LargeEAGLE",
    "Mar",
    "Mon",
    "Pal",
    "Hen",
    "MonPalHen",
    "Harrier_SPEC",
    "EgyptianV",
    "GriffonV",
    "BlackV",
    "BeardedV",
    "LevantSH",
    "EurasianSH",
    "Goshawk",
    "SparrowH_SPEC",
    "SPH_Goshawk",
    "BWKite",
    "LesserKes",
    "CommonKes",
    "RedFF",
    "EleonoraF",
    "Merlin",
    "Hobby",
    "LannerF",
    "SakerF",
    "Peregrine",
    "LargeFALCON",
    "Hobby_RedFF",
    "Kestrel_SPEC",
    "Falcon_SPEC",
    "MediumRaptor",
    "Raptor_SPEC",
    "WhiStork",
    "BlaStork",
    "Stork_SPEC",
    "EurasianCrane",
    "DemCrane",
    "WhiteP",
    "DalmatianP",
    "Roller",
    "TurtleD",
    "OrientalTD",
    "WoodP",
    "StockD",
    "Dove_SPEC",
    "ShortEO",
    "SB_NONJUV",
    "SB_JUV",
    "SHOT"
  )
  data.frame(
    species = species,
    effective_from = 2018L,
    effective_to = 9999L,
    stringsAsFactors = FALSE
  )
}

embedded_alias_rules <- function() {
  aliases <- c(
    BK = "BlackKite",
    Buz_HonBuz = "Buzzard_SPEC",
    ComBuz = "CommonBuz",
    ComKes = "CommonKes",
    EuCrane = "EurasianCrane",
    HB_AD = "HB_NONJUV",
    HB_juv = "HB_JUV",
    HB_nONJUV = "HB_NONJUV",
    Lanner = "LannerF",
    Large_EAGLE = "LargeEAGLE",
    Large_FALCON = "LargeFALCON",
    large_FALCON = "LargeFALCON",
    Marsh = "Mar",
    MonPalHenHen = "MonPalHen",
    OrientalTurtleD = "OrientalTD",
    Pernis_sp = "Pernis_SPEC",
    PernisSpec = "Pernis_SPEC",
    Saker = "SakerF",
    SparrowH = "EurasianSH",
    StepEagle = "SteppeE",
    STOP = "END",
    WhiPel = "WhiteP",
    Black_winged_Kite = "BWKite",
    Black_winged_kite = "BWKite",
    CollDove = "Dove_SPEC",
    `dove_(Columba)_sp.` = "Dove_SPEC"
  )
  event_targets <- c("END")
  data.frame(
    alias = names(aliases),
    species = unname(aliases),
    target_type = ifelse(
      unname(aliases) %in% event_targets,
      "event",
      "species"
    ),
    effective_from = 2018L,
    effective_to = 9999L,
    stringsAsFactors = FALSE
  )
}

embedded_comparable_rules <- function() {
  groups <- list(
    `BlaStork` = c("Stork_SPEC", "BlaStork"),
    `WhiStork` = c("Stork_SPEC", "WhiStork"),
    `HB` = c("Buzzard_SPEC", "MediumRaptor", "Raptor_SPEC", "HB"),
    `BlackKite` = c("MediumRaptor", "Raptor_SPEC", "BlackKite"),
    `RedKite` = c("Raptor_SPEC", "RedKite"),
    `WhiteTE` = c("WhiteTE", "Raptor_SPEC"),
    `GriffonV` = c("GriffonV", "Raptor_SPEC"),
    `BlackV` = c("BlackV", "Raptor_SPEC"),
    `BeardedV` = c("BeardedV", "Raptor_SPEC"),
    `ShortTE` = c("Raptor_SPEC", "Raptor_SPEC"),
    `Mar` = c("Harrier_SPEC", "Raptor_SPEC", "Mar"),
    `Hen` = c("MonPalHen", "Harrier_SPEC", "Raptor_SPEC", "Hen"),
    `Pal` = c("MonPalHen", "Harrier_SPEC", "Raptor_SPEC", "Pal"),
    `Mon` = c("MonPalHen", "Harrier_SPEC", "Raptor_SPEC", "Mon"),
    `Goshawk` = c("SPH_Goshawk", "Raptor_SPEC", "Goshawk"),
    `EurasianSH` = c(
      "SPH_Goshawk",
      "SparrowH_SPEC",
      "Raptor_SPEC",
      "EurasianSH"
    ),
    `LevantSH` = c("SPH_Goshawk", "SparrowH_SPEC", "Raptor_SPEC", "LevantSH"),
    `CommonBuz` = c("Buzzard_SPEC", "MediumRaptor", "Raptor_SPEC", "CommonBuz"),
    `StepBuz` = c("Buzzard_SPEC", "MediumRaptor", "Raptor_SPEC", "StepBuz"),
    `LongLB` = c("Buzzard_SPEC", "MediumRaptor", "Raptor_SPEC", "LongLB"),
    `RoughLB` = c("Buzzard_SPEC", "Raptor_SPEC", "RoughLB"),
    `LesserSE` = c("LargeEAGLE", "Raptor_SPEC", "LesserSE"),
    `GreaterSE` = c("LargeEAGLE", "Raptor_SPEC", "GreaterSE"),
    `SteppeE` = c("LargeEAGLE", "Raptor_SPEC", "SteppeE"),
    `ImperialE` = c("LargeEAGLE", "Raptor_SPEC", "ImperialE"),
    `GoldenE` = c("LargeEAGLE", "Raptor_SPEC", "GoldenE"),
    `Osprey` = c("Raptor_SPEC", "Osprey"),
    `CommonKes` = c("Kestrel_SPEC", "Falcon_SPEC", "Raptor_SPEC", "CommonKes"),
    `RedFF` = c("Hobby_RedFF", "Falcon_SPEC", "Raptor_SPEC", "RedFF"),
    `Merlin` = c("Falcon_SPEC", "Raptor_SPEC", "Merlin"),
    `Hobby` = c("Hobby_RedFF", "Falcon_SPEC", "Raptor_SPEC", "Hobby"),
    `Peregrine` = c("LargeFALCON", "Falcon_SPEC", "Raptor_SPEC", "Peregrine"),
    `EurasianCrane` = c("Crane_SPEC", "EurasianCrane"),
    `StockD` = c("StockD", "Dove_SPEC"),
    `WoodP` = c("WoodP", "Dove_SPEC"),
    `Roller` = "Roller",
    `TurtleD` = c("TurtleD", "Dove_SPEC"),
    `OrientalTD` = c("OrientalTD", "Dove_SPEC"),
    `BWKite` = "BWKite",
    `ShortEO` = "ShortEO",
    `WhiteP` = "WhiteP",
    `DalmatianP` = "DalmatianP",
    `EgyptianV` = c("Raptor_SPEC", "EgyptianV"),
    `BootedE` = c("Raptor_SPEC", "BootedE"),
    `LesserKes` = c("Kestrel_SPEC", "Falcon_SPEC", "Raptor_SPEC", "LesserKes"),
    `EleonoraF` = c("Falcon_SPEC", "Raptor_SPEC", "EleonoraF"),
    `LannerF` = c("LargeFALCON", "Falcon_SPEC", "Raptor_SPEC", "LannerF"),
    `SakerF` = c("LargeFALCON", "Falcon_SPEC", "Raptor_SPEC", "SakerF"),
    `DemCrane` = c("Crane_SPEC", "DemCrane"),
    `CrestedHB` = c("Pernis_SPEC", "CrestedHB"),
    `Hobby_RedFF` = c("Hobby", "RedFF", "Hobby_RedFF"),
    `Kestrel_SPEC` = c("CommonKes", "LesserKes", "Kestrel_SPEC"),
    `LargeFALCON` = c("Peregrine", "SakerF", "LannerF", "LargeFALCON"),
    `MonPalHen` = c("Mon", "Pal", "Hen", "MonPalHen"),
    `Buzzard_SPEC` = c(
      "HB",
      "StepBuz",
      "CommonBuz",
      "RoughLB",
      "CrestedHB",
      "LongLB",
      "Buzzard_SPEC"
    ),
    `SparrowH_SPEC` = c("EurasianSH", "LevantSH", "SparrowH_SPEC"),
    `Stork_SPEC` = c("WhiStork", "BlaStork", "Stork_SPEC"),
    `SPH_Goshawk` = c("Goshawk", "EurasianSH", "LevantSH", "SPH_Goshawk"),
    `Harrier_SPEC` = c("Mon", "Mar", "Pal", "Hen", "MonPalHen", "Harrier_SPEC"),
    `LargeEAGLE` = c(
      "LesserSE",
      "GreaterSE",
      "SteppeE",
      "ImperialE",
      "GoldenE",
      "LargeEAGLE"
    ),
    `Falcon_SPEC` = c(
      "LargeFALCON",
      "Peregrine",
      "SakerF",
      "LannerF",
      "Hobby_RedFF",
      "Hobby",
      "RedFF",
      "LesserKes",
      "CommonKes",
      "EleonoraF",
      "Merlin",
      "Falcon_SPEC"
    ),
    `MediumRaptor` = c(
      "HB",
      "StepBuz",
      "BlackKite",
      "CommonBuz",
      "LongLB",
      "MediumRaptor"
    ),
    `Raptor_SPEC` = c(
      "BlackKite",
      "BootedE",
      "CommonBuz",
      "CommonKes",
      "CrestedHB",
      "EleonoraF",
      "EurasianSH",
      "GoldenE",
      "Goshawk",
      "GreaterSE",
      "HB",
      "Hen",
      "Hobby",
      "ImperialE",
      "LannerF",
      "LargeFALCON",
      "LesserKes",
      "LesserSE",
      "LevantSH",
      "LongLB",
      "Mar",
      "Merlin",
      "Mon",
      "MonPalHen",
      "Pal",
      "Peregrine",
      "RedFF",
      "RedKite",
      "RoughLB",
      "SakerF",
      "StepBuz",
      "SteppeE",
      "Raptor_SPEC"
    ),
    `Pernis_SPEC` = c("CrestedHB", "HB", "Pernis_SPEC")
  )
  records <- lapply(names(groups), function(group) {
    data.frame(
      group = group,
      species = groups[[group]],
      member_type = ifelse(
        groups[[group]] %in% "Crane_SPEC",
        "group",
        "species"
      ),
      rank = seq_along(groups[[group]]),
      effective_from = 2018L,
      effective_to = 9999L,
      stringsAsFactors = FALSE
    )
  })
  result <- do.call(rbind, records)
  rownames(result) <- NULL
  attr(result, "reviewed_group_nodes") <- "Crane_SPEC"
  result
}

embedded_speed_rules <- function() {
  non_soaring <- c(
    "CommonKes",
    "EleonoraF",
    "EurasianSH",
    "Falcon_SPEC",
    "Goshawk",
    "Harrier_SPEC",
    "Hen",
    "Hobby",
    "Hobby_RedFF",
    "Kestrel_SPEC",
    "LannerF",
    "LargeFALCON",
    "LesserKes",
    "LevantSH",
    "Merlin",
    "Mon",
    "MonPalHen",
    "Pal",
    "Peregrine",
    "RedFF",
    "Roller",
    "SakerF",
    "SparrowH_SPEC",
    "StockD",
    "TurtleD",
    "WoodP",
    "OrientalTD",
    "BWKite",
    "Dove_SPEC"
  )
  data.frame(
    species = c("*", non_soaring),
    speed_class = c("soaring", rep("non_soaring", length(non_soaring))),
    seconds = c(900L, rep(600L, length(non_soaring))),
    effective_from = 2018L,
    effective_to = 9999L,
    stringsAsFactors = FALSE
  )
}

embedded_protocol_window_rules <- function() {
  data.frame(
    window_id = c("standard", "harrier_early", "honey_buzzard_phase_one"),
    start_month_day = c("08-17", "08-27", "08-21"),
    end_month_day = c("10-16", "09-27", "09-10"),
    start_offset_seconds = c(3600L, 0L, 1L),
    end_offset_seconds = c(-7200L, 3600L, 1L),
    filter = c(1L, 2L, 0L),
    effective_from = c(2018L, 2018L, 2018L),
    effective_to = 9999L,
    stringsAsFactors = FALSE
  )
}

embedded_exclusion_rules <- function() {
  row <- function(
    id,
    field,
    values,
    condition,
    from,
    to,
    action = "filter_zero"
  ) {
    data.frame(
      rule_id = id,
      field = field,
      values = values,
      condition = condition,
      effective_from = from,
      effective_to = to,
      action = action,
      stringsAsFactors = FALSE
    )
  }
  do.call(
    rbind,
    list(
      row(
        "nonregular_raptors_since_2011",
        "species",
        "SparrowH_SPEC|Falcon_SPEC|Goshawk|Kestrel_SPEC|CommonKes|LesserKes|Merlin|SPH_Goshawk|EurasianSH|Hobby|Hobby_RedFF|LevantSH|RedFF",
        "always",
        2018L,
        9999L
      ),
      row("outer_east", "location", "E4|>E3", "always", 2018L, 9999L),
      row(
        "age_protocol_outer",
        "species",
        "BK_JUV|BK_NONJUV|HB_JUV|HB_NONJUV|SB_NONJUV|SB_JUV",
        "location in E2|E3|W2|W3",
        2018L,
        9999L
      )
    )
  )
}

embedded_profile_tables <- function() {
  list(
    species = embedded_species_rules(),
    aliases = embedded_alias_rules(),
    comparable_groups = embedded_comparable_rules(),
    speeds = embedded_speed_rules(),
    protocol_windows = embedded_protocol_window_rules(),
    exclusions = embedded_exclusion_rules()
  )
}
