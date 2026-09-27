#' Get correct season parameters based on a year as input
#'
#' @param yyyy Year (integer) in YYYY
#'
#' @export
get_season_params <- function(yyyy) {
  # Monitoring period
  if (yyyy > 2018) {
    season_start <- lubridate::ymd("2025-08-12")
    season_end <- lubridate::ymd("2025-10-21")
  } else {
    season_start <- lubridate::ymd("2018-08-17")
    season_end <- lubridate::ymd("2018-10-16")
  }
  lubridate::year(season_start) <- yyyy
  lubridate::year(season_end) <- yyyy

  # Honey Buzzard focus period
  hb_focus_start <- lubridate::ymd("2025-08-21")
  hb_focus_end <- lubridate::ymd("2025-09-09")
  lubridate::year(hb_focus_start) <- yyyy
  lubridate::year(hb_focus_end) <- yyyy
  return(c(
    "season_start" = season_start,
    "season_end" = season_end,
    "hb_focus_start" = hb_focus_start,
    "hb_focus_end" = hb_focus_end
  ))
}

# Time window to use when checking records
window_minutes <- 5 # Minutes

# Overlap zones
overlapping_zones <- list(
  `1. Sakhalvasho` = list(
    W3 = c("W3"),
    W2 = c("W3"),
    W1 = c("W3"),
    O = c("W3"),
    E1 = c("W3"),
    E2 = c("W3", "W2"),
    E3 = c("W3", "W2", "W1", "O", "E1", "E2")
  ),
  `2. Shuamta` = list(
    W3 = c("W2", "W1", "O", "E1", "E2", "E3"),
    W2 = c("E3", "E2"),
    W1 = c("E3"),
    O = c("E3"),
    E1 = c("E3"),
    E2 = c("E3"),
    E3 = c("E3")
  )
)

# Expected species-age-sex combinations
# - NULL in 'age' or 'sex' means it is NOT expected (i.e., value should be NA in the data)
# - Include NA_character_ in vectors where NA is allowed as a valid 'missing' option
expected_combinations <- list(
  BK = list(age = NULL, sex = NULL),
  BK_JUV = list(age = NULL, sex = NULL),
  BK_NONJUV = list(age = NULL, sex = NULL),
  BlackV = list(age = c("J", "I", "A", "Non-Juv", NA_character_), sex = NULL),
  BlaStork = list(age = c("J", "I", "A", "Non-Juv", NA_character_), sex = NULL),
  BootedE = list(age = c("J", "Non-Juv", NA_character_), sex = NULL),
  Buzzard_SPEC = list(age = NULL, sex = NULL),
  CrestedHB = list(age = c("J", "A"), sex = c("M", "F")),
  DalmatianP = list(
    age = c("J", "I", "A", "Non-Juv", NA_character_),
    sex = NULL
  ),
  DemCrane = list(age = c("J", "A", "I", "Non-Juv", NA_character_), sex = NULL),
  Dove_SPEC = list(age = NULL, sex = NULL),
  EgyptianV = list(age = c("J", "I", "A", "Non-Juv"), sex = NULL),
  EleonoraF = list(
    age = c("J", "I", "A", "Non-Juv", NA_character_),
    sex = NULL
  ),
  EuCrane = list(age = c("J", "A", "I", "Non-Juv", NA_character_), sex = NULL),
  GoldenE = list(age = c("J", "I", "A", "Non-Juv"), sex = NULL),
  GreaterSE = list(age = c("J", "I", "A", "Non-Juv"), sex = NULL),
  GriffonV = list(age = c("J", "I", "A", "Non-Juv", NA_character_), sex = NULL),
  Harrier_SPEC = list(age = NULL, sex = NULL),
  HB = list(age = NULL, sex = NULL),
  HB_JUV = list(age = NULL, sex = NULL),
  HB_NONJUV = list(age = NULL, sex = c("M", "F", NA_character_)),
  Hen = list(
    # list of (age, sex) pairs
    list("J", NA_character_),
    list("I", "M"),
    list("A", "M"),
    list("Non-Juv", "M"),
    list("I", "F"),
    list("A", "F"),
    list("Non-Juv", "F"),
    list(NA_character_, "FC"),
    list(NA_character_, NA_character_)
  ),
  ImperialE = list(age = c("J", "I", "A"), sex = NULL),
  Kite_SPEC = list(age = NULL, sex = NULL),
  Lanner = list(age = c("J", "I", "A", "Non-Juv", NA_character_), sex = NULL),
  `Large EAGLE` = list(
    age = c("J", "I", "A", "Non-Juv", NA_character_),
    sex = NULL
  ),
  `Large FALCON` = list(age = NULL, sex = NULL),
  LesserSE = list(age = c("J", "I", "A", "Non-Juv"), sex = NULL),
  LongLB = list(age = c("J", "I", "A", "Non-Juv", NA_character_), sex = NULL),
  Marsh = list(
    list("J", NA_character_),
    list("I", "M"),
    list("A", "M"),
    list("Non-Juv", "M"),
    list("I", "F"),
    list("A", "F"),
    list("Non-Juv", "F"),
    list(NA_character_, "FC"),
    list(NA_character_, NA_character_)
  ),
  MediumRaptor = list(age = NULL, sex = NULL),
  Mon = list(
    list("J", NA_character_),
    list("I", "M"),
    list("A", "M"),
    list("Non-Juv", "M"),
    list("I", "F"),
    list("A", "F"),
    list("Non-Juv", "F")
  ),
  MonPalHen = list(
    list("J", NA_character_),
    list("Non-Juv", "M"),
    list("Non-Juv", "F"),
    list(NA_character_, "FC"),
    list(NA_character_, NA_character_)
  ),
  OrientalTD = list(age = NULL, sex = NULL),
  Osprey = list(
    age = c("J", "I", "A", "Non-Juv", NA_character_),
    sex = c("M", "F", NA_character_)
  ),
  Pal = list(
    list("J", NA_character_),
    list("I", "M"),
    list("A", "M"),
    list("Non-Juv", "M"),
    list("I", "F"),
    list("A", "F"),
    list("Non-Juv", "F")
  ),
  Peregrine = list(
    age = c("J", "I", "A", "Non-Juv", NA_character_),
    sex = NULL
  ),
  Raptor_SPEC = list(age = NULL, sex = NULL),
  RedKite = list(age = NULL, sex = NULL),
  Roller = list(age = NULL, sex = NULL),
  SakerF = list(age = c("J", "I", "A", "Non-Juv"), sex = NULL),
  ShortTE = list(age = c("J", "I", "A", "Non-Juv", NA_character_), sex = NULL),
  StepBuz = list(age = c("J", "I", "A", "Non-Juv", NA_character_), sex = NULL),
  SteppeE = list(age = c("J", "I", "A"), sex = NULL),
  StockD = list(age = NULL, sex = NULL),
  Stork_SPEC = list(age = NULL, sex = NULL),
  TurtleD = list(age = NULL, sex = NULL),
  WhiteP = list(age = c("J", "I", "A", "Non-Juv", NA_character_), sex = NULL),
  WhiStork = list(age = c("J", "A", "Non-Juv", NA_character_), sex = NULL),
  WhiteTE = list(age = c("J", "I", "A", "Non-Juv"), sex = NULL),
  WoodP = list(age = NULL, sex = NULL)
)
