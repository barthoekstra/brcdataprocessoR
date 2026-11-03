#' Write data file to .xlsx format
#'
#' @param data Dataframe
#' @param folder Output folder
#' @param split_by_station Boolean; If TRUE split by station
#' @param overwrite Boolean; FALSE by default.
#'
#' @returns This function is used for the side-effect of writing a file
#' @export
#'
write_data <- function(
  data,
  folder,
  split_by_station = TRUE,
  overwrite = FALSE
) {
  date_out <- unique(lubridate::date(data$datetime))
  if (length(date_out) > 1) {
    stop("Can't write multiple dates at once.")
  }
  datestring_out <- format(date_out, "%Y%m%d")

  # Convert datetimes to strings
  data$datetime <- format(data$datetime, "%Y-%m-%d %H:%M:%S", trim = TRUE)

  if (split_by_station) {
    files <- file.path(folder, paste0(datestring_out, c("_S1.xlsx", "_S2.xlsx")))
    outputs <- list(
      subset(data, data$telpost == "1. Sakhalvasho"),
      subset(data, data$telpost == "2. Shuamta")
    )
  } else {
    files <- file.path(folder, paste0(datestring_out, ".xlsx"))
    outputs <- list(data)
  }
  if (any(file.exists(files)) && !overwrite) {
    stop("Files already exist.")
  }

  # Finish both workbooks before replacing any existing review files.
  staged <- vapply(files, function(path) {
    tempfile(tmpdir = dirname(path), fileext = ".xlsx")
  }, character(1))
  on.exit(unlink(staged), add = TRUE)
  for (i in seq_along(files)) {
    writexl::write_xlsx(outputs[[i]], staged[[i]])
  }
  for (i in seq_along(files)) {
    if (!file.rename(staged[[i]], files[[i]])) {
      stop("Could not move workbook to ", files[[i]], call. = FALSE)
    }
  }
}
