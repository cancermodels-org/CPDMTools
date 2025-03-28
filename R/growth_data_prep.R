#' Import and prep Incucyte/Cytation data
#'
#' @param file_path A character string specifying the full file path to the
#' Incucyte/Cytation text file (inclusive of the full growth .txt file name)
#' @param imaging_equipment A character string specifying the machine in which
#' the file was generated from. Values include "Incucyte" or "Cytation".
#' Defaults to Cytation
#'
#' @returns A data frame of the prepared growth data
#' @importFrom dplyr any_vars case_when mutate mutate_at rename
#' select select_if vars
#' @importFrom magrittr %>%
#' @importFrom data.table fread
#' @importFrom stringr str_remove str_split_i
#' @importFrom tidyr pivot_longer
#' @export
#'
#' @examples
#' growth_data_prep(file_path = "",
#' imaging_equipment = "Incucyte")
growth_data_prep <- function(
    file_path = "",
    imaging_equipment = "Incucyte") {
  ## Incucyte data import and prep
  if (imaging_equipment == "Incucyte") {
    # Create initial row number to skip
    start_row_skip <- 0
    growth_file <- data.table::fread(
      input = file_path,
      skip = start_row_skip,
      data.table = FALSE)
    # Check if first column name is Date Time, if not re-import data
    # skipping by 1 row
    while (colnames(growth_file)[1] != "Date Time") {
      start_row_skip <- start_row_skip + 1

      # Break while loop if not achieved by 8
      if (start_row_skip == 8) {
        stop(
          paste0("Error: 'Date Time' column not found after 8 skipped rows.",
                 "Check function inputs or input data.")
             )
      }

      growth_file <- readr::read_delim(
        file_path,
        skip = start_row_skip,
      )
    }

    # Convert to long dataset, select and rename variables
    growth_file <- growth_file %>%
      tidyr::pivot_longer(
        cols = !c("Date Time", "Elapsed"),
        names_to = "well",
        values_to = "growth_metric"
      ) %>%
      dplyr::mutate(treatment_period_yn = "No") %>%
      dplyr::select(well, treatment_period_yn, Elapsed, growth_metric) %>%
      dplyr::rename(time = Elapsed)

    ## Cytation data import and prep
  } else {
    # Create initial row number to skip
    start_row_skip <- 0
    growth_file <- data.table::fread(
      input = file_path,
      skip = start_row_skip,
      data.table = FALSE)
    # Check if first column name is Time, if not re-import data
    # skipping by 1 row
    while (colnames(growth_file)[1] != "Time") {
      start_row_skip <- start_row_skip + 1

      # Break while loop if not achieved by 8
      if (start_row_skip == 8) {
        stop(
          paste0("Error: 'Time' column not found after 8 skipped rows.",
                 "Check function inputs or input data.")
        )
      }

      growth_file <- data.table::fread(
        input = file_path,
        skip = start_row_skip,
        data.table = FALSE)
    }

    # Clean growth_file columns
    growth_file <- growth_file %>%
      dplyr::select(where(~ any(!is.na(.) & . != "?????"))) %>%
      dplyr::mutate_at(dplyr::vars(-1), as.numeric) %>%
      dplyr::select(Time, where(is.numeric))

    # Convert to long dataset
    growth_file <- growth_file %>%
      dplyr::rename(well = "Time") %>%
      tidyr::pivot_longer(
        cols = !c("well"),
        names_to = "time",
        values_to = "growth_metric"
      )

    # Clean time variables and round the hour if minutes are >= 30 mark
    growth_file <- growth_file %>%
      dplyr::mutate(
        time = stringr::str_remove(time, "\\.\\.\\..*"),
        hour = as.numeric(stringr::str_split_i(time, "\\:", 1)),
        minute = as.numeric(stringr::str_split_i(time, "\\:", 2)),
        time_num = dplyr::case_when(
          minute < 30 ~ hour,
          TRUE ~ hour + 1
        )
      ) %>%
      dplyr::mutate(treatment_period_yn = "No") %>%
      dplyr::select(
        well, treatment_period_yn, time = time_num, growth_metric)
  }

  # Return growth file
  return(growth_file)
}
