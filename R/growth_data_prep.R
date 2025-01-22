#' Import and prep Incucyte/Cytation data
#'
#' @param file_path
#' @param imaging_equipment
#' @param growth_metric_type
#' @param time_unit
#'
#' @returns
#' @export
#'
#' @examples
growth_data_prep <- function(
    file_path = "",
    imaging_equipment = "Incucyte",
    growth_metric_type = "confluency",
    time_unit = "hours") {
  ## Incucyte data import and prep
  if (imaging_equipment == "Incucyte") {
    # Create initial row number to skip
    start_row_skip <- 0
    growth_file <- readr::read_delim(
      file_path,
      skip = start_row_skip,
    )
    # Check if first column name is Date Time, if not re-import data
    # skipping by 1 row
    while (colnames(growth_file)[1] != "Date Time") {
      start_row_skip <- start_row_skip + 1
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
        values_to = "growth"
      ) %>%
      dplyr::select(well, Elapsed, growth) %>%
      dplyr::rename(
        !!paste0("growth_", growth_metric_type) := growth,
        !!paste0("time_", time_unit) := Elapsed
      )

    ## Cytation data import and prep
  } else {
    # Create initial row number to skip
    start_row_skip <- 0
    growth_file <- readr::read_tsv(
      file_path,
      skip = start_row_skip,
    )
    # Check if first column name is Time, if not re-import data
    # skipping by 1 row
    while (colnames(growth_file)[1] != "Time") {
      start_row_skip <- start_row_skip + 1
      growth_file <- readr::read_delim(
        file_path,
        skip = start_row_skip,
      )
    }

    # Clean growth_file columns
    growth_file <- growth_file %>%
      dplyr::filter_all(dplyr::any_vars(. != "?????")) %>%
      dplyr::select_if(~ !all(. == "?????")) %>%
      dplyr::mutate_at(dplyr::vars(-1), as.numeric) %>%
      dplyr::select(Time, where(is.numeric))

    # Convert to long dataset
    growth_file <- growth_file %>%
      dplyr::rename(well = "Time") %>%
      tidyr::pivot_longer(
        cols = !c("well"),
        names_to = "time",
        values_to = "growth"
      )

    # Clean time variables and round the hour if minutes are >= 30 mark
    growth_file <- growth_file %>%
      dplyr::mutate(
        # Remove trailing numbers after "..."
        time = stringr::str_remove(time, "\\.\\.\\..*"),
        hour = as.numeric(stringr::str_split_i(time, "\\:", 1)),
        minute = as.numeric(stringr::str_split_i(time, "\\:", 2)),
        time_num = dplyr::case_when(
          minute < 30 ~ hour,
          TRUE ~ hour + 1
        )
      ) %>%
      dplyr::select(
        well, !!paste0("time_", time_unit) := time_num,
        !!paste0("growth_", growth_metric_type) := growth
      )
  }

  # Return growth file
  return(growth_file)
}
