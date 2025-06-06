#' Import and prep CTG data
#'
#' @param file_path A character string specifying the full file path to the
#' CTG excel file file (inclusive of the full CTG .xlsx file name)
#' @param equipment A character string specifying which CTG machine was used to
#' generate the xlsx file. Values include "GloMax Explorer" or "SpectraMAX iD3".
#' Defaults to "GloMax Explorer".
#'
#' @returns A data frame of the prepared CTG data
#' @importFrom dplyr bind_rows contains filter mutate na_if select rename
#' @importFrom janitor clean_names
#' @importFrom magrittr %>%
#' @importFrom readxl read_excel
#' @importFrom stringr str_replace_all
#' @importFrom tidyr drop_na pivot_longer
#' @export
#'
#' @examples
#' ctg_prep(file_path = "")
ctg_prep <- function(
    file_path = "",
    equipment = "GloMax Explorer"){

  if(equipment == "GloMax Explorer"){
    # Import ctg data file
    data_frame <- readxl::read_excel(
      file_path,
      sheet = "Results By Well",
      skip = 5)

    # Rename column names (add if else depending on ncol due to multiple plates)
    data_frame[1, 1] <- colnames(data_frame)[1]
    data_frame <- janitor::clean_names(data_frame)
    data_frame <- data_frame %>%
      dplyr::select(1, value)
    names(data_frame)[1] <- "well"

    # Fill in missing well id's by filling down the proper well to the row
    for (a in 1:nrow(data_frame)) {
      if (is.na(data_frame$well[a])) {
        data_frame$well[a] <- data_frame$well[a - 1]
      }
    }
    rm(a)

    # Drop missing values and convert data to proper formats
    ctg_data <- data_frame %>%
      dplyr::mutate(
        value = dplyr::na_if(value, "value"),
        value = as.numeric(value),
        well = stringr::str_replace_all(well, ":", "")
      ) %>%
      tidyr::drop_na(value) %>%
      dplyr::rename(value = value)

  }else{
    # Load Excel file
    data <- readxl::read_excel(
      file_path,
      guess_max = 10000,
      col_names = FALSE
    )

    # Get all row indices where a new plate begins
    plate_name_pos <- which(data$...1 == "Plate name")
    plate_names <- as.character(data$...2[plate_name_pos])

    # Loop through and clean plate
    for (a in 1:length(plate_names)) {
      # Import data starting at plate map
      data_temp <- readxl::read_excel(
        file_path,
        guess_max = 10000,
        skip = plate_name_pos[a] + 36
      )

      # Find the first fully blank row
      blank_row_idx <- which(apply(data_temp, 1, function(x) all(is.na(x))))

      # If a blank row is found, trim data_temp
      if (length(blank_row_idx) > 0) {
        data_temp <- data_temp[1:(blank_row_idx[1] - 1), ]
      }

      # Clean and transpose data
      data_long <- data_temp %>%
        dplyr::select(-dplyr::contains("Wave")) %>%
        dplyr::rename(row = "-/0 nm") %>%
        tidyr::pivot_longer(
          cols = !"row",
          names_to = "column",
          values_to = "value"
        ) %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::mutate(
          ctg_plate_number = as.numeric(a),
          ctg_plate_name = plate_names[a],
          well = paste0(row, column)
        ) %>%
        dplyr::select(
          ctg_plate_number, ctg_plate_name,
          well, value
        )

      # Bind data
      if (a == 1) {
        ctg_data <- data_long
      } else {
        ctg_data <- ctg_data %>%
          dplyr::bind_rows(data_long)
      }

      # Remove temporary objects
      rm(data_temp, data_long)
    }
  }

  # Return the CTG Data
  return(ctg_data)

}
