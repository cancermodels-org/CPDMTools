#' Import and prep tecan report file - monotherapy
#'
#' @param file_path A character string specifying the full file path to the
#' Tecan report file for monotherapy-based experiments (inclusive of the tecan
#' report excel file name)
#' @param concentration_units A character string specifying the concentration
#' units. Values incluide "molar", "millimolar", "micromolar", "nonomolar",
#' and "picomolar". Defaults to "micromolar".
#'
#' @returns A data frame of the prepared tecan report data
#' @importFrom magrittr %>%
#' @importFrom readxl read_excel
#' @importFrom janitor make_clean_names
#' @importFrom dplyr case_when filter mutate rename select
#' @importFrom stringr str_remove str_remove_all str_replace_all str_squish
#' @export
#'
#' @examples
#' tecan_report_prep_mono(
#' file_path = "",
#' concentration_units = "micromolar")
tecan_report_prep_mono <- function(
    file_path = "",
    concentration_units = "micromolar") {
  # Import tecan plate map
  data_frame <- readxl::read_excel(
    file_path,
    sheet = "Tabular",
    .name_repair = function(names) {
      janitor::make_clean_names(
        names,
        replace = c("\u00b5" = "u", "%" = "percent")
      )
    },
    guess_max = 10000
  )

  # Select relevant columns based on tecan input
  if (any(stringr::str_detect(colnames(data_frame), "fluid_name"))) {
    data_frame <- data_frame %>%
      dplyr::select(
        plate, dispensed_well, dispensed_row,
        dispensed_col, treatment_name = fluid_name,
        concentration, dmso_percent
      )
  } else {
    data_frame <- data_frame %>%
      dplyr::select(plate, dispensed_well, dispensed_row,
                    dispensed_col, treatment_name = well_contents,
                    concentration = single_fluid_concentration,
                    dmso_percent
      )
  }

  # Create letter version of dispensed_row and store in dispensed_row_ch
  data_frame <- data_frame %>%
    dplyr::mutate(
      dispensed_row_ch = stringr::str_remove_all(dispensed_well, "\\d"),
      well = paste(dispensed_row_ch, dispensed_col, sep = ""))

  # Clean treatment_name variable
  data_frame <- data_frame %>%
    dplyr::mutate(
      treatment_name = stringr::str_remove(treatment_name, "[:digit:][:digit:]mM"),
      treatment_name = stringr::str_remove(treatment_name, "[:digit:]mM"),
      treatment_name = stringr::str_remove(treatment_name, "- 1:3"),
      treatment_name = stringr::str_squish(treatment_name),
      treatment_name = stringr::str_replace_all(treatment_name, " ", "_"))

  # # Add DMSO control wells when applicable
  data_frame <- data_frame %>%
    dplyr::mutate(
      treatment_name = dplyr::case_when(
        (dmso_percent < 0.01 &
           is.na(treatment_name)) ~ "DMSO 0.5%",
        (dmso_percent > 0.095 &
           is.na(treatment_name)) ~ "DMSO 10%",
        TRUE ~ treatment_name
      ),
      treatment_type = dplyr::case_when(
        treatment_name == "DMSO 0.5%" ~ "Negative Control",
        treatment_name == "DMSO 10%" ~ "Positive Control",
        TRUE ~ "Monotherapy"
      )
    ) %>%
    dplyr::filter(!is.na(treatment_name))

  # Select relevant variables in order
  data_frame <- data_frame %>%
    dplyr::select(plate, well, treatment_name, treatment_type,
                  concentration, dmso_percent)

  # Rename concentration variable based on input units
  if(concentration_units == "molar"){
    data_frame <- data_frame %>%
      dplyr::rename(concentration_m = concentration)
  }
  if(concentration_units == "millimolar"){
    data_frame <- data_frame %>%
      dplyr::rename(concentration_mm = concentration)
  }
  if(concentration_units == "micromolar"){
    data_frame <- data_frame %>%
      dplyr::rename(concentration_um = concentration)
  }
  if(concentration_units == "nanomolar"){
    data_frame <- data_frame %>%
      dplyr::rename(concentration_nm = concentration)
  }
  if(concentration_units == "picomolar"){
    data_frame <- data_frame %>%
      dplyr::rename(concentration_pm = concentration)
  }

  # Return the prepared tecan data
  return(data_frame)

}
