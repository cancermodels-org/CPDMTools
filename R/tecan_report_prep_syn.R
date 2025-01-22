#' Import and prep tecan report file - synergy
#'
#' @param file_path A character string specifying the full file path to the
#' Tecan report file for synergy-based experiments (inclusive of the tecan
#' report excel file name)
#' @param concentration_units A character string specifying the concentration
#' units. Values incluide "molar", "millimolar", "micromolar", "nonomolar",
#' and "picomolar". Defaults to "micromolar".
#'
#' @returns A data frame of the prepared tecan report data
#' @importFrom magrittr %>%
#' @importFrom readxl read_excel
#' @importFrom janitor make_clean_names
#' @importFrom stringr str_detect str_remove_all str_remove str_squish
#' str_replace_all str_to_upper str_split_i
#' @importFrom dplyr select filter mutate case_when contains arrange
#' bind_rows distinct
#' @importFrom tidyr pivot_longer
#' @export
#'
#' @examples
#' tecan_report_prep_syn(
#' file_path = "",
#' concentration_units = "micromolar")
tecan_report_prep_syn <- function(
    file_path = "",
    concentration_units = "micromolar"){
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
        concentration, dplyr::contains("conc_"), dmso_percent
      )
  } else {
    data_frame <- data_frame %>%
      dplyr::select(
        plate, dispensed_well, dispensed_row,
        dispensed_col, treatment_name = well_contents,
        concentration = single_fluid_concentration,
        dplyr::contains("conc_"), dmso_percent
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

  # Add DMSO controls and filter out empty wells
  data_frame <- data_frame %>%
    dplyr::mutate(treatment_name = dplyr::case_when(
      (dmso_percent < 0.01 &
         is.na(treatment_name)) ~ "DMSO 0.5%",
      (dmso_percent > 0.095 &
         is.na(treatment_name)) ~ "DMSO 10%",
      TRUE ~ treatment_name
    ),
    treatment_name = stringr::str_to_upper(treatment_name)) %>%
    dplyr::filter(!is.na(treatment_name))

  # Separate data
  mono_data <- data_frame %>%
    dplyr::filter(treatment_name != "2_FLUIDS")
  syn_data <- data_frame %>%
    dplyr::filter(treatment_name == "2_FLUIDS")

  # Convert synergy data into a long dataset by the conc_ variables
  syn_data <- syn_data %>%
    dplyr::select(plate, well, dplyr::contains("conc_"),
                  dmso_percent) %>%
    tidyr::pivot_longer(
      cols = !c(
        "plate", "well", "dmso_percent"
      ),
      names_to = "treatment_name",
      values_to = "concentration"
    )

  # Clean treatment_name, filter out concentrations = 0, arrange data
  syn_data <- syn_data %>%
    dplyr::mutate(
      treatment_name = stringr::str_split_i(treatment_name, "_", 4),
      treatment_name = stringr::str_to_upper(treatment_name)) %>%
    dplyr::filter(concentration != 0) %>%
    dplyr::arrange(plate, well, treatment_name)

  # Create wide dataset based on plate and well replicates
  syn_data_wide <- syn_data %>%
    dplyr::select(plate, well) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      treatment_name = NA,
      treatment_type = "Combotherapy",
      combo_drug1 = NA,
      combo_drug2 = NA,
      combo_conc1 = NA,
      combo_conc2 = NA
    )

  # Loop through and fill in syn_data_wide
  for(a in 1:nrow(syn_data_wide)){
    # Subset syn_data
    data_temp <- syn_data %>%
      dplyr::filter(plate == syn_data_wide$plate[a],
             well == syn_data_wide$well[a])

    # Fill in variables
    syn_data_wide$treatment_name[a] <- paste(data_temp$treatment_name,
                                             collapse = " - ")
    syn_data_wide$combo_drug1[a] <- data_temp$treatment_name[1]
    syn_data_wide$combo_drug2[a] <- data_temp$treatment_name[2]
    syn_data_wide$combo_conc1[a] <- data_temp$concentration[1]
    syn_data_wide$combo_conc2[a] <- data_temp$concentration[2]

    # Remove temporary dataset
    rm(data_temp)
  }

  # Merge mono_data and syn_data_wide, select variables
  tecan_data <- mono_data %>%
    dplyr::mutate(
      treatment_type = dplyr::case_when(
        treatment_name == "DMSO 0.5%" ~ "Negative Control",
        treatment_name == "DMSO 10%" ~ "Positive Control",
        TRUE ~ "Monotherapy"
      )
    ) %>%
    dplyr::select(plate, well, treatment_name, treatment_type,
                  concentration) %>%
    dplyr::bind_rows(syn_data_wide) %>%
    dplyr::arrange(plate, well)

  # Rename concentration variable based on input units
  if(concentration_units == "molar"){
    tecan_data <- tecan_data %>%
      dplyr::rename(concentration_m = concentration)
  }
  if(concentration_units == "millimolar"){
    tecan_data <- tecan_data %>%
      dplyr::rename(concentration_mm = concentration)
  }
  if(concentration_units == "micromolar"){
    tecan_data <- tecan_data %>%
      dplyr::rename(concentration_um = concentration)
  }
  if(concentration_units == "nanomolar"){
    tecan_data <- tecan_data %>%
      dplyr::rename(concentration_nm = concentration)
  }
  if(concentration_units == "picomolar"){
    tecan_data <- tecan_data %>%
      dplyr::rename(concentration_pm = concentration)
  }

  # Return tecan data frame
  return(tecan_data)

}
