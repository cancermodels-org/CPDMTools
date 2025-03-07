#' Transfer well_annotations
#'
#' @param joined_data_frame The joined data frame created by the data wrangling
#' R Shiny when the Labguru plate map has been imported
#'
#' @returns The joined data frame
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate relocate
#' @importFrom stringr str_detect str_split_i str_squish
#' @export
#'
#' @examples
#' joined_data <- well_annotate_transfer(joined_data_frame = joined_data_frame)
well_annotate_transfer <- function(joined_data_frame) {
  stopifnot("well_annotation" %in% colnames(joined_data_frame))

  if (!"treatment_name" %in% colnames(joined_data_frame)) {
    joined_data_frame <- joined_data_frame %>%
      dplyr::mutate(treatment_name = NA)
  }
  if (!"concentration" %in% colnames(joined_data_frame)) {
    joined_data_frame <- joined_data_frame %>%
      dplyr::mutate(concentration = NA)
  }


  for (a in 1:nrow(joined_data_frame)) {
    if (!is.na(joined_data_frame$well_annotation[a]) &
      stringr::str_detect(joined_data_frame$well_annotation[a], "-")) {
      # Extract annotation
      value <- joined_data_frame$well_annotation[a]

      # Extract treatment_name
      treatment <- stringr::str_split_i(value, "-", 1)
      treatment <- stringr::str_squish(treatment)

      # Extract concentration
      conc <- stringr::str_split_i(value, "-", 2)
      conc <- stringr::str_squish(conc)
      conc <- as.numeric(conc)

      # Override treatment_name and concentration values, convert treatment_type
      # to Monotherapy
      joined_data_frame$treatment_name[a] <- treatment
      joined_data_frame$concentration[a] <- conc
      joined_data_frame$treatment_type[a] <- "Monotherapy"

      # Remove temporary objects
      rm(value, treatment, conc)
    }
  }

  # Relocate concentration treatment_name and concentration if applicable
  joined_data_frame <- joined_data_frame %>%
    dplyr::relocate(treatment_name, .before = treatment_type) %>%
    dplyr::relocate(concentration, .after = treatment_type)

  # Return updated joined_data_frame
  return(joined_data_frame)
}
