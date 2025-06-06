#' Convert ctg dataset to PRISM shape
#'
#' @param data_frame A ctg data frame object
#'
#' @returns A dataframe that is shaped for copying and pasting into PRISM
#' @importFrom dplyr arrange distinct filter mutate bind_rows select
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_wider
#' @export
#'
ctg_to_prism <- function(
    data_frame){

  if(all(c("treatment_name", "concentration", "well") %in%
         colnames(data_frame))){
    # Arrange data
    data_frame <- data_frame %>%
      dplyr::arrange(treatment_name, concentration, well)

    # Create data frame of unique conditions
    data_count <- data_frame %>%
      dplyr::distinct(treatment_name, concentration)

    # Loop through and add replicate numbers
    for(a in 1:nrow(data_count)){
      # Subset data and add replicate number
      if (is.na(data_count$concentration[a])) {
        data_temp <- data_frame %>%
          dplyr::filter(treatment_name == data_count$treatment_name[a],
                 is.na(concentration)) %>%
          dplyr::mutate(replicate = row_number())
      } else {
        data_temp <- data_frame %>%
          dplyr::filter(treatment_name == data_count$treatment_name[a],
                 concentration == data_count$concentration[a]) %>%
          dplyr::mutate(replicate = row_number())
      }

      # Append output dataset
      if(a == 1){
        data_replicate <- data_temp
      }else{
        data_replicate <- data_replicate %>%
          dplyr::bind_rows(data_temp)
      }

      rm(data_temp)
    }

    # Create PRISM dataset
    data_prism_v1 <- data_replicate %>%
      dplyr::filter(!is.na(concentration)) %>%
      dplyr::select(treatment_name, concentration, value_norm, replicate) %>%
      dplyr::arrange(treatment_name, desc(concentration)) %>%
      tidyr::pivot_wider(
        names_from = c("treatment_name", "replicate"),
        names_sep = "-",
        values_from = c("value_norm"),
        id_cols = "concentration"
      )

    # If there are any treatments where concentration is NA, re-shape and bind
    # them with data_prism_v1 to append them at end of dataset
    if(any(is.na(data_replicate$concentration))){
      data_prism_v2 <- data_replicate %>%
        dplyr::filter(is.na(concentration)) %>%
        dplyr::select(treatment_name, concentration, value_norm, replicate) %>%
        dplyr::arrange(treatment_name) %>%
        tidyr::pivot_wider(
          names_from = c("treatment_name", "replicate"),
          names_sep = "-",
          values_from = c("value_norm"),
          id_cols = "concentration"
        )
      # Bind datasets together
      data_prism <- data_prism_v1 %>%
        dplyr::bind_rows(data_prism_v2)
    }else{
      data_prism <- data_prism_v1
    }
  }

  # Return PRISM dataset
  return(data_prism)

}
