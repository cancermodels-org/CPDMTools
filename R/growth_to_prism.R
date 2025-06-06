#' Convert growth dataset to PRISM shape
#'
#' @param data_frame A growth-based data frame
#'
#' @returns A dataframe object shaped in preparation for importing into PRISM
#' @importFrom dplyr arrange mutate select
#' @importFrom magrittr %>%
#' @importFrom stringr str_replace_all
#' @importFrom tidyr pivot_wider
#' @export
#'
growth_to_prism <- function(
    data_frame){

  if(all(c("treatment_name", "concentration", "well") %in%
         colnames(data_frame))){
    # Create prism shapped data
    data_prism <- data_frame %>%
      dplyr::arrange(treatment_name, concentration, well) %>%
      dplyr::mutate(
        id = paste(treatment_name, concentration, well,
                        sep = "-")) %>%
      dplyr::select(time, id, growth_metric) %>%
      tidyr::pivot_wider(names_from = id, values_from = growth_metric) %>%
      dplyr::arrange(time)

  }else if(all(c("treatment_name", "well") %in%
               colnames(data_frame))){
    # Create prism shapped data
    data_prism <- data_frame %>%
      dplyr::arrange(treatment_name, well) %>%
      dplyr::mutate(id = paste(treatment_name, well,
                        sep = "-")) %>%
      dplyr::select(time, id, growth_metric) %>%
      tidyr::pivot_wider(names_from = id, values_from = growth_metric) %>%
      dplyr::arrange(time)

  }else{
    # Create prism shapped data from wells only
    data_prism <- data_frame %>%
      dplyr::arrange(well) %>%
      tidyr::pivot_wider(names_from = well, values_from = growth_metric) %>%
      dplyr::arrange(time)
  }

  # Remove any " NA " values if applicable
  col_names <- colnames(data_prism)
  col_names <- stringr::str_replace_all(col_names, "-NA-", "-")
  names(data_prism) <- col_names

  # Return PRISM dataset
  return(data_prism)

}
