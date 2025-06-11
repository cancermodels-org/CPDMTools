#' Create QC output list for growth datasets
#'
#' @param ctg_list A list object containing ctg data
#' @param outlier_manual_only A logical value that when TRUE filters outliers in
#' terms of the outlier_manual_yn variable, if FALSE is will consider outliers
#' as those annotated by both the outlier_manual_yn and the outlier_auto_yn.
#' Defaults to TRUE.
#' @param prism A logical value specifying whether to add a dataset
#' for importing into GraphPad PRISM. Defaults to TRUE.
#'
#' @returns A list object
#' @importFrom dplyr anti_join any_of contains filter select
#' @importFrom magrittr %>%
#' @export
ctg_qc_output <- function(
    ctg_list,
    outlier_manual_only = TRUE,
    prism = TRUE){
  # Extract data frame from ctg_list
  data_frame <- ctg_list[[1]]

  # Remove temporary columns created from QC
  data_frame <- data_frame %>%
    dplyr::select(-dplyr::any_of(c(
      "color")))

  # Create initial clean long dataset
  if (outlier_manual_only == TRUE) {
    data_clean <- data_frame %>%
      dplyr::filter((outlier_manual_yn != "Yes" |
                       is.na(outlier_manual_yn)))
    data_outlier <- data_frame %>%
      dplyr::anti_join(data_clean)

  } else {
    data_clean <- data_frame %>%
      dplyr::filter((outlier_manual_yn != "Yes" |
                       is.na(outlier_manual_yn) |
                       outlier_auto_yn != "Yes"))
    data_outlier <- data_frame %>%
      dplyr::anti_join(data_clean)
  }

  # Remove outlier columns from data_clean
  data_clean <- data_clean %>%
    dplyr::select(-dplyr::contains("outlier"))

  # Initiate output list object
  output_list <- list(
    "clean_data" = data.frame(data_clean),
    "outliers" = data.frame(data_outlier)
  )

  # Create PRISM dataset if applicable
  if(prism == TRUE){
    prism <- ctg_to_prism(data_clean)

    # Append output list
    output_list[[length(output_list) + 1]] <- prism
    names(output_list)[length(output_list)] <- "prism"
  }

  # Return the output_list
  return(output_list)


}
