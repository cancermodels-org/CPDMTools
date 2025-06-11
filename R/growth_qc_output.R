#' Create QC output list for growth datasets
#'
#' @param data_frame A growth data frame object.
#' @param outlier_manual_only A logical value that when TRUE filters outliers in
#' terms of the outlier_manual_yn variable, if FALSE is will consider outliers
#' as those annotated by both the outlier_manual_yn and the outlier_auto_yn.
#' Defaults to TRUE.
#' @param growthcurveme A logical value specifying whether to add a dataset
#' for importing into the GrowthCurveME RShiny. Defaults to TRUE.
#' @param lgrscore A logical value specifying whether to add a dataset
#' for importing into the LGRscore RShiny. Defaults to TRUE.
#' @param prism A logical value specifying whether to add a dataset
#' for importing into GraphPad PRISM. Defaults to TRUE.
#'
#' @returns A list object
#' @importFrom dplyr anti_join any_of filter rename select
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect
#' @export
#'
growth_qc_output <- function(
    data_frame,
    outlier_manual_only = TRUE,
    growthcurveme = TRUE,
    lgrscore = TRUE,
    prism = TRUE) {
  # Remove temporary columns created from QC
  data_frame <- data_frame %>%
    dplyr::select(-dplyr::any_of(c(
      "color", "pred_growth_metric",
      "resid_value", "stand_resid_value"
    )))

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

  # Create growthcurveme dataset if applicable
  if (growthcurveme == TRUE) {
    if (any(stringr::str_detect(data_clean$treatment_type, "Media"))) {
      growthcurveme <- data_clean %>%
        dplyr::filter(
          stringr::str_detect(treatment_type, "Media")
        ) %>%
        dplyr::rename(cluster = well)
    } else {
      growthcurveme <- data_clean %>%
        dplyr::filter(
          stringr::str_detect(treatment_type, "Negative")
        ) %>%
        dplyr::rename(cluster = well)
    }

    # Append output list
    output_list[[length(output_list) + 1]] <- data.frame(growthcurveme)
    names(output_list)[length(output_list)] <- "growthcurveme"
  }

  # Create lgrscore dataset if applicable
  if (lgrscore == TRUE) {
    lgrscore <- data_clean %>%
      dplyr::filter(
        treatment_type %in% c(
          "Negative Control", "Monotherapy",
          "Positive Control"
        )
      )

    # Append output list
    output_list[[length(output_list) + 1]] <- data.frame(lgrscore)
    names(output_list)[length(output_list)] <- "lgrscore"
  }

  # Create PRISM dataset if applicable
  if(prism == TRUE){
    prism <- growth_to_prism(data_clean)

    # Append output list
    output_list[[length(output_list) + 1]] <- prism
    names(output_list)[length(output_list)] <- "prism"
  }

  # Return the output_list
  return(output_list)

}
