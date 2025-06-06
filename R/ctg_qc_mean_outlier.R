#' Detect outlier with z-scores (mean/sd)
#'
#' @param ctg_data A list object (ctg_list) or data frame with CTG data
#' @param z_score_threshold A numeric value specifying the z-score threshold to
#' use for determining outliers. Defaults to 3.
#'
#' @returns A data frame object
#' @importFrom dplyr case_when filter group_by left_join mutate select summarise
#' @importFrom magrittr %>%
#' @importFrom stats sd
#' @export
#'
ctg_qc_mean_outlier <- function(
    ctg_data,
    z_score_threshold = 3){

  # If input is ctg_list, extract data_frame
  if(is.list(ctg_data) & !is.data.frame(ctg_data)){
    data_frame <- ctg_data[[1]]
  }else{
    data_frame <- ctg_data
  }

  # Filter out any wells already marked as outliers
  data_temp <- data_frame %>%
    dplyr::filter(
      (outlier_manual_yn != "Yes" |
         is.na(outlier_manual_yn)))

  # Filter data to controls
  mean_values <- data_temp %>%
    dplyr::group_by(treatment_name, concentration) %>%
    dplyr::summarise(ctg_value_mean = mean(value, na.rm = TRUE),
                     ctg_value_sd = stats::sd(value, na.rm = TRUE))

  # Join mean/sd
  data_frame <- data_frame %>%
    dplyr::left_join(mean_values)

  # Calculate z-scores
  data_frame <- data_frame %>%
    dplyr::mutate(z_score = (value - ctg_value_mean)/abs(ctg_value_sd))

  # Mark outliers based on z_score_threshold
  data_frame <- data_frame %>%
    dplyr::mutate(
      outlier_auto_yn_new = dplyr::case_when(
        abs(z_score) > z_score_threshold ~ "Yes"
      ),
      outlier_auto_flag_reason_new = dplyr::case_when(
        abs(z_score) > z_score_threshold ~
          paste("Mean/SD Outlier - Threshold of", z_score_threshold)
      ),
      outlier_auto_yn = dplyr::case_when(
        outlier_auto_yn_new == "Yes" ~ "Yes",
        TRUE ~ outlier_auto_yn
      ),
      outlier_auto_flag_reason = dplyr::case_when(
        outlier_auto_yn_new == "Yes" ~ outlier_auto_flag_reason_new,
        TRUE ~ outlier_auto_flag_reason
      )
    ) %>%
    dplyr::select(-ctg_value_mean, -ctg_value_sd,
                  -outlier_auto_yn_new, -outlier_auto_flag_reason_new,
                  -z_score)

  # Return data_frame
  return(data_frame)

}
