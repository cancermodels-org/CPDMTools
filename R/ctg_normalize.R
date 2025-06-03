#' Normalize CTG dataa
#'
#' @param data_frame A data frame of CTG data
#' @param use_positive_control A logical value specifying whether to normalize
#' by the postive control as well. Defaults to TRUE.
#'
#' @returns A data frame object with normalized ctg data
#' @importFrom dplyr filter mutate summarise pull
#' @importFrom magrittr %>%
#' @export
#'
ctg_normalize <- function(
    data_frame,
    use_positive_control = TRUE){
  # Remove outliers based on manual selection
  data_temp <- data_frame %>%
    dplyr::filter(
      (outlier_manual_yn != "Yes" |
        is.na(outlier_manual_yn))
    )

  # Calculate control summary statistic for negative controls
  avg_neg_control <- data_temp %>%
    dplyr::filter(treatment_type == "Negative Control") %>%
    dplyr::summarise(mean = mean(ctg_value, na.rm = TRUE)) %>%
    dplyr::pull(mean) %>%
    as.numeric()

  if(use_positive_control == FALSE){
    # Normalize CTG values by negative control only
    data_temp <- data_temp %>%
      dplyr::mutate(ctg_value_norm = (ctg_value / avg_neg_control))
  }else{
    # Calculate control summary statistic for positive controls
    avg_pos_control <- data_temp %>%
      dplyr::filter(treatment_type == "Positive Control") %>%
      dplyr::summarise(mean = mean(ctg_value, na.rm = TRUE)) %>%
      dplyr::pull(mean) %>%
      as.numeric()

    # Normalize CTG values by negative and positive controls
    data_temp <- data_temp %>%
      dplyr::mutate(
        ctg_value_norm = (ctg_value - avg_pos_control) / (avg_neg_control - avg_pos_control)
      )
  }

  # Return data
  return(data_temp)

}
