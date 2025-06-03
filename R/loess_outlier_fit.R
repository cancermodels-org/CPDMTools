#' Loess outlier detection
#'
#' @param data_frame A data frame with at minimum the columns well, time, and
#' growth_metric
#' @param span_value A LOESS regression parameter that controls the level of
#' smoothing that occurs across the data. Defaults to 0.3.
#' @param residual_threshold A threshold for determining the absolute value
#' of a standardized residual from the loess regression to mark as an outlier.
#' Defaults to 3.
#'
#' @returns The input data frame with additional columns for the predicted
#' growth_metric, residuals, and outlier_auto_yn
#' @importFrom broom augment
#' @importFrom dplyr arrange bind_cols group_by group_modify mutate
#' row_number select ungroup
#' @importFrom magrittr %>%
#' @importFrom stats loess loess.control
#' @export
#'
#' @examples
#' data_clean <- loess_outlier_fit(data_frame, span_value = 0.3)
loess_outlier_fit <- function(
    data_frame,
    span_value = 0.3,
    residual_threshold = 3) {
  data_frame <- data_frame %>%
    dplyr::arrange(well, time)

  fit_loess_and_detect <- function(df_well) {
    # Ensure outlier_auto_yn and outlier_auto_flag_reason columns exist
    if (!"outlier_auto_yn" %in% names(df_well)) {
      df_well$outlier_auto_yn <- "No"
    }
    if (!"outlier_auto_flag_reason" %in% names(df_well)) {
      df_well$outlier_auto_flag_reason <- NA_character_
    }

    # Start with the working dataset excluding existing outliers
    df_working <- df_well %>%
      dplyr::filter(
        (outlier_manual_yn != "Yes" |
           is.na(outlier_manual_yn))
        )

    repeat {
      # Check if enough points for LOESS
      if (nrow(df_working) < 3) break

      # Fit LOESS regression model
      loess_model <- stats::loess(
        growth_metric ~ time,
        data = df_working,
        span = span_value,
        control = stats::loess.control(surface = "direct")
      )

      # Get prediction, calculate stand residuals, check outlier threshold
      aug <- broom::augment(loess_model, newdata = df_well)
      stand_resid <- scale(aug$.resid)[, 1]
      is_outlier <- abs(stand_resid) > residual_threshold

      # Filter and check if there are no more outliers
      new_outlier_idx <- which(is_outlier & df_well$outlier_auto_yn == "No")
      if (length(new_outlier_idx) == 0) break

      # Index and mark outliers
      first_outlier_row <- new_outlier_idx[1]
      df_well$outlier_auto_yn[first_outlier_row] <- "Yes"
      df_well$outlier_auto_flag_reason[first_outlier_row] <-
        paste("LOESS Outlier - Threshold of", residual_threshold)

      point_to_remove <- df_well[first_outlier_row, c("time", "growth_metric")]
      df_working <- df_working %>%
        dplyr::filter(!(time == point_to_remove$time &
          growth_metric == point_to_remove$growth_metric))
    }

    # Final model for predictions
    if (nrow(df_working) >= 3) {
      final_loess <- stats::loess(
        growth_metric ~ time,
        data = df_working,
        span = span_value,
        control = stats::loess.control(surface = "direct")
      )
      final_aug <- broom::augment(final_loess, newdata = df_well)

      df_well <- df_well %>%
        dplyr::mutate(
          pred_growth_metric = final_aug$.fitted,
          resid_value = final_aug$.resid,
          stand_resid_value = scale(final_aug$.resid)[, 1]
        )
    } else {
      df_well <- df_well %>%
        dplyr::mutate(
          pred_growth_metric = NA,
          resid_value = NA,
          stand_resid_value = NA
        )
    }

    return(df_well)
  }

  # Clean and return data frame
  cleaned_data <- data_frame %>%
    dplyr::group_by(well) %>%
    dplyr::group_modify(~ fit_loess_and_detect(.x)) %>%
    dplyr::ungroup()

  return(cleaned_data)
}
