#' DR4PL Curve Fitting for CTG QC
#'
#' @param data_frame A data frame of CTG data with a single treatment.
#' @param treatment_name A character string specifying the name of a treatment.
#' @param concentration_unit A character string specifying the concentration
#' units. Defaults to uM.
#' @param method_init Method of obtaining initial values of the parameters.
#' If this parameter is left unassigned, a default "Mead" method will be used.
#' Assign "logistic" to use the logistic method.
#' @param method_robust Parameter to select loss function for the robust
#' estimation method to be used to fit a model. The argument NULL indicates
#' the sum of squares loss, "absolute" indicates the absolute deviation loss,
#' "Huber" indicates Huber's loss and "Tukey" indicates Tukey's biweight loss.
#' @param lb_if_min_gt Threshold to fit the model using boundaries if the
#' minimum normalized value is greater than the designated threshold. Defaults to
#' 0.3.
#' @param ub_if_max_lt Threshold to fit the model using boundaries if the
#' maximum normalized value is less than the designated threshold. Defaults to
#' 0.8.
#'
#' @returns A list object
#' @importFrom dplyr case_when filter left_join mutate select
#' @importFrom dr4pl IC dr4pl
#' @importFrom tibble tibble
#' @export
#'
dr4pl_qc_fit <- function(
    data_frame,
    treatment_name = "treatment",
    concentration_unit = "uM",
    method_init = "logistic",
    method_robust = "Huber",
    lb_if_min_gt = 0.3,
    ub_if_max_lt = 0.8) {
  # Create max deximal function (Remove later)
  max_decimals <- function(x) {
    x <- x[!is.na(x) & is.finite(x)]

    if (!is.numeric(x)) stop("Input must be numeric")
    if (length(x) == 0) return(NA_integer_)

    decimals <- sapply(x, function(val) {
      if (val %% 1 == 0) return(0)
      dec_str <- sub("^[^.]*\\.", "", sub("0+$", "", as.character(val)))
      nchar(dec_str)
    })

    max(decimals)
  }

  # Remove outliers based on manual selection
  data_temp <- data_frame %>%
    dplyr::filter(
      (outlier_manual_yn != "Yes" |
        is.na(outlier_manual_yn))
    )

  # Calculate max decimal of concentration variable
  max_dec_conc <- max_decimals(data_temp$concentration)

  # Initiate null model
  model <- NULL

  # Perform 4-parameter curve fitting
  if (min(data_temp$value_norm) > lb_if_min_gt |
    max(data_temp$value_norm) < ub_if_max_lt) {
    converged <- FALSE
    bound_threshold <- 0.2

    # Loop through and continue to attempt to fit bounded model until converged
    # or if bound_threshold except 0.5
    while (!converged && bound_threshold <= 0.5) {
      try_result <- try(
        {
          model <- dr4pl::dr4pl(
            formula = value_norm ~ concentration,
            data = data_temp,
            method.init = method_init,
            method.robust = method_robust,
            lowerl = c(-Inf, -Inf, -Inf, min(data_temp$value_norm) - bound_threshold),
            upperl = c(max(data_temp$value_norm) + bound_threshold, Inf, Inf, Inf)
          )
        },
        silent = TRUE
      )

      # Check if model converged, if not increase bound_threshold by 0.10
      if (!inherits(try_result, "try-error")) {
        converged <- TRUE
      } else {
        bound_threshold <- bound_threshold + 0.10
      }
    }
  # Fit unbound model
  } else {
    model <- dr4pl::dr4pl(
      formula = value_norm ~ concentration,
      data = data_temp,
      method.init = method_init,
      method.robust = method_robust
    )
  }

  # Stop execution if model did not converge
  if (is.null(model)) {
    stop("Error: Model did not converge")
  }


  # Get summary of parameters
  sum_model <- summary(model)$coefficients

  # Extract model parameters
  model_parameters <- tibble::tibble(
    treatment_name = treatment_name,
    function_type = "4-param logistic",
    num_obs = nrow(data_temp),
    num_unique_conc = length(unique(data_temp$concentration)),
    min_conc = min(data_temp$concentration, na.rm = TRUE),
    max_conc = max(data_temp$concentration, na.rm = TRUE),
    conc_unit = concentration_unit,
    slope_est = sum_model[3, "Estimate"]*-1,
    lower_asy_est = sum_model[4, "Estimate"],
    upper_asy_est = sum_model[1, "Estimate"],
    rel_ic50_est = dr4pl::IC(model, 50)
  )

  # Get potential outliers
  data_temp[model$idx.outlier, "outlier_auto_yn"] <- "Yes"
  data_temp[model$idx.outlier, "outlier_auto_flag_reason"] <-
    paste0("dr4pl potential outlier - ", method_robust, " method")

  # Rename outlier variables
  data_temp <- data_temp %>%
    dplyr::select(
      well,
      outlier_auto_yn_new = outlier_auto_yn,
      outlier_auto_flag_reason_new = outlier_auto_flag_reason
    )

  # Join with original dataset
  data_frame <- data_frame %>%
    dplyr::left_join(data_temp) %>%
    dplyr::mutate(
      outlier_auto_yn =
        dplyr::case_when(
          outlier_auto_yn_new == "Yes" ~ "Yes",
          TRUE ~ outlier_auto_yn),
      outlier_auto_flag_reason =
        dplyr::case_when(
          outlier_auto_yn_new == "Yes" ~ outlier_auto_flag_reason_new,
          TRUE ~ outlier_auto_flag_reason)
    ) %>%
    dplyr::select(-outlier_auto_yn_new, -outlier_auto_flag_reason_new)

  # Return list object
  data_list <- list(
    "data_frame" = data_frame,
    "model_parameters" = model_parameters
  )

  return(data_list)
}
