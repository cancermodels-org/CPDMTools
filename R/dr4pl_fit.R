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
#' @inheritParams dss_calculator
#'
#' @importFrom dplyr across case_when contains mutate select
#' @importFrom dr4pl dr4pl
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @importFrom stats formula
#' @importFrom tidyr pivot_longer
#' @export
#'
dr4pl_fit <- function(
    data_frame,
    treatment_name = "treatment",
    concentration_unit = "uM",
    method_init = "logistic",
    method_robust = "squared",
    lb_if_min_gt = 0.3,
    ub_if_max_lt = 0.8,
    readout = "activity",
    activity_threshold = 10,
    dss_type = 3,
    log_transform = TRUE,
    slope_threshold = 0.1,
    max_dss = 100) {
  # Initiate null model
  model <- NULL

  # Perform 4-parameter curve fitting
  if (min(data_frame$value_norm) > lb_if_min_gt |
    max(data_frame$value_norm) < ub_if_max_lt) {
    converged <- FALSE
    bound_threshold <- 0.2

    # Loop through and continue to attempt to fit bounded model until converged
    # or if bound_threshold except 0.5
    while (!converged && bound_threshold <= 0.5) {
      try_result <- try(
        {
          model <- dr4pl::dr4pl(
            formula = value_norm ~ concentration,
            data = data_frame,
            method.init = method_init,
            method.robust = method_robust,
            lowerl = c(-Inf, -Inf, -Inf, min(data_frame$value_norm) - bound_threshold),
            upperl = c(max(data_frame$value_norm) + bound_threshold, Inf, Inf, Inf)
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
      data = data_frame,
      method.init = method_init,
      method.robust = method_robust
    )
  }

  # Stop execution if model did not converge
  if (is.null(model)) {
    warning(paste0(
      "Warning: Model did not converge for treatment ",
      treatment_name
    ))
    model_parameters <- tibble::tibble(
      treatment_name = treatment_name,
      function_type = "4-param logistic",
      num_obs = nrow(data_frame),
      num_unique_conc = length(unique(data_frame$concentration)),
      min_conc = min(data_frame$concentration, na.rm = TRUE),
      max_conc = max(data_frame$concentration, na.rm = TRUE),
      conc_unit = concentration_unit,
      rel_ic50_est = NA,
      rel_ic50_lb = NA,
      rel_ic50_ub = NA,
      slope_est = NA,
      slope_lb = NA,
      slope_ub = NA,
      lower_asy_est = NA,
      lower_asy_lb = NA,
      lower_asy_ub = NA,
      upper_asy_est = NA,
      upper_asy_lb = NA,
      upper_asy_ub = NA
    )
  } else {
    # Get summary of parameters
    sum_model <- summary(model)$coefficients

    # Extract model parameters
    model_parameters <- tibble::tibble(
      treatment_name = treatment_name,
      function_type = "4-param logistic",
      num_obs = nrow(data_frame),
      num_unique_conc = length(unique(data_frame$concentration)),
      min_conc = min(data_frame$concentration, na.rm = TRUE),
      max_conc = max(data_frame$concentration, na.rm = TRUE),
      conc_unit = concentration_unit,
      rel_ic50_est = 10^sum_model[2, "Estimate"],
      rel_ic50_lb = 10^sum_model[2, "2.5 %"],
      rel_ic50_ub = 10^sum_model[2, "97.5 %"],
      slope_est = sum_model[3, "Estimate"] * -1,
      slope_lb = sum_model[3, "97.5 %"] * -1,
      slope_ub = sum_model[3, "2.5 %"] * -1,
      lower_asy_est = sum_model[4, "Estimate"],
      lower_asy_lb = sum_model[4, "2.5 %"],
      lower_asy_ub = sum_model[4, "97.5 %"],
      upper_asy_est = sum_model[1, "Estimate"],
      upper_asy_lb = sum_model[1, "2.5 %"],
      upper_asy_ub = sum_model[1, "97.5 %"]
    )

    # Calculate DSS score
    dss_score <- dss_calculator(
      data_frame = model_parameters,
      readout = readout,
      activity_threshold = activity_threshold,
      dss_type = dss_type,
      concentration_unit = concentration_unit,
      log_transform = log_transform,
      slope_threshold = slope_threshold,
      max_dss = max_dss
    )

    # Add DSS score to model parameters
    model_parameters <- model_parameters %>%
      dplyr::mutate(dss3 = dss_score)

    # Clean up output by rounding estimates to 8 decimals and bounding IC50
    # when extraneously large
    model_parameters <- model_parameters %>%
      dplyr::mutate(
        dplyr::across(rel_ic50_est:dss3, ~ round(., 8))
      ) %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::contains("ic50"), ~ dplyr::case_when(
            . > 999999 ~ 999999,
            TRUE ~ .
          )
        )
      )

    # Calculate max decimal of concentration variable
    max_dec_conc <- as.numeric(max_decimals(data_frame$concentration))

    # Create long dataset for table
    model_summary_long <- model_parameters %>%
      dplyr::mutate(
        "Treatment name" = treatment_name,
        "Function type" = function_type,
        "Number observations" = as.character(num_obs),
        "Number unique concentrations" = as.character(num_unique_conc),
        !!paste0("Concentration range (", concentration_unit, ")") :=
          paste0(min_conc, " - ", max_conc),
        "Upper asymptote estimate [95% CI]" = paste0(
          round(upper_asy_est, 2),
          " [", round(upper_asy_lb, 2),
          ",", round(upper_asy_ub, 2),
          "]"
        ),
        "Lower asymptote estimate [95% CI]" = paste0(
          round(lower_asy_est, 2),
          " [", round(lower_asy_lb, 2),
          ",", round(lower_asy_ub, 2),
          "]"
        ),
        "Slope estimate [95% CI]" = paste0(
          round(slope_est, 3),
          " [", round(slope_lb, 3),
          ",", round(slope_ub, 3),
          "]"
        ),
        "Relative IC50 estimate [95% CI]" = paste0(
          round(rel_ic50_est, max_dec_conc),
          " [", round(rel_ic50_lb, max_dec_conc),
          ",", round(rel_ic50_ub, max_dec_conc),
          "]"
        ),
        "Drug Sensitivity Score (DSS3)" =
          as.character(round(dss3, 2))
      ) %>%
      dplyr::select(
        "Treatment name", "Function type", "Number observations",
        "Number unique concentrations",
        !!paste0("Concentration range (", concentration_unit, ")"),
        "Upper asymptote estimate [95% CI]",
        "Lower asymptote estimate [95% CI]",
        "Slope estimate [95% CI]",
        "Relative IC50 estimate [95% CI]",
        "Drug Sensitivity Score (DSS3)"
      ) %>%
      tidyr::pivot_longer(
        cols = everything(),
        names_to = "Variable",
        values_to = "Value"
      ) %>%
      dplyr::mutate(
        treatment_name = treatment_name
      ) %>%
      dplyr::select(treatment_name, Variable, Value)
  }

  # Create list object
  output_list <- list(
    "data_frame" = data_frame,
    "model_parameters" = model_parameters,
    "table" = model_summary_long
  )

  # Return list object
  return(output_list)
}
