#' DR4PL Curve Fitting Loop for CTG QC
#'
#' @inheritParams dr4pl_qc_fit
#' @param data_frame A data frame with CTG data
#'
#' @returns A list object
#' @importFrom dplyr arrange bind_rows filter
#' @importFrom magrittr %>%
#' @export
#'
dr4pl_qc_fit_loop <- function(
    data_frame,
    concentration_unit = "uM",
    method_init = "logistic",
    method_robust = "Huber",
    lb_if_min_gt = 0.3,
    ub_if_max_lt = 0.8){

  # Loop through each treatment and calculate 4-parameters curves and outleirs
  data_temp <- data_frame %>%
    dplyr::filter(treatment_type == "Monotherapy")

  treatments <- unique(data_temp$treatment_name)
  for(a in 1:length(treatments)){
    # Subset data
    data_subset <- data_frame %>%
      dplyr::filter(treatment_name == treatments[a])
    # Fit four parameter curves and outlier detection
    data_list <- dr4pl_qc_fit(
      data_frame = data_subset,
      treatment_name = treatments[a],
      concentration_unit = concentration_unit,
      method_init = method_init,
      method_robust = method_robust,
      lb_if_min_gt = lb_if_min_gt,
      ub_if_max_lt = ub_if_max_lt
    )

    # Combine datasets
    if(a == 1){
      data_output <- data_list[[1]]
      data_model <- data_list[[2]]
    }else{
      data_output <- data_output %>%
        dplyr::bind_rows(data_list[[1]])
      data_model <- data_model %>%
        dplyr::bind_rows(data_list[[2]])
    }
    rm(data_subset, data_list)
  }
  rm(a)

  # Create combined original data set with modifications
  data_combined <- data_frame %>%
    dplyr::filter(!treatment_name %in% treatments) %>%
    dplyr::bind_rows(data_output)

  # Arrange data
  data_combined <- data_combined %>%
    dplyr::arrange(treatment_name, concentration)
  data_model <- data_model %>%
    arrange(treatment_name)

  # Create output list
  output_list <- list(
    "data_frame" = data_combined,
    "model_parameters"= data_model
  )

  # Return output list
  return(output_list)

}
