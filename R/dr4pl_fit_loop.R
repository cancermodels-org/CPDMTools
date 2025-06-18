#' DR4PL Curve Fitting Loop for Data Analysis
#'
#' @param data_frame A data frame with ctg monotherapy treatment data
#' @inheritParams dr4pl_fit
#'
#' @returns A list object to be used for plotting, displaying tables, and
#' exporting
#' @importFrom dplyr arrange bind_rows filter
#' @importFrom magrittr %>%
#' @export
#'
dr4pl_fit_loop <- function(
    data_frame,
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
    max_dss = 100){

  # Loop through each treatment and calculate 4-parameters curves and outleirs
  data_frame <- data_frame %>%
    dplyr::filter(treatment_type == "Monotherapy")

  treatments <- unique(data_frame$treatment_name)
  for(a in 1:length(treatments)){
    # Subset data
    data_subset <- data_frame %>%
      dplyr::filter(treatment_name == treatments[a])
    # Perform 4-parameter curve analysis
    data_list <- dr4pl_fit(
      data_frame = data_subset,
      treatment_name = treatments[a],
      concentration_unit = concentration_unit,
      method_init = method_init,
      method_robust = method_robust,
      lb_if_min_gt = lb_if_min_gt,
      ub_if_max_lt = ub_if_max_lt,
      readout = readout,
      activity_threshold = activity_threshold,
      dss_type = dss_type,
      log_transform = log_transform,
      slope_threshold = slope_threshold,
      max_dss = max_dss
    )

    # Combine datasets
    if(a == 1){
      data_monotherapy <- data_list[[1]]
      data_model <- data_list[[2]]
      data_table <- data_list[[3]]
    }else{
      data_monotherapy <- data_monotherapy %>%
        dplyr::bind_rows(data_list[[1]])
      data_model <- data_model %>%
        dplyr::bind_rows(data_list[[2]])
      data_table <- data_table %>%
        dplyr::bind_rows(data_list[[3]])
    }
    rm(data_subset, data_list)
  }
  rm(a)

  # Arrange data
  data_monotherapy <- data_monotherapy %>%
    dplyr::arrange(treatment_name)
  data_model <- data_model %>%
    dplyr::arrange(treatment_name)
  data_table <- data_table %>%
    dplyr::arrange(treatment_name)

  # Create output list
  output_list <- list(
    "model_parameters" = data_model,
    "table" = data_table,
    "monotherapy" = data_monotherapy
  )

  # Return output list
  return(output_list)

}
