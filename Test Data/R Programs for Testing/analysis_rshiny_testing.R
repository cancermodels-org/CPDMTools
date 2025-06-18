# Testing of Analysis RShiny input files
library(tidyverse)
library(readxl)
library(writexl)
library(devtools)
library(usethis)
library(here)
options(scipen = 999)
devtools::load_all()

# Import CTG data
ctg_input <- read_excel(
  here("Test Data", "analysis_rshiny_input_files",
       "endpoint_assay_analysis_input.xlsx")
)

# Perform CTG analysis
ctg_list <- dr4pl_fit_loop(
  data_frame = ctg_input,
  concentration_unit = "µM",
  method_init = "logistic",
  method_robust = "squared",
  lb_if_min_gt = 0.3,
  ub_if_max_lt = 0.8,
  readout = "activity",
  activity_threshold = 10,
  dss_type = 3,
  log_transform = TRUE,
  slope_threshold = 0.1,
  max_dss = 100
)
View(ctg_list$model_parameters)
View(ctg_list$table)

# Create overall plot
ctg_treat_plot_all(
  ctg_list = ctg_list,
  x_scale = "log",
  display_type = "se_bars"
)

# Create overall table
ctg_summary_table_all(
  ctg_list = ctg_list,
  show = "both"
)

# Create dss bar chart
ctg_dss_barplot(
  ctg_list = ctg_list
)

# Create ic50 forest gplot
ctg_ic50_forest_plot(
  ctg_list = ctg_list,
  x_scale = "log",
  n_x_axis_breaks = 8
)


# Create individual plot
ctg_treat_plot_ind(
  ctg_list = ctg_list,
  treat_name = "Drug_C",
  display_type = "points",
  y_limits = c(0, 2)
)

# Create individual table
ctg_summary_table_ind(
  ctg_list = ctg_list,
  treat_name = "Drug_B"
)





## Growth data
growth_data <- read_excel(
  here("Test Data", "qc_rshiny_input_files","growth_assay_qc_input.xlsx")
)
growth_data <- round_concentration(growth_data, 4)
growth_data[which(growth_data$time >= 30), "treatment_period_yn"] <- "Yes"

write_xlsx(
  growth_data,
  here("Test Data", "analysis_rshiny_input_files",
       "growth_assay_analysis_input.xlsx")
)


growth_data <- color_palette_mono(growth_data)
data_frame <- growth_data

growth_analysis_treat_plot(
  data_frame = growth_data,
  treatment_name = "Drug_C",
  show_controls = TRUE,
  display_metric = "mean_se",
  make_interactive = FALSE,
  growth_metric_name = "growth_metric",
  time_units = "hours",
  concentration_units = "µM"
)

growth_analysis_control_plot(
  data_frame = growth_data,
  treatment_name = "DMSO 0.5%",
  display_metric = "wells",
  make_interactive = FALSE,
  growth_metric_name = "growth_metric",
  time_units = "hours",
  concentration_units = "µM"
)





