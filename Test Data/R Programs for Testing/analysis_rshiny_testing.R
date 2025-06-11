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
  max_dss = 100
)
View(ctg_list$model_parameters)
View(ctg_list$table)




