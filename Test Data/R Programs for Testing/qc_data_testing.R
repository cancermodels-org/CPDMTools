library(readxl)
library(here)
library(tidyverse)
library(devtools)
library(usethis)
#library(CPDMTools)
devtools::load_all()
options(scipen = 999)

# Import growth data (mono therapy)
joined_growth_data_frame <- read_excel(
  here("Test Data", "qc_rshiny_input_files","growth_qc_input_mono.xlsx")
)

# Add columns
joined_growth_data_frame <- joined_growth_data_frame %>%
  mutate(outlier_auto_yn = "No",
         outlier_auto_flag_reason = NA,
         outlier_manual_yn = NA,
         outlier_manual_flag_reason = NA)

# Add outliers
joined_growth_data_frame[c(75, 125, 26, 1250),"growth_metric"] <- 25
joined_growth_data_frame[which(joined_growth_data_frame$time == 0),"outlier_manual_yn"] <- "Yes"

# Round concentration
joined_growth_data_frame <- round_concentration(
  joined_growth_data_frame,
  round_by = 4,
  use_nearest_10 = TRUE)

# Create color pallete
joined_growth_data_frame <- color_palette_mono(joined_growth_data_frame)

data_frame <- loess_outlier_fit(
  data_frame = joined_growth_data_frame,
  span_value = 0.3,
  residual_threshold = 3)

data_frame <- loess_outlier_fit(
  data_frame = data_frame,
  span_value = 0.3,
  residual_threshold = 3)

data_temp <- data_frame %>%
  filter(treatment_name == "AMG232")

growth_plot_qc_mono(data_frame = data_frame,
                    treatment_name = "AMG232",
                    growth_metric_name = "Confluency",
                    show_outlier = TRUE,
                    show_only_outlier_wells = TRUE,
                    make_interactive = FALSE)


## CTG

# Import CTG data
data_frame <- read_excel(
  here("Test Data", "qc_rshiny_input_files",
       "ctg_qc_input_mono.xlsx")
)

data_frame <- data_frame %>%
  mutate(outlier_auto_yn = "No",
         outlier_auto_flag_reason = as.character(NA),
         outlier_manual_yn = NA,
         outlier_manual_flag_reason = as.character(NA))

data_frame <- color_palette_mono(data_frame)

data_frame <- round_concentration(
  data_frame = data_frame,
  round_by = 4,
  use_nearest_10 = TRUE)

ctg_qc_control_plot(
  data_frame = data_frame,
  show_outlier = TRUE,
  make_interactive = TRUE)

data_frame <- ctg_normalize(
  data_frame = data_frame,
  use_positive_control = TRUE)

data_frame[which(data_frame$well %in% c("D11", "C11")), "outlier_manual_yn"] <- "Yes"

#data_frame <- data_frame %>%
#  filter(treatment_name == "AMG232")

# Large outlier
data_frame[which(data_frame$well == "G22"), "value_norm"] <- 0.2
# Medium outlier
data_frame[which(data_frame$well == "K12"), "value_norm"] <- 0.4
# Small outlier
data_frame[which(data_frame$well == "D17"), "value_norm"] <- 0.4

data_frame[which(data_frame$well %in% c("G22","K12","D17")), "outlier_manual_yn"] <- "Yes"

data_frame <- ctg_qc_mean_outlier(
  ctg_data = data_frame,
  z_score_threshold = 3
)
#data_frame[which(data_frame$well == "D17"), "outlier_manual_yn"] <- "No"
#data_frame[which(data_frame$well == "D11"), "outlier_manual_yn"] <- "No"

# Raw data plot check
ctg_qc_treat_plot_raw(
  ctg_data = data_frame,
  treat_name = "AMG232",
  make_interactive = TRUE
)

ctg_list <- dr4pl_qc_fit_loop(
  ctg_data = data_frame,
  method_init = "logistic",
  method_robust = "Huber"
)
View(ctg_list$model_parameters)
View(ctg_list$data_frame)

unique(ctg_list$model_parameters$treatment_name)
vec <- unique(ctg_list$model_parameters$treatment_name)
ctg_qc_treat_plot(
  ctg_list = ctg_list,
  treat_name = vec[2],
  show_outlier = TRUE,
  make_interactive = TRUE
)


prism <- ctg_to_prism(ctg_list[[1]])

output <- ctg_qc_output(ctg_list = ctg_list)

# Prepare analysis RShiny input files
ctg_data <- read_excel(
  here("Test Data", "qc_rshiny_input_files",
       "endpoint_assay_qc_input.xlsx")
)

ctg_data <- ctg_data %>%
  mutate(outlier_auto_yn = "No",
         outlier_auto_flag_reason = as.character(NA),
         outlier_manual_yn = NA,
         outlier_manual_flag_reason = as.character(NA))

ctg_data <- ctg_data %>%
  round_concentration(
    round_by = 4,
    use_nearest_10 = TRUE
  )

ctg_data <- ctg_normalize(
  data_frame = ctg_data,
  use_positive_control = TRUE)

ctg_data <- ctg_data %>%
  select(well, treatment_name, treatment_type,
         concentration, value, value_norm)

writexl::write_xlsx(
  ctg_data,
  here("Test Data","analysis_rshiny_input_files",
       "endpoint_assay_analysis_input.xlsx")
)


