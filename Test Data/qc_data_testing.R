library(readxl)
library(here)
library(tidyverse)
library(devtools)
library(usethis)
library(CPDMTools)
library(scales)
devtools::load_all()
options(scipen = 999)

# Import growth data (mono therapy)
joined_growth_data_frame <- read_excel(
  here("Test Data", "qc_rshiny_input_files","growth_qc_input_mono.xlsx")
)

# Round concentration
joined_growth_data_frame <- round_concentration(joined_growth_data_frame)

# Import growth data (combo therapy)
joined_growth_data_frame <- read_excel(
  here("Test Data", "qc_rshiny_input_files","growth_qc_input_syn.xlsx")
)
joined_growth_data_frame <- round_concentration(
  joined_growth_data_frame,
  round_by = 3)
