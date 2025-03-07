library(readxl)
library(here)
library(tidyverse)
library(devtools)
library(usethis)
library(CPDMTools)
devtools::load_all()

# Import growth data (mono therapy)
joined_growth_data_frame <- read_excel(
  here("Test Data", "qc_rshiny_input_files","growth_qc_input_mono.xlsx")
)

# Round concentration
joined_growth_data_frame <- round_concentration(joined_growth_data_frame)

joined_data_frame <- joined_growth_data_frame %>%
  select(-treatment_name, -concentration)
joined_data_frame[2, "well_annotation"] <- "TMZ - 1"

new_data <- well_annotate_transfer(joined_data_frame)
