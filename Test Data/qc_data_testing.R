library(readxl)
library(here)
library(devtools)
library(usethis)
devtools::load_all()

# Import growth data (mono therapy)
joined_growth_data_frame <- read_excel(
  here("Test Data", "qc_rshiny_input_files","growth_qc_input_mono.xlsx")
)

# Round concentration
joined_growth_data_frame <- round_concentration(joined_growth_data_frame)
