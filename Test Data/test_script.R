# Testing file
library(here)
library(magrittr)
options(scipen = 999)

# Labguru plate map
plate <- labguru_plate_prep(
  file_path = here("Test Data", "tecan_incucyte_ctg_monotherapy",
                   "Plate_01_CPDM_2951_CSM_7549_Patient_Sample_20230803.xlsx"))

# Tecan report monotherapy
file_path <- here("Test Data", "tecan_incucyte_ctg_monotherapy",
                  "DT-24.0001_Tecan_File_20230804.xlsx")
tecan_mono <- tecan_report_prep_mono(
  file_path = here("Test Data", "tecan_incucyte_ctg_monotherapy",
                   "DT-24.0001_Tecan_File_20230804.xlsx"),
  concentration_units = "micromolar"
)

# Tecan report synergy
file_path <- here("Test Data", "tecan_incucyte_ctg_synergy",
                  "DT-24.0022_Tecan_Report_20240418.xlsx")
tecan_syn <- tecan_report_prep_syn(
  file_path = here("Test Data", "tecan_incucyte_ctg_synergy",
                   "DT-24.0022_Tecan_Report_20240418.xlsx"),
  concentration_units = "micromolar"
)

# Growth data
file_path <- here("Test Data", "tecan_incucyte_ctg_monotherapy",
                  "CPDM_2951_CSM_7549_Patient_Sample_Incucyte_Confluency.txt")

file_path <- here("Test Data", "cytation_file_example.txt")




