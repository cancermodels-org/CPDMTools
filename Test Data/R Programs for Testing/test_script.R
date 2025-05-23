# Testing file
library(here)
#library(CPDMTools)
options(scipen = 999)
devtools::load_all()

# Labguru plate map
labguru_plate_data_frame <- labguru_plate_prep(
  file_path = here("Test Data", "tecan_incucyte_ctg_monotherapy",
                   "Plate_01_CPDM_2951_CSM_7549_Patient_Sample_20230803.xlsx"))
labguru_plate_data_frame <- labguru_plate_prep(
  file_path = here("Test Data", "tecan_incucyte_ctg_synergy",
                   "Plate_01_CPDM_1279X_PDCL_3DS_M01_P20_20240417.xlsx"))

# Tecan report monotherapy
file_path <- here("Test Data", "tecan_incucyte_ctg_monotherapy",
                  "DT-24.0001_Tecan_File_20230804.xlsx")
tecan_plate_data_frame <- tecan_report_prep_mono(
  file_path = here("Test Data", "tecan_incucyte_ctg_monotherapy",
                   "DT-24.0001_Tecan_Report_20230804.xlsx"))
tecan_plate_data_frame <- tecan_report_prep(
  file_path = here("Test Data", "tecan_incucyte_ctg_monotherapy",
                   "DT-24.0001_Tecan_Report_20230804.xlsx"),
  drugging_type = "Monotherapy")

# Tecan report synergy
file_path <- here("Test Data", "tecan_incucyte_ctg_synergy",
                  "DT-24.0022_Tecan_Report_20240418.xlsx")
tecan_plate_data_frame <- tecan_report_prep_syn(
  file_path = here("Test Data", "tecan_incucyte_ctg_synergy",
                   "DT-24.0022_Tecan_Report_20240418.xlsx"))
tecan_plate_data_frame <- tecan_report_prep(
  file_path = here("Test Data", "tecan_incucyte_ctg_synergy",
                   "DT-24.0022_Tecan_Report_20240418.xlsx"),
  drugging_type = "Synergy")

# Growth data
file_path <- here("Test Data", "tecan_incucyte_ctg_monotherapy",
                  "CPDM_2951_CSM_7549_Patient_Sample_Incucyte_Confluency.txt")

growth_data_frame <- growth_data_prep(
  file_path = here("Test Data", "tecan_incucyte_ctg_monotherapy",
                   "CPDM_2951_CSM_7549_Patient_Sample_Incucyte_Confluency.txt"),
  imaging_equipment = "Incucyte"
)

growth_data_frame <- growth_data_prep(
  file_path = here("Test Data", "tecan_incucyte_ctg_synergy",
                   "CPDM_1279X_Incucyte_Confluency.txt"),
  imaging_equipment = "Incucyte"
)

file_path <- here("Test Data", "cytation_file_example.txt")

growth_data_frame <- growth_data_prep(
  file_path = here("Test Data", "cytation_file_example.txt"),
  imaging_equipment = "Cytation"
)

# CTG
file_path <- here("Test Data", "tecan_incucyte_ctg_monotherapy",
                  "Plate_01_CTG_20230811.xlsx")
ctg_data_frame <- ctg_prep(
  file_path = here("Test Data", "tecan_incucyte_ctg_monotherapy",
                   "Plate_01_CTG_20230811.xlsx")
)
ctg_data_frame <- ctg_prep(
  file_path = here("Test Data", "tecan_incucyte_ctg_synergy",
                   "Plate_01_CTG_20240424.xlsx")
)

# Testing join function
joined_data <- plate_data_join(
  labguru_plate_data_frame = labguru_plate_data_frame,
  tecan_plate_data_frame = tecan_plate_data_frame,
  ctg_data_frame = ctg_data_frame
)

# Make stauro the positive control
growth_sample <- joined_data %>%
  filter(treatment_name != "DMSO 10%") %>%
  mutate(treatment_type = case_when(
    treatment_name == "Staurosporine" & concentration > 7 ~ "Positive Control",
    TRUE ~ treatment_type
  ))

write_xlsx(growth_sample,
           here("Test Data", "Growth_QC_Input_Mono.xlsx"))


# Create growth synergy data
write_xlsx(joined_data,
           here("Test Data", "ctg_qc_input_syn.xlsx"))
