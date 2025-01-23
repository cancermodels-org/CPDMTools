#' Import and prep Labguru plate map
#'
#' @param file_path A character string specifying the full file path to the
#' Labguru plate map file (inclusive of the plate map excel file name)
#'
#' @returns A data frame of the Labguru plate map
#' @importFrom magrittr %>%
#' @importFrom readxl read_excel
#' @importFrom janitor make_clean_names
#' @importFrom dplyr select mutate
#' @export
#'
#' @examples
#' labguru_plate_prep(file_path = "")
labguru_plate_prep <- function(file_path = "") {
  # Import the Labguru plate map, skip 2 rows, tidy variable names
  plate_data <- readxl::read_excel(file_path,
    skip = 2,
    .name_repair = janitor::make_clean_names
  )

  # Subset the plate map to specific variables
  plate_data <- plate_data %>%
    dplyr::mutate(well = paste0(row, column)) %>%
    dplyr::select(inventory_item_sys_id, inventory_collection,
                  inventory_item_name, well_annotation, row, column, well)

  # Return Labguru plate data
  return(plate_data)

}
