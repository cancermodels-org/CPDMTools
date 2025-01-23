#' Import and prep CTG data
#'
#' @param file_path A character string specifying the full file path to the
#' CTG excel file file (inclusive of the full CTG .xlsx file name)
#'
#' @returns A data frame of the prepared CTG data
#' @importFrom dplyr mutate na_if select rename
#' @importFrom janitor clean_names
#' @importFrom magrittr %>%
#' @importFrom readxl read_excel
#' @importFrom stringr str_replace_all
#' @importFrom tidyr drop_na
#' @export
#'
#' @examples
#' ctg_prep(file_path = "")
ctg_prep <- function(
    file_path = ""){
  # Import ctg data file
  data_frame <- readxl::read_excel(
    file_path,
    sheet = "Results By Well",
    skip = 5)

  # Rename column names (add if else depending on ncol due to multiple plates)
  data_frame[1, 1] <- colnames(data_frame)[1]
  data_frame <- janitor::clean_names(data_frame)
  data_frame <- data_frame %>%
    dplyr::select(1, value)
  names(data_frame)[1] <- "well"

  # Fill in missing well id's by filling down the proper well to the row
  for (a in 1:nrow(data_frame)) {
    if (is.na(data_frame$well[a])) {
      data_frame$well[a] <- data_frame$well[a - 1]
    }
  }
  rm(a)

  # Drop missing values and convert data to proper formats
  data_frame <- data_frame %>%
    dplyr::mutate(
      value = dplyr::na_if(value, "value"),
      value = as.numeric(value),
      well = stringr::str_replace_all(well, ":", "")
    ) %>%
    tidyr::drop_na(value) %>%
    dplyr::rename(ctg_value = value)

  # Return the CTG Data
  return(data_frame)

}
