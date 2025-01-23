#' Import and prep tecan report file
#'
#' @param file_path A character string specifying the full file path to the
#' Tecan report file (inclusive of the Tecan report .xlsx file name)
#' @param drugging_type The drugging type scheme of the Tecan file. Values
#' include "Monotherapy" and "Synergy". Defaults to "Monotherapy"
#' @param remove_na A logical value specifying whether to remove non-dispensed
#' wells (i.e. treatment_name is NA). Defaults to TRUE.
#'
#' @returns A data frame of the prepared tecan report data
#' @seealso
#' \code{\link{tecan_report_prep_mono}}
#' \code{\link{tecan_report_prep_syn}}
#' @export
#'
#' @examples
#' tecan_report_prep(
#' file_path = "",
#' drugging_type = "Monotherapy",
#' remove_na = TRUE)
tecan_report_prep <- function(
    file_path = "",
    drugging_type = "Monotherapy",
    remove_na = TRUE){
  # Monotherapy
  if(drugging_type == "Monotherapy"){
      data_frame <- tecan_report_prep_mono(
        file_path = file_path,
        remove_na = remove_na
      )
  # Synergy
  }else{
    data_frame <- tecan_report_prep_syn(
      file_path = file_path,
      remove_na = remove_na
    )
  }

  # Return data frame
  return(data_frame)

}
