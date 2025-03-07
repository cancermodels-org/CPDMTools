#' Round concentration variables
#'
#' @param data_frame A data frame object created from the data wrangling RShiny
#' @param round_by A numeric variable specifying the number of digits to round
#' by. Defaults to 4.
#'
#' @returns A data frame object with the concentration variables rounded
#' @importFrom magrittr %>%
#' @importFrom dplyr across contains mutate
#' @export
#'
#' @examples
#' round_concentration(
#' data_frame = joined_data,
#' round_by = 4)
round_concentration <- function(data_frame,
                                round_by = 4) {
  # Check if data_frame contains any concentration columns
  if (any(stringr::str_detect(colnames(data_frame), "conc"))) {
    # Round all concentration variables in data_frame
    data_frame <- data_frame %>%
      dplyr::mutate(
        dplyr::across(dplyr::contains("conc"), ~ round(., round_by))
        )

    # Return data frame
    return(data_frame)

  }else{
    message("Input data frame does not contain any concentration variables")
  }
}
