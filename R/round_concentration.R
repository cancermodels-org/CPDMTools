#' Round concentration variables
#'
#' @param data_frame A data frame object created from the data wrangling RShiny
#' @param round_by A numeric variable specifying the number of digits to round
#' by. Defaults to 4.
#' @param use_nearest_10 A logical value specying whether to round the
#' concentrations to nearest 10 unit. Defaults to TRUE.
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
                                round_by = 4,
                                use_nearest_10 = TRUE) {
  # Check if data_frame contains any concentration columns
  if (any(stringr::str_detect(colnames(data_frame), "conc"))) {
    # Round all concentration variables in data_frame
    data_frame <- data_frame %>%
      dplyr::mutate(
        dplyr::across(dplyr::contains("conc"), ~ round(., round_by))
        )

    # Round to the nearest 10th number if applicable
    if(use_nearest_10 == TRUE){
      # Create function to round to nearest power of 10
      round_conc_10 <- function(x) {
        sapply(x, function(val) {
          if (is.na(val)) {
            NA
          } else if (val == 0) {
            0
          } else {
            round(val, -floor(log10(abs(val))))
          }
        })
      }

      # Round all "conc" columns to nearest 10-based value
      data_frame <- data_frame %>%
        dplyr::mutate(
          dplyr::across(dplyr::contains("conc"), ~ round_conc_10(.x))
        )
    }

    # Return data frame
    return(data_frame)

  }else{
    message("Input data frame does not contain any concentration variables")
  }
}
