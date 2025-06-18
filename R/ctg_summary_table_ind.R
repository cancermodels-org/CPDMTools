#' CTG Summary Table All Treatments
#'
#' @param treat_name A character string specifying the treatment_name to
#' filter on.
#' @inheritParams ctg_summary_table_all
#'
#' @returns A flextable object
#' @importFrom dplyr filter select
#' @importFrom flextable align autofit bold hline_top flextable
#' fontsize fp_border_default
#' @importFrom knitr knit_print
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#' @export
#'
ctg_summary_table_ind <- function(ctg_list,
                                  treat_name = "",
                                  font_name = "Calibri",
                                  font_size_header = 14,
                                  font_size_body = 12,
                                  use_knit_print = FALSE){

  # Extract table elements
  data_table <- ctg_list[["table"]]

  # Filter and prepare data_table
  data_table <- data_table %>%
    dplyr::filter(treatment_name == treat_name) %>%
    dplyr::select(
      Variable, Value
    )

  # Create flextable object
  flx_tbl <- flextable::flextable(data_table) %>%
    flextable::hline_top(
      border = flextable::fp_border_default(width = 0),
      part = "header"
    ) %>%
    flextable::align(align = "center", part = "header") %>%
    flextable::align(
      align = "center", part = "body",
      j = c(2:ncol(data_table))
    ) %>%
    flextable::align(align = "left", part = "body", j = 1) %>%
    flextable::fontsize(size = font_size_header, part = "header") %>%
    flextable::fontsize(size = font_size_body, part = "body") %>%
    flextable::font(fontname = font_name, part = "body") %>%
    flextable::font(fontname = font_name, part = "header") %>%
    flextable::bold(part = "header", bold = TRUE) %>%
    flextable::autofit()

  # If use_knitr is TRUE, knit the flx_tbl using knit_print,
  # otherwise return the flx_tbl object
  if (use_knit_print == TRUE) {
    knitr::knit_print(flx_tbl)
  } else {
    return(flx_tbl)
  }

  # Return flextable
  return(flx_tbl)

}
