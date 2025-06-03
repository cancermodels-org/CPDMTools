#' Generate colors for monotherapy experiments
#'
#' @param joined_data_frame A data frame object created from the data wrangling
#' RShiny
#' @param category The variable used for determining where each condition is
#' located. Values include "treatment_name" and "well_annotation". Defaults to
#' "well_annotation"
#'
#' @returns A data frame with the variable "color" for the color pallete
#' @importFrom dplyr arrange bind_rows distinct filter left_join
#' mutate pull select
#' @importFrom rlang sym
#' @importFrom viridis viridis
#' @export
#'
#' @examples
#' data_frame_colors <- color_palette_mono(joined_data_frame = data_frame)
color_palette_mono <- function(joined_data_frame,
                               category = "treatment_name") {
  all_colors <- list()

  has_conc <- all(c(category, "treatment_type", "concentration")
                  %in% colnames(joined_data_frame))

  if (has_conc) {
    color_matrix <- joined_data_frame %>%
      dplyr::select(!!rlang::sym(category), treatment_type, concentration) %>%
      dplyr::distinct()
  } else {
    color_matrix <- joined_data_frame %>%
      dplyr::select(!!rlang::sym(category), treatment_type) %>%
      dplyr::distinct()
  }

  for (a in unique(dplyr::pull(color_matrix, !!rlang::sym(category)))) {
    data_temp <- color_matrix %>%
      dplyr::filter(!!rlang::sym(category) == a)

    data_colors <- data_temp[0, ] %>%
      dplyr::mutate(color = NA_character_)

    # Monotherapy
    if (any(data_temp$treatment_type == "Monotherapy")) {
      data_mono <- data_temp %>%
        dplyr::filter(treatment_type == "Monotherapy") %>%
        {
          if (has_conc) {
            dplyr::arrange(., concentration)
          } else {
            dplyr::arrange(., !!rlang::sym(category))
          }
        }

      data_mono <- data_mono %>%
        dplyr::mutate(color = viridis::viridis(nrow(.)))

      data_colors <- dplyr::bind_rows(data_colors, data_mono)
    }

    # Media Control
    if (any(data_temp$treatment_type == "Media Control")) {
      data_media <- data_temp %>%
        dplyr::filter(treatment_type == "Media Control") %>%
        dplyr::mutate(color = "#FF6699")

      data_colors <- dplyr::bind_rows(data_colors, data_media)
    }

    # Negative Control
    if (any(data_temp$treatment_type == "Negative Control")) {
      data_neg <- data_temp %>%
        dplyr::filter(treatment_type == "Negative Control") %>%
        dplyr::mutate(color = "black")

      data_colors <- dplyr::bind_rows(data_colors, data_neg)
    }

    # Positive Control
    if (any(data_temp$treatment_type == "Positive Control")) {
      data_pos <- data_temp %>%
        dplyr::filter(treatment_type == "Positive Control") %>%
        dplyr::mutate(color = "#FF6600")

      data_colors <- dplyr::bind_rows(data_colors, data_pos)
    }

    all_colors[[as.character(a)]] <- data_colors
  }

  all_colors_df <- dplyr::bind_rows(all_colors)

  joined_data_frame <- joined_data_frame %>%
    dplyr::left_join(all_colors_df)

  return(joined_data_frame)
}

