#' CTG QC Control Plot
#'
#' @inheritParams growth_plot_qc_mono
#' @param data_frame A data frame of CTG data
#'
#' @returns Returns a ggplot2 plot of the input data. When make_interactive is
#' TRUE, returns an interactive plotly object version of the ggplot2 plot.
#' @import ggplot2
#' @importFrom dplyr case_when filter mutate pull summarise
#' @importFrom plotly ggplotly
#' @importFrom magrittr %>%
#' @export
#'
ctg_qc_control_plot <- function(
    data_frame,
    show_outlier = TRUE,
    make_interactive = FALSE,
    x_axis_text_size = 12,
    y_axis_text_size = 12,
    x_axis_title_size = 14,
    y_axis_title_size = 14,
    plot_title_size = 20,
    geom_point_size = 2.5) {
  # Filter data by show_outlier and re-assign colors
  if (show_outlier == TRUE) {
    data_temp <- data_frame %>%
      dplyr::mutate(
        color_new = dplyr::case_when(
          (outlier_auto_yn == "Yes" &
            (outlier_manual_yn != "No" |
              is.na(outlier_manual_yn))) ~ "red",
          outlier_manual_yn == "Yes" ~ "red",
          TRUE ~ color
        )
      )
  } else {
    data_temp <- data_frame %>%
      dplyr::filter(
        outlier_auto_yn == "No",
        (outlier_manual_yn == "No" |
          is.na(outlier_manual_yn))
      ) %>%
      dplyr::mutate(color_new = color)
  }

  # Calculate control summary statistics
  avg_neg_control <- data_temp %>%
    dplyr::filter(treatment_type == "Negative Control") %>%
    dplyr::summarise(mean = mean(value, na.rm = TRUE)) %>%
    dplyr::pull(mean) %>%
    as.numeric()

  sd_neg_control <- data_temp %>%
    dplyr::filter(treatment_type == "Negative Control") %>%
    dplyr::summarise(sd = sd(value, na.rm = TRUE)) %>%
    dplyr::pull(sd) %>%
    as.numeric()

  if ("Positive Control" %in% unique(data_temp$treatment_type)) {
    avg_pos_control <- data_temp %>%
      dplyr::filter(treatment_type == "Positive Control") %>%
      dplyr::summarise(mean = mean(value, na.rm = TRUE)) %>%
      dplyr::pull(mean) %>%
      as.numeric()

    sd_pos_control <- data_temp %>%
      dplyr::filter(treatment_type == "Positive Control") %>%
      dplyr::summarise(sd = sd(value, na.rm = TRUE)) %>%
      dplyr::pull(sd) %>%
      as.numeric()

    # Calculate Z-factor
    z_factor <- 1 - (3 * (sd_pos_control + sd_neg_control)) / abs(avg_pos_control - avg_neg_control)
    z_factor <- round(z_factor, 3)
  } else {
    z_factor <- "NA"
  }

  # Make treatment type a factor
  data_temp <- data_temp %>%
    dplyr::mutate(
      treatment_type =
        factor(treatment_type,
          levels = c(
            "Media Control",
            "Negative Control",
            "Positive Control",
            "Monotherapy",
            "Combotherapy"
          )
        )
    )

  # Create plot
  plot_01 <- ggplot2::ggplot(
    data_temp,
    ggplot2::aes(x = treatment_type, y = value)
  ) +
    ggplot2::geom_boxplot(outlier.shape = NA) +
    ggplot2::geom_point(
      ggplot2::aes(
        color = color_new,
        text = paste(
          "Treatment Type:", treatment_type, "<br>",
          "Value:", value, "<br>",
          "Treatment Name:", treatment_name, "<br>",
          "Concentration", concentration, "<br>",
          "Well:", well
        )
      ),
      size = geom_point_size,
      position = ggplot2::position_jitter(width = 0.3),
      show.legend = FALSE
    ) +
    ggplot2::theme_classic() +
    ggplot2::scale_color_identity() +
    ggplot2::ggtitle(paste0(
      "Controls and Therapies QC\nZ-Factor: ",
      z_factor
    )) +
    ggplot2::xlab("Treatment Type") +
    ggplot2::ylab("Raw Value") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        size = plot_title_size,
        face = "bold"
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0.5,
        size = 18,
        face = "bold"
      ),
      axis.title.x = ggplot2::element_text(
        size = x_axis_title_size,
        color = "black", face = "bold"
      ),
      axis.title.y = ggplot2::element_text(
        size = y_axis_title_size,
        color = "black", face = "bold"
      ),
      axis.text.x = ggplot2::element_text(
        size = x_axis_text_size,
        color = "black"
      ),
      axis.text.y = ggplot2::element_text(
        size = y_axis_text_size,
        color = "black"
      ),
      legend.title = ggplot2::element_text(hjust = 0.5, face = "bold")
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::labs(color = "Treatment Name")

  # Make a plotly if applicable
  if (make_interactive == TRUE) {
    plot_01 <- plotly::ggplotly(plot_01, tooltip = "text") %>%
      plotly::layout(showlegend = FALSE)
  }

  # Return plot
  return(plot_01)
}
