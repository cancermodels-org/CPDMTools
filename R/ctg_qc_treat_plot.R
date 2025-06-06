#' CTG QC Treatment Plot
#'
#' @inheritParams growth_plot_qc_mono
#' @param ctg_list A list object generated from dr4pl_qc_fit_loop function
#' @param treat_name A character string specifying the treatment_name to filter
#' on.
#' @param n_x_axis_breaks An integer specifying the number of major breaks
#' for the x-axis. Defaults to 6.
#' @param geom_point_size A numeric value specifying the size of the points
#' on the graph. Defaults to 2.5. See \code{\link[ggplot2]{geom_point}}.
#' @param geom_line_width A numeric value specifying the width of the line.
#' Defaults to 1.
#'
#' @returns Returns a ggplot2 plot of the input data. When make_interactive is
#' TRUE, returns an interactive plotly object version of the ggplot2 plot.
#' @import ggplot2
#' @importFrom dplyr case_when filter mutate
#' @importFrom magrittr %>%
#' @importFrom plotly ggplotly layout
#' @importFrom plyr round_any
#' @importFrom viridis viridis
#' @export
#'
ctg_qc_treat_plot <- function(
    ctg_list,
    treat_name = "",
    show_outlier = TRUE,
    show_dose_response_curve = TRUE,
    make_interactive = FALSE,
    x_limits = c(NA, NA),
    x_axis_breaks = ggplot2::waiver(),
    n_x_axis_breaks = 6,
    y_limits = c(NA, NA),
    y_axis_breaks = ggplot2::waiver(),
    n_y_axis_breaks = NULL,
    x_axis_text_size = 12,
    y_axis_text_size = 12,
    x_axis_title_size = 14,
    y_axis_title_size = 14,
    plot_title_size = 20,
    geom_point_size = 2.5,
    geom_line_width = 1) {
  # Assign colors
  ctg_list[["model_parameters"]] <- ctg_list[["model_parameters"]] %>%
    dplyr::mutate(color = viridis::viridis(nrow(ctg_list[["model_parameters"]])))

  # Extract data
  data_long <- ctg_list[["data_frame"]] %>%
    dplyr::filter(treatment_name == treat_name)
  data_model <- ctg_list[["model_parameters"]] %>%
    dplyr::filter(treatment_name == treat_name)

  # Add color
  data_long <- data_long %>%
    dplyr::mutate(color = data_model$color[1])

  if (show_outlier == TRUE) {
    data_temp <- data_long %>%
      dplyr::mutate(
        color_new = dplyr::case_when(
          (outlier_auto_yn == "Yes" &
            (outlier_manual_yn != "No" | is.na(outlier_manual_yn))) ~ "red",
          outlier_manual_yn == "Yes" ~ "red",
          TRUE ~ color
        )
      )
  } else {
    data_temp <- dplyr::filter(
      data_long,
      outlier_auto_yn == "No",
      (outlier_manual_yn == "No" | is.na(outlier_manual_yn))
    ) %>%
      dplyr::mutate(color_new = color)
  }

  # Create y_limits if not provided
  if (all(is.na(y_limits))) {
    max_y <- max(data_temp$value_norm, na.rm = TRUE)
    max_y <- plyr::round_any(max_y, 0.5, ceiling)
    if (max_y < 1.5) max_y <- 1.5

    min_y <- min(data_temp$value_norm, na.rm = TRUE)
    min_y <- plyr::round_any(min_y, 0.5, floor)
    if (min_y > 0) min_y <- 0

    y_limits <- c(min_y, max_y)
  } else {
    min_y <- min(data_temp$value_norm, na.rm = TRUE)
    min_y <- plyr::round_any(min_y, 0.5, floor)
    if (min_y > 0) min_y <- 0
  }

  # Create initial base plot
  plot_01 <- ggplot2::ggplot(
    data = data_temp,
    ggplot2::aes(x = log10(concentration), y = value_norm)
  )

  # Add horizontal line at y=0 if values below 0 exist
  if (min_y < 0) {
    plot_01 <- plot_01 + ggplot2::geom_hline(yintercept = 0)
  }

  # Add dose-response curve if applicable
  if(show_dose_response_curve == TRUE){
    plot_01 <- plot_01 +
      ggplot2::stat_function(
        data = data_temp,
        fun = function(x) {
          conc <- 10^x
          data_model$lower_asy_est[1] +
            (data_model$upper_asy_est[1] - data_model$lower_asy_est[1]) /
            (1 + ((conc / data_model$rel_ic50_est[1])^data_model$slope_est[1]))
        },
        color = data_model$color[1],
        linewidth = geom_line_width,
        alpha = 0.90
      )
  }

  # Finalize plot
  plot_01 <- plot_01 +
    ggplot2::geom_point(
      data = data_temp,
      ggplot2::aes(
        x = log10(concentration),
        y = value_norm,
        color = color_new,
        text = paste0(
          "Well: ", well, "<br>",
          "Concentration: ", concentration, "<br>",
          "Value Norm: ", round(value_norm, 3)
        )
      ),
      size = geom_point_size,
      alpha = 0.85
    ) +
    ggplot2::theme_classic() +
    ggplot2::scale_color_identity() +
    ggplot2::scale_x_continuous(
      limits = x_limits,
      n.breaks = n_x_axis_breaks,
      breaks = x_axis_breaks
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      limits = y_limits,
      n.breaks = n_y_axis_breaks,
      breaks = y_axis_breaks
    ) +
    ggplot2::ggtitle(treat_name) +
    ggplot2::xlab(
      paste0("Log10(Concentration (", data_model$conc_unit[1], "))")
    ) +
    ggplot2::ylab("Normalized Luminescence") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5, size = plot_title_size, face = "bold"
      ),
      axis.title.x = ggplot2::element_text(
        size = x_axis_title_size, color = "black", face = "bold"
      ),
      axis.title.y = ggplot2::element_text(
        size = y_axis_title_size, color = "black", face = "bold"
      ),
      axis.text.x = ggplot2::element_text(
        size = x_axis_text_size, color = "black"
      ),
      axis.text.y = ggplot2::element_text(
        size = y_axis_text_size, color = "black"
      ),
      legend.position = "none"
    ) +
    ggplot2::coord_cartesian(clip = "off")

  # Make interactive if applicable
  if (make_interactive == TRUE) {
    plot_01 <- plotly::ggplotly(plot_01, tooltip = "text") %>%
      plotly::layout(showlegend = FALSE)
  }

  # Return plot
  return(plot_01)
}
