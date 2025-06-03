#' Growth plot for QC
#'
#' @param data_frame A data frame output from the loess_outlier_fit function
#' @param treatment_name A character string specifying the name of the current
#' treatment. Defaults to "".
#' @param show_outlier A logical value specifying whether to show the points
#' marked as outliers/ Defaults to TRUE.
#' @param show_only_outlier_wells A logical value specifying whether to only
#' show the wells that contain outliers. Defaults to FALSE.
#' @param make_interactive A logical value specifying whether to convert the
#' plot into an interactive plotly object. Defaults to FALSE.
#' @param growth_metric_name A character string for specifying the name of
#' the growth metric (y-axis title) to be displayed on the plot.
#' Defaults to "growth_metric".
#' @param time_units A character string for specifying the name of the time
#' variable (x-axis title) to be displayed on the plot. Defaults to "hours".
#' @param x_limits A numeric vector of length two providing limits for
#' the x-axis. Use NA to refer to the existing minimum or maximum.
#' Defaults to c(NA, NA). See \code{\link[ggplot2]{scale_x_continuous}}.
#' @param x_axis_breaks A numeric vector specifying manual numeric breaks.
#' Defaults to ggplot2::waiver(). See \code{\link[ggplot2]{scale_x_continuous}}.
#' @param n_x_axis_breaks An integer specifying the number of major breaks
#' for the x-axis. Defaults to NULL.
#' @param y_limits A numeric vector of length two providing limits for
#' the y-axis. Use NA to refer to the existing minimum or maximum.
#' Defaults to c(NA, NA). See \code{\link[ggplot2]{scale_y_continuous}}.
#' @param y_axis_breaks A numeric vector specifying manual numeric breaks.
#' Defaults to ggplot2::waiver(). See \code{\link[ggplot2]{scale_y_continuous}}.
#' @param n_y_axis_breaks An integer specifying the number of major breaks
#' for the x-axis. Defaults to NULL.
#' @param x_axis_text_size A numeric value specifying the size of the
#' x-axis text. Defaults to 12. See \code{\link[ggplot2]{element_text}}.
#' @param y_axis_text_size A numeric value specifying the size of the
#' y-axis text. Defaults to 12. See \code{\link[ggplot2]{element_text}}.
#' @param x_axis_title_size A numeric value specifying the size of the
#' x-axis title. Defaults to 14. See \code{\link[ggplot2]{element_text}}.
#' @param y_axis_title_size A numeric value specifying the size of the
#' y-axis title. Defaults to 14. See \code{\link[ggplot2]{element_text}}.
#' @param plot_title_size A numeric value specifying the size of the plot
#' title. Defaults to 20. See \code{\link[ggplot2]{element_text}}.
#' @param geom_point_size A numeric value specifying the size of the points
#' on the graph. Defaults to 2. See \code{\link[ggplot2]{geom_point}}.
#' @param geom_line_width A numeric value specifying the width of the line.
#' Defaults to 0.5.
#'
#' @returns Returns a ggplot2 plot of the input data. When make_interactive is
#' TRUE, returns an interactive plotly object version of the ggplot2 plot.
#' @import ggplot2
#' @importFrom dplyr case_when filter mutate pull
#' @importFrom plotly ggplotly layout
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @export
#'
#' @examples
#' growth_plot_qc_mono(data_frame)
growth_plot_qc_mono <- function(data_frame,
                                treatment_name = "",
                                show_outlier = TRUE,
                                show_only_outlier_wells = FALSE,
                                make_interactive = FALSE,
                                growth_metric_name = "growth_metric",
                                time_units = "hours",
                                x_limits = c(NA, NA),
                                x_axis_breaks = ggplot2::waiver(),
                                n_x_axis_breaks = NULL,
                                y_limits = c(NA, NA),
                                y_axis_breaks = ggplot2::waiver(),
                                n_y_axis_breaks = NULL,
                                x_axis_text_size = 12,
                                y_axis_text_size = 12,
                                x_axis_title_size = 14,
                                y_axis_title_size = 14,
                                plot_title_size = 20,
                                geom_point_size = 2,
                                geom_line_width = 0.5) {
  # Check data_frame and function parameter inputs
  stopifnot(
    is.character(treatment_name),
    is.logical(make_interactive),
    is.character(growth_metric_name),
    is.character(time_units),
    is.vector(x_limits),
    length(x_limits) == 2,
    (is.null(n_x_axis_breaks) | is.numeric(n_x_axis_breaks)),
    is.vector(y_limits),
    length(y_limits) == 2,
    (is.null(n_y_axis_breaks) | is.numeric(n_y_axis_breaks)),
    is.numeric(x_axis_text_size),
    is.numeric(y_axis_text_size),
    is.numeric(x_axis_title_size),
    is.numeric(y_axis_title_size),
    is.numeric(plot_title_size),
    is.numeric(geom_point_size),
    is.numeric(geom_line_width)
  )

  # Extract start of treatment time
  if (any(data_frame$treatment_period_yn == "Yes") &
    any(data_frame$treatment_period_yn == "No")) {
    treatment_time_value <- data_frame %>%
      dplyr::filter(!!rlang::sym("treatment_period_yn") == "Yes") %>%
      dplyr::filter(!!rlang::sym("time") ==
        min(!!rlang::sym("time"), na.rm = TRUE)) %>%
      dplyr::pull(!!rlang::sym("time")) %>%
      unique()
  }

  # Prepare colors based on show_outlier status
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

    if (show_only_outlier_wells == TRUE) {
      outlier_wells <- data_temp %>%
        dplyr::filter(
          (outlier_auto_yn == "Yes" |
            outlier_manual_yn == "Yes")) %>%
        dplyr::pull(well) %>%
        unique()
      data_temp <- data_temp %>%
        filter(well %in% outlier_wells)
    }
  } else {
    data_temp <- data_frame %>%
      filter(
        outlier_auto_yn == "No",
        (outlier_manual_yn == "No" |
           is.na(outlier_manual_yn))) %>%
      mutate(color_new = color)
  }

  # Create initial base plot
  # Base plot with grouping for lines
  plot_01 <- ggplot2::ggplot(
    data_temp,
    ggplot2::aes(
      x = time,
      y = growth_metric,
      group = well
    )
  )

  # Add vertical line if applicable
  if (any(data_frame$treatment_period_yn == "Yes") &
    any(data_frame$treatment_period_yn == "No")) {
    plot_01 <- plot_01 +
      ggplot2::geom_vline(
        xintercept = treatment_time_value,
        linetype = "dashed",
        color = "#666666",
        linewidth = 1
      )
  }

  # Add lines (no color aesthetic here, only group by well)
  plot_01 <- plot_01 +
    ggplot2::geom_line(
      linewidth = geom_line_width,
      alpha = 0.75,
      show.legend = FALSE
    )

  # Create well annotations depending on if concentration is present
  if("concentration" %in% colnames(data_temp)){
    plot_01 <- plot_01 +
      ggplot2::geom_point(
        ggplot2::aes(
          color = color_new,
          text = paste0(
            "Well: ", well, "<br>",
            "Treatment: ", treatment_name, "<br>",
            "Concentration: ", concentration, "<br>",
            growth_metric_name, ": ", round(growth_metric, 2)
          )
        ),
        size = geom_point_size,
        alpha = 0.75,
        show.legend = FALSE
      )
  }else{
    plot_01 <- plot_01 +
      ggplot2::geom_point(
        ggplot2::aes(
          color = color_new,
          text = paste0(
            "Well: ", well, "<br>",
            "Treatment: ", treatment_name, "<br>",
            growth_metric_name, ": ", round(growth_metric, 2)
          )
        ),
        size = geom_point_size,
        alpha = 0.75,
        show.legend = FALSE
      )
  }

  # Add points with color and tooltip labels
  plot_01 <- plot_01 +
    ggplot2::scale_color_identity() +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous(
      limits = x_limits,
      breaks = x_axis_breaks,
      n.breaks = n_x_axis_breaks
    ) +
    ggplot2::scale_y_continuous(
      limits = y_limits,
      breaks = y_axis_breaks,
      n.breaks = n_y_axis_breaks
    ) +
    ggplot2::ggtitle(treatment_name) +
    ggplot2::xlab(paste0("Time (", time_units, ")")) +
    ggplot2::ylab(growth_metric_name) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5, size = plot_title_size, face = "bold"),
      axis.title.x = ggplot2::element_text(
        size = x_axis_title_size, color = "black", face = "bold"),
      axis.title.y = ggplot2::element_text(
        size = y_axis_title_size, color = "black", face = "bold"),
      axis.text.x = ggplot2::element_text(
        size = x_axis_text_size, color = "black"),
      axis.text.y = ggplot2::element_text(
        size = y_axis_text_size, color = "black"),
      legend.position = "none"
    ) +
    ggplot2::coord_cartesian(clip = "off")

  # Convert to plotly
  if (make_interactive == TRUE) {
    plot_01 <- plotly::ggplotly(plot_01, tooltip = "text") %>%
      plotly::layout(showlegend = FALSE)
  }

  # Return plot
  return(plot_01)
}
