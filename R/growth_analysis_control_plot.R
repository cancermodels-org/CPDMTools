#' Growth Analysis Controls Plot
#'
#' @inheritParams growth_analysis_treat_plot
#'
#' @returns A ggplot2 or plotly object
#' @import ggplot2
#' @importFrom dplyr arrange filter group_by mutate pull summarise ungroup
#' @importFrom magrittr %>%
#' @importFrom plotly ggplotly
#' @importFrom rlang sym
#' @importFrom stats median sd
#' @export
#'
growth_analysis_control_plot <- function(data_frame,
                                         treatment_name = "",
                                         display_metric = "wells",
                                         make_interactive = FALSE,
                                         growth_metric_name = "growth_metric",
                                         time_units = "hours",
                                         concentration_units = "µM",
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
  # Check that treatment_name is part of analyzed treatments
  control_treats <- data_frame %>%
    dplyr::filter(treatment_type != "Monotherapy") %>%
    dplyr::pull(treatment_name) %>%
    unique()

  if (treatment_name != "" &
    !treatment_name %in% control_treats) {
    stop_msg <- paste0(
      treatment_name, " does not match the following control treatments: ",
      paste(control_treats, collapse = ", "), ". Please specify a single",
      " control treatment from the list above."
    )
    stop(stop_msg)
  }

  # Assign treat_ch variable
  treatment_ch <- treatment_name

  # Filter data to treatment_name and control data
  data_temp <- data_frame %>%
    dplyr::filter(treatment_name == treatment_ch,
                  treatment_type %in% c(
                    "Media Control",
                    "Negative Control",
                    "Positive Control"
                  ))

  # Create treatment variable
  data_temp <- data_temp %>%
    dplyr::arrange(treatment_name, concentration, time) %>%
    dplyr::mutate(
      treatment = dplyr::case_when(
        !is.na(concentration) ~ paste0(
          treatment_name, "-",
          concentration, concentration_units
        ),
        TRUE ~ treatment_name
      )
    )

  # Create new categories for plots legends
  # data_temp <- data_temp %>%
  # dplyr::arrange(treatment_name, concentration, time) %>%
  #  dplyr::mutate(treatment = dplyr::case_when(
  #    treatment_type %in% c(
  #      "Media Control",
  #      "Negative Control",
  #      "Positive Control"
  #    ) ~ treatment_name,
  #    TRUE ~ paste0(
  #      treatment_name, "-",
  #      concentration, concentration_units
  #    )
  #  ))
  data_temp <- data_temp %>%
    dplyr::arrange(treatment_name, concentration, time) %>%
    dplyr::mutate(
      treatment = dplyr::case_when(
        !is.na(concentration) ~ paste0(
          treatment_name, "-",
          concentration, concentration_units
        ),
        TRUE ~ treatment_name
      )
    )

  # Get treatment start period
  if ("treatment_period_yn" %in% colnames(data_temp)) {
    # Extract start of treatment time
    treatment_time_value <- data_temp %>%
      dplyr::filter(!!rlang::sym("treatment_period_yn") == "Yes")

    if (nrow(treatment_time_value > 0)) {
      treatment_time_value <- treatment_time_value %>%
        dplyr::filter(!!rlang::sym("time") ==
          min(!!rlang::sym("time"), na.rm = TRUE)) %>%
        dplyr::pull(!!rlang::sym("time")) %>%
        unique()
    } else {
      treatment_time_value <- min(data_temp$time, na.rm = TRUE)
    }
  } else {
    treatment_time_value <- min(data_temp$time, na.rm = TRUE)
  }

  # Pull color
  color_value <- data_temp$color[1]

  # Calculate summary metric version of growth_metric if applicable
  if (display_metric %in% c("mean_se", "mean", "median")) {
    # Create summary data
    data_temp <- data_temp %>%
      dplyr::group_by(
        time, treatment
      ) %>%
      dplyr::summarise(
        mean_value = mean(growth_metric, na.rm = TRUE),
        median_value = stats::median(growth_metric, na.rm = TRUE),
        sd_value = stats::sd(growth_metric, na.rm = TRUE),
        se_value = sd_value / sqrt(n()),
        y_min = mean_value - se_value,
        y_max = mean_value + se_value
      ) %>%
      dplyr::ungroup()

    # Create initial plot base, add a dashed vertical line if
    # treatment_time_value > min()
    if (display_metric == "mean_se") {
      plot_01 <- ggplot2::ggplot(
        data_temp,
        ggplot2::aes(
          x = time, y = mean_value,
        ),
        color = color_value
      )

      # Add a dashed vertical line if treatment_time_value > min()
      if (treatment_time_value > min(data_temp$time)) {
        plot_01 <- plot_01 +
          ggplot2::geom_vline(
            xintercept = treatment_time_value,
            linetype = "dashed",
            color = "#666666",
            linewidth = 1,
            alpha = 1
          )
      }

      plot_01 <- plot_01 +
        ggplot2::geom_line(
          linewidth = geom_line_width,
          alpha = 0.75,
          color = color_value
        ) +
        ggplot2::geom_errorbar(
          data = data_temp,
          aes(x = time, ymin = y_min, ymax = y_max),
          color = color_value,
          inherit.aes = FALSE,
          width = 4,
          alpha = 0.75
        ) +
        ggplot2::geom_point(
          data = data_temp,
          aes(x = time, y = mean_value),
          color = color_value,
          size = geom_point_size,
          inherit.aes = FALSE,
          alpha = 0.75
        )

      # Create new y-axis label
      growth_metric_name <- paste0("Mean ", growth_metric_name)

    } else if (display_metric == "mean") {
      plot_01 <- ggplot2::ggplot(
        data_temp,
        ggplot2::aes(
          x = time, y = mean_value,
        )
      )

      # Add a dashed vertical line if treatment_time_value > min()
      if (treatment_time_value > min(data_temp$time)) {
        plot_01 <- plot_01 +
          ggplot2::geom_vline(
            xintercept = treatment_time_value,
            linetype = "dashed",
            color = "#666666",
            linewidth = 1,
            alpha = 1
          )
      }

      plot_01 <- plot_01 +
        ggplot2::geom_line(
          linewidth = geom_line_width,
          alpha = 0.75,
          color = color_value
        ) +
        ggplot2::geom_point(
          data = data_temp,
          aes(x = time, y = mean_value),
          color = color_value,
          size = geom_point_size,
          inherit.aes = FALSE,
          alpha = 0.75
        )

      # Create new y-axis label
      growth_metric_name <- paste0("Mean ", growth_metric_name)

    } else if (display_metric == "median") {
      plot_01 <- ggplot2::ggplot(
        data_temp,
        ggplot2::aes(
          x = time, y = median_value
        )
      )

      # Add a dashed vertical line if treatment_time_value > min()
      if (treatment_time_value > min(data_temp$time)) {
        plot_01 <- plot_01 +
          ggplot2::geom_vline(
            xintercept = treatment_time_value,
            linetype = "dashed",
            color = "#666666",
            linewidth = 1,
            alpha = 1
          )
      }

      plot_01 <- plot_01 +
        ggplot2::geom_line(
          linewidth = geom_line_width,
          alpha = 0.75,
          color = color_value
        ) +
        ggplot2::geom_point(
          data = data_temp,
          aes(x = time, y = median_value),
          color = color_value,
          size = geom_point_size,
          inherit.aes = FALSE,
          alpha = 0.75
        )

      # Create new y-axis label
      growth_metric_name <- paste0("Median ", growth_metric_name)

    }
  } else {
    # Show all points and group by well
    plot_01 <- ggplot2::ggplot(
      data_temp,
      ggplot2::aes(
        x = time, y = growth_metric,
        group = well
      ),
      color = color_value,
    )

    # Add a dashed vertical line if treatment_time_value > min()
    if (treatment_time_value > min(data_temp$time)) {
      plot_01 <- plot_01 +
        ggplot2::geom_vline(
          xintercept = treatment_time_value,
          linetype = "dashed",
          color = "#666666",
          linewidth = 1,
          alpha = 1
        )
    }

    plot_01 <- plot_01 +
      ggplot2::geom_line(
        linewidth = geom_line_width,
        alpha = 0.75,
        color = color_value
      ) +
      ggplot2::geom_point(
        data = data_temp,
        aes(x = time, y = growth_metric),
        color = color_value,
        size = geom_point_size,
        inherit.aes = FALSE,
        alpha = 0.75
      )
  }

  # Finalize plot aesthetics
  plot_01 <- plot_01 +
    ggplot2::theme_classic() +
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
    ggplot2::ggtitle(treatment_ch) +
    ggplot2::xlab(paste0("Time (", time_units, ")")) +
    ggplot2::ylab(growth_metric_name) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        size = plot_title_size,
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
      legend.title = ggplot2::element_text(
        hjust = 0.5, face = "bold"
      )
    ) +
    ggplot2::coord_cartesian(clip = "off")

  # Convert to plotly if applicable
  if (make_interactive == TRUE) {
    plot_01 <- plotly::ggplotly(plot_01)
  }

  # Return plot
  return(plot_01)
}
