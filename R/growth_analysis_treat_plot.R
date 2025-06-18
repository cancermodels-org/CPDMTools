#' Growth Analysis Monotherapy Treatment Plot
#'
#' @param data_frame A data frame object for growth based data
#' @param display_metric A character string specifying the type of points to
#' display. Values include "wells" for well-level data, "mean_se" for displaying
#' the mean points and se bars, "mean" for just the mean of the points, or
#' "median" for the median of the points. Defaults to "well".
#' @param concentration_units A character string specifying the concetration
#' units. Defaults to "µM".
#' @inheritParams growth_plot_qc_mono
#'
#' @returns A ggplot2 or plotly object
#' @import ggplot2
#' @importFrom dplyr arrange distinct filter group_by mutate pull select
#' summarise ungroup
#' @importFrom magrittr %>%
#' @importFrom plotly ggplotly
#' @importFrom rlang sym
#' @importFrom stats median sd
#' @export
#'
growth_analysis_treat_plot <- function(data_frame,
                                       treatment_name = "",
                                       show_controls = TRUE,
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
  mono_treats <- data_frame %>%
    dplyr::filter(treatment_type == "Monotherapy") %>%
    dplyr::pull(treatment_name) %>%
    unique()

  if (treatment_name != "" &
    !treatment_name %in% mono_treats) {
    stop_msg <- paste0(
      treatment_name, " does not match the following monotherapy treatments: ",
      paste(mono_treats, collapse = ", "), ". Please specify a single",
      " monotherapy treatment from the list above."
    )
    stop(stop_msg)
  }

  # If treatment_name is blank, select the first treatment_name in dataset
  if (treatment_name == "") {
    treatment_ch <- data_frame %>%
      dplyr::filter(treatment_type == "Monotherapy") %>%
      dplyr::pull(treatment_name) %>%
      unique()
    treatment_ch <- treatment_ch[1]
  } else {
    treatment_ch <- treatment_name
  }

  # Filter data to treatment_name and control data
  data_temp <- data_frame %>%
    dplyr::filter((treatment_name == treatment_ch |
      treatment_type %in% c(
        "Media Control",
        "Negative Control",
        "Positive Control"
      )))

  # Create treatment variable
  if (show_controls == TRUE) {
    # Create new categories for plots legends
    #data_temp <- data_temp %>%
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
          !is.na(concentration) ~ paste0(treatment_name, "-",
                           concentration, concentration_units),
          TRUE ~ treatment_name)
      )

    # Initiate factor_vec
    factor_vec <- c()
    # Check if there is a Media Control
    if (any(data_temp$treatment_type == "Media Control")) {
      media_control_name <-
        as.character(data_temp[
          which(data_temp$treatment_type == "Media Control"),
          "treatment"
        ][1, ])
      factor_vec <- c(factor_vec, media_control_name)
    }
    # Check if there is a Negative Control
    if (any(data_temp$treatment_type == "Negative Control")) {
      negative_control_name <-
        as.character(data_temp[
          which(data_temp$treatment_type == "Negative Control"),
          "treatment"
        ][1, ])
      factor_vec <- c(factor_vec, negative_control_name)
    }
    # Pull treatment and concentration variables
    treat_conc_vec <- data_temp %>%
      dplyr::filter(treatment_name == treatment_ch) %>%
      dplyr::arrange(concentration) %>%
      dplyr::select(treatment) %>%
      dplyr::distinct() %>%
      pull(treatment)
    factor_vec <- c(factor_vec, treat_conc_vec)
    # Check if there is a Positive Control
    if (any(data_temp$treatment_type == "Negative Control")) {
      positive_control_name <-
        as.character(data_temp[
          which(data_temp$treatment_type == "Positive Control"),
          "treatment"
        ][1, ])
      factor_vec <- c(factor_vec, positive_control_name)
    }
  } else {
    data_temp <- data_temp %>%
      dplyr::filter(treatment_name == treatment_ch) %>%
      dplyr::mutate(
        treatment = treatment_name
      )
  }

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

  # Create plot if show_controls == TRUE
  if(show_controls == TRUE){
    # Calculate summary metric version of growth_metric if applicable
    if (display_metric %in% c("mean_se", "mean", "median")) {
      # Create summary data
      data_temp <- data_temp %>%
        dplyr::group_by(
          time, treatment, color
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

      if(show_controls == TRUE){
        # Assign factors
        data_temp <- data_temp %>%
          mutate(treatment = factor(treatment, levels = factor_vec))
      }

      # Create color map
      color_map <- setNames(data_temp$color, data_temp$treatment)

      # Create initial plot base, add a dashed vertical line if
      # treatment_time_value > min()
      if (display_metric == "mean_se") {
        plot_01 <- ggplot2::ggplot(
          data_temp,
          ggplot2::aes(
            x = time, y = mean_value,
            group = treatment, color = treatment
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
            alpha = 0.75
          ) +
          ggplot2::geom_errorbar(
            data = data_temp,
            aes(x = time, ymin = y_min, ymax = y_max, color = treatment),
            inherit.aes = FALSE,
            width = 4,
            alpha = 0.75
          ) +
          ggplot2::geom_point(
            data = data_temp,
            aes(x = time, y = mean_value, color = treatment),
            size = geom_point_size,
            inherit.aes = FALSE,
            alpha = 0.75
          ) +
          ggplot2::scale_color_manual(
            values = color_map
          )

        # Create new y-axis label
        growth_metric_name <- paste0("Mean ", growth_metric_name)
      } else if (display_metric == "mean") {
        plot_01 <- ggplot2::ggplot(
          data_temp,
          ggplot2::aes(
            x = time, y = mean_value,
            group = treatment, color = treatment
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
            alpha = 0.75
          ) +
          ggplot2::geom_point(
            data = data_temp,
            aes(x = time, y = mean_value, color = treatment),
            size = geom_point_size,
            inherit.aes = FALSE,
            alpha = 0.75
          ) +
          ggplot2::scale_color_manual(
            values = color_map
          )

        # Create new y-axis label
        growth_metric_name <- paste0("Mean ", growth_metric_name)
      } else if (display_metric == "median") {
        plot_01 <- ggplot2::ggplot(
          data_temp,
          ggplot2::aes(
            x = time, y = median_value,
            group = treatment, color = treatment
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
            alpha = 0.75
          ) +
          ggplot2::geom_point(
            data = data_temp,
            aes(x = time, y = median_value, color = treatment),
            size = geom_point_size,
            inherit.aes = FALSE,
            alpha = 0.75
          ) +
          ggplot2::scale_color_manual(
            values = color_map
          )

        # Create new y-axis label
        growth_metric_name <- paste0("Median ", growth_metric_name)
      }
    }else{
      # Create color map
      color_map <- setNames(data_temp$color, data_temp$treatment)

      if(show_controls == TRUE){
        # Assign factors
        data_temp <- data_temp %>%
          mutate(treatment = factor(treatment, levels = factor_vec))
      }

      # Show all points and group by well
      plot_01 <- ggplot2::ggplot(
        data_temp,
        ggplot2::aes(
          x = time, y = growth_metric,
          group = well, color = treatment
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
          alpha = 0.75
        ) +
        ggplot2::geom_point(
          data = data_temp,
          aes(x = time, y = growth_metric, color = treatment),
          size = geom_point_size,
          inherit.aes = FALSE,
          alpha = 0.75
        ) +
        ggplot2::scale_color_manual(
          values = color_map
        )
    }
  # If show_controls == FALSE, color by concentration
  }else{
    # Convert concentration to a factor variable
    data_temp <- data_temp %>%
      dplyr::mutate(concentration = factor(concentration))

    # Calculate summary metric version of growth_metric if applicable
    if (display_metric %in% c("mean_se", "mean", "median")) {
      # Create summary data
      data_temp <- data_temp %>%
        dplyr::group_by(
          time, concentration, color
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

      # Create color map
      color_map <- setNames(data_temp$color, data_temp$concentration)

      # Create initial plot base, add a dashed vertical line if
      # treatment_time_value > min()
      if (display_metric == "mean_se") {
        plot_01 <- ggplot2::ggplot(
          data_temp,
          ggplot2::aes(
            x = time, y = mean_value,
            group = concentration, color = concentration
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
            alpha = 0.75
          ) +
          ggplot2::geom_errorbar(
            data = data_temp,
            aes(x = time, ymin = y_min, ymax = y_max,
                color = concentration),
            inherit.aes = FALSE,
            width = 4,
            alpha = 0.75
          ) +
          ggplot2::geom_point(
            data = data_temp,
            aes(x = time, y = mean_value,
                color = concentration),
            size = geom_point_size,
            inherit.aes = FALSE,
            alpha = 0.75
          ) +
          ggplot2::scale_color_manual(
            values = color_map
          )

        # Create new y-axis label
        growth_metric_name <- paste0("Mean ", growth_metric_name)
      } else if (display_metric == "mean") {
        plot_01 <- ggplot2::ggplot(
          data_temp,
          ggplot2::aes(
            x = time, y = mean_value,
            group = concentration, color = concentration
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
            alpha = 0.75
          ) +
          ggplot2::geom_point(
            data = data_temp,
            aes(x = time, y = mean_value,
                color = concentration),
            size = geom_point_size,
            inherit.aes = FALSE,
            alpha = 0.75
          ) +
          ggplot2::scale_color_manual(
            values = color_map
          )

        # Create new y-axis label
        growth_metric_name <- paste0("Mean ", growth_metric_name)
      } else if (display_metric == "median") {
        plot_01 <- ggplot2::ggplot(
          data_temp,
          ggplot2::aes(
            x = time, y = median_value,
            group = concentration, color = concentration
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
            alpha = 0.75
          ) +
          ggplot2::geom_point(
            data = data_temp,
            aes(x = time, y = median_value,
                color = concentration),
            size = geom_point_size,
            inherit.aes = FALSE,
            alpha = 0.75
          ) +
          ggplot2::scale_color_manual(
            values = color_map
          )

        # Create new y-axis label
        growth_metric_name <- paste0("Median ", growth_metric_name)
      }
    }else{
      # Create color map
      color_map <- setNames(data_temp$color, data_temp$concentration)

      # Show all points and group by well
      plot_01 <- ggplot2::ggplot(
        data_temp,
        ggplot2::aes(
          x = time, y = growth_metric,
          group = well, color = concentration
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
          alpha = 0.75
        ) +
        ggplot2::geom_point(
          data = data_temp,
          aes(x = time, y = growth_metric,
              color = concentration),
          size = geom_point_size,
          inherit.aes = FALSE,
          alpha = 0.75
        ) +
        ggplot2::scale_color_manual(
          values = color_map
        )
    }
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

  # Label legend
  if (show_controls == TRUE) {
    plot_01 <- plot_01 +
      ggplot2::labs(color = "Treatment")
  } else {
    plot_01 <- plot_01 +
      ggplot2::labs(color = paste0("Concentration (", concentration_units, ")"))
  }

  # Convert to plotly if applicable
  if (make_interactive == TRUE) {
    plot_01 <- plotly::ggplotly(plot_01)
  }

  # Return plot
  return(plot_01)
}
