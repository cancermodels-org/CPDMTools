#' Create Dose-Response Curve Plot for All Treatments
#'
#' @inheritParams ctg_treat_plot_ind
#' @param plot_title_size A numeric value specifying the size of the plot
#' title. Defaults to 18. See \code{\link[ggplot2]{element_text}}.
#' @param plot_sub_title_size A numeric value specifying the size of the plot
#' title. Defaults to 16. See \code{\link[ggplot2]{element_text}}.
#'
#' @returns A ggplot2 or plotly object
#' @import ggplot2
#' @importFrom dplyr group_by mutate summarize ungroup
#' @importFrom magrittr %>%
#' @importFrom plotly ggplotly layout
#' @importFrom scales math_format trans_breaks trans_format
#' @importFrom stats sd
#' @importFrom viridis scale_color_viridis viridis
#' @export
#'
ctg_treat_plot_all <- function(
    ctg_list,
    x_scale = "log",
    display_type = "points",
    y_axis_title = "Normalized Value",
    make_interactive = FALSE,
    x_limits = c(NA, NA),
    x_axis_breaks = ggplot2::waiver(),
    n_x_axis_breaks = 6,
    y_limits = c(NA, NA),
    y_axis_breaks = ggplot2::waiver(),
    n_y_axis_breaks = 7,
    x_axis_text_size = 12,
    y_axis_text_size = 12,
    x_axis_title_size = 14,
    y_axis_title_size = 14,
    plot_title_size = 18,
    plot_sub_title_size = 16,
    geom_point_size = 2.5,
    geom_line_width = 1) {
  # Assign colors
  ctg_list[["model_parameters"]] <- ctg_list[["model_parameters"]] %>%
    dplyr::mutate(color = viridis::viridis(nrow(ctg_list[["model_parameters"]])))

  # Extract data
  data_long <- ctg_list[["monotherapy"]]
  data_model <- ctg_list[["model_parameters"]]
  data_table <- ctg_list[["table"]]

  # Create y_limits if not provided
  if (all(is.na(y_limits))) {
    y_limits <- c(0, 1.5)
  }

  if (x_scale == "log") {
    # Create initial base plot
    plot_01 <- ggplot2::ggplot(
      data = data_long,
      ggplot2::aes(x = log10(concentration),
                   y = value_norm,
                   color = treatment_name)
    )
  } else {
    # Create initial base plot
    plot_01 <- ggplot2::ggplot(
      data = data_long,
      ggplot2::aes(x = concentration,
                   y = value_norm,
                   color = treatment_name)
    ) +
      ggplot2::scale_x_log10(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
      ) +
      ggplot2::annotation_logticks(sides = "b")
  }

  # Loop through each row in data_model
  if(x_scale == "log"){
    for (a in 1:nrow(data_model)) {
      # Subset data
      data_model_sub <- data_model[a, ]

      # Check if parameters are complete, if not skip that treatment
      if(any(
        is.na(data_model_sub$lower_asy_est),
        is.na(data_model_sub$upper_asy_est),
        is.na(data_model_sub$rel_ic50_est),
        is.na(data_model_sub$slope_est)
      )){
        next
      }

      # Add dose-reponse curve to plot
      plot_01 <- plot_01 +
        ggplot2::stat_function(
          fun = local({
            # Capture parameters locally for each iteration
            lower <- data_model_sub$lower_asy_est
            upper <- data_model_sub$upper_asy_est
            ic50  <- data_model_sub$rel_ic50_est
            slope <- data_model_sub$slope_est
            function(x) {
              conc <- 10^x
              lower + (upper - lower) / (1 + ((conc / ic50)^slope))
            }
          }),
          color = data_model_sub$color,
          linewidth = geom_line_width,
          alpha = 0.90,
          inherit.aes = FALSE
        )
    }
  }else{
    for (a in 1:nrow(data_model)) {
      # Subset data
      data_model_sub <- data_model[a, ]

      # Check if parameters are complete, if not skip that treatment
      if(any(
        is.na(data_model_sub$lower_asy_est),
        is.na(data_model_sub$upper_asy_est),
        is.na(data_model_sub$rel_ic50_est),
        is.na(data_model_sub$slope_est)
      )){
        next
      }

      # Add dose-reponse curve to plot
      plot_01 <- plot_01 +
        ggplot2::stat_function(
          fun = local({
            # Capture parameters locally for each iteration
            lower <- data_model_sub$lower_asy_est
            upper <- data_model_sub$upper_asy_est
            ic50  <- data_model_sub$rel_ic50_est
            slope <- data_model_sub$slope_est
            function(x) {
              conc <- x
              lower + (upper - lower) / (1 + ((conc / ic50)^slope))
            }
          }),
          color = data_model_sub$color,
          linewidth = geom_line_width,
          alpha = 0.90,
          inherit.aes = FALSE
        )
    }
  }

  # Display points if applicable
  if (display_type == "points") {
    if (x_scale == "log") {
      # Add data points
      plot_01 <- plot_01 +
        ggplot2::geom_point(
          data = data_long,
          ggplot2::aes(
            x = log10(concentration),
            y = value_norm,
            color = treatment_name,
            text = paste0(
              "Treatment Name", treatment_name, "<br>",
              "Well: ", well, "<br>",
              "Concentration: ", concentration, "<br>",
              "Value Norm: ", round(value_norm, 3)
            )
          ),
          size = geom_point_size,
          alpha = 0.85
        )
    } else {
      plot_01 <- plot_01 +
        ggplot2::geom_point(
          data = data_long,
          ggplot2::aes(
            x = concentration,
            y = value_norm,
            color = treatment_name,
            text = paste0(
              "Treatment Name", treatment_name, "<br>",
              "Well: ", well, "<br>",
              "Concentration: ", concentration, "<br>",
              "Value Norm: ", round(value_norm, 3)
            )
          ),
          size = geom_point_size,
          alpha = 0.85
        )
    }
  }

  # Display mean/se bars if applicable
  if (display_type == "se_bars") {
    # Calculate summary statistics
    ctg_summary <- data_long %>%
      dplyr::group_by(treatment_name, concentration) %>%
      dplyr::summarize(
        mean_value = mean(value_norm, na.rm = TRUE),
        sd_value = stats::sd(value_norm, na.rm = TRUE),
        se_value = sd_value / sqrt(n()),
        y_min = mean_value - se_value,
        y_max = mean_value + se_value
      ) %>%
      dplyr::ungroup()

    # Create Mean/SE bars if applicable
    if (display_type == "se_bars") {
      if (x_scale == "log") {
        # Add mean_se bars
        plot_01 <- plot_01 +
          ggplot2::geom_point(
            data = ctg_summary,
            aes(
              x = log10(concentration),
              y = mean_value,
              color = treatment_name
            ),
            size = 2,
            alpha = 0.75
          ) +
          ggplot2::geom_errorbar(
            data = ctg_summary,
            aes(
              x = log10(concentration),
              ymin = y_min,
              ymax = y_max,
              color = treatment_name
            ),
            inherit.aes = FALSE,
            width = 0.1,
            alpha = 0.75
          )
      } else {
        # Add mean_se bars
        plot_01 <- plot_01 +
          ggplot2::geom_point(
            data = ctg_summary,
            aes(
              x = concentration,
              y = mean_value,
              color = treatment_name
            ),
            size = 2,
            alpha = 0.75
          ) +
          ggplot2::geom_errorbar(
            data = ctg_summary,
            aes(
              x = concentration,
              ymin = mean_value - se_value,
              ymax = mean_value + se_value,
              color = treatment_name
            ),
            inherit.aes = FALSE,
            width = 0.1,
            alpha = 0.75
          )
      }
    }
  }

  # Finalize plot layers
  if (x_scale == "log") {
    plot_01 <- plot_01 +
      ggplot2::theme_classic() +
      viridis::scale_color_viridis(discrete = TRUE) +
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
      ggplot2::xlab(
        paste0("Log10(Concentration (", data_model$conc_unit[1], "))")
      )
  } else {
    plot_01 <- plot_01 +
      ggplot2::theme_classic() +
      viridis::scale_color_viridis(discrete = TRUE) +
      ggplot2::scale_y_continuous(
        expand = c(0, 0),
        limits = y_limits,
        n.breaks = n_y_axis_breaks,
        breaks = y_axis_breaks
      ) +
      ggplot2::xlab(
        paste0("Concentration (", data_model$conc_unit[1], ")")
      )
  }

  # Add titles and themes
  plot_01 <- plot_01 +
    ggplot2::ggtitle("Dose-Response Curves",
                     subtitle = "All Treatments") +
    ggplot2::ylab(y_axis_title) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5, size = plot_title_size, face = "bold"
      ),
      plot.subtitle = ggplot2::element_text(
        hjust = 0.5, size = plot_sub_title_size, face = "bold"
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
      legend.title = ggplot2::element_text(
        hjust = 0.5, face = "bold"
      )
    ) +
    ggplot2::labs(color = "Treatment Name") +
    ggplot2::coord_cartesian(clip = "off")

  # Make interactive if applicable
  if (make_interactive == TRUE) {
    plot_01 <- plotly::ggplotly(plot_01, tooltip = "text") %>%
      plotly::layout(showlegend = FALSE)
  }

  # Return plot
  return(plot_01)
}
