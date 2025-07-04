#' Create Dose-Response Curve Plot for Single Treatment
#'
#' @param ctg_list A list object generated by the dr4pl_git_loop function
#' @param treat_name A character string specifying the treatment_name to filter
#' on.
#' @param x_scale A character string specifying whether the x-axis should be
#' transformed. Values include "log" for log-transformed concentration and
#' "standard" for non-transformed concentration scale. Defaults to "log".
#' @param display_type A character string specifying whether to display the
#' normalized values "points" or the mean+/-se bars "se_bars". Defaults to
#' "points"
#' @param sub_title A character string specifying what kind of sub titles to
#' display on the plat. Values include "none" for no subtitle, "ic50" for the
#' relative IC50, "dss3" for the drug sensitivity score, or "both" for
#' displaying the relative IC50 and the DSS3 score. Defaults to "both"
#' @param plot_title_size A numeric value specifying the size of the plot
#' title. Defaults to 18. See \code{\link[ggplot2]{element_text}}.
#' @inheritParams ctg_qc_treat_plot
#'
#' @returns A ggplot2 object or a plotly object
#' @import ggplot2
#' @importFrom dplyr filter group_by mutate summarize ungroup
#' @importFrom magrittr %>%
#' @importFrom plotly ggplotly layout
#' @importFrom scales math_format trans_breaks trans_format
#' @importFrom stats sd
#' @importFrom stringr str_split_i
#' @importFrom viridis viridis
#' @export
#'
ctg_treat_plot_ind <- function(
    ctg_list,
    treat_name = "",
    x_scale = "log",
    display_type = "points",
    sub_title = "both",
    y_axis_title = "Normalized Value",
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
    plot_title_size = 18,
    geom_point_size = 2.5,
    geom_line_width = 1) {
  # Assign colors
  ctg_list[["model_parameters"]] <- ctg_list[["model_parameters"]] %>%
    dplyr::mutate(color = viridis::viridis(nrow(ctg_list[["model_parameters"]])))

  # Extract data
  data_long <- ctg_list[["monotherapy"]] %>%
    dplyr::filter(treatment_name == treat_name)
  data_model <- ctg_list[["model_parameters"]] %>%
    dplyr::filter(treatment_name == treat_name)
  data_table <- ctg_list[["table"]] %>%
    dplyr::filter(treatment_name == treat_name)

  # Create plot title and subtitle if applicable
  if (sub_title == "none") {
    plot_title <- treat_name
  }
  if (sub_title == "ic50") {
    ic50 <- as.character(data_table[
      which(data_table$Variable == "Relative IC50 estimate [95% CI]"),
      "Value"
    ])
    ic50 <- stringr::str_split_i(ic50, " \\[", 1)
    plot_title <- paste0(
      treat_name, "\n",
      "Relative IC50: ", ic50
    )
  }
  if (sub_title == "dss3") {
    dss3 <- as.character(data_table[
      which(data_table$Variable == "Drug Sensitivity Score (DSS3)"),
      "Value"
    ])
    plot_title <- paste0(
      treat_name, "\n",
      "DSS3: ", dss3
    )
  }
  if (sub_title == "both") {
    ic50 <- as.character(data_table[
      which(data_table$Variable == "Relative IC50 estimate [95% CI]"),
      "Value"
    ])
    ic50 <- stringr::str_split_i(ic50, " \\[", 1)
    dss3 <- as.character(data_table[
      which(data_table$Variable == "Drug Sensitivity Score (DSS3)"),
      "Value"
    ])
    plot_title <- paste0(
      treat_name, "\n",
      "Relative IC50: ", ic50, " - DSS3: ", dss3
    )
  }

  # Add color
  data_long <- data_long %>%
    dplyr::mutate(color = data_model$color[1])

  # Create y_limits if not provided
  if (all(is.na(y_limits))) {
    y_limits <- c(0, 1.5)
  }

  if (x_scale == "log") {
    # Create initial base plot
    plot_01 <- ggplot2::ggplot(
      data = data_long,
      ggplot2::aes(x = log10(concentration), y = value_norm)
    )
  } else {
    # Create initial base plot
    plot_01 <- ggplot2::ggplot(
      data = data_long,
      ggplot2::aes(x = concentration, y = value_norm)
    ) +
      ggplot2::scale_x_log10(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
      ) +
      ggplot2::annotation_logticks(sides = "b")
  }

  # Add dose-response curve
  if (all(
    !is.na(data_model$slope_est[1]),
    !is.na(data_model$lower_asy_est[1]),
    !is.na(data_model$upper_asy_est[1]),
    !is.na(data_model$rel_ic50_est[1])
  )) {
    if (x_scale == "log") {
      plot_01 <- plot_01 +
        ggplot2::stat_function(
          data = data_long,
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
    } else {
      plot_01 <- plot_01 +
        ggplot2::stat_function(
          data = data_long,
          fun = function(x) {
            conc <- x
            data_model$lower_asy_est[1] +
              (data_model$upper_asy_est[1] - data_model$lower_asy_est[1]) /
                (1 + ((conc / data_model$rel_ic50_est[1])^data_model$slope_est[1]))
          },
          color = data_model$color[1],
          linewidth = geom_line_width,
          alpha = 0.90
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
            text = paste0(
              "Well: ", well, "<br>",
              "Concentration: ", concentration, "<br>",
              "Value Norm: ", round(value_norm, 3)
            )
          ),
          color = data_model$color[1],
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
            text = paste0(
              "Well: ", well, "<br>",
              "Concentration: ", concentration, "<br>",
              "Value Norm: ", round(value_norm, 3)
            )
          ),
          color = data_model$color[1],
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
              text = paste0(
                "Treatment: ", treatment_name, "<br>",
                "Mean Value Norm: ", round(mean_value, 3)
              )
            ),
            color = data_model$color[1],
            size = 2,
            inherit.aes = FALSE,
            alpha = 0.75
          ) +
          ggplot2::geom_errorbar(
            data = ctg_summary,
            aes(
              x = log10(concentration),
              ymin = y_min,
              ymax = y_max,
            ),
            color = data_model$color[1],
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
              text = paste0(
                "Treatment: ", treatment_name, "<br>",
                "Mean Value Norm: ", round(mean_value, 3)
              )
            ),
            color = data_model$color[1],
            size = 2,
            inherit.aes = FALSE,
            alpha = 0.75
          ) +
          ggplot2::geom_errorbar(
            data = ctg_summary,
            aes(
              x = concentration,
              ymin = mean_value - se_value,
              ymax = mean_value + se_value,
            ),
            color = data_model$color[1],
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
      ggplot2::xlab(
        paste0("Log10(Concentration (", data_model$conc_unit[1], "))")
      )
  } else {
    plot_01 <- plot_01 +
      ggplot2::theme_classic() +
      ggplot2::scale_color_identity() +
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
    ggplot2::ggtitle(plot_title) +
    ggplot2::ylab(y_axis_title) +
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
