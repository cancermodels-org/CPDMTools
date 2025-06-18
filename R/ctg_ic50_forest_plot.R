#' CTG IC50 Forest Plot
#'
#' @param ctg_list A list object output from the d4pl_fit_loop function
#' @param x_scale A character string specifying whether the x-axis should be
#' transformed. Values include "log" for log-transformed concentration and
#' "standard" for non-transformed concentration scale. Defaults to "log".
#' @param n_x_axis_breaks An integer specifying the number of major breaks
#' for the x-axis. Defaults to 6.
#' @inheritParams ctg_qc_treat_plot
#'
#' @returns A ggplot2 object
#' @import ggplot2
#' @importFrom dplyr case_when filter mutate
#' @importFrom grid arrow unit
#' @importFrom magrittr %>%
#' @importFrom scales label_math log_breaks
#' @importFrom viridis viridis
#' @export
#'
ctg_ic50_forest_plot <- function(
    ctg_list,
    x_scale = "log",
    x_axis_text_size = 12,
    y_axis_text_size = 12,
    x_axis_title_size = 14,
    n_x_axis_breaks = 8,
    plot_title_size = 20){

  # Extract data
  ic50_data <- ctg_list[["model_parameters"]]

  # Add colors
  ic50_data <- ic50_data %>%
    dplyr::mutate(color =
                    viridis::viridis(nrow(ic50_data)))

  # Define concentration bounds
  min_conc_value <- min(ic50_data$min_conc, na.rm = TRUE)
  max_conc_value <- max(ic50_data$max_conc, na.rm = TRUE)

  # Prepare data
  data_temp <- ic50_data %>%
    dplyr::mutate(
      treatment_name = factor(treatment_name,
                              levels = rev(treatment_name)),
      ci_lb_clip = pmax(rel_ic50_lb, min_conc_value),
      ci_ub_clip = pmin(rel_ic50_ub, max_conc_value),
      ci_lb_trunc = rel_ic50_lb < min_conc_value,
      ci_ub_trunc = rel_ic50_ub > max_conc_value,
      rel_ic50_est = dplyr::case_when(
        (rel_ic50_est > ci_lb_clip &
          rel_ic50_est < ci_ub_clip) ~ rel_ic50_est
      )
    )

  if(x_scale == "log"){
    # Plot data
    plot_01 <- ggplot2::ggplot(data = data_temp,
                               aes(color = color)) +
      ggplot2::geom_segment(
        ggplot2::aes(
          x = log10(ci_lb_clip), xend = log10(ci_ub_clip),
          y = treatment_name, yend = treatment_name),
        linewidth = 1) +
      ggplot2::geom_point(
        ggplot2::aes(x = log10(rel_ic50_est), y = treatment_name),
        shape = 16, size = 3) +
      ggplot2::geom_segment(
        data = data_temp %>% dplyr::filter(ci_lb_trunc),
        ggplot2::aes(
          x = log10(ci_lb_clip), xend = log10(ci_lb_clip)-0.2,
          y = treatment_name, yend = treatment_name
        ),
        arrow = grid::arrow(length = unit(0.2, 'cm'), type = "closed"),
        linewidth = 1
      ) +
      ggplot2::geom_segment(
        data = data_temp %>% dplyr::filter(ci_ub_trunc),
        ggplot2::aes(
          x = log10(ci_ub_clip), xend = log10(ci_ub_clip)+0.2,
          y = treatment_name, yend = treatment_name
        ),
        arrow = grid::arrow(length = unit(0.2, 'cm'), type = "closed"),
        linewidth = 1
      ) +
      ggplot2::theme_classic() +
      ggplot2::scale_x_continuous(
        limits = c(log10(min_conc_value/10), log10(max_conc_value*10)),
        n.breaks = n_x_axis_breaks
      ) +
      ggplot2::xlab(
        paste0("Log10(Concentration (", data_temp$conc_unit[1], "))")
      )
  }else{
    # Plot data
    plot_01 <- ggplot2::ggplot(data = data_temp,
                               aes(color = color)) +
      ggplot2::geom_segment(
        ggplot2::aes(
          x = ci_lb_clip, xend = ci_ub_clip,
          y = treatment_name, yend = treatment_name),
        linewidth = 1) +
      ggplot2::geom_point(
        ggplot2::aes(x = rel_ic50_est, y = treatment_name),
        shape = 16, size = 3) +
      ggplot2::geom_segment(
        data = data_temp %>% dplyr::filter(ci_lb_trunc),
        ggplot2::aes(
          x = ci_lb_clip, xend = ci_lb_clip,
          y = treatment_name, yend = treatment_name
        ),
        arrow = grid::arrow(length = unit(0.2, 'cm'), type = "closed"),
        linewidth = 1,
      ) +
      ggplot2::geom_segment(
        data = data_temp %>% dplyr::filter(ci_ub_trunc),
        ggplot2::aes(
          x = ci_ub_clip, xend = ci_ub_clip+0.2,
          y = treatment_name, yend = treatment_name
        ),
        arrow = grid::arrow(length = unit(0.2, 'cm'), type = "closed"),
        linewidth = 1
      ) +
      ggplot2::theme_classic() +
      ggplot2::scale_x_log10(
        limits = c(min_conc_value/10, max_conc_value*10),
        #breaks = scales::trans_breaks("log10", function(x) 10^x),
        #labels = scales::trans_format("log10", scales::math_format(10^.x))
        breaks = scales::log_breaks(n = n_x_axis_breaks),
        labels = scales::label_math(10^.x)
      ) +
      ggplot2::annotation_logticks(sides = "b") +
      ggplot2::xlab(
        paste0("Concentration (", data_temp$conc_unit[1], ")")
      )
  }

  # Add themes
  plot_01 <- plot_01 +
    ggplot2::scale_color_identity() +
    ggplot2::ggtitle(
      "Relative IC50 with 95% CI\nBy Treatment"
    ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        size = plot_title_size,
        face = "bold"
      ),
      axis.title.y = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_text(
        size = x_axis_title_size,
        color = "black", face = "bold"
      ),
      axis.text.x = ggplot2::element_text(
        size = x_axis_text_size,
        color = "black",
        vjust = 0.5
      ),
      axis.text.y = ggplot2::element_text(
        size = y_axis_text_size,
        color = "black"
      ),
      legend.title = ggplot2::element_text(
        hjust = 0.5, face = "bold"
      )
    )

  # Return the plot
  return(plot_01)
}
