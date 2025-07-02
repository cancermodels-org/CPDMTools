#' DSS3 Bar Chart
#'
#' @param ctg_list A list object output from the dr4pl_fit_loop function
#' @param score_label_size A numeric value specifying the size of the
#' score labels above the bars. Defaults to 4.
#' @inheritParams growth_plot_qc_mono
#'
#' @returns A ggplot2 bar chart
#' @import ggplot2
#' @importFrom viridis viridis
#' @export
#'
ctg_dss_barplot <- function(
    ctg_list,
    x_axis_text_size = 12,
    y_axis_text_size = 12,
    y_axis_title_size = 14,
    score_label_size = 4,
    plot_title_size = 20){
  # Extract data
  dss_data <- ctg_list[["model_parameters"]]

  # Add colors
  dss_data <- dss_data %>%
    dplyr::mutate(color = viridis::viridis(nrow(dss_data)))

  # Create barchart
  bar_plot <- ggplot2::ggplot(
    dss_data,
    ggplot2::aes(
      x = treatment_name,
      y = dss3,
      fill = color
    )
  ) +
    ggplot2::geom_bar(
      stat = "identity",
      position = "dodge",
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = round(dss3, 2)),
      position = position_dodge(width = 0.9),
      vjust = -0.5,
      size = score_label_size
    ) +
    ggplot2::theme_classic() +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_y_continuous(
      expand = c(0, 0),
      limits = c(0, 110)
    ) +
    ggplot2::ggtitle("DSS3 by Treatment") +
    ggplot2::ylab("DSS3") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        hjust = 0.5,
        size = plot_title_size,
        face = "bold"
      ),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(
        size = y_axis_title_size,
        color = "black", face = "bold"
      ),
      axis.text.x = ggplot2::element_text(
        size = x_axis_text_size,
        color = "black",
        angle = 60,
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

  # Return bar chart
  return(bar_plot)

}
