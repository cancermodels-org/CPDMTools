# Add y=0 baseline if needed
if (min(y_limits, na.rm = TRUE) < 0) {
  plot_01 <- plot_01 + geom_hline(yintercept = 0)
}

# Create plot
plot_01 <- ggplot(
  data = predicted_df,
  aes(x = log10(concentration), y = pred_ctg_value_norm)
)

if (min_y < 0) {
  plot_01 <- plot_01 +
    geom_hline(yintercept = 0)
}

# Replace geom_line with stat_function
plot_01 <- plot_01 +
  stat_function(
    fun = function(x) {
      conc <- 10^x
      data_model$lower_asy_est[1] +
        (data_model$upper_asy_est[1] - data_model$lower_asy_est[1]) /
        (1 + (conc / data_model$rel_ic50_est[1])^data_model$slope_est[1])
    },
    color = data_model$color[1],
    linewidth = geom_line_width
  ) +
  geom_point(
    data = data_temp,
    aes(
      x = log10(concentration),
      y = ctg_value_norm,
      color = color_new,
      text = paste0(
        "Well: ", well, "<br>",
        "Concentration: ", concentration, "<br>",
        "CTG Value Norm: ", ctg_value_norm
      )
    ),
    size = geom_point_size,
    inherit.aes = FALSE
  ) +
  theme_classic() +
  scale_color_identity() +
  scale_x_continuous(
    limits = x_limits,
    n.breaks = n_x_axis_breaks,
    breaks = x_axis_breaks
  ) +
  scale_y_continuous(
    limits = y_limits,
    n.breaks = n_y_axis_breaks,
    breaks = y_axis_breaks
  )
