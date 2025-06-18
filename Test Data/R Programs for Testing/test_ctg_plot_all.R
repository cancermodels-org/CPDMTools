# Base plot
plot_01 <- ggplot2::ggplot(
  data = data_long,
  ggplot2::aes(x = log10(concentration),
               y = value_norm,
               color = treatment_name)
) +
  annotation_logticks(sides = "b") +
  geom_point() +
  theme_classic() +
  scale_color_viridis_d()

# --- Add Drug_A ---
params_A <- data_model %>% filter(treatment_name == "Drug_A")
plot_01 <- plot_01 +
  ggplot2::stat_function(
    fun = function(x) {
      conc <- 10^x
      params_A$lower_asy_est[1] +
        (params_A$upper_asy_est[1] - params_A$lower_asy_est[1]) /
        (1 + ((conc / params_A$rel_ic50_est[1])^params_A$slope_est[1]))
    },
    color = params_A$color[1],
    linewidth = geom_line_width,
    alpha = 0.90,
    inherit.aes = FALSE
  )

# --- Add Drug_F ---
params_F <- data_model %>% filter(treatment_name == "Drug_F")
plot_01 <- plot_01 +
  ggplot2::stat_function(
    fun = function(x) {
      conc <- 10^x
      params_F$lower_asy_est[1] +
        (params_F$upper_asy_est[1] - params_F$lower_asy_est[1]) /
        (1 + ((conc / params_F$rel_ic50_est[1])^params_F$slope_est[1]))
    },
    color = params_F$color[1],
    linewidth = geom_line_width,
    alpha = 0.90,
    inherit.aes = FALSE
  )

# Plot
plot_01


## Version two
# Start base plot
plot_01 <- ggplot2::ggplot(
  data = data_long,
  ggplot2::aes(x = log10(concentration),
               y = value_norm,
               color = treatment_name)
) +
  annotation_logticks(sides = "b") +
  geom_point() +
  theme_classic() +
  scale_color_viridis_d()

# Loop through each row in data_model
for (a in 1:nrow(data_model)) {
  data_model_sub <- data_model[a, ]  # grab one row at a time

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

# Show the plot
plot_01


