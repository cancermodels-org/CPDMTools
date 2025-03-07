color_palette_mono <- function(
    joined_growth_data_frame,
    category = "treatment_name"){

  # If data includes well_annotation, treatment_name,
  # treatment_type, and concentration
  if(all(c(category, "treatment_type", "concentration")
         %in% colnames(joined_growth_data_frame))){
    color_matrix <- joined_growth_data_frame %>%
      select(!!rlang::sym(category), treatment_type, concentration) %>%
      distinct()

    for(a in unique(color_matrix[,category])){
      # Filter data
      data_temp <- color_matrix %>%
        filter(!!rlang::sym(category) == a)

      # Create empty dataframe
      data_colors <- data_temp[0,]
      data_colors <- data_colors %>%
        mutate(color = NA)

      # Monotherapies
      if(any(data_temp$treatment_type == "Monotherapy")){
        data_mono <- data_temp %>%
          filter(treatment_type == "Monotherapy") %>%
          arrange(concentration)

        data_mono <- data_mono %>%
          mutate(color = viridis::viridis(nrow(data_mono)))

        data_colors <- data_colors %>%
          bind_rows(data_mono)
      }

      # Media Control
      if(any(data_temp$treatment_type == "Media Control")){
        data_media <- data_temp %>%
          filter(treatment_type == "Media Control") %>%
          mutate(color = "#FF6699")

        data_colors <- data_colors %>%
          bind_rows(data_media)
      }

      # Negative Control
      if(any(data_temp$treatment_type == "Negative Control")){
        data_neg <- data_temp %>%
          filter(treatment_type == "Negative Control") %>%
          mutate(color = "black")

        data_colors <- data_colors %>%
          bind_rows(data_neg)
      }

      # Positive Control
      if(any(data_temp$treatment_type == "Positive Control")){
        data_pos <- data_temp %>%
          filter(treatment_type == "Positive Control") %>%
          mutate(color = "#FF6600")

        data_colors <- data_colors %>%
          bind_rows(data_neg)
      }

    }

  }

}

