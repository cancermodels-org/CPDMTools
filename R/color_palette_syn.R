color_palette_syn <- function(
    joined_growth_data_frame){
    # Create color range functions
    blue_generator <- colorRampPalette(c("#99FFFF", "#0000FF"))
    red_generator <- colorRampPalette(c("#FF9999", "#FF0000"))

    # Extract unique category, treatment_type, concentrations,
    # and combo concentrations
    color_matrix <- joined_growth_data_frame %>%
      dplyr::select(treatment_name,
                    treatment_type, concentration,
                    combo_drug1, combo_drug2,
                    combo_conc1, combo_conc2) %>%
      dplyr::distinct()

    # Extract combotherapy and its respective monotherapy drugs
    combo_drugs <- color_matrix %>%
      filter(treatment_type == "Combotherapy") %>%
      select(treatment_name, combo_drug1, combo_drug2) %>%
      distinct()

    # Create colors for combo drug1 based on range of monotherapy and combo
    combo_drug1_concs <- color_matrix %>%
      filter((treatment_name == combo_drugs$combo_drug1[1] |
               !is.na(combo_drug1))) %>%
      mutate(shared_conc =
               case_when(
                 !is.na(concentration) ~ concentration,
                 !is.na(combo_conc1) ~ combo_conc1,
               )) %>%
      pull(shared_conc) %>%
      unique() %>%
      sort()

    combo_drug2_concs <- color_matrix %>%
      filter((treatment_name == combo_drugs$combo_drug2[1] |
                !is.na(combo_drug2))) %>%
      mutate(shared_conc =
               case_when(
                 !is.na(concentration) ~ concentration,
                 !is.na(combo_conc2) ~ combo_conc2,
               )) %>%
      select(shared_conc) %>%
      pull(shared_conc) %>%
      unique() %>%
      sort()

    # Create an expanded grid
    combo_grid <- expand.grid(drug1 = combo_drug1_concs,
                              drug2 = combo_drug2_concs)

    # Generate individual colors for each drug concentration
    combo_grid$RedColor <- red_generator(length(unique(combo_drug1_concs)))[match(combo_grid$drug1, sort(unique(combo_drug1_concs)))]
    combo_grid$BlueColor <- blue_generator(length(unique(combo_drug2_concs)))[match(combo_grid$drug2, sort(unique(combo_drug2_concs)))]

    # Convert hex colors to RGB for blending
    red_rgb  <- hex2RGB(combo_grid$RedColor)
    blue_rgb <- hex2RGB(combo_grid$BlueColor)

    # Blend colors based on relative drug proportions
    blend_ratio <- combo_grid$drug2 / (combo_grid$drug1 + combo_grid$drug2 + 1e-6)  # Avoid division by zero
    combo_grid$MixedColor <- hex(mixcolor(blend_ratio, red_rgb, blue_rgb))

    ggplot(combo_grid, aes(x = factor(drug1), y = factor(drug2), fill = MixedColor)) +
      geom_tile() +
      scale_fill_identity() +  # Use direct hex colors
      theme_minimal() +
      labs(title = "Synergy Gradient Based on Drug Combination",
           x = "Drug 1 Concentration",
           y = "Drug 2 Concentration") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))


    for(a in unique(color_matrix[,category])){
      # Filter data
      data_temp <- color_matrix %>%
        filter(!!rlang::sym(category) == a)

      # Create empty dataframe
      data_colors <- data_temp[0,]
      data_colors <- data_colors %>%
        dplyr::mutate(color = NA)

      # Monotherapy
      if(any(data_temp$treatment_type == "Monotherapy")){
        data_mono <- data_temp %>%
          dplyr::filter(treatment_type == "Monotherapy") %>%
          dplyr::arrange(concentration)

        data_mono <- data_mono %>%
          dplyr::mutate(color = viridis::viridis(nrow(data_mono)))

        data_colors <- data_colors %>%
          dplyr::bind_rows(data_mono)
      }

      # Media Control
      if(any(data_temp$treatment_type == "Media Control")){
        data_media <- data_temp %>%
          dplyr::filter(treatment_type == "Media Control") %>%
          dplyr::mutate(color = "#FF6699")

        data_colors <- data_colors %>%
          dplyr::bind_rows(data_media)
      }

      # Negative Control
      if(any(data_temp$treatment_type == "Negative Control")){
        data_neg <- data_temp %>%
          dplyr::filter(treatment_type == "Negative Control") %>%
          dplyr::mutate(color = "black")

        data_colors <- data_colors %>%
          dplyr::bind_rows(data_neg)
      }

      # Positive Control
      if(any(data_temp$treatment_type == "Positive Control")){
        data_pos <- data_temp %>%
          dplyr::filter(treatment_type == "Positive Control") %>%
          dplyr::mutate(color = "#FF6600")

        data_colors <- data_colors %>%
          dplyr::bind_rows(data_neg)
      }
    }
}
