# Load required libraries
library(dplyr)
library(colorspace)
library(ggplot2)

# Create color range functions
blue_generator <- colorRampPalette(c("#99FFFF", "#0000FF"))
red_generator <- colorRampPalette(c("#FF9999", "#FF0000"))

# Extract unique category, treatment_type, concentrations, and combo concentrations
color_matrix <- joined_growth_data_frame %>%
  dplyr::select(
    treatment_name, treatment_type, concentration,
    combo_drug1, combo_drug2, combo_conc1, combo_conc2
  ) %>%
  dplyr::distinct()

# Extract combotherapy and its respective monotherapy drugs
combo_drugs <- color_matrix %>%
  filter(treatment_type == "Combotherapy") %>%
  select(treatment_name, combo_drug1, combo_drug2) %>%
  distinct()

# Create colors for combo drug1 based on range of monotherapy and combo
combo_drug1_concs <- color_matrix %>%
  filter(treatment_name == combo_drugs$combo_drug1[1] | !is.na(combo_drug1)) %>%
  mutate(
    shared_conc =
      case_when(
        !is.na(concentration) ~ concentration,
        !is.na(combo_conc1) ~ combo_conc1
      )
  ) %>%
  select(shared_conc) %>%
  distinct()
combo_drug1_concs <- combo_drug1_concs %>%
  arrange(shared_conc) %>%
  mutate(rank1 = seq(0, 1, length.out = nrow(combo_drug1_concs)))

combo_drug2_concs <- color_matrix %>%
  filter(treatment_name == combo_drugs$combo_drug2[1] | !is.na(combo_drug2)) %>%
  mutate(
    shared_conc =
      case_when(
        !is.na(concentration) ~ concentration,
        !is.na(combo_conc2) ~ combo_conc2
      )
  ) %>%
  select(shared_conc) %>%
  distinct()
combo_drug2_concs <- combo_drug2_concs %>%
  arrange(shared_conc) %>%
  mutate(rank2 = seq(0, 1, length.out = nrow(combo_drug2_concs)))

# Create an expanded grid
combo_grid <- expand.grid(
  rank1 = combo_drug1_concs$rank1,
  rank2 = combo_drug2_concs$rank2
)

# Generate individual colors for each drug concentration
combo_grid$redcolor <- red_generator(
  length(combo_drug1_concs$rank1))[match(combo_grid$rank1,
                                         sort(unique(combo_drug1_concs$rank1)))]
combo_grid$bluecolor <- blue_generator(
  length(combo_drug2_concs$rank2))[match(combo_grid$rank2,
                                         sort(unique(combo_drug2_concs$rank2)))]

# Convert hex colors to RGB for blending
red_rgb <- hex2RGB(combo_grid$redcolor)
blue_rgb <- hex2RGB(combo_grid$bluecolor)

# Blend colors based on relative drug proportions
blend_ratio <- combo_grid$rank2 / (combo_grid$rank1 + combo_grid$rank2 + 1e-6)

# Ensure proper referencing of column names
combo_grid$mixedcolor <- hex(mixcolor(blend_ratio, red_rgb, blue_rgb))

# Join comb_grid with concentrations
combo_grid <- combo_grid %>%
  left_join(combo_drug1_concs %>%
    rename(shared_drug1_conc = shared_conc)) %>%
  left_join(combo_drug2_concs %>%
    rename(shared_drug2_conc = shared_conc))

# Loop through and join colors to concentrations
data_temp1 <- color_matrix %>%
  filter((treatment_type == "Combotherapy" |
           treatment_name %in% c(combo_drugs$combo_drug1[1],
                                 combo_drugs$combo_drug2[1]))) %>%
  mutate(color = NA)
for(a in 1:nrow(data_temp1)){
  if(data_temp1$treatment_type[a] == "Monotherapy" &
     data_temp1$treatment_name[a] == combo_drugs$combo_drug1[1]){
    # Extract color value
    color_value <- combo_grid %>%
      filter(shared_drug1_conc == data_temp1$concentration[a]) %>%
      pull(redcolor) %>%
      unique()

    # Assign color value
    data_temp1$color[a] <- color_value

  }

  if(data_temp1$treatment_type[a] == "Monotherapy" &
     data_temp1$treatment_name[a] == combo_drugs$combo_drug2[1]){
    # Extract color value
    color_value <- combo_grid %>%
      filter(shared_drug2_conc == data_temp1$concentration[a]) %>%
      pull(bluecolor) %>%
      unique()

    # Assign color value
    data_temp1$color[a] <- color_value

  }

  if(data_temp1$treatment_type[a] == "Combotherapy"){
    color_value <- combo_grid %>%
      filter(shared_drug1_conc == data_temp1$combo_conc1[a],
             shared_drug2_conc == data_temp1$combo_conc2[a]) %>%
      pull(mixedcolor) %>%
      unique()

    # Assign color value
    data_temp1$color[a] <- color_value
  }

  rm(color_value)

}
rm(a, combo_drug1_concs, combo_drug2_concs,
   combo_drugs, combo_grid, blend_ratio, blue_rgb,
   red_rgb, blue_generator, red_generator)




# Generate heatmap visualization
ggplot(combo_grid, aes(x = factor(shared_drug1_conc),
                       y = factor(shared_drug2_conc),
                       fill = mixedcolor)) +
  geom_tile() +
  scale_fill_identity() + # Use direct hex colors
  theme_minimal() +
  labs(
    title = "Synergy Gradient Based on Drug Combination",
    x = "Drug 1 Concentration",
    y = "Drug 2 Concentration"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
