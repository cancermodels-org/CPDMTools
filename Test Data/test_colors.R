# Load required libraries
library(dplyr)
library(colorspace)
library(ggplot2)

# Create color range functions
blue_generator <- colorRampPalette(c("#99FFFF", "#0000FF"))
red_generator <- colorRampPalette(c("#FF9999", "#FF0000"))

# Extract unique category, treatment_type, concentrations, and combo concentrations
color_matrix <- joined_growth_data_frame %>%
  dplyr::select(treatment_name, treatment_type, concentration,
                combo_drug1, combo_drug2, combo_conc1, combo_conc2) %>%
  dplyr::distinct()

# Extract combotherapy and its respective monotherapy drugs
combo_drugs <- color_matrix %>%
  filter(treatment_type == "Combotherapy") %>%
  select(treatment_name, combo_drug1, combo_drug2) %>%
  distinct()

# Create colors for combo drug1 based on range of monotherapy and combo
combo_drug1_concs <- color_matrix %>%
  filter(treatment_name == combo_drugs$combo_drug1[1] | !is.na(combo_drug1)) %>%
  mutate(shared_conc =
           case_when(
             !is.na(concentration) ~ concentration,
             !is.na(combo_conc1) ~ combo_conc1
           )) %>%
  select(shared_conc) %>%
  distinct()
combo_drug1_concs <- combo_drug1_concs %>%
  arrange(shared_conc) %>%
  mutate(rank1 = seq(0, 1, length.out = nrow(combo_drug1_concs)))

combo_drug2_concs <- color_matrix %>%
  filter(treatment_name == combo_drugs$combo_drug2[1] | !is.na(combo_drug2)) %>%
  mutate(shared_conc =
           case_when(
             !is.na(concentration) ~ concentration,
             !is.na(combo_conc2) ~ combo_conc2
           )) %>%
  select(shared_conc) %>%
  distinct()
combo_drug2_concs <- combo_drug2_concs %>%
  arrange(shared_conc) %>%
  mutate(rank2 = seq(0, 1, length.out = nrow(combo_drug2_concs)))

# Create an expanded grid
combo_grid <- expand.grid(rank1 = combo_drug1_concs$rank1,
                          rank2 = combo_drug2_concs$rank2)

# Generate individual colors for each drug concentration
combo_grid$redcolor <- red_generator(length(combo_drug1_concs$rank1))[match(combo_grid$rank1, sort(unique(combo_drug1_concs$rank1)))]
combo_grid$bluecolor <- blue_generator(length(combo_drug2_concs$rank2))[match(combo_grid$rank2, sort(unique(combo_drug2_concs$rank2)))]

# Convert hex colors to RGB for blending
red_rgb  <- hex2RGB(combo_grid$redcolor)
blue_rgb <- hex2RGB(combo_grid$bluecolor)

# Blend colors based on relative drug proportions
blend_ratio <- combo_grid$rank2 / (combo_grid$rank1 + combo_grid$rank2 + 1e-6)  # Avoid division by zero

# Ensure proper referencing of column names
combo_grid$mixedcolor <- hex(mixcolor(blend_ratio, red_rgb, blue_rgb))

# Generate heatmap visualization
ggplot(combo_grid, aes(x = factor(rank1), y = factor(rank2), fill = mixedcolor)) +
  geom_tile() +
  scale_fill_identity() +  # Use direct hex colors
  theme_minimal() +
  labs(title = "Synergy Gradient Based on Drug Combination",
       x = "Rank 1 Concentration",
       y = "Rank 2 Concentration") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
