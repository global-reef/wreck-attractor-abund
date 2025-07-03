#### BRAY-CURTIS DISSIMILARITY ####
library(dplyr)
library(tidyr)
library(vegan)

# Prepare Bray-Curtis input
bray_df <- fish_long %>%
  group_by(survey_id, Type, Species) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%
  pivot_wider(names_from = Species, values_from = Count, values_fill = 0)

# Separate metadata and species matrix
meta_df <- bray_df %>% select(survey_id, Type)
sp_mat <- bray_df %>% select(-survey_id, -Type)

# Bray–Curtis dissimilarity
bray_dist <- vegdist(sp_mat, method = "bray")

# NMDS ordination
nmds <- metaMDS(sp_mat, distance = "bray", k = 2, trymax = 100)

# Extract scores and add back metadata
nmds_scores <- as.data.frame(scores(nmds, display = "sites"))
nmds_scores$survey_id <- bray_df$survey_id  # match row order
plot_df <- left_join(nmds_scores, meta_df, by = "survey_id")

# Plot
ggplot(plot_df, aes(x = NMDS1, y = NMDS2, fill = Type)) +
  stat_ellipse(geom = "polygon", alpha = 0.3) +
  geom_point(aes(color = Type), size = 2) +
  scale_fill_manual(values = c("Natural" = "#007A87", "Artificial" = "#66BFA6")) +
  scale_color_manual(values = c("Natural" = "#007A87", "Artificial" = "#66BFA6")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "NMDS Plot (Bray–Curtis)",
    subtitle = "Community composition by Reef Type",
    x = "NMDS1",
    y = "NMDS2"
  )+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


## PERMANOVA test whether reef type (Artificial vs Natural) explains variation in fish community composition
# Make sure meta_df matches the order of sp_mat / bray_dist
adonis_result <- adonis2(
  bray_dist ~ Type,
  data = meta_df,
  permutations = 999,
  method = "bray"
)

print(adonis_result)

# check if due to unequal variances 
# Run ANOSIM
anosim_result <- anosim(bray_dist, grouping = meta_df$Type, permutations = 999)

# View result
summary(anosim_result)
plot(anosim_result)


# Both PERMANOVA (adonis2) and ANOSIM analyses revealed significant differences in fish community composition between Artificial and Natural reef types (PERMANOVA: R² = 0.10, p < 0.001; ANOSIM: R = 0.27, p = 0.001). While the overall effect was moderate, results suggest consistent compositional shifts between habitat types.



#### SIMPER ANALYSIS ####
# Run SIMPER
simper_result <- simper(sp_mat, group = meta_df$Type, permutations = 999)

# View summary of the result
summary(simper_result)

# SIMPER analysis revealed that Russels Snapper, small groupers, and butterflyfish were the strongest contributors to fish community dissimilarity between Artificial and Natural reefs, with several species showing consistent and significant differences in abundance across reef types (p < 0.01).

# Extract and process SIMPER results
simper_df <- summary(simper_result)[[1]] %>%
  as.data.frame() %>%
  rownames_to_column("Species") %>%
  arrange(desc(average)) %>%
  slice(1:10) %>%
  mutate(
    Direction = ifelse(ava > avb, "Artificial", "Natural"),
    cum_percent = round(cumsum(average) / sum(average) * 100, 1),
    Significant = ifelse(p <= 0.05, "p ≤ 0.05", "n.s."),
    Species = fct_reorder(Species, average)
  )
# Plot
ggplot(simper_df, aes(x = average, y = Species, fill = Direction, alpha = Significant)) +
  geom_col(color = "black") +
  scale_fill_manual(values = c("Natural" = "#007A87", "Artificial" = "#66BFA6")) +
  scale_alpha_manual(values = c("p ≤ 0.05" = 1, "n.s." = 0.5)) +
  labs(
    title = "Top 10 Species Contributing to Reef Type Dissimilarity",
    subtitle = "Based on Bray–Curtis SIMPER analysis",
    x = "Average Contribution to Dissimilarity",
    y = NULL,
    fill = "More Abundant On",
    alpha = "Significance"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

