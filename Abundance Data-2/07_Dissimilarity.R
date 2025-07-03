# DISSIMILARITY : Bray-Curtis, SIMPER

library(vegan)
library(dplyr)
library(ggplot2)

# Make species matrix
species_data <- clean_abundance %>%
  select(parrotfish:barracuda)  # Replace with actual species column range
metadata <- clean_abundance %>%
  select(site, reeftype)

# Bray-Curtis Dissimilarity
bray_dist <- vegdist(species_data, method = "bray")

# Visualise
nmds <- metaMDS(species_data, distance = "bray", k = 2, trymax = 100)

#STATISTICS: PERMANOVA
adonis2(bray_dist ~ reeftype, data = metadata)
# community compositions between artificial and natural reefs are significantly different p=0.001

# Visualise with NMDS
nmds <- metaMDS(species_data, distance = "bray", k = 2)
plot(nmds, type = "n")
points(nmds, col = metadata$reeftype, pch = 19)
legend("topright", legend = unique(metadata$reeftype), col = 1:2, pch = 19)

# Check the dispersion
dispersion <- betadisper(bray_dist, metadata$reeftype)
anova(dispersion)
# Dispersion is statistically different p<0.001 which may affect interpretation of the PERMANOVA

# Visualise group dispersion
boxplot(dispersion, main = "Group Dispersion by Reef Type", ylab = "Distance to Centroid")

# Visualise nMDS group spread
ordiplot(nmds, type = "n")
ordiellipse(nmds, metadata$reeftype, kind = "sd", label = TRUE)
points(nmds, display = "sites", col = metadata$reeftype, pch = 19)
legend("topright", legend = levels(metadata$reeftype), col = 1:2, pch = 19)

# Check ANOSIM which is less sensitive to differences in varience 
summary(anosim_result <- anosim(bray_dist, metadata$reeftype))
plot(anosim_result)
# R = 0.33 indicates moderate community separation, statistically significant p < 0.001
# This confirms previous PERMANOVA result was not just due to unequal variences

# SIMPER
community_matrix <- species_data  # Just species columns
grouping <- metadata$reeftype
summary(simper_result <- simper(community_matrix, grouping, permutations = 999))

# Visualisation 
simper_df <- as.data.frame(summary(simper_result)[[1]])
simper_df$species <- rownames(simper_df)
simper_sig <- simper_df %>% # Filter: keep only species with p < 0.05
  filter(p < 0.05)
simper_sig <- simper_sig %>% # reorder species by contribution
  arrange(desc(average)) %>%
  mutate(species = factor(species, levels = species))
# Plot
ggplot(simper_sig, aes(x = species, y = average, fill = average)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_viridis_c(option = "D") +
  coord_flip() +
  labs(
    title = "Top Contributing Species to \nCommunity Dissimilarity",
    subtitle = "Based on SIMPER analysis (p < 0.05)",
    x = "Species",
    y = "Average Contribution to Dissimilarity",
    fill = "Contribution"
  ) +
  theme_minimal(base_size = 14)






















