##EVENNESS

# Begin with Diversity Data 
# Make Diversity Index
shannon_matrix <- clean_abundance %>%
  select(parrotfish, rabbitfish, butterflyfish, angelfish, cleanerwrasse, 
         batfish, thicklip, redbreast, slingjaw, sweetlips, squirrel.soldier, 
         triggerfish, porcupine.puffer, ray, brownstripe, russels, snapperover30, 
         eel, trevally, emperorfish, grouperless30, grouperover30, barracuda)
# Calculate Shannon diversity
shannon_index <- vegan::diversity(shannon_matrix, index = "shannon")
# Combine dataframes
shannon_results <- clean_abundance %>%
  mutate(shannon = shannon_index) %>%
  rename(diversity = shannon)

# Calculate Evenness
species_richness <- specnumber(shannon_matrix) # Adds species richness
pielou_evenness <- shannon_index / log(species_richness) # Pielou's evenness

# Combine dataframes
pielou_results <- shannon_results %>%
  mutate(
    shannon = shannon_index,
    richness = species_richness,
    evenness = pielou_evenness)

# Set order
pielou_results$site <- factor(pielou_results$site, levels = c("Aow Mao", "Aow Mao Wreck", "No Name Pinnacle", "No Name Wreck", "Hin Pee Wee", "Sattakut"))


## PLOT: Evenness between reef types
ggplot(pielou_results, aes(x = reeftype, y = evenness, fill = reeftype)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Pielou Evenness Index by Reef Type",
       x = "Reef Type",
       y = "Pielou Evenness Index") +
  theme_minimal()
## PLOT: Evenness between reef type
ggplot(pielou_results, aes(x = site, y = evenness, fill = reeftype)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Pielou Evenness Index by Site",
       x = "Site",
       y = "Pielou Evenness Index") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


### STATISTICS: Aow Mao and No Name Pair
evenness_new_wrecks <- pielou_results %>%
  filter(site %in% c('No Name Wreck', 'No Name Pinnacle', 'Aow Mao Wreck', 'Aow Mao')) %>%
  droplevels()

#Interaction model
summary(model_evenness <- lmer(evenness ~ reeftype * days_since_scaled + (1 | site), data = evenness_new_wrecks))
# days_since_scaled: Positive and highly significant (Estimate ~0.1, p < 0.001), meaning evenness increases over time on artificial reefs.
# Interaction (reeftypeNatural:days_since_scaled): Negative and significant (Estimate ~ -0.061, p = 0.0002), indicating the rate of increase in evenness over time is significantly less on natural reefs compared to artificial reefs.

ggplot(evenness_new_wrecks, aes(x = days_since, y = evenness, color = reeftype)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, aes(fill = reeftype)) +
  theme_minimal() +
  labs(title = "Evenness at Aow Mao and No Name Paired reefs \nin relation to Days since Deployment",
       x = "Days Since Deployment", y = "Pielou's Evenness Index")

#Check Assumptions
residuals <- resid(model_evenness)
fitted <- fitted(model_evenness)
par(mfrow = c(2, 2))
plot(fitted, residuals, # Residuals vs Fitted plot (homoscedasticity)
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")
qqnorm(residuals)
qqline(residuals, col = "red")
hist(residuals, main = "Histogram of residuals", xlab = "Residuals")
# Fine




# PLOT: Diversity between paired sites
pielou_No_Name_Pair <- pielou_results %>%
  filter(site %in% c('No Name Wreck', 'No Name Pinnacle')) %>%
  droplevels()
ggplot(pielou_No_Name_Pair, aes(x = site, y = diversity, fill = reeftype)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Pielou's Evenness Index at No Name Pair\n(Pinnacle & Shipwreck)",
       x = "Site",
       y = "Pielou's Evenness Index") +
  theme_minimal()
pielou_Aow_Mao_Pair <- pielou_results %>%
  filter(site %in% c('Aow Mao Wreck', 'Aow Mao')) %>%
  droplevels()
ggplot(pielou_Aow_Mao_Pair, aes(x = site, y = evenness, fill = reeftype)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Pielou's Evenness Index at Aow Mao Pair\n(Fringing Reef & Shipwreck)",
       x = "Site",
       y = "Pielou's Evenness Index") +
  theme_minimal()

# STATISTICS : No Name Pair
summary(No_Name_Evenness_model <- lm(evenness ~ reeftype,data = pielou_No_Name_Pair ))
summary(No_Name_Evenness_model_interact <- lm(
  evenness ~ reeftype * days_since_scaled, 
  data = pielou_No_Name_Pair ))
anova(No_Name_Evenness_model, No_Name_Evenness_model_interact)

ggplot(pielou_No_Name_Pair, aes(x = days_since, y = evenness, color = reeftype)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, aes(fill = reeftype)) +
  theme_minimal() +
  labs(title = "Evenness at No Name Pinnacle and No Name Wreck \nin relation to Days since Deployment",
       x = "Days Since Deployment", y = "Pielou's Evenness Index")

# STATISTICS : Aow Mao Pair
summary(Aow_Mao_Evenness_model <- lm(evenness ~ reeftype,data = pielou_Aow_Mao_Pair ))
summary(Aow_Mao_Evenness_model_interact <- lm(evenness ~ reeftype * days_since_scaled ,data = pielou_Aow_Mao_Pair ))
anova(Aow_Mao_Evenness_model, Aow_Mao_Evenness_model_interact)
ggplot(pielou_Aow_Mao_Pair, aes(x = days_since, y = evenness, color = reeftype)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, aes(fill = reeftype)) +
  theme_minimal() +
  labs(title = "Evenness at Aow Mao and Aow Mao Wreck \nin relation to Days since Deployment",
       x = "Days Since Deployment", y = "Pielou's Evenness Index")















# STATISTICS: Sattakut Pair
evenness_sattakut_pair <- pielou_results %>%
  filter(site %in% c('Sattakut', 'Hin Pee Wee')) %>%
  droplevels()
summary (model_interact_Sat_Pair <- lm(evenness ~ reeftype * days_since_scaled, data = evenness_sattakut_pair))
anova(model_interact_Sat_Pair)

# reeftype: p = 0.8993 (no difference in evenness by reef type)
# days_since_scaled: p = 0.6796 (no change in evenness over time)
# reeftype:days_since_scaled: p = 0.3442 (no interaction effect)










