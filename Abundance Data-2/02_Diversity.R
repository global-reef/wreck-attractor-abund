### DIVERSITY

library(vegan)
library(lme4)
library(lmerTest)
library(ggeffects)

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
# Set order
shannon_results$site <- factor(shannon_results$site, levels = c("Aow Mao", "Aow Mao Wreck", "No Name Pinnacle", "No Name Wreck", "Hin Pee Wee", "Sattakut"))

## PLOT: Diversity between reef types
ggplot(shannon_results, aes(x = reeftype, y = diversity, fill = reeftype)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Shannon Diversity Index by Reef Type",
       x = "Reef Type",
       y = "Shannon Diversity Index") +
  theme_minimal()
## PLOT: Diversity between sites
ggplot(shannon_results, aes(x = site, y = diversity, fill = reeftype)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Shannon Diversity Index by Site",
       x = "Site",
       y = "Shannon Diversity Index") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# PLOT: Diversity between paired sites
shannon_No_Name_Pair <- shannon_results %>%
  filter(site %in% c('No Name Wreck', 'No Name Pinnacle')) %>%
           droplevels()
ggplot(shannon_No_Name_Pair, aes(x = site, y = diversity, fill = reeftype)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Shannon Diversity at No Name Pair\n(Pinnacle & Shipwreck)",
       x = "Site",
       y = "Shannon Diversity Index") +
  theme_minimal()
shannon_Aow_Mao_Pair <- shannon_results %>%
  filter(site %in% c('Aow Mao Wreck', 'Aow Mao')) %>%
  droplevels()
ggplot(shannon_Aow_Mao_Pair, aes(x = site, y = diversity, fill = reeftype)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Shannon Diversity at Aow Mao Pair\n(Fringing Reef & Shipwreck)",
       x = "Site",
       y = "Shannon Diversity Index") +
  theme_minimal()

# STATISTICS : No Name Pair
summary(No_Name_Diversity_model <- lm(diversity ~ reeftype,data = shannon_No_Name_Pair ))
summary(No_Name_Diversity_model_interact <- lm(diversity ~ reeftype * days_since_scaled ,data = shannon_No_Name_Pair ))
anova(No_Name_Diversity_model, No_Name_Diversity_model_interact)
ggplot(shannon_No_Name_Pair, aes(x = days_since, y = diversity, color = reeftype)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, aes(fill = reeftype)) +
  theme_minimal() +
  labs(title = "Diversity at No Name Pinnacle and No Name Wreck \nin relation to Days since Deployment",
       x = "Days Since Deployment", y = "Shannon Diversity")

# STATISTICS : Aow Mao Pair
summary(Aow_Mao_Diversity_model <- lm(diversity ~ reeftype,data = shannon_Aow_Mao_Pair ))
summary(Aow_Mao_Diversity_model_interact <- lm(diversity ~ reeftype * days_since_scaled ,data = shannon_Aow_Mao_Pair ))
anova(Aow_Mao_Diversity_model, Aow_Mao_Diversity_model_interact)
ggplot(shannon_Aow_Mao_Pair, aes(x = days_since, y = diversity, color = reeftype)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, aes(fill = reeftype)) +
  theme_minimal() +
  labs(title = "Diversity at Aow Mao and Aow Mao Wreck \nin relation to Days since Deployment",
       x = "Days Since Deployment", y = "Shannon Diversity")

# STATISTICS : Aow Mao & No Name Pairs
diversity_new_wrecks <- shannon_results %>%
  filter(site %in% c('No Name Wreck', 'No Name Pinnacle', 'Aow Mao Wreck', 'Aow Mao')) %>%
  droplevels()

##Full model
summary(model_full <- lmer(diversity ~ days_since_scaled + reeftype + Depth + (1 | site), data = diversity_new_wrecks))
#Drop Depth 
summary(model_2 <- lmer(diversity ~ days_since_scaled + reeftype + (1 | site), data = diversity_new_wrecks))
anova(model_full, model_2)
#Drop Reeftype
summary(model_3 <- lmer(diversity ~ days_since_scaled + (1 | site), data = diversity_new_wrecks))
anova(model_2, model_3)
# model 2 is the best fit

#Add an interraction of reeftype and days since
summary (model_interact <- lmer(diversity ~ reeftype * days_since_scaled + (1 | site), data = diversity_new_wrecks))
anova(model_2, model_interact)  
# model_interact is the best fit

ggplot(diversity_new_wrecks, aes(x = days_since, y = diversity, color = reeftype)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, aes(fill = reeftype)) +
  theme_minimal() +
  labs(title = "Diversity at Aow Mao and No Name Paired reefs \nin relation to Days since Deployment",
       x = "Days Since Deployment", y = "Shannon Diversity")

#Check Assumptions
residuals <- resid(model_interact)
fitted <- fitted(model_interact)
par(mfrow = c(2, 2))
plot(fitted, residuals, # Residuals vs Fitted plot (homoscedasticity)
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")
qqnorm(residuals)
qqline(residuals, col = "red")
hist(residuals, main = "Histogram of residuals", xlab = "Residuals")
# Very nice


# STATISTICS: Sattakut Pair
diversity_sattakut_pair <- shannon_results %>%
  filter(site %in% c('Sattakut', 'Hin Pee Wee')) %>%
  droplevels()
summary (model_interact_Sat_Pair <- lm(diversity ~ reeftype * days_since_scaled, data = diversity_sattakut_pair))
anova(model_interact_Sat_Pair)

# reeftype alone: Not significant (p = 0.69) — no clear overall difference in diversity between artificial vs natural reefs when ignoring time.
# days_since_scaled: significant effect on diversity (p = 0.0141). This means diversity changes over time, averaged across both reef types.

#Check Assumptions
residuals <- resid(model_interact_Sat_Pair)
fitted <- fitted(model_interact_Sat_Pair)
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


#Natural Reef Diversity
summary(model_full <- lmer(
  diversity ~ days_since_scaled + reeftype + Depth + (1 | site), 
  data = new_wrecks_data))





#Ignore: Transformations
#Box-cox
shannon_results <- shannon_results %>%
  mutate(diversity_shifted = diversity + 1e-6) # As some values are 0, shift a small amount without altering distribution

boxcox_result <- boxcox(lm(diversity_shifted ~ 1, data = shannon_results))
lambda <- boxcox_result$x[which.max(boxcox_result$y)]
# Transform using lambda
if (lambda == 0) {
  transformed <- log(shannon_results$diversity_shifted)
} else {
  transformed <- (shannon_results$diversity_shifted^lambda - 1) / lambda
}

#Log transform
shannon_results$diversity_log <- log(shannon_results$diversity + 1)
shapiro.test(shannon_results$diversity_log)
hist(shannon_results$diversity_log)
