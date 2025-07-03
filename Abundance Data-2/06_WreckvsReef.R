# Wreck vs Nat Pairs 

library(ggeffects)
library(ggplot2)
library(glmmTMB)

# Overall
### No Name Pair
nonamepair_data <- clean_abundance %>%
  filter(site %in% c('No Name Wreck', 'No Name Pinnacle')) %>%
  droplevels()
long_noname <- nonamepair_data %>%
  pivot_longer(
    cols = parrotfish:barracuda,  # Replace with your actual species column range
    names_to = "species",
    values_to = "abundance")

summary (model_noname <- glmmTMB(
  abundance ~ reeftype * days_since_scaled,
  family = nbinom2,
  data = long_noname))
summary (model_no_interaction <- glmmTMB(
  abundance ~ reeftype + days_since_scaled,
  family = nbinom2,
  data = long_noname))

anova(model_noname, model_no_interaction)  # Compare models
# model_no_interaction is the better model, therefore when plotting the fitted values, the slopes are the same becuase interraction was non-significant.

# Get actual min/max days for each reef type
range_df <- long_noname %>%
  group_by(reeftype) %>%
  summarise(
    min_scaled = min(days_since_scaled, na.rm = TRUE),
    max_scaled = max(days_since_scaled, na.rm = TRUE))

# Create prediction grids manually for each reef type
grid_art <- data.frame(
  days_since_scaled = seq(range_df$min_scaled[range_df$reeftype == "Artificial"],
                          range_df$max_scaled[range_df$reeftype == "Artificial"],
                          length.out = 100), reeftype = "Artificial")
grid_nat <- data.frame(
  days_since_scaled = seq(range_df$min_scaled[range_df$reeftype == "Natural"],
                          range_df$max_scaled[range_df$reeftype == "Natural"],
                          length.out = 100), reeftype = "Natural")

preds_simple <- ggpredict(model_no_interaction, terms = c("days_since_scaled", "reeftype")) # Combine grids and predict
preds_simple$days_since <- preds_simple$x * days_sd_nn + days_mean_nn # Back-transform

ggplot(preds_simple, aes(x = days_since, y = predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  labs(
    x = "Days Since Deployment",
    y = "Predicted Abundance",
    color = "Reef Type",
    fill = "Reef Type",
    title = "Predicted Abundance Over Time (No Name Pair)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")


### Aow Mao Pair
# Overall temporal changes
aowmaopair_data <- clean_abundance %>%
  filter(site %in% c('Aow Mao Wreck', 'Aow Mao')) %>%
  droplevels()
long_aowmao <- aowmaopair_data %>%
  pivot_longer(
    cols = parrotfish:barracuda,  # Replace with your actual species column range
    names_to = "species",
    values_to = "abundance")
summary (model_aowmao <- glmmTMB(
  abundance ~ reeftype * days_since_scaled,
  family = nbinom2,
  data = long_aowmao))

#Visualise
# Get observed scaled day ranges per reef type
range_df <- long_aowmao %>%
  group_by(reeftype) %>%
  summarise(
    min_scaled = min(days_since_scaled, na.rm = TRUE),
    max_scaled = max(days_since_scaled, na.rm = TRUE))

# Create prediction grids within observed ranges
grid_art <- data.frame(
  days_since_scaled = seq(range_df$min_scaled[range_df$reeftype == "Artificial"],
                          range_df$max_scaled[range_df$reeftype == "Artificial"],
                          length.out = 100),reeftype = "Artificial")
grid_nat <- data.frame(
  days_since_scaled = seq(range_df$min_scaled[range_df$reeftype == "Natural"],
                          range_df$max_scaled[range_df$reeftype == "Natural"],
                          length.out = 100),reeftype = "Natural")

pred_grid <- bind_rows(grid_art, grid_nat)
pred_link <- predict(model_aowmao, newdata = pred_grid, type = "link", se.fit = TRUE) # Predict on link scale with standard errors
pred_grid$fit_link <- pred_link$fit # Add predictions and confidence intervals on link scale
pred_grid$se_link <- pred_link$se.fit
pred_grid$conf.low_link <- pred_grid$fit_link - 1.96 * pred_grid$se_link
pred_grid$conf.high_link <- pred_grid$fit_link + 1.96 * pred_grid$se_link
pred_grid$predicted <- exp(pred_grid$fit_link) # Back-transform to response scale
pred_grid$conf.low <- exp(pred_grid$conf.low_link)
pred_grid$conf.high <- exp(pred_grid$conf.high_link)

days_mean <- mean(long_aowmao$days_since, na.rm = TRUE) # Back-transform x-axis to original days_since
days_sd <- sd(long_aowmao$days_since, na.rm = TRUE)
pred_grid$days_since <- pred_grid$days_since_scaled * days_sd + days_mean

# Plot
ggplot(pred_grid, aes(x = days_since, y = predicted, color = reeftype)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = reeftype), alpha = 0.2, color = NA) +
  labs(
    x = "Days Since Deployment",
    y = "Predicted Abundance",
    color = "Reef Type",
    fill = "Reef Type",
    title = "Predicted Abundance Over Time at the \nAow Mao Pair") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

# Species differences
# Plot
ggplot(long_aowmao, aes(x = days_since, y = abundance, color = site)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE) +  # Smooth trend lines
  facet_wrap(~ species, scales = "free_y") +    # One plot per species
  theme_minimal() +
  labs(
    title = "Species Abundance Over Time at Aow Mao Pair",
    x = "Days Since Deployment",
    y = "Abundance"
  ) +
  theme(strip.text = element_text(face = "bold"),
        legend.position = "bottom")


### Sattakut Pair

# Overall temporal changes
sattakutpair_data <- clean_abundance %>%
  filter(site %in% c('Sattakut', 'Hin Pee Wee')) %>%
  droplevels()
long_sattakut <- sattakutpair_data %>%
  pivot_longer(
    cols = parrotfish:barracuda,  # Replace with your actual species column range
    names_to = "species",
    values_to = "abundance")

summary (model_sattakut_interact <- glmmTMB(
  abundance ~ reeftype * days_since_scaled,
  family = nbinom2,
  data = long_sattakut))
summary (model_sattakut <- glmmTMB(
  abundance ~ reeftype + days_since_scaled,
  family = nbinom2,
  data = long_sattakut))

anova(model_sattakut, model_sattakut_interact)
# simpler model is better.
# The abundance changes over time similarly in both reef types (no interaction effect).





### Species Differences
# Visualise species over time
ggplot(long_noname, aes(x = days_since, y = abundance, color = site)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE) +  # Smooth trend lines
  facet_wrap(~ species, scales = "free_y") +    # One plot per species
  theme_minimal() +
  labs(
    title = "Species Abundance Over Time at No Name Pair",
    x = "Days Since Deployment",
    y = "Abundance") +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom")



# Parrot#
summary(model_noname_species <- glmmTMB(
  parrotfish ~ reeftype * period, 
  family = nbinom2,
  data = nonamepair_data))
preds <- ggpredict(model_noname_species, terms = c("period", "reeftype"))
ggplot(preds, aes(x = x, y = predicted, fill = group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.6) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2, position = position_dodge(width = 0.8)) +
  labs(x = "Period", y = "Predicted Parrotfish Abundance", fill = "Reef Type") +
  theme_minimal(base_size = 14)
# No sig diff

# Rabbit
summary (model_rabbit <- glmmTMB(
  rabbitfish ~ period + (1 | site),
  family = nbinom2,
  data = new_wrecks_data))
ggplot(new_wrecks_data, aes(x = period, y = rabbitfish)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Rabbitfish abundance pre vs post deployment",
       y = "Rabbitfish abundance",
       x = "Period")
# rabbitfish abundance decreased by ~40% post deployment, significant at p = 0.0065

# Butterfly
summary (model_butterfly <- glmmTMB(
  butterflyfish ~ period + (1 | site),
  family = nbinom2,
  data = new_wrecks_data))
ggplot(new_wrecks_data, aes(x = period, y = butterflyfish)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "butterflyfish abundance pre vs post deployment",
       y = "butterflyfish abundance",
       x = "Period")
# No sig diff

# Angel
summary (model_angel <- glmmTMB(
  angelfish ~ period + (1 | site),
  family = nbinom2,
  data = new_wrecks_data))
ggplot(new_wrecks_data, aes(x = period, y = angelfish)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "angelfish abundance pre vs post deployment",
       y = "angelfish abundance",
       x = "Period")
# Many 0s may not be reliable. 

# Cleaner
summary (model_cleaner <- glmmTMB(
  cleanerwrasse ~ period + (1 | site),
  family = nbinom2,
  data = new_wrecks_data))
ggplot(new_wrecks_data, aes(x = period, y = cleanerwrasse)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Cleaner Wrasse abundance pre vs post deployment",
       y = "Cleaner Wrasse Abundance",
       x = "Period")
# about a 48% decrease in cleaner wrasse abundance after deployment, highly significant (p < 0.001)

#Bat
summary (model_bat <- glmmTMB(
  angelfish ~ period + (1 | site),
  family = nbinom2,
  data = new_wrecks_data))
ggplot(new_wrecks_data, aes(x = period, y = angelfish)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "angelfish abundance pre vs post deployment",
       y = "angelfish abundance",
       x = "Period")
#No sig diff

#Thicklip
summary (model_thicklip <- glmmTMB(
  thicklip ~ period + (1 | site),
  family = nbinom2,
  data = new_wrecks_data))
ggplot(new_wrecks_data, aes(x = period, y = thicklip)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "thicklip abundance pre vs post deployment",
       y = "thicklip abundance",
       x = "Period")
#No sig diff

#Redbreast
summary (model_redbreast <- glmmTMB(
  redbreast ~ period + (1 | site),
  family = nbinom2,
  data = new_wrecks_data))
ggplot(new_wrecks_data, aes(x = period, y = thicklip)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "thicklip abundance pre vs post deployment",
       y = "thicklip abundance",
       x = "Period")
# about a 30% decrease in redbreast abundance post deployment, statistically significant (p ≈ 0.041)

#Slingjaw
summary (model_slingjaw <- glmmTMB(
  slingjaw ~ period + (1 | site),
  family = nbinom2,
  data = new_wrecks_data))
ggplot(new_wrecks_data, aes(x = period, y = slingjaw)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "slingjaw abundance pre vs post deployment",
       y = "slingjaw abundance",
       x = "Period")
#No sig diff

#Sweetlips
summary (model_sweetlips <- glmmTMB(
  sweetlips ~ period + (1 | site),
  family = nbinom2,
  data = new_wrecks_data))
ggplot(new_wrecks_data, aes(x = period, y = sweetlips)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "sweetlips abundance pre vs post deployment",
       y = "sweetlips abundance",
       x = "Period")
#no sig diff

#squirrel/soldier
summary (model_squirrel.soldier <- glmmTMB(
  squirrel.soldier ~ period + (1 | site),
  family = nbinom2,
  data = new_wrecks_data))
ggplot(new_wrecks_data, aes(x = period, y = squirrel.soldier)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "squirrel/soldier abundance pre vs post deployment",
       y = "squirrel/soldier abundance",
       x = "Period")
# 8-fold increase in abundance, statistically significant at p ≈ 0.02

#trigger
summary (model_trigger <- glmmTMB(
  triggerfish ~ period + (1 | site),
  family = nbinom2,
  data = new_wrecks_data))
ggplot(new_wrecks_data, aes(x = period, y = triggerfish)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "trigger abundance pre vs post deployment",
       y = "trigger abundance",
       x = "Period")
# no sig diff

#porcupine.puffer
summary (model_porcupine.puffer <- glmmTMB(
  porcupine.puffer ~ period + (1 | site),
  family = nbinom2,
  data = new_wrecks_data))
ggplot(new_wrecks_data, aes(x = period, y = porcupine.puffer)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "trigger abundance pre vs post deployment",
       y = "trigger abundance",
       x = "Period")
#no sig diff

# ray
summary (model_ray <- glmmTMB(
  ray ~ period + (1 | site),
  family = nbinom2,
  data = new_wrecks_data))
ggplot(new_wrecks_data, aes(x = period, y = ray)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "ray abundance pre vs post deployment",
       y = "ray abundance",
       x = "Period")
#no sig diff

# brownstripe
summary (model_brownstripe <- glmmTMB(
  brownstripe ~ period + (1 | site),
  family = nbinom2,
  data = new_wrecks_data))
ggplot(new_wrecks_data, aes(x = period, y = brownstripe)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "brownstripe abundance pre vs post deployment",
       y = "brownstripe abundance",
       x = "Period")
#no sig diff

# russels
summary (model_russels <- glmmTMB(
  russels ~ period + (1 | site),
  family = nbinom2,
  data = new_wrecks_data))
ggplot(new_wrecks_data, aes(x = period, y = russels)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "russels abundance pre vs post deployment",
       y = "russels abundance",
       x = "Period")
#no sig diff

# snapperover30
summary (model_snapperover30 <- glmmTMB(
  snapperover30 ~ period + (1 | site),
  family = nbinom2,
  data = new_wrecks_data))
ggplot(new_wrecks_data, aes(x = period, y = snapperover30)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "snapperover30 abundance pre vs post deployment",
       y = "snapperover30 abundance",
       x = "Period")
# ~68% decrease in large snapper abundance after deployment, statistically significant (p = 0.0075)

# eel
summary (model_eel <- glmmTMB(
  eel ~ period + (1 | site),
  family = nbinom2,
  data = new_wrecks_data))
ggplot(new_wrecks_data, aes(x = period, y = eel)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "eel abundance pre vs post deployment",
       y = "eel abundance",
       x = "Period")
# 92% decrease, this decline is statistically significant (p ≈ 0.036) <- seems wrong????

# trevally
summary (model_trevally <- glmmTMB(
  trevally ~ period + (1 | site),
  family = nbinom2,
  data = new_wrecks_data))
ggplot(new_wrecks_data, aes(x = period, y = trevally)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "trevally abundance pre vs post deployment",
       y = "trevally abundance",
       x = "Period")
#no sig diff

# emperorfish
summary (model_emperorfish <- glmmTMB(
  emperorfish ~ period + (1 | site),
  family = nbinom2,
  data = new_wrecks_data))
ggplot(new_wrecks_data, aes(x = period, y = emperorfish)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "emperorfish abundance pre vs post deployment",
       y = "emperorfish abundance",
       x = "Period")
#no sig diff

# grouperless30
summary (model_grouperless30 <- glmmTMB(
  grouperless30 ~ period + (1 | site),
  family = nbinom2,
  data = new_wrecks_data))
ggplot(new_wrecks_data, aes(x = period, y = grouperless30)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "grouperless30 abundance pre vs post deployment",
       y = "grouperless30 abundance",
       x = "Period")
# ~41.6% decrease in grouper abundance post-deployment, very small p value.

# grouperover30
summary (model_grouperover30 <- glmmTMB(
  grouperover30 ~ period + (1 | site),
  family = nbinom2,
  data = new_wrecks_data))
ggplot(new_wrecks_data, aes(x = period, y = grouperover30)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "grouperover30 abundance pre vs post deployment",
       y = "grouperover30 abundance",
       x = "Period")
#no sig diff

# barracuda
summary (model_barracuda <- glmmTMB(
  barracuda ~ period + (1 | site),
  family = nbinom2,
  data = new_wrecks_data))
ggplot(new_wrecks_data, aes(x = period, y = barracuda)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "barracuda abundance pre vs post deployment",
       y = "barracuda abundance",
       x = "Period")
# model unstable, too many 0s



































