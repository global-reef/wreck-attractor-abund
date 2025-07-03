## FISH ABUNDANCE AT NATURAL REEF PRE & POST DEPLOYMENT

library(ggplot2)
library(lme4)
library(mgcv)
library(glmmTMB)
library(ggeffects)
library(emmeans)
library(tidyverse)

#Filter 
new_wrecks_data <- clean_abundance %>%
  filter(site %in% c('No Name Wreck', 'No Name Pinnacle', 'Aow Mao Wreck', 'Aow Mao')) %>%
  droplevels()
natty_data <- new_wrecks_data %>%
  filter(reeftype == "Natural") %>%
  droplevels()
wreck_data <- new_wrecks_data %>%
  filter( reeftype == "Artificial")
View(wreck_data)
#Natural Reef Abundance Over Time
summary(model_natural <- glmer(
  total.N ~ days_since_scaled + (1 | site), 
  family = poisson, 
  data = natty_data))
#Overdispersed so...

summary(model_over_time_binom <- glmmTMB(
  total.N ~ days_since_scaled + (1 | site),
  family = nbinom2,
  data = natty_data))
# Strong negative effect of time: counts decline over time since deployment

#Check Assumptions
residuals <- resid(model_over_time_binom)
fitted <- fitted(model_over_time_binom)
par(mfrow = c(2, 2))
plot(fitted, residuals, # Residuals vs Fitted plot (homoscedasticity)
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")
qqnorm(residuals)
qqline(residuals, col = "red")
hist(residuals, main = "Histogram of residuals", xlab = "Residuals")
# Histogram is a bit right-skewed??

par(mfrow = c(1,1))
ggplot(natty_data, aes(x = days_since, y = total.N)) +
  geom_line() +
  geom_point() +
  labs(title = "Fish Abundance at Natural Reefs Over Time", x = "Date", y = "Total Abundance")


# Abundance at the Natural Reef Pre and Post Deployment
summary(model_pre_post <- glmer(total.N ~ period+ (1 | site), family = poisson, data = natty_data))

#Overdispersed so...

summary(model_pre_post_binom <- glmmTMB(
  total.N ~ period + (1 | site),
  family = nbinom2,
  data = natty_data))
# periodpost: Estimate = -0.336, p = 0.0013. Significant decrease in fish abundance post-deployment.
# model_over_time_binom has a lower AIC value indicating a better model.

#Check Assumptions
residuals <- resid(model_pre_post_binom)
fitted <- fitted(model_pre_post_binom)
par(mfrow = c(2, 2))
plot(fitted, residuals, # Residuals vs Fitted plot (homoscedasticity)
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")
qqnorm(residuals)
qqline(residuals, col = "red")
hist(residuals, main = "Histogram of residuals", xlab = "Residuals")
# Histogram is a bit right-skewed??

ggplot(natty_data, aes(x = period, y = total.N)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Abundance Before and After Deployment")


# Hin Pee Wee Pre and Post Deployment
hinpeewee_abundance <- clean_abundance %>%
  filter(site %in% "Hin Pee Wee") %>%
  droplevels()
summary(model_hinpeewee <- glmmTMB(
  total.N ~ period + (1 | site),
  family = nbinom2,
  data = hinpeewee_abundance))
ggplot(hinpeewee_abundance, aes(x = period, y = total.N)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Total abundance pre vs post deployment at Hin Pee Wee",
       y = "Total abundance",
       x = "Period")

# no sig difference (as expected)


#Species specific Differences

# Parrot
summary (model_parrot <- glmmTMB(
  parrotfish ~ period + (1 | site),
  family = nbinom2,
  data = natty_data))
ggplot(new_wrecks_data, aes(x = period, y = parrotfish)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Parrotfish abundance pre vs post deployment",
       y = "Parrotfish abundance",
       x = "Period")
# No sig diff

# Rabbit
summary (model_rabbit <- glmmTMB(
  rabbitfish ~ period + (1 | site),
  family = nbinom2,
  data = natty_data))
ggplot(natty_data, aes(x = period, y = rabbitfish)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Rabbitfish abundance pre vs post deployment",
       y = "Rabbitfish abundance",
       x = "Period")
# rabbitfish abundance decreased by ~40% post deployment, significant at p = 0.0065

# But did it increase at the aritficial reefs post deployment?
summary (model_rabbit <- glmmTMB(
  rabbitfish ~ period + (1 | site),
  family = nbinom2,
  data = wreck_data))
ggplot(wreck_data, aes(x = period, y = rabbitfish)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Rabbitfish abundance at the Wrecks",
       y = "Rabbitfish abundance",
       x = "Period")
# no sig diff at the artificial reefs


# Butterfly
summary (model_butterfly <- glmmTMB(
  butterflyfish ~ period + (1 | site),
  family = nbinom2,
  data = natty_data))
ggplot(natty_data, aes(x = period, y = butterflyfish)) +
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
  data = natty_data))
ggplot(natty_data, aes(x = period, y = angelfish)) +
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
  data = natty_data))
ggplot(natty_data, aes(x = period, y = cleanerwrasse)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Cleaner Wrasse abundance pre vs post deployment",
       y = "Cleaner Wrasse Abundance",
       x = "Period")
# about a 48% decrease in cleaner wrasse abundance after deployment, highly significant (p < 0.001)

# But did it increase at the aritficial reefs post deployment?
summary (model_cleanerwrasse <- glmmTMB(
  cleanerwrasse ~ period + (1 | site),
  family = nbinom2,
  data = wreck_data))
ggplot(wreck_data, aes(x = period, y = cleanerwrasse)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Cleaner Wrasse abundance at the Wrecks",
       y = "Cleaner Wrasse abundance",
       x = "Period")
# significant decreases at the artificial reef post deployment too

#Bat
summary (model_bat <- glmmTMB(
  angelfish ~ period + (1 | site),
  family = nbinom2,
  data = natty_data))
ggplot(natty_data, aes(x = period, y = angelfish)) +
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
  data = natty_data))
ggplot(natty_data, aes(x = period, y = thicklip)) +
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
  data = natty_data))
ggplot(natty_data, aes(x = period, y = thicklip)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "thicklip abundance pre vs post deployment",
       y = "thicklip abundance",
       x = "Period")
# about a 30% decrease in redbreast abundance post deployment, statistically significant (p ≈ 0.041)

# But did it increase at the aritficial reefs post deployment?
summary (model_thicklip <- glmmTMB(
  thicklip ~ period + (1 | site),
  family = nbinom2,
  data = wreck_data))
ggplot(wreck_data, aes(x = period, y = thicklip)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "thicklip abundance at the Wrecks",
       y = "thicklip abundance",
       x = "Period")
# too many 0s, model is unstable


#Slingjaw
summary (model_slingjaw <- glmmTMB(
  slingjaw ~ period + (1 | site),
  family = nbinom2,
  data = natty_data))
ggplot(natty_data, aes(x = period, y = slingjaw)) +
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
  data = natty_data))
ggplot(natty_data, aes(x = period, y = sweetlips)) +
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
  data = natty_data))
ggplot(natty_data, aes(x = period, y = squirrel.soldier)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "squirrel/soldier abundance pre vs post deployment",
       y = "squirrel/soldier abundance",
       x = "Period")
# 8-fold increase in abundance, statistically significant at p ≈ 0.02

# But did it increase at the aritficial reefs post deployment?
summary (model_squirrel.soldier <- glmmTMB(
  squirrel.soldier ~ period + (1 | site),
  family = nbinom2,
  data = wreck_data))
ggplot(wreck_data, aes(x = period, y = squirrel.soldier)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "squirrel.soldier abundance at the Wrecks",
       y = "squirrel.soldier abundance",
       x = "Period")
# too many 0s, model is unstable


#trigger
summary (model_trigger <- glmmTMB(
  triggerfish ~ period + (1 | site),
  family = nbinom2,
  data = natty_data))
ggplot(natty_data, aes(x = period, y = triggerfish)) +
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
  data = natty_data))
ggplot(natty_data, aes(x = period, y = porcupine.puffer)) +
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
  data = natty_data))
ggplot(natty_data, aes(x = period, y = ray)) +
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
  data = natty_data))
ggplot(natty_data, aes(x = period, y = brownstripe)) +
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
  data = natty_data))
ggplot(natty_data, aes(x = period, y = russels)) +
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
  data = natty_data))
ggplot(natty_data, aes(x = period, y = snapperover30)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "snapperover30 abundance pre vs post deployment",
       y = "snapperover30 abundance",
       x = "Period")
# ~68% decrease in large snapper abundance after deployment, statistically significant (p = 0.0075)

# But did it increase at the aritficial reefs post deployment?
summary (model_snapperover30 <- glmmTMB(
  snapperover30 ~ period + (1 | site),
  family = nbinom2,
  data = wreck_data))
ggplot(wreck_data, aes(x = period, y = snapperover30)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "snapperover30 abundance at the Wrecks",
       y = "snapperover30 abundance",
       x = "Period")
# very bad model, try simpler model...
library(MASS)
summary(glm.nb(snapperover30 ~ period, data = wreck_data))
# still very bad??

# eel
summary (model_eel <- glmmTMB(
  eel ~ period + (1 | site),
  family = nbinom2,
  data = natty_data))
ggplot(natty_data, aes(x = period, y = eel)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "eel abundance pre vs post deployment",
       y = "eel abundance",
       x = "Period")
# 92% decrease, this decline is statistically significant (p ≈ 0.036) <- seems wrong????
# not even going to try this one on the wrecks


# trevally
summary (model_trevally <- glmmTMB(
  trevally ~ period + (1 | site),
  family = nbinom2,
  data = natty_data))
ggplot(natty_data, aes(x = period, y = trevally)) +
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
  data = natty_data))
ggplot(natty_data, aes(x = period, y = emperorfish)) +
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
  data = natty_data))
ggplot(natty_data, aes(x = period, y = grouperless30)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "grouperless30 abundance pre vs post deployment",
       y = "grouperless30 abundance",
       x = "Period")
# ~41.6% decrease in grouper abundance post-deployment, very small p value.

# But did it increase at the artificial reefs post deployment?
summary (model_grouperless30 <- glmmTMB(
  grouperless30 ~ period + (1 | site),
  family = nbinom2,
  data = wreck_data))
ggplot(wreck_data, aes(x = period, y = grouperless30)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "grouperless30 abundance at the Wrecks",
       y = "grouperless30 abundance",
       x = "Period")
# no observable differences


# grouperover30
summary (model_grouperover30 <- glmmTMB(
  grouperover30 ~ period + (1 | site),
  family = nbinom2,
  data = natty_data))
ggplot(natty_data, aes(x = period, y = grouperover30)) +
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
  data = natty_data))
ggplot(natty_data, aes(x = period, y = barracuda)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "barracuda abundance pre vs post deployment",
       y = "barracuda abundance",
       x = "Period")
# model unstable, too many 0s

## grouperless30, eel, snapperover30, redbreast, cleaner, rabbitfish significantly decreased post deployment
## squirrel.soldier significantly increased ??











# Periodic differences 

# 3 months before vs 3 months after
# 3 months = 91 days roughly
periodic_data <- new_wrecks_data %>%
  filter(days_since >= -310 & days_since <= 310)
# over time, continuous
summary(model_periodic <- glmmTMB(
  total.N ~ days_since_scaled + (1 | site),
  family = nbinom2,
  data = periodic_data))
# period pre vs post
summary(model_periodic <- glmmTMB(
  total.N ~ period + (1 | site),
  family = nbinom2,
  data = periodic_data))
# no sig diffs


# 3 month periods. 
periodic_separated_data <- new_wrecks_data %>%
  mutate(period_3mo = case_when(
    days_since >= -310 & days_since <= -221 ~ "T-4",
    days_since >= -220 & days_since <= -131 ~ "T-3",
    days_since >= -130 & days_since <= -41  ~ "T-2",
    days_since >= -40  & days_since <= -1   ~ "T-1",
    days_since == 0                          ~ "T0",
    days_since >= 1    & days_since <= 90   ~ "T+1",
    days_since >= 91   & days_since <= 180  ~ "T+2",
    days_since >= 181  & days_since <= 270  ~ "T+3",
    days_since >= 271  & days_since <= 360  ~ "T+4",
    days_since >= 361  & days_since <= 450  ~ "T+5",
    days_since >= 451  & days_since <= 540  ~ "T+6",
    days_since >= 541  & days_since <= 641  ~ "T+7"
))

periodic_separated_data$period_3mo <- factor(periodic_separated_data$period_3mo, ordered = TRUE,
                               levels = c("T-4", "T-3", "T-2", "T-1", "T0", 
                                          "T+1", "T+2", "T+3", "T+4", "T+5", 
                                          "T+6", "T+7"))

summary(model_abundance_period <- glmmTMB(
  total.N ~ period_3mo + (1 | site), 
  family = nbinom2, 
  data = periodic_separated_data
))
# the quadratic term (period_3mo.Q), the relationship between abundance and time period is curvilinear. 


# Visualsise: plot predicted values as this reduces noise and accounts for site variation
# Get predicted abundance with confidence intervals by period_3mo
preds <- ggpredict(model_abundance_period, terms = "period_3mo", bias_correction = TRUE)
# Convert to data frame if needed
preds_df <- as.data.frame(preds)

ggplot(preds_df, aes(x = x, y = predicted)) +
  geom_line(group = 1, color = "blue") +                 # predicted line
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),    # confidence ribbon
              alpha = 0.2, fill = "lightblue") +
  labs(x = "3-month period", y = "Predicted abundance",
       title = "Abundance changes over time periods") +
  theme_minimal()

#Estimated means
emm <- emmeans(model_abundance_period, ~ period_3mo)
summary(emm)
ggplot(preds, aes(x = x, y = predicted)) +
  geom_line(group = 1, color = "steelblue", linewidth = 1.2) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "grey40") +
  labs(
    x = "3-Month Period",
    y = "Predicted Abundance",
    title = "Predicted Fish Abundance by 3-Month Period"
  ) +
  theme_minimal()

# Have a look at contributing species
long_periods <- periodic_separated_data %>%
  pivot_longer(
    cols = c(parrotfish:barracuda),
    names_to = "species",
    values_to = "abundance"
  )
species_summary <- long_periods %>%
  group_by(period_3mo, species) %>%
  summarise(total_abundance = sum(abundance, na.rm = TRUE)) %>%
  arrange(desc(total_abundance))

# Plot the top 5 most abundant species
top_species <- species_summary %>%
  group_by(species) %>%
  summarise(total = sum(total_abundance)) %>%
  slice_max(total, n = 5) %>%
  pull(species)
long_periods %>%
  filter(species %in% top_species) %>%
  group_by(period_3mo, species) %>%
  summarise(mean_abundance = mean(abundance, na.rm = TRUE)) %>%
  ggplot(aes(x = period_3mo, y = mean_abundance, color = species, group = species)) +
  geom_line() +
  geom_point() +
  labs(title = "Top 5 Species Abundance Over Time",
       x = "Period", y = "Mean Abundance") +
  theme_minimal()

# Refit model without brownstripe
long_periods_no_brownstripe <- long_periods %>%
  filter(species != "brownstripe")

summary(model_no_brownstripe <- glmmTMB(
  abundance ~ period_3mo + (1 | site),
  family = nbinom2,
  data = long_periods_no_brownstripe
))
# quadratic term no loner significant, brownstripe likely contributed to this significantly- not hugely interesting.






# COULD LOOK AT SITE SPECIFIC DIFFERENCES 




















#ignore
#creates survey id column where each row is a separate survey
natural_abundance$surveyID_perperson_perday <- seq_len(nrow(natural_abundance)) 
#creates a new column where surveys conducted on the same day are the same survey ID
natural_abundance$surveyID_perday <- as.integer(factor(natural_abundance$date))








