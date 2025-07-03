## DIFFERENCES IN FEEDING GUILD BETWEEN REEF TYPE
library(dplyr)
library(tidyr)
library(lme4)
library(glmmTMB)

#Pivot Longer
long_abundance <- clean_abundance %>%
  pivot_longer(cols = parrotfish:barracuda, 
               names_to = "species", #name of the new column
               values_to = "abundance")

#Map each species to a guild
long_abundance <- long_abundance %>%
  mutate(feeding_guild = case_when(
    species %in% c("parrotfish", "rabbitfish", "butterflyfish") ~ "Grazer",
    species %in% c("angelfish", "cleanerwrasse", "batfish", "thicklip", "redbreast", "slingjaw", "sweetlips", "squirrel.soldier", "triggerfish", "porcupine.puffer") ~ "Invertivore",
    species %in% c( "ray","brownstripe", "russels", "grouperless30", "emperorfish", "eel") ~ "Mesopredator",
    species %in% c("snapperover30", "trevally", "grouperover30", "barracuda") ~ "Higher-Trophic",
    TRUE ~ "Unknown"
  ))

long_abundance <- long_abundance %>%
  mutate(feeding_guild = as.factor(feeding_guild))

#Poisson GLM
summary (model_guild <- glmer(
  abundance ~ reeftype * feeding_guild + (1 | site), 
  family = poisson, 
  data = long_abundance))
# Data is over dispersed so...

#Negative binomial GLM
summary (model_guild_nbinom <- glmmTMB(
  abundance ~ reeftype * feeding_guild + (1 | site), 
  family = nbinom2, 
  data = long_abundance))


# Main effect: Lower abundance overall compared to grazers (Estimate = -0.715, p < 0.001).
# Interaction with reef type: Not significant (Estimate = 0.06, p = 0.70).

#Visualise
preds <- ggpredict(model_guild_nbinom, terms = c("feeding_guild", "reeftype"))
ggplot(preds, aes(x = x, y = predicted, fill = group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge(width = 0.8), width = 0.2) +
  labs(x = "Feeding Guild", y = "Predicted Abundance",
       fill = "Reef Type", title = "Predicted Abundance by Feeding Guild and \nReef Type") +
  theme_minimal()






### ignore
#Normalise by number of species within each guild
guild_species_counts <- long_abundance %>% #get species counts
  select(species, feeding_guild) %>%
  distinct() %>%
  group_by(feeding_guild) %>%
  summarise(species_count = n())
long_abundance <- long_abundance %>% #join species count back to abundance data
  left_join(guild_species_counts, by = "feeding_guild")
norm_abundance <- long_abundance %>% #create a normalised abundance column
  mutate(norm_abundance = abundance / species_count)

model_guild_norm <- lmer(
  norm_abundance ~ reeftype * feeding_guild + (1 | site), 
  data = norm_abundance)
summary(model_guild_norm)

## For counts: No. individuals in each feeding guild by reef type
guild <- long_abundance %>% #Aggregate by reeftype
  group_by(reeftype, feeding_guild) %>%
  summarise(total_abundance = sum(abundance, na.rm = TRUE))
View(guild)

ggplot(guild, aes(x = feeding_guild, y = total_abundance)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Feeding guild")





