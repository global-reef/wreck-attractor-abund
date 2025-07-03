### diversity and evenness 
library(tidyr)
library(vegan)
library(lme4)
library(lmerTest)
library(ggeffects)
library(merTools)
library(car)
library(tidyverse) # includes dplyr, readr, tibble and ggplot2

###### calulate and compare shannon diversity ##### 
# Create wide format with species as columns
shannon<- fish_long %>%
  group_by(survey_id, Species) %>%
  summarise(count = sum(Count), .groups = "drop") %>%
  pivot_wider(names_from = Species, values_from = count, values_fill = list(count = 0))

# Calculate Shannon index using vegan::diversity()
shannon <- shannon %>%
  rowwise() %>%
  mutate(shannon = diversity(c_across(where(is.numeric))))%>% 
  ungroup()

# add metadata back in 
meta <- fish_long %>%
  distinct(survey_id, Site, pair, Type, deployment_period, Researcher, Date)

shannon_data <- shannon%>%
  left_join(meta, by = "survey_id")

# plot diversity btwn reeftypes 
ggplot(shannon_data, aes(x = Type, y = shannon, color = Type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 0.8) +
  scale_color_manual(values = c("Natural" = "#007A87", "Artificial" = "#66BFA6")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Reef Type",
    y = "Shannon Diversity Index",
    title = "Shannon Diversity by Reef Type",
    color = "Reef Type"
  ) +
  theme_minimal()


# plot diversity btwn sites
site_order <- c(
  "Aow Mao", "Aow Mao Wreck",
  "No Name Pinnacle", "No Name Wreck",
  "Hin Pee Wee", "Sattakut"
)
shannon_data$Site <- factor(shannon_data$Site, levels = site_order) # choosing site order 

ggplot(shannon_data, aes(x = Site, y = shannon, color = Type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, position = position_dodge(width = 0.75)) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 0.8) +
  scale_color_manual(values = c("Natural" = "#007A87", "Artificial" = "#66BFA6")) +
  labs(
    x = "Site",
    y = "Shannon Diversity Index",
    color = "Reef Type",
    title = "Shannon Diversity by Site and Reef Type"
  ) +
  theme_minimal()

# faceted 
ggplot(shannon_data, aes(x = Site, y = shannon, color = Type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, position = position_dodge(width = 0.75)) +
  geom_jitter(width = 0.2, alpha = 0.6, size = 1) +
  scale_color_manual(values = c("Natural" = "#007A87", "Artificial" = "#66BFA6")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  facet_wrap(~ pair, scales = "free_x") +
  labs(
    x = "Site",
    y = "Shannon Diversity Index",
    color = "Reef Type",
    title = "Shannon Diversity by Site Within Pairs"
  ) +
  theme_minimal()


#### models ######

# (1) no time information; just between types 
# Aow Mao pair
aowmao_mod <- lm(shannon ~ Type, data = filter(shannon_data, pair == "Aow Mao"))
summary(aowmao_mod)

# No Name pair
noname_mod <- lm(shannon ~ Type, data = filter(shannon_data, pair == "No Name"))
summary(noname_mod)

# (2) integrating time 
# convert date to numeric 
shannon_data <- shannon_data %>%
  mutate(date_num = as.numeric(Date),  # days since 1970-01-01
         deployment_period = factor(deployment_period, levels = c("Pre", "Post")))

shannon_trends <- function(data, pair_name, deployment_date = as.Date("2023-09-07")) {
  # Filter for the selected pair
  df_pair <- data %>%
    filter(pair == pair_name) %>%
    mutate(date_num = as.numeric(Date))
  
  # Fit model
  model <- lm(shannon ~ Type * (date_num + deployment_period), data = df_pair)
  
  print(summary(model))
  print(car::Anova(model))
  
  # Prediction grid
  preds <- df_pair %>%
    select(Date, Type, deployment_period) %>%
    distinct() %>%
    mutate(date_num = as.numeric(Date))
  
  pred_out <- predict(model, newdata = preds, se.fit = TRUE)
  
  preds <- preds %>%
    mutate(
      fit = pred_out$fit,
      se = pred_out$se.fit,
      lower = fit - 1.96 * se,
      upper = fit + 1.96 * se
    )
  
  # Plot
  p <- ggplot(df_pair, aes(x = Date, y = shannon, color = Type)) +
    geom_point(alpha = 0.6, size = 1) +
    geom_ribbon(data = preds,
                inherit.aes = FALSE,
                aes(x = Date, ymin = lower, ymax = upper, fill = Type),
                alpha = 0.2, color = NA) +
    geom_line(data = preds,
              aes(x = Date, y = fit, color = Type),
              linewidth = 1) +
    geom_vline(xintercept = deployment_date, linetype = "dashed", color = "gray40") +
    scale_color_manual(values = c("Natural" = "#007A87", "Artificial" = "#66BFA6")) +
    scale_fill_manual(values = c("Natural" = "#007A87", "Artificial" = "#66BFA6")) +
    labs(
      title = paste("Shannon Diversity Over Time —", pair_name, "Pair"),
      x = "Date",
      y = "Shannon Diversity Index",
      color = "Reef Type",
      fill = "Reef Type"
    ) +
    theme_minimal()+
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  
  print(p)
  
  return(list(model = model, plot = p))
}
                           
aowmao_shannon <- shannon_trends(shannon_data, pair_name = "Aow Mao")
noname_shannon <- shannon_trends(shannon_data, pair_name = "No Name")


### full model (add random effect for site) ####
# Fit the mixed model
shannon_mixed <- lmer(
  shannon ~ Type * (date_num + deployment_period) + (1 | Site),
  data = shannon_data
)

summary(shannon_mixed)
car::Anova(shannon_mixed)

# Create grid of prediction values (all combinations of Date, Type, deployment)
pred_grid <- shannon_data %>%
  select(Date, Type, deployment_period, Site) %>%
  distinct() %>%
  mutate(date_num = as.numeric(Date))
# Predict including random effects (re.form = NULL)
pred_grid$fit <- predict(shannon_mixed, newdata = pred_grid, re.form = NULL)

pred_grid <- pred_grid %>%
  left_join(shannon_data %>% select(Site, pair) %>% distinct(), by = "Site")

# Plot
ggplot(shannon_data, aes(x = Date, y = shannon, color = Type)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_line(data = pred_grid,
            aes(x = Date, y = fit, group = interaction(Site, Type), color = Type),
            linewidth = 1) +
  geom_vline(xintercept = as.Date("2023-09-07"),
             linetype = "dashed", color = "gray40") +
  scale_color_manual(values = c("Natural" = "#007A87", "Artificial" = "#66BFA6")) +
  facet_wrap(~ pair, scales = "free_x") +
  labs(
    title = "Shannon Diversity Trends by Site Pair",
    x = "Date",
    y = "Shannon Diversity Index",
    color = "Reef Type"
  ) +
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

