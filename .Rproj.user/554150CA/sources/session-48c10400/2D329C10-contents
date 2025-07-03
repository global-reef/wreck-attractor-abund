## functonal groups between reef types 
library(tidyr)
library(lme4)
library(glmmTMB)
library(emmeans)
library(tidyverse)


fish_long$Functional_Group <- factor(
  fish_long$Functional_Group,
  levels = c("Grazer", "Invertivore", "Mesopredator", "HTLP"),
  ordered = FALSE
)


# Fit negative binomial GLMM
fun_groups <- glmmTMB(
  Count ~ Type * Functional_Group + (1 | Site),
  data = fish_long,
  family = nbinom2()
)

# Model summary
summary(fun_groups)
# Type II ANOVA (optional)
car::Anova(fun_groups)
# Estimated marginal means (optional)
emmeans(fun_groups, ~ Type | Functional_Group)

# Get predicted marginal means (on response scale)
emm <- emmeans(fun_groups, ~ Type | Functional_Group, type = "response")

# Convert to a dataframe for ggplot
emm_df <- as.data.frame(emm)

# Plot
ggplot(emm_df, aes(x = Functional_Group, y = response, fill = Type)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(0.8), width = 0.2) +
  scale_fill_manual(values = c("Natural" = "#007A87", "Artificial" = "#66BFA6")) +
  labs(
    x = "Functional Group",
    y = "Estimated Fish Count (mean ± 95% CI)",
    fill = "Reef Type"
  ) +
  theme_minimal(base_size = 12)+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )



ggplot(emm_df, aes(x = Type, y = response, fill = Type)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                position = position_dodge(0.7), width = 0.2) +
  facet_wrap(~ Functional_Group, scales = "free_y") +
  scale_fill_manual(values = c("Natural" = "#007A87", "Artificial" = "#66BFA6")) +
  labs(
    x = "Reef Type",
    y = "Estimated Fish Count (mean ± 95% CI)",
    fill = "Reef Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold")) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


# add time in 
fish_long <- fish_long %>%
  mutate(date_num = as.numeric(Date - min(Date)))

fun_groups_time <- glmmTMB(
  Count ~ Type * Functional_Group + 
    Type * (date_num + deployment_period) + 
    (1 | Site),
  data = fish_long,
  family = nbinom2()
)
summary(fun_groups_time)
car::Anova(fun_groups_time)

library(ggeffects)
library(ggplot2)

# Get predicted values across time, for each Type × Functional_Group combo
preds <- ggpredict(fun_groups_time, terms = c("date_num", "Type", "Functional_Group"))
# mutate back to real dates 
origin_date <- min(fish_long$Date)
preds$x_date <- origin_date + preds$x


ggplot(preds, aes(x = x_date, y = predicted, color = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  facet_wrap(~ facet, scales = "free_y") +
  scale_color_manual(values = c("Natural" = "#007A87", "Artificial" = "#66BFA6")) +
  scale_fill_manual(values = c("Natural" = "#007A87", "Artificial" = "#66BFA6")) +
  labs(
    x = "Date",
    y = "Predicted Fish Count",
    color = "Reef Type",
    fill = "Reef Type",
    title = "Modelled Fish Abundance Over Time by Functional Group"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )



# pairwise contrasts

# Estimate marginal means for Type at each date_num and Functional_Group
emm_time <- emmeans(fun_groups_time, ~ Type | date_num * Functional_Group, at = list(date_num = unique(fish_long$date_num)))

# Get pairwise contrasts (Artificial - Natural) at each time point
contrasts <- contrast(emm_time, method = "revpairwise")  # gives AR - NR
contrasts_df <- as.data.frame(contrasts)

# Add actual dates
origin_date <- min(fish_long$Date)
contrasts_df$date <- origin_date + contrasts_df$date_num
contrasts_df <- contrasts_df %>%
  mutate(
    ratio = exp(estimate),
    lower = exp(estimate - 1.96 * SE),
    upper = exp(estimate + 1.96 * SE)
  )
# shows ratio of AR to NR 
ggplot(contrasts_df, aes(x = date, y = ratio)) +
  geom_line(color = "#66BFA6", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#66BFA6", alpha = 0.3) +
  facet_wrap(~ Functional_Group, scales = "free_y") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(
    title = "Relative Abundance Ratio (Artificial / Natural)",
    x = "Date",
    y = "Abundance Ratio",
    subtitle = "Shaded ribbon shows 95% CI; dashed line = equal abundance"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

fish_long %>%
  group_by(Date, Type, Functional_Group) %>%
  summarise(mean_count = mean(Count), .groups = "drop") %>%
  ggplot(aes(x = Date, y = mean_count, color = Type)) +
  geom_line() +
  facet_wrap(~ Functional_Group, scales = "free_y") +
  scale_color_manual(values = c("Natural" = "#007A87", "Artificial" = "#66BFA6")) +
  theme_minimal()

