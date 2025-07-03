library(glmmTMB)
library(broom.mixed)
library(dplyr)
library(purrr)
library(ggeffects)
library(tidyverse)


# check site pairs 
model_pairwise <- glmmTMB(
  Count ~ Type * deployment_period * pair + Functional_Group + (1 | Site),
  data = fish_long,
  family = nbinom2()
)
summary(model_pairwise)

valid_pairs <- unique(fish_long$pair)
valid_pairs <- valid_pairs[valid_pairs != "Sattakut"]



pairwise_results <- map_dfr(valid_pairs, function(p) {
  
  df_sub <- fish_long %>% filter(pair == p)
  
  model <- glmmTMB(
    Count ~ Type * deployment_period + Functional_Group + (1 | Site),
    data = df_sub,
    family = nbinom2()
  )
  
  tidy(model) %>%
    filter(term == "TypeNatural:deployment_periodPre") %>%
    mutate(pair = p)
})
pairwise_results %>%
  select(pair, estimate, std.error, p.value) %>%
  arrange(desc(estimate))

# Add confidence intervals
pairwise_results <- pairwise_results %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

# Plot attraction effect per site pair 
ggplot(pairwise_results, aes(x = estimate, y = reorder(pair, estimate))) +
  geom_point(color = "#66BFA6", size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "#66BFA6") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Attraction Effect per Site Pair",
    x = "Estimated Interaction: Type × Deployment Period (log scale)",
    y = "Site Pair",
    subtitle = "Positive values suggest stronger post-deployment increases on Artificial reefs"
  ) +
  theme_minimal(base_size = 12)



# with functional groups 
valid_pairs <- unique(fish_long$pair)
valid_pairs <- valid_pairs[valid_pairs != "Sattakut"]
groups <- unique(fish_long$Functional_Group)


combinations <- expand_grid(pair_name = valid_pairs, group_name = groups)


pair_group_results <- pmap_dfr(combinations, function(pair_name, group_name) {
  
  df_sub <- fish_long %>%
    filter(pair == pair_name, Functional_Group == group_name, !is.na(Count))
  
  model <- tryCatch({
    glmmTMB(
      Count ~ Type * deployment_period + (1 | Site),
      data = df_sub,
      family = nbinom2()
    )
  }, error = function(e) return(NULL))
  
  if (is.null(model)) return(NULL)
  
  tidy(model) %>%
    filter(term == "TypeNatural:deployment_periodPre") %>%
    mutate(pair = pair_name, Functional_Group = group_name)
})


pair_group_results <- pair_group_results %>%
  mutate(
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

#  excluded invertivores from the attraction analysis due to near-zero counts on artificial reefs pre-deployment, which caused model instability
pair_group_results %>%
  filter(Functional_Group != "Invertivore") %>%
  ggplot(aes(x = estimate, y = reorder(pair, estimate), color = Functional_Group)) +
  geom_point(size = 2.8) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  facet_wrap(~ Functional_Group, scales = "free_x") +
  scale_color_manual(values = c(
    "Grazer" = "#007A87",        # Deep teal
    "Mesopredator" = "#66BFA6",  # Soft seafoam
    "HTLP" = "#5DA9E9"           # Cool sky blue
  )) +
  labs(
    title = "Attraction Effect (AR − NR Post-Deployment) by Site Pair and Functional Group",
    subtitle = "Estimate = Type × Deployment Period interaction (log scale)",
    x = "Interaction Estimate (log count)",
    y = "Site Pair"
  ) +
  theme_minimal(base_size = 12)




##### lets get predicted abundances over time for each site pair ### 
fish_totals <- fish_long %>%
  group_by(Site, Date, Type, pair, date_num, Species) %>%
  summarise(mean_count = mean(Count, na.rm = TRUE), .groups = "drop") %>%
  group_by(Site, Date, Type, pair, date_num) %>%
  summarise(Total_Count = ceiling(sum(mean_count)), .groups = "drop")


get_total_abundance_preds <- function(pair_name, data = fish_totals, origin = origin_date) {
  df_sub <- data %>% filter(pair == pair_name)
  
  model <- glmmTMB(
    Total_Count ~ Type * date_num + (1 | Site),
    data = df_sub,
    family = nbinom2()
  )
  
  preds <- ggpredict(model, terms = c("date_num", "Type"), type = "fixed")
  preds$Date <- as.Date(round(preds$x), origin = origin)
  preds$pair <- pair_name
  
  return(list(preds = preds, model = model))
}

result_nn <- get_total_abundance_preds("No Name")
predictions_nn <- result_nn$preds
model_obj_nn <- result_nn$model
summary(model_obj_nn)

result_am <- get_total_abundance_preds("Aow Mao")
predictions_am <- result_am$preds
model_obj_am <- result_am$model
summary(model_obj_am)

result_s <- get_total_abundance_preds("Sattakut")
predictions_s <- result_s$preds
model_obj_s <- result_s$model
summary(model_obj_s)


all_preds <- map_dfr(c("Aow Mao", "No Name", "Sattakut"), get_total_abundance_preds)
ggplot(all_preds, aes(x = Date, y = predicted, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.3, color = NA) +
  facet_wrap(~ pair, ncol = 3) +
  scale_color_manual(values = c("Natural" = "#007A87", "Artificial" = "#66BFA6")) +
  scale_fill_manual(values = c("Natural" = "#007A87", "Artificial" = "#66BFA6")) +
  labs(
    title = "Predicted Total Fish Abundance Over Time",
    x = "Date",
    y = "Predicted Total Abundance",
    color = "Reef Type",
    fill = "Reef Type"
  ) +
  theme_minimal(base_size = 14)+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


