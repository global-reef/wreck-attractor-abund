# ABUNDANCE EXPLORATORY ANALYSIS


#Normality & Homogeneity of Varience

residuals_ <- residuals(anova_result)
hist(residuals_anova, main = "Residuals Histogram")
qqnorm(residuals_anova)
qqline(residuals_anova, col = "red")
shapiro.test(residuals_anova)
leveneTest(evenness ~ site, data = pielou_results)

#Compare total abundance between NR and AR
abundance_by_reeftype <- clean_abundance %>%
  group_by(reeftype) %>%
  summarise(total.N = sum(total.N, na.rm = TRUE), .groups = "drop") # Sums total.N column, ignore missing values and ungroup after

ggplot(abundance_by_reeftype, aes(x = reeftype, y = total.N, fill = reeftype)) +
  geom_bar(stat = "identity") + # height of each bar = value
  scale_fill_manual(values = c("Natural" = "peachpuff", "Artificial" = "lightblue")) +
  labs(title = "Total Fish Abundance by Reef Type",
       x = "Reef Type", y = "Total Abundance (N)", fill = "Reef Type") +
  theme_minimal()



#Normalise by survey effort
abundance_total_norm <- clean_abundance %>%
  group_by(reeftype) %>%
  summarise(total.N = sum(total.N, na.rm = TRUE), #Sums all fish counts for that reeftype, ignoring missing values
            survey_effort = n(), # Counts the number of rows per group
            .groups = "drop"  ) %>% # Ungroup
  mutate(normalized_abundance = total.N / survey_effort) # Adds a new column, (average abundance)

ggplot(abundance_total_norm, aes(x = reeftype, y = normalized_abundance, fill = reeftype)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("Natural" = "peachpuff", "Artificial" = "lightblue")) +
  labs(title = "Abundance Normalised by Survey Effort",
       x = "Reef Type",
       y = "Abundance (no. of individuals)",
       fill = "Reef Type") +
  theme_minimal() 




# Compare total abundance by site
custom_order <- c(
   "Hin Pee Wee","Sattakut","No Name", 
   "No Name Wreck", "No Name AR", "Aow Mao", "Aow Mao Wreck") # Set custom order
clean_abundance <- clean_abundance %>%
  mutate(site = factor(site, levels = custom_order))

ggplot(clean_abundance, aes(x = site, y = total.N, fill = reeftype)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("Natural" = "peachpuff", "Artificial" = "lightblue")) +
  labs(title = "Total Fish Abundance by Site",
       x = "Site", 
       y = "Total Abundance (No. of Individuals)", 
       fill = "Reef Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))



# Counts per site
survey_counts <- clean_abundance %>%
  distinct(site, date) %>%
  count(site, name = "n_surveys")
survey_counts



#Normalise by survey effort
abundance_norm <- clean_abundance %>%
  group_by(site, reeftype) %>%
  summarise(total.N = sum(total.N, na.rm = TRUE), #Sums all fish counts for that site, ignoring missing values
            survey_effort = n(), # Counts the number of rows per group
            .groups = "drop"  ) %>% # Ungroup
  mutate(normalized_abundance = total.N / survey_effort) # Adds a new column, (average abundance)

ggplot(abundance_norm, aes(x = site, y = normalized_abundance, fill = reeftype)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("Natural" = "peachpuff", "Artificial" = "lightblue")) +
  labs(title = "Normalised Abundance by survey Effort",
    x = "Site",
    y = "Abundance (No. of Individuals)",
    fill = "Reef Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))














