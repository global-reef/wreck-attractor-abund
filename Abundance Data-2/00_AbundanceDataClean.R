#ABUNDANCE DATA CLEAN

#Load Packages
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)

# Load Data 
setwd("/Users/milliecharmoy/Documents/UNIVERSITY/MSc/DISSERTATION/DATA/Abundance")
abundance_data <- read.csv("Abundance_ORIGINAL.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

#Remove/Add Columns
abundance_data <- abundance_data %>% 
  select(-X, -X.1, -X.2, -X.3, -X.4, -X.5, -X.6, -X.7)

#Rename Columns
abundance_data <- abundance_data %>%
  rename(grouperless30 = grouper.30) 
abundance_data <- abundance_data %>%
  rename(snapperover30 = snapper.30)
abundance_data <- abundance_data %>%
  rename(grouperover30 = grouper.30.1) 

#Check site names are unique 
unique_sites <- unique(abundance_data$site)
abundance_data <- abundance_data %>%
  mutate(site = str_trim(site)) %>% # remove leading/trailing spaces
  mutate(site = ifelse(site == "", NA, site)) %>%
  filter(!is.na(site)) %>% # remove rows with no site name
  mutate(site = ifelse(site == "Shark island", "Shark Island", site)) %>%
  rename(reeftype = typereef)

#Check reef types are unique
unique_reeftype <- unique(abundance_data$reeftype)
abundance_data <- abundance_data %>%
  mutate(reeftype = str_trim(reeftype)) # remove leading/trailing spaces

# Remove NAs & change variable type
abundance_data <- abundance_data %>%
  mutate(sweetlips = as.numeric(replace(sweetlips, sweetlips == "-", "0"))) %>% # Replace "-" with "0" in sweetlips, then convert to numeric
  mutate(across(parrotfish:barracuda, ~ replace(., is.na(.), 0))) %>% # Replace NAs in fish count columns with 0
  mutate(across(c(site, reeftype, classification), as.factor)) # Convert grouping/categorical variables to factors

#Keep only sites of interest
sites_of_interest <- c("Hin Pee Wee","Sattakut","No Name Pinnacle", "No Name Wreck", "Aow Mao", "Aow Mao Wreck")
abundance_data <- abundance_data %>%
  filter(site %in% sites_of_interest)

#Round values up
abundance_data <- abundance_data %>%
  mutate(across(c(parrotfish : barracuda), ceiling))

#Create a new totals column
abundance_data <- abundance_data %>%
  mutate(total = rowSums(across(c(parrotfish : barracuda))))
abundance_data <- abundance_data %>% # Remove old totals column
  select(-total.N)
abundance_data <- abundance_data %>%
  rename(total.N = total)

# Fix bad dates
abundance_data$date <- as.Date(abundance_data$date, format = "%m/%d/%Y") #tells R the format of the date
year_vals <- year(abundance_data$date) # Extract the year from the date
rows_to_fix <- which(year_vals == 25) # Identify rows where year == 25
abundance_data$date[rows_to_fix] <- 
  abundance_data$date[rows_to_fix] + years(2000) # Fix those dates by adding 2000 years (0025 to 2025)
summary(abundance_data$date)

# Add days since deployment column
deploy_date <- as.Date("2023-09-07", format = "%Y-%m-%d") #stores this date as deployment_date
abundance_data$days_since <- as.numeric(abundance_data$date - as.Date("2023-09-07", format = "%Y-%m-%d"))
abundance_data$days_since_scaled <- scale(abundance_data$days_since) #center the variable (mean = 0) and standardize it (SD = 1) 

# Create Pre and Post Deployment categories
abundance_data$period <- ifelse(abundance_data$date < deploy_date, "pre", "post") # If this condition is TRUE, assign the first value; otherwise, assign the second
abundance_data$period <- factor(abundance_data$period, levels = c("pre", "post")) # Sets an order, pre before post

clean_abundance <- abundance_data
View(clean_abundance)





















