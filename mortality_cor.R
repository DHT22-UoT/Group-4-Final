# Load packages
library(dplyr)
library(Hmisc)
library(corrplot)
library(ggplot2)
library(devtools)
library(ggcorrplot)

# Load RDS file containing country data 
country_info <- readRDS("country.info.rds")
# Mortality rate is column 26

# Run a correlation matrix for initial analysis with only numeric indicators

first_cor_matrix <- cor(country_info[,4:20], country_info[,22:30], use="pairwise.complete.obs")
ggcorrplot(first_cor_matrix)

# Create individual correlation plots for selected top predictors
# Identified a moderate-high correlation between mortality rate and human development (col 12)

human_dev_plot <- ggcorrplot(country_info["human_development"], country_info["mortality_rate"])

# Indentify number of NAs in human_development - could impact results

# Create plots for categorical indicators

# Create new data frame with only region and mortality rate
region_mort <- select(country_info, c("region", "mortality_rate"))

region_mort %>% group_by(region)


# Bar plot for region and mortality rate
ggplot(region_mort, aes(x=region, y=mortality_rate)) +
  geom_bar(color="blue")
