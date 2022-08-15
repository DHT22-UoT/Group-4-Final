# Load packages
library(dplyr)
library(corrplot)
library(ggplot2)
library(ggcorrplot)
library(tidyr)
library(gridExtra) # unnecessary??
library(cowplot)
library(smplot2)

# Load RDS file containing country data 
country_info <- readRDS("country.info.rds")
# Mortality rate is column 26

# Run a correlation matrix for initial analysis with only numeric indicators

first_cor_matrix <- cor(country_info[,4:20], country_info[,22:30], use="pairwise.complete.obs")
ggcorrplot(first_cor_matrix)

# Create individual correlation plots for selected top predictors
# Identified a moderate-high correlation between mortality rate and human development (col 12)

# Identify number of NAs in human_development - could impact results
sum(is.na(country_info$human_development))
# 167 NAs under human development index 

human_dev_plot <- ggcorrplot(country_info["human_development"], country_info["mortality_rate"])

#### Scatter plots for mortality rate ####

# Potential positive relations

lit_sp <- ggplot(country_info, aes(x = literacy_rate, y = mortality_rate)) +
  geom_point()

human_dev_sp <- ggplot(country_info, aes(x = human_development, y = mortality_rate)) +
  geom_point()

human_cap_sp <- ggplot(country_info, aes(x = human_capital_index, y = mortality_rate)) +
  geom_point()

life_expect_sp <- ggplot(country_info, aes(x = life_expectancy_birth, y = mortality_rate)) +
  geom_point()

gdp_growth_sp <- ggplot(country_info, aes(x = gdp_growth, y = mortality_rate)) +
  geom_point()

gni_percapita_sp <- ggplot(country_info, aes(x = gni_percapita, y = mortality_rate)) +
  geom_point()

health_expend_percap_sp <- ggplot(country_info, aes(x = health_expenditure_per_capita, y = mortality_rate)) +
  geom_point()

gdp_percapita_sp <- ggplot(country_info, aes(x = gdp_percapita, y = mortality_rate)) +
  geom_point()

# Potential negative relations

infant_mort_sp <- ggplot(country_info, aes(x = infant_mortality_rate, y = mortality_rate)) +
  geom_point()

poverty_gap_sp <- ggplot(country_info, aes(x = poverty_gap, y = mortality_rate)) +
  geom_point()

poverty_headcount_sp <- ggplot(country_info, aes(x = poverty_headcount_ratio, y = mortality_rate)) +
  geom_point()

gini_sp <- ggplot(country_info, aes(x = gini, y = mortality_rate)) +
  geom_point()

# Scatter plot grid for mortality rate

sp_mort_grid <- plot_grid(lit_sp, human_dev_sp, human_cap_sp,gdp_growth_sp,life_expect_sp
                          ,gni_percapita_sp, health_expend_percap_sp,
                          gdp_percapita_sp,infant_mort_sp,poverty_gap_sp,poverty_headcount_sp, gini_sp, 
                          labels=c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"),
                          ncol = 4, nrow = 3)

#### Scatter plots for morbidity rate ####

# Potential postive relations

lit_morbsp <- ggplot(country_info, aes(x = literacy_rate, y = morbidity_rate)) +
  geom_point()

human_dev_morbsp <- ggplot(country_info, aes(x = human_development, y = morbidity_rate)) +
  geom_point()

human_cap_morbsp <- ggplot(country_info, aes(x = human_capital_index, y = morbidity_rate)) +
  geom_point()

life_expect_morbsp <- ggplot(country_info, aes(x = life_expectancy_birth, y = morbidity_rate)) +
  geom_point()

gdp_growth_morbsp <- ggplot(country_info, aes(x = gdp_growth, y = morbidity_rate)) +
  geom_point()

gni_percapita_morbsp <- ggplot(country_info, aes(x = gni_percapita, y = morbidity_rate)) +
  geom_point()

health_expend_percap_morbsp <- ggplot(country_info, aes(x = health_expenditure_per_capita, y = morbidity_rate)) +
  geom_point()

gdp_percapita_morbsp <- ggplot(country_info, aes(x = gdp_percapita, y = morbidity_rate)) +
  geom_point()

# Potential negative relations

infant_mort_morbsp <- ggplot(country_info, aes(x = infant_mortality_rate, y = morbidity_rate)) +
  geom_point()

poverty_gap_morbsp <- ggplot(country_info, aes(x = poverty_gap, y = morbidity_rate)) +
  geom_point()

poverty_headcount_morbsp <- ggplot(country_info, aes(x = poverty_headcount_ratio, y = morbidity_rate)) +
  geom_point()

gini_morbsp <- ggplot(country_info, aes(x = gini, y = morbidity_rate)) +
  geom_point()

# Scatter plot grid morbidity rate

morb_sp_grid <- plot_grid(lit_morbsp, human_dev_morbsp, human_cap_morbsp,gdp_growth_morbsp,life_expect_morbsp,
                     gdp_growth_morbsp,gni_percapita_morbsp, health_expend_percap_morbsp,
                     infant_mort_morbsp,poverty_gap_morbsp,poverty_headcount_morbsp, gini_morbsp, 
                     labels=c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"),
                     ncol = 4, nrow = 3)

#### Individual scatter plots for confirmed cases ####

lit_cases_sp <- ggplot(country_info, aes(x = literacy_rate, y = confirmed)) +
  geom_point()

human_dev_cases_sp <- ggplot(country_info, aes(x = human_development, y = confirmed)) +
  geom_point()

human_cap_cases_sp <- ggplot(country_info, aes(x = human_capital_index, y = confirmed)) +
  geom_point()

life_expect_cases_sp <- ggplot(country_info, aes(x = life_expectancy_birth, y = confirmed)) +
  geom_point()

gdp_growth_cases_sp <- ggplot(country_info, aes(x = gdp_growth, y = confirmed)) +
  geom_point()

gni_percapita_cases_sp <- ggplot(country_info, aes(x = gni_percapita, y = confirmed)) +
  geom_point()

health_expend_percap_cases_sp <- ggplot(country_info, aes(x = health_expenditure_per_capita, y = confirmed)) +
  geom_point()

gdp_percapita_cases_sp <- ggplot(country_info, aes(x = gdp_percapita, y = confirmed)) +
  geom_point()

infant_mort_cases_sp <- ggplot(country_info, aes(x = infant_mortality_rate, y = confirmed)) +
  geom_point()

poverty_gap_cases_sp <- ggplot(country_info, aes(x = poverty_gap, y = confirmed)) +
  geom_point()

poverty_headcount_cases_sp <- ggplot(country_info, aes(x = poverty_headcount_ratio, y = confirmed)) +
  geom_point()

gini_cases_sp <- ggplot(country_info, aes(x = gini, y = confirmed)) +
  geom_point()

# Scatter plot grid for confirmed cases

sp_cases_grid <- plot_grid(lit_cases_sp, human_dev_cases_sp, human_cap_cases_sp,gdp_growth_cases_sp,life_expect_cases_sp,
                     gni_percapita_cases_sp, health_expend_percap_cases_sp,
                     gdp_percapita_cases_sp,infant_mort_cases_sp,poverty_gap_cases_sp,poverty_headcount_cases_sp, gini_cases_sp, 
                     labels=c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L"),
                     ncol = 4, nrow = 3)


#### Individual plots for best indicator candidates ####

# Low GINI 

gini_sp_2 <- ggplot(country_info, aes(x = gini, y = mortality_rate)) +
  geom_point() + ylab("Mortality Rate") + xlab("GINI") + geom_smooth(method = lm)

# Low GNI per capita

gni_percapita_sp_2 <- ggplot(country_info, aes(x = gni_percapita, y = mortality_rate)) +
  geom_point() + ylab("Mortality Rate") + xlab("GNI Per Capita") + geom_smooth(method = lm)

# Low health expenditure per capita 

health_expend_percap_sp_2 <- ggplot(country_info, aes(x = health_expenditure_per_capita, y = mortality_rate)) +
  geom_point()+ ylab("Mortality Rate") + xlab("Health Expenditure per Capita") + geom_smooth(method = lm)

# High life expectancy birth

life_expect_sp_2 <- ggplot(country_info, aes(x = life_expectancy_birth, y = mortality_rate)) +
  geom_point() + ylab("Mortality Rate") + xlab("Life Expectancy") + geom_smooth(method = lm)

# High human capital index - best 

human_cap_sp_2 <- ggplot(country_info, aes(x = human_capital_index, y = mortality_rate)) +
  geom_point() + ylab("Mortality Rate") + xlab("Human Capital Index") + geom_smooth(method = lm)

# High GDP growth rate - best

gdp_growth_sp_2 <- ggplot(country_info, aes(x = gdp_growth, y = mortality_rate)) +
  geom_point() + ylab("Mortality Rate") + xlab("GDP Growth Rate") + geom_smooth(method = lm)

# High literacy rate 

lit_sp_2 <- ggplot(country_info, aes(x = literacy_rate, y = mortality_rate)) +
  geom_point()+ ylab("Mortality Rate") + xlab("Literacy Rate") + geom_smooth(method = lm)

#### Bar plots for categorical indicators ####

# Create new data frame with only region and mortality rate
region_mort <- select(country_info, c("region", "mortality_rate"))

sum(is.na(region_mort$mortality_rate))

region_mort <- region_mort %>%
  drop_na(mortality_rate)

# Calculate mean mortality rates for each region
region_mort_2 <- aggregate(region_mort$mortality_rate, by = list(region = region_mort$region), FUN = mean)

# Rename the x with mortality rate mean
region_mort_2 <- dplyr::rename(region_mort_2, mortality_mean = x)

# Making region_mort_2 into a table 

# Bar plot for region and mortality rate 
region_barplot <- ggplot(region_mort_2, aes(region, mortality_mean)) + 
  geom_col(fill = "green") + ggtitle("Mean Mortality Rate by Region") +
  ylab("Mean Mortality Rate") + xlab("Region") + theme(plot.title = element_text(hjust = 0.5))

# Also want to include values on the graphs above each bar
