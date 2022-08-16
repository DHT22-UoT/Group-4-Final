# Load packages
library(dplyr)
library(corrplot)
library(ggplot2)
library(ggcorrplot)
library(tidyr)
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

# Low GDP per capita

gdp_percap_sp_2 <- ggplot(country_info, aes(x = gdp_percapita, y = mortality_rate)) +
  geom_point()+ ylab("Mortality Rate") + xlab("GDP per Capita") + geom_smooth(method = lm)

#### Bar plots for categorical indicators ####

# Create new data frame with only region and mortality rate
region_mort <- select(country_info, c("region", "mortality_rate"))

sum(is.na(region_mort$mortality_rate))

region_mort <- region_mort %>%
  drop_na(mortality_rate)

# Creating boxplots for mortality rate based on Region
region_boxplot <- ggplot(region_mort, aes(region, mortality_rate)) + 
  geom_boxplot(fill = "#0072B2") + ggtitle("Mortality Rate by Region") +
  ylab("Mortality Rate") + xlab("Region") + theme(plot.title = element_text(hjust = 0.5))

# Calculate mean mortality rates for each region
region_mort_2 <- aggregate(region_mort$mortality_rate, by = list(region = region_mort$region), FUN = mean)

# Rename the x with mortality rate mean
region_mort_2 <- rename(region_mort_2, mortality_mean = x)

# Saving as an RDS file

saveRDS(region_mort_2, file = "region.mort.df.2.RDS")

# Bar plot for region and mortality rate 
region_barplot <- ggplot(region_mort_2, aes(region, mortality_mean)) + 
  geom_col(fill = "#0072B2") + ggtitle("Mean Mortality Rate by Region") +
  ylab("Mean Mortality Rate") + xlab("Region") + theme(plot.title = element_text(hjust = 0.5))

#### Creating classification for countries based on GNI ####

gni_percap_df <- country_info %>%
  select(country, gni_percapita, mortality_rate) %>%
  # Create new column with categorization of GNI per capita
  mutate(gni_class = ifelse(gni_percapita >= 12535, "High Income",
         ifelse(gni_percapita >= 4046, "Upper-middle Income",
         ifelse(gni_percapita >= 1036, "Lower-middle Income",
         ifelse(gni_percapita < 1035, "Low Income", NA))))) %>%
  # Filter out NA values
  drop_na(mortality_rate) %>%
  drop_na(gni_class)

# Make categories of income a factor
gni_percap_df$gni_class <- factor(gni_percap_df$gni_class, levels = c("Low Income", "Lower-middle Income", 
                                                                      "Upper-middle Income", "High Income"))

# Create boxplot for mortality rates and gni classification
gni_class_boxplot <- ggplot(gni_percap_df, aes(gni_class, mortality_rate)) + 
  geom_boxplot(fill = "#0072B2") + ggtitle("Mortality Rate by GNI Class") +
  ylab("Mortality Rate") + xlab("GNI Class") + theme(plot.title = element_text(hjust = 0.5))

# Find mean mortality rate per gni class
gni_percap_df_2 <- aggregate(gni_percap_df$mortality_rate, by = list(gni_class = gni_percap_df$gni_class), 
                             FUN = mean)

gni_percap_df_2 <- rename(gni_percap_df_2, mortality_mean = x)

# Saving as an RDS file

saveRDS(gni_percap_df_2, file = "gni.percap.df.2.RDS")

# Plot on bar plot 

gni_class_barplot <- ggplot(gni_percap_df_2, aes(gni_class, mortality_mean)) + 
  geom_col(fill = "#0072B2") + ggtitle("Mean Mortality Rate by GNI Class") +
  ylab("Mean Mortality Rate") + xlab("GNI Class") + theme(plot.title = element_text(hjust = 0.5))

#### Health expenditure per capita Categorization ####

# ~6870 highest - Norway 
# ~1.8 lowest - Cameroon

# Categories 
# 0 - 550 Low
# 551 - 1600 Lower-middle
# 1601 - 3050 Upper-middle
# 3051+ High 

health_expend_df <- country_info %>%
  select(country, health_expenditure_per_capita, mortality_rate) %>%
  # Create new column with categorization of Health expend per capita
  mutate(health_expend_class = ifelse(health_expenditure_per_capita >= 3051, "High Expenditure",
                            ifelse(health_expenditure_per_capita >= 1601, "Upper-middle Expenditure",
                                   ifelse(health_expenditure_per_capita >= 551, "Lower-middle Expenditure",
                                          ifelse(health_expenditure_per_capita < 550, "Low Expenditure", NA))))) %>%
  # Filter out NA values
  drop_na(mortality_rate) %>%
  drop_na(health_expenditure_per_capita)

# Make health expend classes into factors
health_expend_df$health_expend_class <- factor(health_expend_df$health_expend_class,
                                               levels = c("Low Expenditure", "Lower-middle Expenditure", "Upper-middle Expenditure", "High Expenditure"))

# Create boxplot for mortality rates and health expend classification
health_expend_boxplot <- ggplot(health_expend_df, aes(health_expend_class, mortality_rate)) + 
  geom_boxplot(fill = "#0072B2") + ggtitle("Mortality Rate by Health Expenditure Class") +
  ylab("Mortality Rate") + xlab("Health Expenditure Class") + theme(plot.title = element_text(hjust = 0.5))

# Find mean mortality rate per health expend class
health_expend_df_2 <- aggregate(health_expend_df$mortality_rate, by = list(health_expend_class = health_expend_df$health_expend_class), 
                             FUN = mean)

health_expend_df_2 <- rename(health_expend_df_2, mortality_mean = x)

# Save as an RDS file

saveRDS(health_expend_df_2, file = "health.expend.df.2.RDS")

# Plot on bar plot 

health_expend_barplot <- ggplot(health_expend_df_2, aes(health_expend_class, mortality_mean)) + 
  geom_col(fill = "#0072B2") + ggtitle("Mean Mortality Rate by Health Expenditure Class") +
  ylab("Mean Mortality Rate") + xlab("Expenditure Class") + theme(plot.title = element_text(hjust = 0.5))

#### GDP Class Categorization ####

# Categorizations
# < 0.25 low
# 0.25 - 1.02 low middle
# 1.02 - 2.10 middle
# 2.10 - 4.94 upper middle
# 4.94 + high

gdp_df <- country_info %>%
  select(country, gdp, mortality_rate) %>%
  # Make a new column for gdp per trillion
  mutate(gdp_in_tril = gdp/1000000000000) %>%
  # Create new column with categorization of GDP (USD in - trillions )
  mutate(gdp_class = ifelse(gdp_in_tril >= 4.94, "High",
                            ifelse(gdp_in_tril >= 2.10, "Upper-middle",
                                   ifelse(gdp_in_tril >= 1.02, "Middle",
                                          ifelse(gdp_in_tril >= 0.25, "Lower-middle",
                                                 ifelse(gdp_in_tril < 0.25, "Low", NA)))))) %>%
  # Filter out NA values
  drop_na(mortality_rate) %>%
  drop_na(gdp_class)

# Make GDP classes into factors 
gdp_df$gdp_class <- factor(gdp_df$gdp_class,
                           levels = c("Low", "Lower-middle", "Middle", "Upper-middle","High"))

# Create boxplot for mortality rates and gdp classification
gdp_class_boxplot <- ggplot(gdp_df, aes(gdp_class, mortality_rate)) + 
  geom_boxplot(fill = "#0072B2") + ggtitle("Mortality Rate by GDP Class") +
  ylab("Mortality Rate") + xlab("GDP Class") + theme(plot.title = element_text(hjust = 0.5))

# Find mean mortality rate per health expend class
gdp_df_2 <- aggregate(gdp_df$mortality_rate, by = list(gdp_class = gdp_df$gdp_class), 
                                FUN = mean)

gdp_df_2 <- rename(gdp_df_2, mortality_mean = x)

# Save as an RDS file

saveRDS(gdp_df_2, file = "gdp.df.2.RDS")

# Plot on bar plot 

gdp_barplot <- ggplot(gdp_df_2, aes(gdp_class, mortality_mean)) + 
  geom_col(fill = "#0072B2") + ggtitle("Mean Mortality Rate by GDP Class") +
  ylab("Mean Mortality Rate") + xlab("GDP Class") + theme(plot.title = element_text(hjust = 0.5))

#### GDP Growth Rate Categorization ####

# < -8.08 low income
# -8.08 - 1.02 low-middle income
# 1.02 - 5.04 middle income
# 5.04 - 9.41 upper-middle income
# 9.41+ high income

gdp_growth_df <- country_info %>%
  select(country, gdp_growth, mortality_rate) %>%
  # Create new column with categorization of GDP growth rate
  mutate(gdp_growth_class = ifelse(gdp_growth >= 9.41, "High",
                            ifelse(gdp_growth >= 5.04, "Upper-middle",
                                   ifelse(gdp_growth >= 1.02, "Middle",
                                          ifelse(gdp_growth >= -8.08, "Lower-middle",
                                                 ifelse(gdp_growth < -8.08, "Low", NA)))))) %>%
  # Filter out NA values
  drop_na(mortality_rate) %>%
  drop_na(gdp_growth_class)

# Make GDP growth classes into factors 
gdp_growth_df$gdp_growth_class <- factor(gdp_growth_df$gdp_growth_class,
                           levels = c("Low", "Lower-middle", "Middle", "Upper-middle","High"))

# Create boxplot for mortality rates and gdp classification
gdp_growth_class_boxplot <- ggplot(gdp_growth_df, aes(gdp_growth_class, mortality_rate)) + 
  geom_boxplot(fill = "#0072B2") + ggtitle("Mortality Rate by GDP Growth Class") +
  ylab("Mortality Rate") + xlab("GDP Growth Class") + theme(plot.title = element_text(hjust = 0.5))

# Find mean mortality rate per health expend class
gdp_growth_df_2 <- aggregate(gdp_growth_df$mortality_rate, by = list(gdp_growth_class = gdp_growth_df$gdp_growth_class), 
                      FUN = mean)

gdp_growth_df_2 <- rename(gdp_growth_df_2, mortality_mean = x)

# Save as an RDS file

saveRDS(gdp_growth_df_2, file = "gdp.growth.df.2.RDS")

# Plot on bar plot 

gdp_growth_barplot <- ggplot(gdp_growth_df_2, aes(gdp_growth_class, mortality_mean)) + 
  geom_col(fill = "#0072B2") + ggtitle("Mean Mortality Rate by GDP Growth Class") +
  ylab("Mean Mortality Rate") + xlab("GDP Growth Class") + theme(plot.title = element_text(hjust = 0.5))

#### Human Capital Index Categorization #### 

# < 0.39 low
# 0.39 - 0.47 low middle 
# 0.47 - 0.56 middle 
# 0.56 - 0.64 upper middle
# 0.64+ high

human_cap_df <- country_info %>%
  select(country, human_capital_index, mortality_rate) %>%
  # Create new column with categorization of human cap index
  mutate(human_cap_class = ifelse(human_capital_index >= 0.64, "High",
                                   ifelse(human_capital_index >= 0.56, "Upper-middle",
                                          ifelse(human_capital_index >= 0.47, "Middle",
                                                 ifelse(human_capital_index >= 0.39, "Lower-middle",
                                                        ifelse(human_capital_index < 0.39, "Low", NA)))))) %>%
  # Filter out NA values
  drop_na(mortality_rate) %>%
  drop_na(human_cap_class)

# Make human cap index classes into factors 
human_cap_df$human_cap_class <- factor(human_cap_df$human_cap_class,
                                         levels = c("Low", "Lower-middle", "Middle", "Upper-middle","High"))

# Create boxplot for mortality rates and human capital index
human_cap_boxplot <- ggplot(human_cap_df, aes(human_cap_class, mortality_rate)) + 
  geom_boxplot(fill = "#0072B2") + ggtitle("Mortality Rate by Human Capital Index Class") +
  ylab("Mortality Rate") + xlab("Human Capital Index Class") + theme(plot.title = element_text(hjust = 0.5))

# Find mean mortality rate per human capital index
human_cap_df_2 <- aggregate(human_cap_df$mortality_rate, by = list(human_cap_class = human_cap_df$human_cap_class), 
                             FUN = mean)

human_cap_df_2 <- rename(human_cap_df_2, mortality_mean = x)

# Save as an RDS file

saveRDS(human_cap_df_2, file = "human.cap.df.2.RDS")

# Plot on bar plot 

human_cap_barplot <- ggplot(human_cap_df_2, aes(human_cap_class, mortality_mean)) + 
  geom_col(fill = "#0072B2") + ggtitle("Mean Mortality Rate by Human Capital Index Class") +
  ylab("Mean Mortality Rate") + xlab("Human Capital Index Class") + theme(plot.title = element_text(hjust = 0.5))


#### Life Expectancy at Birth Categorization ####

# < 59.69 low 
# 59.70 - 65.53 low-moderate
# 65.54 - 70.06 moderate
# 70.07 - 74.75 high-moderate
# 74.76 high

life_expect_df <- country_info %>%
  select(country, life_expectancy_birth, mortality_rate) %>%
  # Create new column with categorization of life expectancy at birth
  mutate(life_expect_class = ifelse(life_expectancy_birth >= 74.76, "High",
                                  ifelse(life_expectancy_birth >= 70.07, "High-moderate",
                                         ifelse(life_expectancy_birth >= 65.54, "Moderate",
                                                ifelse(life_expectancy_birth >= 59.70, "Low-moderate",
                                                       ifelse(life_expectancy_birth < 59.69, "Low", NA)))))) %>%
  # Filter out NA values
  drop_na(life_expect_class) %>%
  drop_na(mortality_rate)

# Make life expectancy classes into factors 
life_expect_df$life_expect_class <- factor(life_expect_df$life_expect_class,
                                       levels = c("Low", "Low-moderate", "Moderare", "High-moderate","High"))

# Create boxplot for mortality rates and life expectancy classes
life_expect_boxplot <- ggplot(life_expect_df, aes(life_expect_class, mortality_rate)) + 
  geom_boxplot(fill = "#0072B2") + ggtitle("Mortality Rate by Life Expectancy") +
  ylab("Mortality Rate") + xlab("Life Expectancy") + theme(plot.title = element_text(hjust = 0.5))

# Find mean mortality rate per life expectancy
life_expect_df_2 <- aggregate(life_expect_df$mortality_rate, by = list(life_expect_class = life_expect_df$life_expect_class), 
                            FUN = mean)

life_expect_df_2 <- rename(life_expect_df_2, mortality_mean = x)

# Save as an RDS file

saveRDS(life_expect_df_2, file = "life.expect.df.2.RDS")

# Plot on bar plot 

life_expect_barplot <- ggplot(life_expect_df_2, aes(life_expect_class, mortality_mean)) + 
  geom_col(fill = "#0072B2") + ggtitle("Mean Mortality Rate by Life Expectancy") +
  ylab("Mean Mortality Rate") + xlab("Life Expectancy") + theme(plot.title = element_text(hjust = 0.5))


