#Load packages
library(dplyr)
library(corrplot)
library(ggplot2)
library(ggcorrplot)
library(tidyr)
library(cowplot)
library(tidyverse)
library(multcomp)
library("ggpubr")
library(multcompView)
library(ggthemes)
library(plotrix)

### 1. ANOVA FOR GDP GROWTH RATE ###

#### GDP Growth Rate Categorization ####

# < -8.08 low income
# -8.08 - 1.02 low-middle income
# 1.02 - 5.04 middle income
# 5.04 - 9.41 upper-middle income
# 9.41+ high income

# gdp_growth_df <- country_info %>%
#   select(country, gdp_growth, mortality_rate) %>%
#   # Create new column with categorization of GDP growth rate
#   mutate(gdp_growth_class = ifelse(gdp_growth >= 9.41, "High",
#                                    ifelse(gdp_growth >= 5.04, "Upper-middle",
#                                           ifelse(gdp_growth >= 1.02, "Middle",
#                                                  ifelse(gdp_growth >= -8.08, "Lower-middle",
#                                                         ifelse(gdp_growth < -8.08, "Low", NA)))))) %>%
#   # Filter out NA values
#   drop_na(mortality_rate) %>%
#   drop_na(gdp_growth_class)

# # Make GDP growth classes into factors 
# gdp_growth_df$gdp_growth_class <- factor(gdp_growth_df$gdp_growth_class,
#                                          levels = c("Low", "Lower-middle", "Middle", "Upper-middle","High"))
levels(gdp_growth_df$gdp_growth_class) <- c("Low", "Lower_middle", "Middle", "Upper_middle", "High")


### 1.1 Preliminary analyses
## 1.1.1. Draw and compare boxplots of the quantitative variable mortality rate for each GDP growth rate category.
# First, select columns containing GDP growth rate categories and mortality rates. 
gdp_anova_data <- gdp_growth_df %>% 
  dplyr::select(gdp_growth_class, mortality_rate)

gdp_growth_boxplot <- ggboxplot(gdp_anova_data, x = "gdp_growth_class", y = "mortality_rate", 
          color = "gdp_growth_class", palette = c("#00AFBB", "#E7B800", "#FC4E07", "pink", "black"),
          ylab = "COVID-19 Mortality Rate", xlab = "GDP Growth Rate Class")
# The box plot shows the "High" GDP growth class seems to have the highest mortality rate.

## 1.1.2. Descriptive stats for the data
gdp_descriptive_stats <- group_by(gdp_anova_data, gdp_growth_class) %>%
  summarise(
    mean = mean(mortality_rate, na.rm = TRUE),
    sd = sd(mortality_rate, na.rm = TRUE))

### 1.2. ANOVA analysis for GDP growth rate
gdp_aov <- aov(mortality_rate ~ gdp_growth_class,
               data = gdp_anova_data)

gdp_anova_result <- data.frame(unclass(summary(gdp_aov)))
# The p value is 0.000159, which is smaller than 0.05, we reject the null hypothesis.
# We reject the hypothesis that all means are equal and conclude that at least 
# one category (one GDP growth rate class) is different than the others in terms of mortality rate.

### 1.3. Post-hoc test - Tukey’s test to determine which groups are different
gdp_tukey_result <- TukeyHSD(gdp_aov, conf.level = 0.95)
gdp_tukey_matrix <- as.matrix (gdp_tukey_result) 
gdp_tukey_df <- as.data.frame(gdp_tukey_matrix[1])
# The adjusted p-values show that at alpha = 0.05, mean mortality rate for high GDP growth rate class
# differs significantly from that for low, lower-middle, and middle classes. In addition, the mean mortality rate for 
# upper-middle GDP growth rate class is significantly different from that for lower-middle class. Mortality rate is 
# higher for the high and upper-middle (as compared to the lower-middle class) GDP growth rate classes. 
# All other comparisons are not statistically significant.

## 1.3.1. Plotting the Tukey's test result, showing significant results in red
plot(TukeyHSD(gdp_aov, conf.level= 0.95), las = 2, col= ifelse(gdp_tukey_df[,4] < 0.05, 'red', 'black'))
# Some of the confidence intervals for the mean value between groups do not contain the value zero.
# These groups are the ones with p values lower than 0.05 in the Tukey's test.

## 1.3.2. Use bar plot with standard error or 95% confidence interval to show the group information and alternatively show
#  the Tukey test result better, where the height of each bar is proportional to the mean of each group.
cld <- multcompLetters4(gdp_aov, gdp_tukey_result)

dt <- group_by(gdp_anova_data, gdp_growth_class) %>%
  summarize(mean_mortality_rate = mean(mortality_rate), standard_error = std.error(mortality_rate)) %>%
  arrange(desc(mean_mortality_rate))

cld <- as.data.frame.list(cld$gdp_growth_class)
dt$cld <- cld$Letters

ggplot(dt, aes(gdp_growth_class, mean_mortality_rate)) + 
  geom_bar(stat = "identity", aes(fill = mean_mortality_rate), show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean_mortality_rate - standard_error, ymax = mean_mortality_rate + standard_error), width = 0.2) +
  labs(x = "GDP Growth Rate Class", y = "COVID-19 Mortality Rate") +
  geom_text(aes(label = cld), vjust = -0.5, hjust = -0.5) +
  theme_minimal() 

# The bar plot shows that the "High" GDP growth class seems to have the highest mortality rate. 
# Means not sharing any letter are significantly different at the 5% significance level.These are consistent with 
# the previous plot.

### 2. ANOVA FOR HUMAN CAPITAL INDEX ###
#### Categorization #### 

# < 0.39 low
# 0.39 - 0.47 low middle 
# 0.47 - 0.56 middle 
# 0.56 - 0.64 upper middle
# 0.64+ high

# human_cap_df <- country_info %>%
#   select(country, human_capital_index, mortality_rate) %>%
#   # Create new column with categorization of human cap index
#   mutate(human_cap_class = ifelse(human_capital_index >= 0.64, "High",
#                                   ifelse(human_capital_index >= 0.56, "Upper-middle",
#                                          ifelse(human_capital_index >= 0.47, "Middle",
#                                                 ifelse(human_capital_index >= 0.39, "Lower-middle",
#                                                        ifelse(human_capital_index < 0.39, "Low", NA)))))) %>%
#   # Filter out NA values
#   drop_na(mortality_rate) %>%
#   drop_na(human_cap_class)
# 
# # Make human cap index classes into factors 
# human_cap_df$human_cap_class <- factor(human_cap_df$human_cap_class,
#                                        levels = c("Low", "Lower-middle", "Middle", "Upper-middle","High"))

levels(human_cap_df$human_cap_class) <- c("Low", "Lower_middle", "Middle", "Upper_middle", "High")

### 2.1 Preliminary analyses
## 2.1.1. Draw and compare boxplots of the quantitative variable mortality rate for each Human Capital Index category.
# First, select columns containing Human Capital Index categories and mortality rates. 
hci_anova_data <- human_cap_df %>% 
  dplyr::select(human_cap_class, mortality_rate)

hci_boxplot <- ggboxplot(hci_anova_data, x = "human_cap_class", y = "mortality_rate", 
                                color = "human_cap_class", palette = c("#FF5733", "#4F33FF", "#188023", "#449D9A", "#700623"),
                                ylab = "COVID-19 Mortality Rate", xlab = "Human Capital Index Class")
# The box plot shows the "Upper_middle" and "High" Human Capital Index classes seem to have the highest mortality rate.

## 2.1.2. Descriptive stats for the data
hci_descriptive_stats <- group_by(hci_anova_data, human_cap_class) %>%
  summarise(
    mean = mean(mortality_rate, na.rm = TRUE),
    sd = sd(mortality_rate, na.rm = TRUE))

### 2.2. ANOVA analysis for Human Capital Index Classes
hci_aov <- aov(mortality_rate ~ human_cap_class,
               data = hci_anova_data)

hci_anova_result <- data.frame(unclass(summary(hci_aov)))
# The p value is 2e-11, which is smaller than 0.05, we reject the null hypothesis.
# We reject the hypothesis that all means are equal and conclude that at least 
# one category (one Human Capital Index class) is different than the others in terms of mortality rate.

### 2.3. Post-hoc test - Tukey’s test to determine which groups are different
hci_tukey_result <- TukeyHSD(hci_aov, conf.level = 0.95)
hci_tukey_matrix <- as.matrix (hci_tukey_result) 
hci_tukey_df <- as.data.frame(hci_tukey_matrix[1])
# The adjusted p-values show that at alpha = 0.05, mean mortality rate for High and Upper-middle Human Capital Index classes
# differs significantly from that for low, lower-middle, and middle classes. The mortality rate is 
# higher for the high and upper-middle classes. All other comparisons are not statistically significant.

## 2.3.1. Plotting the Tukey's test result, showing significant results in red
par(mar=c(5, 12, 4, 1))
plot(TukeyHSD(hci_aov, conf.level= 0.95), las = 2, col= ifelse(hci_tukey_df[,4] < 0.05, 'red', 'black'))
# Some of the confidence intervals for the mean value between groups do not contain the value zero.
# These groups are the ones with p values lower than 0.05 in the Tukey's test.

## 2.3.2. Use bar plot with standard error or 95% confidence interval to show the group information and alternatively show
#  the Tukey test result better, where the height of each bar is proportional to the mean of each group.
cld2 <- multcompLetters4(hci_aov, hci_tukey_result)

dt2 <- group_by(hci_anova_data, human_cap_class) %>%
  summarize(mean_mortality_rate = mean(mortality_rate), standard_error = std.error(mortality_rate)) %>%
  arrange(desc(mean_mortality_rate))

cld2 <- as.data.frame.list(cld2$human_cap_class)
dt2$cld <- cld2$Letters

ggplot(dt2, aes(human_cap_class, mean_mortality_rate)) + 
  geom_bar(stat = "identity", aes(fill = mean_mortality_rate), show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean_mortality_rate - standard_error, ymax = mean_mortality_rate + standard_error), width = 0.2) +
  labs(x = "Human Capital Index Class", y = "COVID-19 Mortality Rate") +
  geom_text(aes(label = cld2$Letters), vjust = -0.5, hjust = -0.5) +
  theme_minimal() 

# The bar plot shows that the "Upper_middle" and "High" Human Capital Index classes seem to have the highest mortality rate. 
# Means not sharing any letter are significantly different at the 5% significance level.These are consistent with 
# the previous plot.

### 3. ANOVA FOR LIFE EXPECTANCY AT BIRTH ###
#### Life Expectancy at Birth Categorization ####

# < 59.69 low 
# 59.70 - 65.53 low-moderate
# 65.54 - 70.06 moderate
# 70.07 - 74.75 high-moderate
# 74.76 high

# life_expect_df <- country_info %>%
#   select(country, life_expectancy_birth, mortality_rate) %>%
#   # Create new column with categorization of life expectancy at birth
#   mutate(life_expect_class = ifelse(life_expectancy_birth >= 74.76, "High",
#                                     ifelse(life_expectancy_birth >= 70.07, "High-moderate",
#                                            ifelse(life_expectancy_birth >= 65.54, "Moderate",
#                                                   ifelse(life_expectancy_birth >= 59.70, "Low-moderate",
#                                                          ifelse(life_expectancy_birth < 59.69, "Low", NA)))))) %>%
#   # Filter out NA values
#   drop_na(life_expect_class) %>%
#   drop_na(mortality_rate)
# 
# # Make life expectancy classes into factors 
# life_expect_df$life_expect_class <- factor(life_expect_df$life_expect_class,
#                                            levels = c("Low", "Low-moderate", "Moderate", "High-moderate","High"))
levels(life_expect_df$life_expect_class) <- c("Low", "Low_moderate", "Moderate", "High_moderate","High")

### 3.1 Preliminary analyses
## 3.1.1. Draw and compare boxplots of the quantitative variable mortality rate for each life expectancy category.
# First, select columns containing life expectancy categories and mortality rates. 
le_anova_data <- life_expect_df %>% 
  dplyr::select(life_expect_class, mortality_rate)

le_boxplot <- ggboxplot(le_anova_data, x = "life_expect_class", y = "mortality_rate", 
                         color = "life_expect_class", palette = c("#1a6aeb", "#eb1a78", "#1aebc5", "#4beb1a", "#baeb1a"),
                         ylab = "COVID-19 Mortality Rate", xlab = "Life Expectancy at Birth Class")
# The box plot shows the "High" Life Expectancy at Birth class seems to have the highest mortality rate.

## 3.1.2. Descriptive stats for the data
le_descriptive_stats <- group_by(le_anova_data, life_expect_class) %>%
  summarise(
    mean = mean(mortality_rate, na.rm = TRUE),
    sd = sd(mortality_rate, na.rm = TRUE))

### 3.2. ANOVA analysis for Life Expectancy at Birth Classes
le_aov <- aov(mortality_rate ~ life_expect_class,
               data = le_anova_data)

le_anova_result <- data.frame(unclass(summary(le_aov)))
# The p value is 2.36e-11, which is smaller than 0.05, we reject the null hypothesis.
# We reject the hypothesis that all means are equal and conclude that at least 
# one category (one Life Expectancy at Birth class) is different than the others in terms of mortality rate.

### 3.3. Post-hoc test - Tukey’s test to determine which groups are different
le_tukey_result <- TukeyHSD(le_aov, conf.level = 0.95)
le_tukey_matrix <- as.matrix (le_tukey_result) 
le_tukey_df <- as.data.frame(le_tukey_matrix[1])
# The adjusted p-values show that at alpha = 0.05, mean mortality rate for 
# High Life Expectancy at Birth class differs significantly from that for low, low_moderate, 
# moderate, and high_moderate classes. In addition, mean mortality rate for 
# High_moderate Life Expectancy at Birth class differs significantly from that for low, 
# low_moderate, and moderate classes. The mortality rate is 
# higher for the high and high_moderate classes. All other comparisons are not statistically significant.

## 3.3.1. Plotting the Tukey's test result, showing significant results in red
par(mar=c(5, 13, 4, 1))
plot(TukeyHSD(le_aov, conf.level= 0.95), las = 2, col= ifelse(le_tukey_df[,4] < 0.05, 'red', 'black'))
# Some of the confidence intervals for the mean value between groups do not contain the value zero.
# These groups are the ones with p values lower than 0.05 in the Tukey's test.

## 3.3.2. Use bar plot with standard error or 95% confidence interval to show the group information and alternatively show
#  the Tukey test result better, where the height of each bar is proportional to the mean of each group.
cld3 <- multcompLetters4(le_aov, le_tukey_result)

dt3 <- group_by(le_anova_data, life_expect_class) %>%
  summarize(mean_mortality_rate = mean(mortality_rate), standard_error = std.error(mortality_rate)) %>%
  arrange(desc(mean_mortality_rate))

cld3 <- as.data.frame.list(cld3$life_expect_class)
dt3$cld <- cld3$Letters

ggplot(dt3, aes(life_expect_class, mean_mortality_rate)) + 
  geom_bar(stat = "identity", aes(fill = mean_mortality_rate), show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean_mortality_rate - standard_error, ymax = mean_mortality_rate + standard_error), width = 0.2) +
  labs(x = "Life Expectancy at Birth Class", y = "COVID-19 Mortality Rate") +
  geom_text(aes(label = cld3$Letters), vjust = -0.5, hjust = -0.5) +
  theme_minimal() 

# The bar plot shows that the "High_moderate" and "High" Life Expectancy at Birth classes seem to have the highest mortality rate. 
# Means not sharing any letter are significantly different at the 5% significance level.These are consistent with 
# the previous plot.

