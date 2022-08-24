library(shiny)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(cowplot)

country_info <- readRDS("country.info.rds")

# Summarize data for the scatter plots
region_mort_tb1 <- country_info %>%
  dplyr::select(country, region, life_expectancy_birth, mortality_rate)

region_mort_tb2 <- region_mort_tb1 %>%
  # Create new column with categorization of life expectancy at birth
  mutate(life_expect_class = ifelse(life_expectancy_birth >= 74.76, "High",
                                    ifelse(life_expectancy_birth >= 70.07, "High-moderate",
                                           ifelse(life_expectancy_birth >= 65.54, "Moderate",
                                                  ifelse(life_expectancy_birth >= 59.70, "Low-moderate",
                                                         ifelse(life_expectancy_birth < 59.69, "Low", NA)))))) %>%
  # Filter out NA values
  drop_na(life_expect_class) %>%
  drop_na(mortality_rate) %>%
  group_by(region)

region_mort_tb2$life_expect_class <- factor(region_mort_tb2$life_expect_class,
                                            levels = c("Low", "Low-moderate", "Moderate","High-moderate","High"))

# Subset Europe and Central Asia
region_mort_eur_ca <- subset(region_mort_tb2, region == "Europe & Central Asia")

region_mort_eur_ca <- aggregate(region_mort_eur_ca$mortality_rate, by = list(life_expect_class = region_mort_eur_ca$life_expect_class),FUN = mean)

region_mort_eur_ca <- rename(region_mort_eur_ca, mortality_mean = x)

bar_eur_ca <- ggplot(region_mort_eur_ca, aes(x= life_expect_class, y= mortality_mean)) +
  geom_col()


# Subset South Asia
region_mort_sa <- subset(region_mort_tb2, region == "South Asia")

region_mort_sa <- aggregate(region_mort_sa$mortality_rate, by = list(life_expect_class = region_mort_sa$life_expect_class),FUN = mean)

region_mort_sa <- rename(region_mort_sa, mortality_mean = x)

bar_sa <- ggplot(region_mort_sa, aes(x= life_expect_class, y= mortality_mean)) +
  geom_col()

# Subset Sub-Sahran Africa

region_mort_ssa <- subset(region_mort_tb2, region == "Sub-Saharan Africa")

region_mort_ssa <- aggregate(region_mort_ssa$mortality_rate, by = list(life_expect_class = region_mort_ssa$life_expect_class),FUN = mean)

region_mort_ssa <- rename(region_mort_ssa, mortality_mean = x)

bar_ssa <- ggplot(region_mort_ssa, aes(x= life_expect_class, y= mortality_mean)) +
  geom_col()

# Subset Latin America & Caribbean

region_mort_la_cab <- subset(region_mort_tb2, region == "Latin America & Caribbean")

region_mort_la_cab <- aggregate(region_mort_la_cab$mortality_rate, by = list(life_expect_class = region_mort_la_cab$life_expect_class),FUN = mean)

region_mort_la_cab <- rename(region_mort_la_cab, mortality_mean = x)

bar_la_cab <- ggplot(region_mort_la_cab, aes(x= life_expect_class, y= mortality_mean)) +
  geom_col()

# Subset Middle East & North Africa

region_mort_me_na <- subset(region_mort_tb2, region == "Middle East & North Africa")

region_mort_me_na <- aggregate(region_mort_me_na$mortality_rate, by = list(life_expect_class = region_mort_me_na$life_expect_class),FUN = mean)

region_mort_me_na <- rename(region_mort_me_na, mortality_mean = x)

bar_me_na <- ggplot(region_mort_me_na, aes(x= life_expect_class, y= mortality_mean)) +
  geom_col()

# Subset East Asia & Pacific

region_mort_ea_pac <- subset(region_mort_tb2, region == "East Asia & Pacific")

region_mort_ea_pac <- aggregate(region_mort_ea_pac$mortality_rate, by = list(life_expect_class = region_mort_ea_pac$life_expect_class),FUN = mean)

region_mort_ea_pac <- rename(region_mort_ea_pac, mortality_mean = x)

bar_ea_pac <- ggplot(region_mort_ea_pac, aes(x= life_expect_class, y= mortality_mean)) +
  geom_col()

# Subset North America

region_mort_na <- subset(region_mort_tb2, region == "North America")

region_mort_na <- aggregate(region_mort_na$mortality_rate, by = list(life_expect_class = region_mort_na$life_expect_class),FUN = mean)

region_mort_na <- rename(region_mort_na, mortality_mean = x)

bar_na <- ggplot(region_mort_na, aes(x= life_expect_class, y= mortality_mean)) +
  geom_col()

region_life_grid <- plot_grid(bar_eur_ca,bar_sa, bar_ssa, bar_la_cab,bar_me_na, bar_ea_pac,bar_na,
                              labels = c("Europe and Central Asia", "South Asia", "Sub-Sahran Africa", "Latin America & Caribbean", 
                              "Middle East & North Africa", "East Asia & Pacific","North America"),
                              ncol = 4, nrow = 2)

