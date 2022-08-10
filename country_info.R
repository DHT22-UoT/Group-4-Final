library(dplyr)
library(httr)
library(jsonlite)
library(wbstats)
library(countrycode)
library(sys)

##### Extract data from the worldbank database

search_new <- wb_search("health expenditure")

search_new %>% print(n = Inf)

# Create a list of indicators of interest
my_indicators = c("gni_percapita" = "NY.GNP.PCAP.CD", #economy
                  "gdp" = "NY.GDP.MKTP.CD", #economy
                  "gdp_percapita" = "NY.GDP.PCAP.CD", #economy
                  "gdp_growth" = "NY.GDP.MKTP.KD.ZG", #economy
                  "life_expectancy_birth" = "SP.DYN.LE00.IN", #health
                  "poverty_gap" = "SI.POV.UMIC.GP", # economy
                  "infant_mortality_rate" = "SP.DYN.IMRT.IN", #health
                  "human_development" = "UNDP.HDI.XD", #education
                  "human_capital_index" = "HD.HCI.OVRL", #education
                  "health_expenditure_per_capita" = "SH.XPD.GHED.PC.CD", #health
                  "health_expenditure_gdp" = "SH.XPD.KHEX.GD.ZS", #health
                  "gender_equality" = "5.51.01.07.gender", # education
                  "unemployment" = "SL.UEM.TOTL.ZS", # economy
                  "literacy_rate" = "UIS.LR.AG25T64", # education
                  "poverty_headcount_ratio" = "SI.POV.DDAY", # dispairty
                  "gini" = "SI.POV.GINI") #disparity

# Create a new data frame that has all of the countries based off of population data availability
country_indicators <- wb_data("SP.POP.TOTL", country = "countries_only", mrnev = 1, freq = "Y")
country_indicators <- dplyr::rename(country_indicators, population = SP.POP.TOTL)

# Remove columns that are not important for analysis
country_indicators <- country_indicators %>% select(-c(date, obs_status, footnote, last_updated))

# Add indicator information to dataframe
for (i in seq(1, length(my_indicators))){
  try({
    indicator_data <- wb_data(my_indicators[i], country = "countries_only", mrnev = 1, freq = "Y")
    country_indicators <- merge(country_indicators, indicator_data[c(3,5)], by = "country", all.x = TRUE)
  }, silent = F)
  
  Sys.sleep(1)
}

# Add region information to dataframe
wb_countries <- wb_countries()
country_indicators <- merge(country_indicators, wb_countries[c("country","region")], by = "country", all.x = TRUE)

##### Gather information from COVID-19 API

# Get confirmed COVID-19 for all countries
confirmed_cases <- GET('https://covid19-stats-api.herokuapp.com/api/v1/cases/country/confirmed')
confirmed_text <- content(confirmed_cases, "text")
confirmed_json <- fromJSON(confirmed_text, flatten = T) 
confirmed_df <- as.data.frame(confirmed_json)

# Rename counts to confirmed
confirmed_df <- dplyr::rename(confirmed_df, confirmed = count)

# Retrieve iso3c codes and add as new column
confirmed_df <- dplyr::mutate(confirmed_df, iso3c = countrycode::countryname(confirmed_df$country, destination = "iso3c"))
confirmed_df["iso3c"][confirmed_df["country"] == "Micronesia"] <- "FSM"
confirmed_df <- dplyr::select(confirmed_df, -country)

# Get deaths due to COVID-19 for all countries

deaths_cases <- GET('https://covid19-stats-api.herokuapp.com/api/v1/cases/country/deaths')
deaths_text <- content(deaths_cases, "text")
deaths_json <- fromJSON(deaths_text, flatten = T) 
deaths_df <- as.data.frame(deaths_json)

# Rename counts to deaths 
deaths_df <- dplyr::rename(deaths_df, deaths = count)

# Retrieve iso3c codes and add as new column
deaths_df <- dplyr::mutate(deaths_df, iso3c = countrycode::countryname(deaths_df$country, destination = "iso3c"))
deaths_df["iso3c"][deaths_df["country"] == "Micronesia"] <- "FSM"
deaths_df <- dplyr::select(deaths_df, -country)

# Get recovered due to COVID-19 for all countries

recovered_cases <- GET('https://covid19-stats-api.herokuapp.com/api/v1/cases/country/recovered')
recovered_text <- content(recovered_cases, "text")
recovered_json <- fromJSON(recovered_text, flatten = T) 
recovered_df <- as.data.frame(recovered_json)

# Rename counts to recovered 
recovered_df <- dplyr::rename(recovered_df, recovered = count)

# Retrieve iso3c codes and add as new column
recovered_df <- dplyr::mutate(recovered_df, iso3c = countrycode::countryname(recovered_df$country, destination = "iso3c"))
recovered_df["iso3c"][recovered_df["country"] == "Micronesia"] <- "FSM"
recovered_df <- dplyr::select(recovered_df, -country)

##### Merge COVID and WorldBank data based on iso3c
country_info <- merge(country_indicators, confirmed_df, by = "iso3c", all.x = T)
country_info <- merge(country_info, deaths_df, by = "iso3c", all.x = T)
country_info <- merge(country_info, recovered_df, by = "iso3c", all.x = T)

##### Calculate morbidity and mortality rates
country_info <- country_info %>%
  # morbidity rate per 100 000 individuals
  mutate(morbidity_rate = round((confirmed / population) * 100000, 2)) %>%
  # mortality rate per 100 000 individuals
  mutate(mortality_rate = round((deaths / population) * 100000, 2)) %>%
  # recovery rate per 100 000 individuals
  mutate(recovery_rate = round((recovered / population) * 100000, 2))
  

##### Calculate ratios of confirmed, deaths, and recovered cases
country_info <- country_info %>%
  # confirmed ratio
  mutate(case_ratio = confirmed / population) %>%
  # deaths ratio
  mutate(deaths_ratio = deaths / population) %>%
  # recovered ratio
  mutate(recovered_ratio = recovered / population)


saveRDS(country_info, file = "country.info.RDS")



