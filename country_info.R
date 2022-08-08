library(dplyr)
library(httr)
library(jsonlite)
library(wbstats)
library(countrycode)

##### Extract data from the worldbank database

new_cache <- wb_cache()

search_new <- wb_search("population")

search_new %>% print(n = Inf)

# Create a list of indicators
my_indicators = c("gni_percapita" = "NY.GNP.PCAP.CD", #gross national income
                  "gdp" = "NY.GDP.MKTP.CD",
                  "gdp_percapita" = "NY.GDP.PCAP.CD",
                  "gdp_growth" = "NY.GDP.MKTP.KD.ZG",
                  "life_expectancy_birth" = "SP.DYN.LE00.IN",
                  "infant_mortality_rate" = "SP.DYN.IMRT.IN",
                  "human_development" = "UNDP.HDI.XD",
                  "human_capital_index" = "HD.HCI.OVRL",
                  "gender_equality" = "5.51.01.07.gender", # ratio of females to males that are educated
                  "unemployment" = "SL.UEM.TOTL.ZS",
                  # "poverty_gap" = "SI.POV.NGAP", # gives 400 error may have to find another indicator
                  # "poverty_rate" = "SI.POV.NAPR.ZS", # gives 400 error may have to find another indicator
                  # "literacy_rate" = "IN.POV.LIT.RAT.TOTL", # gives 400 error may have to find another indicator
                  "gini" = "SI.POV.GINI")

# Create a new data frame that has all of the countries based off of population data availability
country_indicators <- wb_data("SP.POP.TOTL", country = "countries_only", mrnev = 1, freq = "Y")
country_indicators <- dplyr::rename(country_indicators, population = SP.POP.TOTL)

# Remove columns that are not important for analysis
country_indicators <- country_indicators %>% select(-c(date, obs_status, footnote, last_updated))

# Add indicator information to dataframe
for (i in seq(1, length(my_indicators))){
  indicator_data <- wb_data(my_indicators[i], country = "countries_only", mrnev = 1, freq = "Y")
  country_indicators <- merge(country_indicators, indicator_data[c(3,5)], by = "country", all.x = TRUE)
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
deaths_df <- dplyr::select(deaths_df, -country)


# Renme counts to deaths 
deaths_df <- dplyr::rename(deaths_df, deaths = count)

# Retrieve iso3c codes and add as new column
deaths_df <- dplyr::mutate(deaths_df, iso3c = countrycode::countryname(deaths_df$country, destination = "iso3c"))
deaths_df["iso3c"][deaths_df["country"] == "Micronesia"] <- "FSM"

##### Merge COVID and WorldBank data based on iso3c
country_info <- merge(country_indicators, confirmed_df, by = "iso3c", all.x = T)
country_info <- merge(country_info, deaths_df, by = "iso3c", all.x = T)

##### Calculate morbidity and mortality rate
country_info <- country_info %>%
  #morbidity rate per 100 000 individuals
  mutate(morbidity_rate = round((confirmed / population) * 100000, 2)) %>%
  #mortality rate per 100 000 individuals
  mutate(mortality_rate = round((deaths / population) * 100000, 2))
                





