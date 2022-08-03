library(httr)
library(jsonlite)
library(devtools)
library(dplyr)

#COVID-19 API (India)
covid_url <- "https://covid19-stats-api.herokuapp.com/api/v1/cases?country=India"
covid_cases <- GET(url = covid_url)
covid_text <- content(covid_cases, "text")
covid_json <- fromJSON(covid_text, flatten = T)


#socio-economic data (below is only for India)
str_row <- 1
end_row <- 20
base <- "http://api.worldbank.org/"
endpoint <- "v2/country/ind?" #? marks the end of an endpoint
query <- "format=json" #maybe midify here like (change indicators): expr=heart+attack&fields=NCTId,Condition&fmt=JSON
row_cnt <- paste0("&min_rnk=",str_row,"&max_rnk=", end_row)
whole_thing <- paste0(base,endpoint,query,row_cnt)
trial <- GET(paste0(base,endpoint,query))
trial_text <- content(trial, "text")
trial_json <- fromJSON(trial_text, flatten = T)
#get_trials_df <- as.data.frame(get_trials_json$StudyFieldsResponse$StudyFields)



