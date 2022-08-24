# Group 4: COVID-19 Dashboard

This dashboard provides a summary of COVID-19 cases from 2020 globally and potential relationships with various socioeconomic factors. 

# Data Sources:

[**World Bank (wbstats)**:](https://datahelpdesk.worldbank.org/) Used to access information on economic, regional, socioeconomic, and inequity indicators for each country.

[**COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University**:](https://github.com/CSSEGISandData/COVID-19) Used to access COVID-19 information of various countries from 2020 including total cases, deaths, and recoveries.

# Branches:

## API (Imran)
 * Collected general and indicator information of countries using wbstats package
 * Collected COVID-19 case information for all countries using [COVID-19 stats API](https://documenter.getpostman.com/view/5352730/SzYbyxR5)
 * **main files:****
   *country_info.R: all data collection and retrieval from APIs
   *country.info.RDS: dataframe containing all information retrieved from APIs


