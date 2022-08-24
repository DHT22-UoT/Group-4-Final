# Group 4: COVID-19 Dashboard

This dashboard provides a summary of COVID-19 cases from 2020 globally and potential relationships with various socioeconomic factors. 

# Data Sources:

[**World Bank (wbstats)**:](https://datahelpdesk.worldbank.org/) Used to access information on economic, regional, socioeconomic, and inequity indicators for each country.

[**COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University**:](https://github.com/CSSEGISandData/COVID-19) Used to access COVID-19 information of various countries from 2020 including total cases, deaths, and recoveries.

# Branches:

## API (Imran)
 * Collected general and indicator information of countries using wbstats package
 * Collected COVID-19 case information for all countries using [COVID-19 stats API](https://documenter.getpostman.com/view/5352730/SzYbyxR5)
 * **main files:**
   * country_info.R: all data collection and retrieval from APIs
   * country.info.RDS: dataframe containing all information retrieved from APIs

## anova_analysis_ (Selena)
 * Drew and compared boxplots of the quantitative variable mortality rate for each category of Life Expectancy at Birth, GDP Growth Rate, and Human Capital Index (we initially selected these 3 indicators).
 * Generated descriptive stats for the data.
 * Determined whether the mean mortality rate between the different country categories based on the three selected indicators is different.
 * Used Post-hoc test - Tukey’s test to determine which groups were different if the null hypothesis of ANOVA was rejected.
 * Plotted the Tukey's test result using 95% confidence intervals, showing significant results in red.
 * Also used a bar plot with standard error to show the group information and alternatively show the Tukey test result better, where the height of each bar is proportional to the mean of each group.
 * Determined that the Life Expectancy at Birth was the best indicator and stored data into the cld3.RDS and dt3.RDS files.
 * **main files:**
   * anova_analysis.R
   * cld3.RDS
   * dt3.RDS
   

