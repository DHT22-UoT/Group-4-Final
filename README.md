# Group 4: COVID-19 Dashboard

This dashboard provides a summary of COVID-19 cases from 2020 globally and potential relationships with various socioeconomic factors. 

# Data Sources:

[**World Bank (wbstats)**:](https://datahelpdesk.worldbank.org/) Used to access information on economic, regional, socioeconomic, and inequity indicators for each country.

[**COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University**:](https://github.com/CSSEGISandData/COVID-19) Used to access COVID-19 information of various countries from 2020 including total cases, deaths, and recoveries.

# Branches:

## API (Imran)
 * Collected general and indicator information of countries using wbstats package
 * Collected COVID-19 case information for all countries using [COVID-19 stats API](https://documenter.getpostman.com/view/5352730/SzYbyxR5)
 * **Main files:**
   * country_info.R: all data collection and retrieval from APIs
   * country.info.RDS: dataframe containing all information retrieved from APIs

## mortality_cor (Olivia)
  * Completed correlation analysis of various indicators and mortality rate, morbidity rate, and confirmed case ratios.
  * Plotted strongest indicators on scatterplots and plotted regression lines to create a visual of potential linear relationships of indicators with mortality rate.
  * Created categories or "classes" of various indicator levels by calculating mean mortality rates of countries determined to be in each category and created box and bar plots to create visual of potential relationships with mortality rate.
  * Chose top 3 indicators (high life expectancy at birth, GDP growth rate and human capital index) based on relationships seen in correlation plots, scatterplots, boxplots and barplots. 
  * **Main files:**
    * mortality_cor.R
    * life.expect.df.2.RDS
    * life.expect.2.RDS
    * gdp.growth.df.2.RDS
    * human.cap.df.2.RDS

## anova_analysis_ (Selena)
 * Drew and compared boxplots of the quantitative variable mortality rate for each category of Life Expectancy at Birth, GDP Growth Rate, and Human Capital Index (we initially selected these 3 indicators).
 * Generated descriptive stats for the data.
 * Determined whether the mean mortality rate between the different country categories based on the three selected indicators was different.
 * Used Post-hoc test - Tukey’s test to determine which groups were different if the null hypothesis of ANOVA was rejected.
 * Plotted the Tukey's test result using 95% confidence intervals, showing significant results in red.
 * Also used a bar plot with standard error to show the group information and alternatively show the Tukey test result better, where the height of each bar was proportional to the mean of each group.
 * Determined that the Life Expectancy at Birth was the best indicator and stored data into the cld3.RDS and dt3.RDS files for the dashboard.
 * **Main files:**
   * anova_analysis.R
   * cld3.RDS
   * dt3.RDS
   
## dashboard (Danni)
 * Formatting & Structuring dashboard
 * Created interactive maps for the visualisation of COVID-19 cases
 * **Tab contents:**
   * Overview: Danni
   * Regional data: Imran 
   * Life Expectancy data: Olivia & Selena
   * About: Danni
   * Citation: Danni

