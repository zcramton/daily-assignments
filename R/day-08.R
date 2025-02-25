# Zachary Cramton
# 20 Feburary 2025
# ESS 330 - Daily Assignment 08
# This is the R script for assignment 08 where the COVID-19 data from 
# assignment 06 will be visualized expanding on the visualization from
# assignment 07.

# Prepare packages
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(datasets)

#Read in and story NY-Times Data
covid <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

# Create state-region data frame
df <- data.frame(region = state.region,
                 abbr = state.abb,
                 state = state.name)

# Check data frame
head(df)

# Join df to covid data
covid_joined <- inner_join(df,covid, by = "state")

# Aggregate (Split-apply) COVID-19 data based on region.
covid_summary <- covid_joined %>% 
  group_by(region, date) %>% #Split by region and date
  summarize(
    Cases = sum(cases, na.rm = TRUE), #Apply sum function
    Deaths = sum(deaths, na.rm = TRUE)
  )
  
# Pivot data from to long format
covid_long <- covid_summary %>% 
  pivot_longer(cols = c(Cases, Deaths), 
              names_to = "metric", 
              values_to = "count")

# Plot the long data
us_regional_covid_trends = ggplot(covid_long, aes(x = date, y = count, color = region)) +
  geom_line() +
  facet_grid(metric~region, scales = "free_y",) +
  labs(title = "COVID-19 Trends by Region",
       caption = "Based on NY-Times COVID-19 Data.",
       x = "Date",
       y = "Cumulative Count",
       color = "Regions") +
  theme_bw() + 
  theme(legend.position = "none") + 
  scale_x_date(date_breaks = "8 month", date_labels = "%b %y")
  
# Save plot as an image
ggsave(us_regional_covid_trends,
      file = "images/us_regional_covid_trends.png",
      width = 10,
      height = 6)

      