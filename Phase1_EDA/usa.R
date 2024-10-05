library(dplyr)
library(ggplot2)
library(maps)
library(sf)
library(tidyverse)
library(usmap) # Assuming this package is already installed
library(readr)

# Load the dataset
covid_data <- read_csv("C:/Users/saisa/Downloads/COVID-19_cases_plus_census.csv")

# Aggregate confirmed cases, deaths, and population by state
state_summary <- covid_data %>%
  filter(confirmed_cases > 100) %>%
  group_by(state) %>%
  summarise(total_confirmed_cases = sum(confirmed_cases, na.rm = TRUE),
            total_deaths = sum(deaths, na.rm = TRUE),
            total_population = sum(total_pop, na.rm = TRUE)) %>%
  ungroup() # remove grouping

# Normalize the data by population per 1000
state_summary <- state_summary %>%
  mutate(cases_per_1000 = (total_confirmed_cases / total_population) * 1000,
         deaths_per_1000 = (total_deaths / total_population) * 1000)

# View the summarized and normalized data
print(state_summary)

# Top 10 most affected states based on normalized confirmed cases
top_10_most_affected <- state_summary %>%
  arrange(desc(cases_per_1000)) %>%
  slice_head(n = 10)
print(top_10_most_affected)

# Top 10 least affected states based on normalized confirmed cases
top_10_least_affected <- state_summary %>%
  arrange(cases_per_1000) %>%
  slice_head(n = 10)
print(top_10_least_affected)


# For mapping, we'll use the normalized data

# Using 'usmap' for plotting as it seems more straightforward
plot_usmap(data = state_summary, values = "cases_per_1000", lines = "black") +
  scale_fill_continuous(low = "lightblue", high = "darkred", name = "Cases per 1000", label = scales::comma) +
  theme(legend.position = "right") +
  labs(title = "COVID-19 Confirmed Cases per 1000 by State")