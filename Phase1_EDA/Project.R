library(tidyverse)
library(ggplot2)
library(pacman)
library(dplyr)
library(scales)
library(tidyr)
library(naniar)

data <- read.csv("COVID-19_cases_plus_census.csv")
summary(data)
str(data)
head(data)


#checking for missing values
colSums(is.na(data)) #observation is that there is little to no missing data in the csv file

missing_dates <- sum(is.na(data$date))

# Check for missing values in the confirmed_cases column
missing_cases <- sum(is.na(data$confirmed_cases))

# Print the results
print(paste("Missing dates:", missing_dates))
print(paste("Missing confirmed cases:", missing_cases))

#we can start by visualizing the code to better understand the data

trend_data <- ggplot(data, aes(x = date, y = confirmed_cases)) +
  geom_line(color = "blue") +
  labs(title = "Trend of COVID-19 Cases Over Time",
       x = "Date",
       y = "Confirmed Cases")

# Display the plot
print(trend_data)

#We observe that all of the data is from a singluar date that is (01/19/2021)
#Probably because the census data was conducted and reported on that day

age_data <- ggplot(data, aes(x = median_age)) +
  geom_histogram(fill = "orange", color = "black", bins = 30) +
  labs(title = "Distribution of Median Age",
       x = "Median Age",
       y = "Frequency")
age_data

#The distribution of age for the census data is as follows (Normal distribution with the age of 40 being the peak)

#Geographical distributions by state
state_cases <- ggplot(data, aes(x = state, y = confirmed_cases)) +
  geom_bar(stat = "summary", fun = "sum", fill = "red") +
  labs(title = "COVID-19 Cases by State",
       x = "State",
       y = "Total Confirmed Cases") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = comma)
state_cases

#By visualization we can observe that California has the highest number of covid-19 cases (around 3million)
#And Hawaii, Arkansas and alaska being the lowest

max_cases <- max(data$confirmed_cases)
min_cases <- min(data$confirmed_cases)
state_max_cases <- subset(data, confirmed_cases == max_cases)$state
state_min_cases <- subset(data, confirmed_cases == min_cases)$state
state_max_cases
state_min_cases

#We can confirm this by printing the numeric data as well


#We also have to do visualization on age groups and the confirmed cases for each of the age groups
#We can now start visualizing the data for people data (Socioeconomic, Race, State)

#First let us start by visualizing deaths across the country
ggplot(data, aes(x = state, y = deaths)) +
  geom_bar(stat = "summary", fun = "sum", fill = "red") +
  labs(title = "COVID-19 Deaths by State",
       x = "State",
       y = "Total Deaths") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

# Display the maximum and minimum deaths in text
max_deaths <- max(data$deaths)
min_deaths <- min(data$deaths)
state_max_deaths <- subset(data, deaths == max_deaths)$state
state_min_deaths <- subset(data, deaths == min_deaths)$state
paste("State with Maximum Deaths:", state_max_deaths, "-", max_deaths)
paste("State with Minimum Deaths:", state_min_deaths, "-", min_deaths)

#For Minimimum deaths I feel there is missing data or outliers which needs to be dealt with
#gg_miss_var(data)

#let us visualize the total population
#Since the columns names are divided for age groups 1 and over and 3 and over 
#we only consider for age groups 1 and over
population_plot <- ggplot(data, aes(x = state, y = population_1_year_and_over)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Population by State",
       x = "State",
       y = "Population") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(labels = scales::comma)

print(population_plot)

#Let us visualize the male and female population in our dataset
male_female_population <- data.frame(
  state = data$state,
  male_population = data$male_pop,
  female_population = data$female_pop
)

# Reshape the data into long format
male_female_population <- pivot_longer(male_female_population, 
                                       cols = c(male_population, female_population), 
                                       names_to = "gender", 
                                       values_to = "population")

# Plot the male and female population
male_female_plot <- ggplot(male_female_population, aes(x = state, y = population, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Male and Female Population by State",
       x = "State",
       y = "Population",
       fill = "Gender") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::comma)

print(male_female_plot)


data2 <- read.csv("COVID-19_cases_TX.csv")
summary_data2 <- summary(data2[c("date", "confirmed_cases", "deaths")])
print(summary_data2)
range_values2 <- apply(data2[, c("confirmed_cases", "deaths")], 2, range)
print(range_values2)
