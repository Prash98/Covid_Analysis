library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)
library(tidyr)
library(ggplot2)
library(reshape2)


data <-  read.csv("COVID-19_cases_plus_census.csv")
colnames(data)


selected_columns = c("county_name", "state", "date", "confirmed_cases", 
                     "deaths", "total_pop", "male_pop", "female_pop", "median_age", 
                     "commuters_by_public_transportation", "median_income", "income_per_capita", "median_rent", 
                     "aggregate_travel_time_to_work", "income_less_10000", "income_10000_14999", "income_15000_19999", 
                     "income_20000_24999", "income_25000_29999", "income_30000_34999", "income_35000_39999", 
                     "income_40000_44999", "income_45000_49999", "income_50000_59999", "income_60000_74999", 
                     "income_75000_99999", "income_100000_124999", "income_125000_149999", "income_150000_199999", 
                     "income_200000_or_more", "male_under_5", "male_5_to_9", "male_10_to_14", "male_15_to_17", 
                     "male_18_to_19", "male_20", "male_21", "male_22_to_24", "male_25_to_29", "male_30_to_34", 
                     "male_35_to_39", "male_40_to_44", "male_45_to_49", "male_50_to_54", "male_55_to_59", 
                     "male_60_61", "male_62_64", "male_65_to_66", "male_67_to_69", "male_70_to_74", "male_75_to_79", 
                     "male_80_to_84", "male_85_and_over", "female_under_5", "female_5_to_9", "female_10_to_14", 
                     "female_15_to_17", "female_18_to_19", "female_20", "female_21", "female_22_to_24", 
                     "female_25_to_29", "female_30_to_34", "female_35_to_39", "female_40_to_44", "female_45_to_49", 
                     "female_50_to_54", "female_55_to_59", "female_60_to_61", "female_62_to_64", "female_65_to_66", 
                     "female_67_to_69", "female_70_to_74", "female_75_to_79", "female_80_to_84", "female_85_and_over", 
                     "commute_5_9_mins", "commute_35_39_mins", "commute_40_44_mins", "commute_60_89_mins", 
                     "commute_90_more_mins", "male_45_to_64", "no_car", "no_cars", "not_us_citizen_pop", 
                     "one_car", "two_cars", "three_cars", "poverty", 
                     "walked_to_work", "worked_at_home", "commute_10_14_mins","commute_15_19_mins","commute_20_24_mins" ,"commute_25_29_mins"  , "commute_30_34_mins" ,
                     "commute_45_59_mins", "commute_5_9_mins","commute_35_39_mins"  , "commute_40_44_mins"  ,"commute_60_89_mins"  , "commute_90_more_mins", 
                     "commute_35_44_mins","commute_60_more_mins" ,"commute_less_10_mins", "commuters_by_bus","commuters_by_car_truck_van","commuters_by_carpool",	"commuters_by_subway_or_elevated","commuters_drove_alone",
                     "employed_agriculture_forestry_fishing_hunting_mining",	"employed_arts_entertainment_recreation_accommodation_food",	"employed_construction",	
                     "employed_education_health_social",	"employed_finance_insurance_real_estate",	"employed_information",	"employed_manufacturing",	"employed_other_services_not_public_admin",	
                     "employed_public_administration",	"employed_retail_trade"	,"employed_science_management_admin_waste","employed_transportation_warehousing_utilities","employed_wholesale_trade",
                     "households_retirement_income","employed_pop",	"unemployed_pop",	"not_in_labor_force",	"pop_16_over",	"pop_in_labor_force",
                     "four_more_cars",	"gini_index", "children")

selected_df <- data[selected_columns]

# Check for missing values
missing_values <- sapply(selected_df, function(x) sum(is.na(x)))
missing_values
cols_to_be_cleaned <- c('aggregate_travel_time_to_work', 'median_rent')
column_means <- colMeans(selected_df[cols_to_be_cleaned], na.rm = TRUE)
cols_with_missing <- colnames(selected_df)[colSums(is.na(selected_df)) > 0]

# Replace missing values with column means
for (col in cols_with_missing) {
  selected_df[is.na(selected_df[, col]), col] <- column_means[col]
}


# Check data types
data_types <- sapply(data, class)
data_types
#Change all char to factor
selected_df <- selected_df %>% mutate_if(is.character, factor)

# Summary statistics for numeric columns
numeric_summary <- summary(data[, sapply(data, is.numeric)])
numeric_summary

# Check for outliers (e.g., using box plots)
boxplot(data$confirmed_cases, main="Confirmed Cases")
boxplot(data$deaths, main="Deaths")

boxplot(data$confirmed_cases, main="Box Plot", xlab="Variable Name")

# Identify outliers
outliers <- boxplot(data$confirmed_cases, plot=FALSE)$out


# Define percentile-based thresholds
lower_threshold <- quantile(data$confirmed_cases, 0.25)
upper_threshold <- quantile(data$confirmed_cases, 0.75)

# Winsorize COVID cases data
data$COVID_cases_winsorized <- ifelse(data$confirmed_cases < lower_threshold, lower_threshold,
                                      ifelse(data$COVID_cases > upper_threshold, upper_threshold,
                                             data$COVID_cases))

# Print outliers

length(data)
print(length(outliers))


# Check for duplicates
duplicates <- data[duplicated(selected_df), ]
duplicates 

# Check for consistency between related columns
consistent_pop <- sum(data$male_pop + data$female_pop == data$total_pop)
consistent_pop
# Check for valid values
# For example, ensure median_age is non-negative

numeric_cols <- sapply(selected_df, is.numeric)

# Check for negative values in numeric columns
negative_values <- sapply(selected_df[, numeric_cols], function(x) any(x < 0))

# Find columns with negative values
cols_with_negative_values <- names(negative_values)[negative_values]

if (length(cols_with_negative_values) > 0) {
  print(paste("Columns with negative values:", paste(cols_with_negative_values, collapse = ", ")))
} else {
  print("No numeric columns have negative values.")
}



#Merging commuting times
selected_df <- selected_df %>%
  mutate(
    less_than_20_mins = commute_5_9_mins + commute_10_14_mins + commute_15_19_mins,
    between_20_and_44_mins = commute_20_24_mins + commute_25_29_mins + commute_30_34_mins + commute_35_39_mins + commute_40_44_mins,
    between_45_and_89_mins = commute_45_59_mins + commute_60_89_mins + commute_35_44_mins + commute_60_more_mins,
    more_than_90_mins = commute_90_more_mins
  ) %>%
  select(-starts_with("commute_"))
colnames(selected_df)

#merging Male Age group
selected_df$male_0_17 <- rowSums(selected_df[, c("male_under_5", "male_5_to_9", 
                                                 "male_10_to_14", "male_15_to_17")])

selected_df$male_18_44 <- rowSums(selected_df[, c("male_18_to_19", "male_20", 
                                                  "male_21", "male_22_to_24", 
                                                  "male_25_to_29", "male_30_to_34", 
                                                  "male_35_to_39", "male_40_to_44")])

selected_df$male_45_64 <- rowSums(selected_df[, c("male_45_to_49", "male_50_to_54", 
                                                  "male_55_to_59", "male_60_61", 
                                                  "male_62_64")])

selected_df$male_65_and_over <- rowSums(selected_df[, c("male_65_to_66", "male_67_to_69", 
                                                        "male_70_to_74", "male_75_to_79", 
                                                        "male_80_to_84", "male_85_and_over")])

#drop the original columns
selected_df <- selected_df[, !(names(selected_df) %in% c("male_under_5", "male_5_to_9", 
                                                         "male_10_to_14", "male_15_to_17",
                                                         "male_18_to_19", "male_20", 
                                                         "male_21", "male_22_to_24",
                                                         "male_25_to_29", "male_30_to_34", 
                                                         "male_35_to_39", "male_40_to_44", 
                                                         "male_45_to_49", "male_50_to_54", 
                                                         "male_55_to_59", "male_60_61", 
                                                         "male_62_64", "male_65_to_66", 
                                                         "male_67_to_69", "male_70_to_74", 
                                                         "male_75_to_79", "male_80_to_84", 
                                                         "male_85_and_over"))]

colnames(selected_df)

# Merge female age group columns

# Sum the counts of females in each age group and store the result in the corresponding columns
selected_df$female_0_17 <- rowSums(selected_df[, c("female_under_5", "female_5_to_9", 
                                                   "female_10_to_14", "female_15_to_17")])

selected_df$female_18_44 <- rowSums(selected_df[, c("female_18_to_19", "female_20", 
                                                    "female_21", "female_22_to_24", 
                                                    "female_25_to_29", "female_30_to_34", 
                                                    "female_35_to_39", "female_40_to_44")])

selected_df$female_45_64 <- rowSums(selected_df[, c("female_45_to_49", "female_50_to_54", 
                                                    "female_55_to_59", "female_60_to_61", 
                                                    "female_62_to_64")])

selected_df$female_65_and_over <- rowSums(selected_df[, c("female_65_to_66", "female_67_to_69", 
                                                          "female_70_to_74", "female_75_to_79", 
                                                          "female_80_to_84", "female_85_and_over")])

# Now you can remove the original age group columns
selected_df <- selected_df[, !(names(selected_df) %in% c("female_under_5", "female_5_to_9", 
                                                         "female_10_to_14", "female_15_to_17",
                                                         "female_18_to_19", "female_20", 
                                                         "female_21", "female_22_to_24",
                                                         "female_25_to_29", "female_30_to_34", 
                                                         "female_35_to_39", "female_40_to_44", 
                                                         "female_45_to_49", "female_50_to_54", 
                                                         "female_55_to_59", "female_60_to_61", 
                                                         "female_62_to_64", "female_65_to_66", 
                                                         "female_67_to_69", "female_70_to_74", 
                                                         "female_75_to_79", "female_80_to_84", 
                                                         "female_85_and_over"))]

colnames(selected_df)

#Merge income columns 
# Create new columns by summing up the corresponding income ranges
selected_df$income_less_25000 <- rowSums(selected_df[, c("income_less_10000", "income_10000_14999", "income_15000_19999", "income_20000_24999")])
selected_df$income_25000_49999 <- rowSums(selected_df[, c("income_25000_29999", "income_30000_34999", "income_35000_39999", "income_40000_44999", "income_45000_49999")])
selected_df$income_50000_99999 <- rowSums(selected_df[, c("income_50000_59999", "income_60000_74999", "income_75000_99999")])
selected_df$income_100000_199999 <- rowSums(selected_df[, c("income_100000_124999", "income_125000_149999", "income_150000_199999")])
selected_df$income_200000_or_more <- selected_df$income_200000_or_more
colnames(selected_df)
# Remove the original income columns
selected_df <- selected_df[, !(names(selected_df) %in% c("income_less_10000", "income_10000_14999", "income_15000_19999", 
                                                         "income_20000_24999", "income_25000_29999", "income_30000_34999",
                                                         "income_35000_39999", "income_40000_44999", "income_45000_49999",
                                                         "income_50000_59999", "income_60000_74999", "income_75000_99999",
                                                         "income_100000_124999", "income_125000_149999", "income_150000_199999"
))]

colnames(selected_df)


#checking for correlation
columns <- c("county_name","state","confirmed_cases", "deaths", "total_pop", "male_pop", "female_pop", "median_age", "median_income", "income_per_capita", "median_rent", "aggregate_travel_time_to_work", "income_200000_or_more", "male_45_to_64", "not_us_citizen_pop", "poverty", "walked_to_work", "worked_at_home", "commuters_by_bus", "commuters_by_car_truck_van", "commuters_by_carpool", "commuters_by_subway_or_elevated", "commuters_drove_alone", "employed_agriculture_forestry_fishing_hunting_mining", "employed_arts_entertainment_recreation_accommodation_food", "employed_construction", "employed_education_health_social", "employed_finance_insurance_real_estate", "employed_information", "employed_manufacturing", "employed_other_services_not_public_admin", "employed_public_administration", "employed_retail_trade", "employed_science_management_admin_waste", "employed_transportation_warehousing_utilities", "employed_wholesale_trade", "households_retirement_income", "employed_pop", "unemployed_pop", "children", "less_than_20_mins", "between_20_and_44_mins", "between_45_and_89_mins", "more_than_90_mins", "male_0_17", "male_18_44", "male_45_64", "male_65_and_over", "female_0_17", "female_18_44", "female_45_64", "female_65_and_over", "income_less_25000", "income_25000_49999", "income_50000_99999", "income_100000_199999"
)
corr_cols <- selected_df[columns]
corr_cols <- corr_cols %>%
  filter(state == "TX")
corr_cols

corr_cols <- corr_cols %>%
  mutate(
    #state_encoded = factor(state, levels = unique(state), ordered = FALSE),
    county_encoded = factor(county_name, levels = unique(county_name), ordered = FALSE)
  ) %>%
  mutate_at(vars(county_encoded), as.numeric)  # Convert to numeric

colnames(corr_cols)
corr_cols

scaled_df <- scale(corr_cols[, -c(1:2)])
colnames(scaled_df)

# Remove constant columns

# Perform PCA
pca_result <- prcomp(scaled_df, center = TRUE, scale. = TRUE)
pca_result
# Summarize PCA results
summary(pca_result)

plot(cumsum(pca_result$sdev^2) / sum(pca_result$sdev^2), type = "b", xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained")

# Scree plot
screeplot(pca_result, type = "line", main = "Scree Plot")
biplot(pca_result)

#Select the Number of dimensions

pca_result <- prcomp(scaled_df, center = TRUE, scale. = TRUE, rank. = 10)  # Specify 'rank.' parameter to retain 2 components
biplot(pca_result)
reduced_dimensions <- predict(pca_result, scaled_df)
head(reduced_dimensions)


#Coorelation between the selected set of featues

filter_df <- selected_df[columns]
colnames(filter_df)


correlation_matrix <- cor(filter_df[, c(3:56)])

#Visualize the correlation matrix
correlation_df <- melt(correlation_matrix)

# Plot the heatmap with larger dimensions
ggplot(correlation_df, aes(Var1, Var2, fill=value)) +
  geom_tile(color="white") +
  scale_fill_gradient2(low="blue", high="red", mid="white", 
                       midpoint=0, limit=c(-1,1), space="Lab", 
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(size = 20),  # Increase title size
        axis.title = element_text(size = 15),  # Increase axis label size
        legend.text = element_text(size = 12)) +  # Increase legend text size
  coord_fixed() +
  labs(title = "Correlation Matrix",  # Add title
       x = "Variables",  # Add x-axis label
       y = "Variables")  # Add y-axis label



colnames(filter_df)
head(filter_df)

tx_fil <- filter_df %>%
  filter(state=='TX')
head(tx_fil)


write.csv(selected_df, 'Wokring_Version_v2.csv') #Saved a working version for reference


#Compare Texas with best and worst performing states

state_comparison_data_cols <- c("county_name", "state", "confirmed_cases", "deaths", "total_pop")
state_comparison_data <- selected_df[state_comparison_data_cols]
state_comparison_data <- state_comparison_data %>% filter(confirmed_cases > 100)


state_comparison_data <- state_comparison_data %>%
  group_by(state) %>%
  mutate(cases_per_1000_population = (confirmed_cases / total_pop) * 1000)

state_comparison_data  <- state_comparison_data %>%
  filter(state != "TX")
state_comparison_data

best_states <- state_comparison_data %>%
  group_by(state) %>%
  summarise(avg_cases_per_1000 = mean(cases_per_1000_population)) %>%
  top_n(-3)

worst_states <- state_comparison_data %>%
  filter(state != "TX") %>%
  group_by(state) %>%
  summarise(avg_cases_per_1000 = mean(cases_per_1000_population)) %>%
  top_n(3)

texas_data <- selected_df %>%
  filter(state == "TX") %>%
  mutate(cases_per_1000_population = (confirmed_cases / total_pop) * 1000)
mean_cases_texas <- mean(texas_data$cases_per_1000_population)

# Calculate the mean cases per 1,000 population for other states
mean_cases_other_states <- state_comparison_data %>%
  group_by(state) %>%
  summarise(mean_cases_per_1000_population = mean(cases_per_1000_population)) %>%
  ungroup()

# Combine Texas mean with other states means
combined_means <- rbind(data.frame(state = "Texas", mean_cases_per_1000_population = mean_cases_texas),
                        mean_cases_other_states)

ggplot() +
  geom_col(data = combined_means, aes(x = state, y = mean_cases_per_1000_population, fill = state), alpha = 0.6) +
  geom_point(data = best_states, aes(x = state, y = avg_cases_per_1000), color = "green", size = 3) +
  geom_point(data = worst_states, aes(x = state, y = avg_cases_per_1000), color = "red", size = 3) +
  labs(x = "State", y = "Mean Cases per 1,000 Population", title = "COVID-19 Cases per 1,000 Population: Texas vs Other States") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Texas" = "blue", "green", "red"))  # Assign colors to Texas, best and worst performing states


#Analysing the counties of the most and least affected states

top_states_data <- state_comparison_data %>%
  filter(state %in% c(best_states$state, worst_states$state))

top_states_data <- state_comparison_data %>%
  filter(state %in% c(best_states$state, worst_states$state))

top_states_data

top_states_data <- top_states_data %>%
  group_by(state) %>%
  mutate(rank = rank(cases_per_1000_population))

top_counties <- top_states_data %>%
  filter(rank <= 3) %>%
  arrange(state, desc(cases_per_1000_population))

bottom_counties <- top_states_data %>%
  filter(rank > n() - 3) %>%
  arrange(state, cases_per_1000_population)

top_counties
bottom_counties

combined_counties <- rbind(top_counties, bottom_counties)

# Create a bar plot
ggplot(combined_counties, aes(x = reorder(county_name, cases_per_1000_population), y = cases_per_1000_population, fill = rank <= 3)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ state, scales = "free", ncol = 2) +
  labs(x = "County", y = "Cases per 1,000 Population", fill = "Top 3 Counties") +
  scale_fill_manual(values = c("gray", "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top") +
  coord_flip()

#Analysing the Covid Texas Dataset

#time series plot for cases in Texas to idnetify the trend of cases
texas_data = read.csv('COVID-19_cases_TX.csv')
head(texas_data)
texas_data$date <- as.Date(texas_data$date, format = "%Y-%m-%d")
texas_data

#Data Quakity checks

missing_values_tex <- sapply(texas_data, function(x) sum(is.na(x)))
missing_values_tex

# Check data types
data_types <- sapply(texas_data, class)
data_types

#Change all char to factor
texas_data <- texas_data %>% mutate_if(is.character, factor)

# Summary statistics for numeric columns
numeric_summary <- summary(texas_data[, sapply(texas_data, is.numeric)])
numeric_summary

# Check for outliers (e.g., using box plots)
boxplot(texas_data$confirmed_cases, main="Confirmed Cases")

# Check for duplicates
duplicates <- data[duplicated(texas_data), ]
duplicates 


numeric_cols <- sapply(texas_data, is.numeric)

# Check for negative values in numeric columns
negative_values <- sapply(texas_data[, numeric_cols], function(x) any(x < 0))

# Find columns with negative values
cols_with_negative_values <- names(negative_values)[negative_values]

if (length(cols_with_negative_values) > 0) {
  print(paste("Columns with negative values:", paste(cols_with_negative_values, collapse = ", ")))
} else {
  print("No numeric columns have negative values.")
}

#Get the Month and Year from data
texas_data$month <- month(texas_data$date)
texas_data$year <- year(texas_data$date)

texas_data_daily <- texas_data %>%
  group_by(year, month) %>%
  mutate(daily_cases = confirmed_cases - lag(confirmed_cases, default = first(confirmed_cases)),
         daily_deaths = deaths - lag(deaths, default = first(deaths))) %>%
  ungroup() %>%
  group_by(year, month) %>%
  summarise(total_cases = sum(daily_cases, na.rm = TRUE),
            total_deaths = sum(daily_deaths, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-")))

texas_data_daily

# Create a time series plot for total cases to visualize the trend
ggplot(texas_data, aes(x = date, y = confirmed_cases)) +
  geom_line() +
  labs(x = "Month", y = "Total Confirmed Cases", title = "COVID-19 Confirmed Cases in Texas by Month") +
  theme_minimal()

#County Poupulation density comparison

colnames(selected_df)
pop_cols <- c('county_name', 'state', 'total_pop')
texas_pop <- selected_df[pop_cols]
texas_pop

texas_pop <- texas_pop %>%
  filter(state == "TX") 

county_population_data <- texas_pop %>%
  mutate(population_category = case_when(
    total_pop < 50000 ~ "Small",
    total_pop >= 50000 & total_pop < 200000 ~ "Medium",
    total_pop >= 200000 ~ "Large"
  ))

county_population_data
#texas_data_monthly

texas_data 

texas_case <- texas_data #read.csv('COVID-19_cases_TX.csv')
texas_case
colnames(texas_case)
texas_case$month <- month(texas_case$date)
texas_case$year <- year(texas_case$date)
colnames(texas_case)
texas_case

#using lag calculate the daily cases and deaths in Texas

covid_data_monthly <- texas_case %>%
  arrange(county_name, date) %>%
  group_by(county_name) %>%
  mutate(daily_cases = confirmed_cases - lag(confirmed_cases, default = first(confirmed_cases)),
         daily_deaths = deaths - lag(deaths, default = first(deaths))) %>%
  ungroup()

"deaths" %in% colnames(texas_case)

covid_data_monthly <- covid_data_monthly %>%
  group_by(county_name, year, month) %>%
  summarise(total_cases = sum(daily_cases),
            total_deaths = sum(daily_deaths),
            .groups = "drop")


#Merge to get the population across county for analysis by population groups
covid_population_data <- left_join(covid_data_monthly, county_population_data, by = "county_name")
covid_population_data

covid_population_category_data <- covid_population_data %>%
  group_by(population_category, year, month) %>%
  summarise(total_cases = sum(total_cases), total_deaths = sum(total_deaths),.groups = "drop")

covid_population_category_data


# Line plot for total cases and total deaths by population category

ggplot(covid_population_category_data, aes(x = as.Date(paste(year, month, "01", sep = "-")), group = population_category)) +
  geom_line(aes(y = total_cases, color = population_category)) +
  labs(x = "Month", y = "Total Cases", color = "Population Category",
       title = "COVID-19 Total Cases by Population Category") +
  scale_color_manual(values = c("blue", "red", "green")) +
  theme_minimal()  # Adjust the range of y-axis as needed



ggplot(covid_population_category_data, aes(x = as.Date(paste(year, month, "01", sep = "-")), group = population_category)) +
  geom_line(aes(y = total_deaths, color = population_category)) +
  labs(x = "Month", y = "Total Deaths", color = "Population Category",
       title = "COVID-19 Total Deaths by Population Category") +
  scale_color_manual(values = c("blue", "red", "green")) +
  theme_minimal()


covid_population_category_data <- covid_population_category_data[!is.na(covid_population_category_data$population_category), ]

covid_population_data <- covid_population_data %>%
  mutate(cases_per_1000_population = total_cases / (total_pop / 1000))

#Relation between cases to death for every county

# ggplot(final_data, aes(x = state, y = deaths)) +
#   geom_bar(stat = "identity", fill = "skyblue") +
#   labs(title = "Deaths per 1000 People by State",
#        x = "State",
#        y = "Aggregating Deaths per 1000 People across county") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Visualizing Commute times in the covid census data

colnames(selected_df)

commute_df_cols <- c('county_name','state','confirmed_cases','deaths',
                     'total_pop', 'less_than_20_mins','between_20_and_44_mins',
                     'between_45_and_89_mins','more_than_90_mins')

commute_df <- selected_df[commute_df_cols]
commute_df <- commute_df %>%
  filter(state == "TX") 

commute_df <- commute_df %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000, 
  deaths_per_1000 = deaths/total_pop*1000, 
  death_per_case = deaths/confirmed_cases,
  less_than_20_mins_1000 = less_than_20_mins/total_pop *1000,
  between_20_and_44_mins_1000 = between_20_and_44_mins/total_pop *1000,
  between_45_and_89_mins_1000 = between_45_and_89_mins/total_pop *1000,
  more_than_90_mins_1000 = more_than_90_mins/total_pop *1000)


commute_df

#Relation between cases and commute times - option 2
ggplot(commute_df, aes(x = cases_per_1000, y = less_than_20_mins_1000)) +
  geom_point(aes(color = "less_than_20_mins_1000"), size = 3) +
  geom_point(aes(x = cases_per_1000, y = between_20_and_44_mins_1000, color = "between_20_and_44_mins_1000"), size = 3) +
  geom_point(aes(x = cases_per_1000, y = between_45_and_89_mins_1000, color = "between_45_and_89_mins_1000"), size = 3) +
  geom_point(aes(x = cases_per_1000, y = more_than_90_mins_1000, color = "more_than_90_mins_1000"), size = 3) +
  geom_line(aes(y = deaths_per_1000), color = "black", size = 1, linetype = "solid") +
  scale_color_manual(values = c("less_than_20_mins_1000" = "blue", "between_20_and_44_mins_1000" = "red", "between_45_and_89_mins_1000" = "green", "more_than_90_mins_1000" = "orange")) +
  labs(title = "Relationship between Commute Time Groups, COVID-19 Cases, and Deaths",
       x = "COVID-19 Cases per 1000 people",
       y = "Commute Time Group / Deaths",
       color = "Commute Time Group") +
  theme_minimal() 


ggplot(commute_df, aes(x = less_than_20_mins_1000, y = cases_per_1000)) +
  geom_point(aes(color = "less_than_20_mins_1000"), size = 3) +
  geom_point(aes(x = between_20_and_44_mins_1000, y = cases_per_1000, color = "between_20_and_44_mins_1000"), size = 3) +
  geom_point(aes(x = between_45_and_89_mins_1000, y = cases_per_1000, color = "between_45_and_89_mins_1000"), size = 3) +
  geom_point(aes(x = more_than_90_mins_1000, y = cases_per_1000, color = "more_than_90_mins_1000"), size = 3) +
  geom_line(aes(x = deaths_per_1000), color = "black", size = 1, linetype = "solid") +
  scale_color_manual(values = c("less_than_20_mins_1000" = "blue", "between_20_and_44_mins_1000" = "red", "between_45_and_89_mins_1000" = "green", "more_than_90_mins_1000" = "orange")) +
  labs(title = "Relationship between Commute Time Groups and COVID-19 Cases",
       x = "Commute Time Group",
       y = "COVID-19 Cases per 1000 people",
       color = "Commute Time Group") +
  theme_minimal() 


#Relation between employment in Texas and Covid 19

employment_df_cols <- c('county_name','state','confirmed_cases','deaths',
                        'total_pop', 'employed_agriculture_forestry_fishing_hunting_mining', 'employed_arts_entertainment_recreation_accommodation_food', 'employed_construction', 'employed_education_health_social', 'employed_finance_insurance_real_estate', 'employed_information', 'employed_manufacturing', 'employed_other_services_not_public_admin', 'employed_public_administration', 'employed_retail_trade', 'employed_science_management_admin_waste', 'employed_transportation_warehousing_utilities', 
                        'employed_wholesale_trade')
employment_df <- selected_df[employment_df_cols]
employment_df <- employment_df %>%
  filter(state == "TX") 

employment_variables <- c(
  "employed_agriculture_forestry_fishing_hunting_mining",
  "employed_arts_entertainment_recreation_accommodation_food",
  "employed_construction",
  "employed_education_health_social",
  "employed_finance_insurance_real_estate",
  "employed_information",
  "employed_manufacturing",
  "employed_other_services_not_public_admin",
  "employed_public_administration",
  "employed_retail_trade",
  "employed_science_management_admin_waste",
  "employed_transportation_warehousing_utilities",
  "employed_wholesale_trade"
)


# Applying the same calculation for all employment variables
employment_df <- employment_df %>%
  mutate(
    cases_per_1000 = confirmed_cases / total_pop * 1000,
    deaths_per_1000 = deaths / total_pop * 1000,
    death_per_case = deaths / confirmed_cases,
    across(
      all_of(employment_variables),
      ~ . / total_pop * 1000,
      .names = "{.col}_per_1000"
    )
  )


colnames(employment_df)


ggplot(employment_df, aes(x = cases_per_1000)) +
  geom_point(aes(y = employed_agriculture_forestry_fishing_hunting_mining_per_1000, color = "Agriculture, Forestry, Fishing, Hunting, Mining"), size = 3) +
  geom_point(aes(y = employed_arts_entertainment_recreation_accommodation_food_per_1000, color = "Arts, Entertainment, Recreation, Accommodation, Food"), size = 3) +
  geom_point(aes(y = employed_construction_per_1000, color = "Construction"), size = 3) +
  geom_point(aes(y = employed_education_health_social_per_1000, color = "Education, Health, Social"), size = 3) +
  geom_point(aes(y = employed_finance_insurance_real_estate_per_1000, color = "Finance, Insurance, Real Estate"), size = 3) +
  geom_point(aes(y = employed_information_per_1000, color = "Information"), size = 3) +
  geom_point(aes(y = employed_manufacturing_per_1000, color = "Manufacturing"), size = 3) +
  geom_point(aes(y = employed_other_services_not_public_admin_per_1000, color = "Other Services (not Public Admin)"), size = 3) +
  geom_point(aes(y = employed_public_administration_per_1000, color = "Public Administration"), size = 3) +
  geom_point(aes(y = employed_retail_trade_per_1000, color = "Retail Trade"), size = 3) +
  geom_point(aes(y = employed_science_management_admin_waste_per_1000, color = "Science, Management, Admin, Waste"), size = 3) +
  geom_point(aes(y = employed_transportation_warehousing_utilities_per_1000, color = "Transportation, Warehousing, Utilities"), size = 3) +
  geom_point(aes(y = employed_wholesale_trade_per_1000, color = "Wholesale Trade"), size = 3) +
  labs(title = "Relationship between Employment in Various Sectors and COVID-19 Cases",
       x = "COVID-19 Cases per 1000 people",
       y = "Employment per 1000 people") +
  scale_color_manual(values = c("red", "blue", "green", "orange", "purple", "pink", "brown", "gray", "black", "cyan", "yellow", "magenta", "darkgreen", "lightblue", "lightgreen")) +
  theme_minimal()

colnames(selected_df)

#Cases effects based on age-gender
#Male

# Selecting only the relevant columns for male age groups
male_age_groups <- selected_df %>%
  filter(state == "TX") 
toto_pop <- selected_df %>%
  filter(state == "TX") 

toto_pop <- toto_pop %>% select(total_pop)
male_age_groups <- male_age_groups %>%
  select(confirmed_cases, deaths, male_0_17, male_18_44, male_45_64, male_65_and_over)



# Selecting only the relevant columns for female age groups
female_age_groups <- selected_df %>%
  filter(state == "TX") 
female_age_groups <- female_age_groups %>%
  select(confirmed_cases, deaths, female_0_17, female_18_44, female_45_64, female_65_and_over)

male_age_groups
# Divide each column by total population and multiply by 1000 to get per 1000 people
male_age_groups_1000 <- male_age_groups / (toto_pop$total_pop / 1000)
female_age_groups_1000 <- female_age_groups / (toto_pop$total_pop / 1000)

male_age_groups_1000

ggplot(male_age_groups_1000, aes(x = confirmed_cases)) +
  geom_line(aes(y = male_0_17, color = "0-17"), size = 1) +
  geom_line(aes(y = male_18_44, color = "18-44"), size = 1) +
  geom_line(aes(y = male_45_64, color = "45-64"), size = 1) +
  geom_line(aes(y = male_65_and_over, color = "65 and over"), size = 1) +
  labs(title = "Relationship between Male Age-Groups and COVID-19 Cases",
       x = "COVID-19 Cases per 1000 people",
       y = "Male Age-Group per 1000 people") +
  scale_color_manual(values = c("blue", "red", "green", "orange")) +
  theme_minimal()

#Female

ggplot(female_age_groups_1000, aes(x = confirmed_cases)) +
  geom_line(aes(y = female_0_17, color = "0-17"), size = 1) +
  geom_line(aes(y = female_18_44, color = "18-44"), size = 1) +
  geom_line(aes(y = female_45_64, color = "45-64"), size = 1) +
  geom_line(aes(y = female_65_and_over, color = "65 and over"), size = 1) +
  labs(title = "Relationship between Female Age-Groups and COVID-19 Cases",
       x = "COVID-19 Cases per 1000 people",
       y = "Female Age-Group per 1000 people") +
  scale_color_manual(values = c("blue", "red", "green", "orange")) +
  theme_minimal()

#CountyWise Cases and Deaths 

county_df_cols <- c('county_name', 'total_pop' ,'confirmed_cases','deaths')
county_tx_df <- selected_df %>%
  filter(state=="TX")
county_tx_df <- county_tx_df[county_df_cols]
county_tx_df <- county_tx_df %>%
  mutate(
    cases_per_1000 = confirmed_cases / total_pop * 1000,
    deaths_per_1000 = deaths / total_pop * 1000,
    death_per_case = deaths / confirmed_cases)
county_tx_df

library(stringr)

county_tx_df <- county_tx_df %>% mutate(county = county_name %>% str_to_lower() %>% 
                                          str_replace('\\s+county\\s*$', ''))

county_tx_df                                      
counties <- as_tibble(map_data("county"))

counties_TX <- counties %>% dplyr::filter(region == "texas") %>% rename(c(county = subregion))

counties_TX <- counties_TX %>% left_join(county_tx_df %>% 
                                           select(c(county, cases_per_1000, deaths_per_1000, death_per_case)))


ggplot(counties_TX, aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = cases_per_1000)) +
  # geom_text_repel(data = counties_TX %>% filter(complete.cases(.)) %>% group_by(county) %>% 
  #    summarize(long = mean(long), lat = mean(lat)) %>% mutate(county = str_to_title(county))) +
  coord_quickmap() + 
  scale_fill_gradient(low="yellow", high="red") +
  labs(title = "COVID-19 Cases per 1000 People", subtitle = "Only counties reporting 100+ cases")

county_tx_df


colnames(selected_df)

#Relation between income and the number of cases/
tx_income_data <- selected_df %>%
  filter(state == "TX")

tx_income_data <- tx_income_data %>%
  mutate(
    proportion_less_25000 = income_less_25000 / total_pop * 1000,
    proportion_25000_49999 = income_25000_49999 / total_pop * 1000,
    proportion_50000_99999 = income_50000_99999 / total_pop * 1000,
    proportion_100000_199999 = income_100000_199999 / total_pop * 1000,
    cases_per_capita = (confirmed_cases / total_pop) * 1000
  )

# # 3. Visualize the proportions for each income range along with cases per capita
# ggplot(tx_income_data) +
#   geom_bar(aes(x = "", y = proportion_less_25000, fill = "Less than $25,000"), stat = "identity", width = 0.5) +
#   geom_bar(aes(x = "", y = proportion_25000_49999, fill = "$25,000 - $49,999"), stat = "identity", width = 0.5) +
#   geom_bar(aes(x = "", y = proportion_50000_99999, fill = "$50,000 - $99,999"), stat = "identity", width = 0.5) +
#   geom_bar(aes(x = "", y = proportion_100000_199999, fill = "$100,000 - $199,999"), stat = "identity", width = 0.5) +
#   geom_line(aes(x = "", y = cases_per_capita, group = 1, color = "red")) +  # Line plot for cases per capita
#   coord_polar(theta = "y") +
#   labs(title = "Income Distribution per 1000 People in Texas with COVID-19 Cases per Capita",
#        fill = "Income Range",
#        color = "Cases per Capita",
#        y = "Proportion per 1000 People") +
#   theme_void() +
#   theme(legend.position = "bottom")
# 
# ggplot(tx_income_data) +
#   geom_bar(aes(x = "", y = proportion_less_25000, fill = "Less than $25,000"), stat = "identity", width = 0.5) +
#   geom_bar(aes(x = "", y = proportion_25000_49999, fill = "$25,000 - $49,999"), stat = "identity", width = 0.5) +
#   geom_bar(aes(x = "", y = proportion_50000_99999, fill = "$50,000 - $99,999"), stat = "identity", width = 0.5) +
#   geom_bar(aes(x = "", y = proportion_100000_199999, fill = "$100,000 - $199,999"), stat = "identity", width = 0.5) +
#   geom_line(aes(x = "", y = cases_per_capita, group = 1, color = "Cases per Capita")) +  # Line plot for cases per capita
#   scale_fill_manual(values = c("Less than $25,000" = "blue", 
#                                "$25,000 - $49,999" = "green",
#                                "$50,000 - $99,999" = "orange",
#                                "$100,000 - $199,999" = "gray")) +  # Assigning colors to income groups
#   coord_polar(theta = "y") +
#   labs(title = "Income Distribution per 1000 People in Texas with COVID-19 Cases per Capita",
#        fill = "Income Range",
#        color = "Cases per Capita",
#        y = "Proportion per 1000 People") +
#   theme_void() +
#   theme(legend.position = "bottom")


#Relation between Poverty and Covid cases
tx_data <- selected_df %>%
  filter(state == "TX")

tx_data <- tx_data %>%
  mutate(cases_per_1000 = (confirmed_cases / total_pop) * 1000,
         poverty_rate_per_1000 = (poverty / total_pop) * 1000)

ggplot(tx_data) +
  geom_point(aes(x = cases_per_1000, y = poverty_rate_per_1000), color = "blue", alpha = 0.6) +
  labs(title = "COVID-19 Cases per Capita vs. Poverty Rate per Capita in Texas",
       x = "COVID-19 Cases per 1000 People",
       y = "Poverty Rate per 1000 People") +
  theme_minimal()



#Getting into best and worst counties

texas_counties <- county_tx_df 

# Best performing counties
best_performing_counties <- texas_counties %>%
  top_n(3, wt = -cases_per_1000)

# Worst performing counties
worst_performing_counties <- texas_counties %>%
  top_n(3, wt = cases_per_1000)

# View the results
best_performing_counties
worst_performing_counties

library(ggplot2)

# Best performing counties
ggplot(best_performing_counties, aes(x = county, y = cases_per_1000)) +
  geom_bar(stat = "identity", fill = "green", alpha = 0.7) +
  labs(title = "Top 3 Best Performing Counties in Texas",
       x = "County", y = "Cases per 1000 People") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()

# Worst performing counties
ggplot(worst_performing_counties, aes(x = county, y = cases_per_1000)) +
  geom_bar(stat = "identity", fill = "red", alpha = 0.7) +
  labs(title = "Top 3 Worst Performing Counties in Texas",
       x = "County", y = "Cases per 1000 People") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()


#Commute mode

commute_mode_df  <- selected_df %>%
  filter(state != "TX")
commute_mode_df
commute_mode_df <- commute_mode_df[, c("walked_to_work", "worked_at_home", "commuters_by_bus",
                                       "commuters_by_car_truck_van", "commuters_by_carpool",
                                       "commuters_by_subway_or_elevated", "commuters_drove_alone",
                                       "confirmed_cases", "total_pop")]

commute_mode_df

# Calculate confirmed cases per 1000 people
commute_mode_df$confirmed_cases_per_1000 <- commute_mode_df$confirmed_cases / (commute_mode_df$total_pop / 1000)

# Calculate commuters per 1000 people for each mode of transportation
commute_mode_df$walked_to_work_per_1000 <- (commute_mode_df$walked_to_work / commute_mode_df$total_pop) * 1000
commute_mode_df$worked_at_home_per_1000 <- (commute_mode_df$worked_at_home / commute_mode_df$total_pop) * 1000
commute_mode_df$commuters_by_car_truck_van_per_1000 <- (commute_mode_df$commuters_by_car_truck_van / commute_mode_df$total_pop) * 1000
commute_mode_df$commuters_by_carpool_per_1000 <- (commute_mode_df$commuters_by_carpool / commute_mode_df$total_pop) * 1000
commute_mode_df$commuters_by_subway_or_elevated_per_1000 <- (commute_mode_df$commuters_by_subway_or_elevated / commute_mode_df$total_pop) * 1000
commute_mode_df$commuters_drove_alone_per_1000 <- (commute_mode_df$commuters_drove_alone / commute_mode_df$total_pop) * 1000
commute_mode_df$commuters_by_bus_per_1000 <- (commute_mode_df$commuters_by_bus / commute_mode_df$total_pop) * 1000

# Optionally, you may want to remove NA values if any
commute_mode_df <- na.omit(commute_mode_df)

str(commute_mode_df)

ggplot(commute_mode_df, aes(x = confirmed_cases_per_1000)) +
  geom_point(aes(y = walked_to_work_per_1000, color = "Walked to Work"), size = 3) +
  geom_point(aes(y = worked_at_home_per_1000, color = "Worked at Home"), size = 3) +
  geom_point(aes(y = commuters_by_car_truck_van_per_1000, color = "Commuters by Car/Truck/Van"), size = 3) +
  geom_point(aes(y = commuters_by_carpool_per_1000, color = "Commuters by Carpool"), size = 3) +
  geom_point(aes(y = commuters_by_subway_or_elevated_per_1000, color = "Commuters by Subway/Elevated"), size = 3) +
  geom_point(aes(y = commuters_drove_alone_per_1000, color = "Commuters Drove Alone"), size = 3) +
  geom_point(aes(y = commuters_by_bus_per_1000, color = "Commuters by Bus"), size = 3) +
  labs(title = "Commute Modes v/s COVID-19 Cases per 1000 People",
       x = "Confirmed Cases per 1000 People",
       y = "Commuters per 1000 People",
       color = "Commute Mode") +
  theme_minimal()


#Relation between Income per Capita and Covid cases

texas_selected_df <- selected_df %>%
  filter(state == 'TX')
colnames(texas_selected_df)

texas_selected_df$cases_per_1000 <- texas_selected_df$confirmed_cases / (texas_selected_df$total_pop / 1000)

# Plot the relationship between income per capita and COVID-19 cases per 1000 people
ggplot(texas_selected_df, aes(x = income_per_capita, y = cases_per_1000)) +
  geom_point() +
  labs(x = "Income Per Capita", y = "COVID-19 Cases per 1000 People", 
       title = "Relationship between Income Per Capita and COVID-19 Cases in Texas") +
  theme_minimal()

colnames(texas_selected_df)
texas_data_income <- texas_selected_df[c('income_less_25000','income_25000_49999','income_50000_99999','income_100000_199999','income_200000_or_more')]


income_counts <- texas_data_income %>%
  summarise_all(sum) %>%
  t() %>%
  as.data.frame()

# Rename the column
names(income_counts) <- "count"
income_counts$income_group <- rownames(income_counts)

# Reorder income_group factor based on income
income_counts$income_group <- factor(income_counts$income_group, levels = c('income_less_25000', 'income_25000_49999', 'income_50000_99999', 'income_100000_199999', 'income_200000_or_more'))

ggplot(income_counts, aes(x = income_group, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = count), vjust = -0.5, size = 3) +
  labs(x = "Income Group", y = "Count", 
       title = "Count of Individuals in Each Income Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
