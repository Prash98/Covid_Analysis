library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)
library(tidyr)
library(ggplot2)
library(reshape2)
library(cluster)
library(dbscan)

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
colnames(selected_df)

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
upper_threshold <- quantile(data$confirmed_cases, 0.95)


# Check for duplicates
duplicates <- data[duplicated(selected_df), ]
duplicates 

# Check for consistency between related columns
consistent_pop <- sum(data$male_pop + data$female_pop == data$total_pop)
consistent_pop
# Check for valid values

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
selected_df 

dataset_cols <- c("county_name", "state","deaths","confirmed_cases" ,"income_less_25000" ,"income_25000_49999" ,                                      
                  "income_50000_99999"                                       
                  ,"income_100000_199999"
                  ,"income_200000_or_more"
                  ,"employed_pop"
                  ,"unemployed_pop"
                  ,"median_income"
                  , "less_than_20_mins"
                  ,"between_20_and_44_mins"
                  ,"between_45_and_89_mins"
                  ,"more_than_90_mins"
                  , "children"
                  # ,"male_pop"
                  # ,"female_pop"
                  ,"poverty"
                  ,"commuters_by_public_transportation"
                  ,"commuters_by_car_truck_van"
                  ,"commuters_by_carpool"
                  ,"commuters_drove_alone"
                  ,"worked_at_home"
                  , "walked_to_work",
                  "male_0_17",
                  "male_18_44",
                  "male_45_64",
                  "male_65_and_over",
                  "female_0_17",
                  "female_18_44",
                  "female_45_64",
                  "female_65_and_over"                
                  ,"total_pop")
dataset = selected_df[dataset_cols]
dataset


dataset <- dataset %>%
  mutate(
    commute_other_private_means = commuters_by_car_truck_van + commuters_by_carpool + commuters_drove_alone
  )

# Dropping the individual commute columns if needed
dataset <- dataset %>%
  select(-commuters_by_car_truck_van, 
         -commuters_by_carpool, 
         -commuters_drove_alone)

dataset$low_income <- rowSums(dataset[, c("income_less_25000", "income_25000_49999")])
dataset$medium_income <- rowSums(dataset[, c("income_50000_99999", "income_100000_199999")])
dataset$high_income <- dataset$income_200000_or_more
dataset <- dataset[, !grepl("^income_", names(dataset))]
colnames(dataset)

dataset <- dataset %>%
  mutate(
    high_travel_time = between_45_and_89_mins + more_than_90_mins,
    low_travel_time = less_than_20_mins + between_20_and_44_mins
  )

# Dropping the individual travel time columns if needed
dataset <- dataset %>%
  select(-less_than_20_mins, 
         -between_20_and_44_mins, 
         -between_45_and_89_mins, 
         -more_than_90_mins)
colnames(dataset)

# dataset$deaths_per_case <- ifelse(dataset$confirmed_cases == 0, 0, dataset$deaths / dataset$confirmed_cases)

dataset <- dataset %>%
  mutate(
    male_0_17_per_1000 = male_0_17 / total_pop * 1000,
    male_18_44_per_1000 = male_18_44 / total_pop * 1000,
    male_45_64_per_1000 = male_45_64 / total_pop * 1000,
    male_65_and_over_per_1000 = male_65_and_over / total_pop * 1000,
    female_0_17_per_1000 = female_0_17 / total_pop * 1000,
    female_18_44_per_1000 = female_18_44 / total_pop * 1000,
    female_45_64_per_1000 = female_45_64 / total_pop * 1000,
    female_65_and_over_per_1000 = female_65_and_over / total_pop * 1000,    
    confirmed_cases_per_1000 = confirmed_cases / total_pop * 1000,
    deaths_per_1000 = deaths / total_pop * 1000,
    low_income_per_1000 = low_income / total_pop * 1000,
    medium_income_per_1000 = medium_income / total_pop * 1000,
    high_income_per_1000 = high_income / total_pop * 1000,
    employed_pop_per_1000 = employed_pop / total_pop * 1000,
    unemployed_pop_per_1000 = unemployed_pop / total_pop * 1000,
    children_per_1000 = children / total_pop * 1000,
    # male_pop_per_1000 = male_pop / total_pop * 1000,
    # female_pop_per_1000 = female_pop / total_pop * 1000,
    poverty_per_1000 = poverty / total_pop * 1000,
    high_travel_time_per_1000 = high_travel_time / total_pop * 1000,
    low_travel_time_per_1000 = low_travel_time / total_pop * 1000,
    commute_other_private_means_per_1000 = commute_other_private_means / total_pop * 1000,
    commuters_by_public_transportation_per_1000 = commuters_by_public_transportation / total_pop * 1000,
    worked_at_home_per_1000 = worked_at_home / total_pop * 1000,
    walked_to_work_per_1000 = walked_to_work / total_pop * 1000
  )

 
dataset <- subset(dataset, select = -c(low_income, medium_income, high_income,
                                      employed_pop, unemployed_pop, 
                                      #male_pop, female_pop,
                                       commute_other_private_means, 
                                      children, 
                                       commuters_by_public_transportation,
                                         poverty, total_pop, high_travel_time, low_travel_time, deaths, confirmed_cases,
                                      male_0_17,
                                      male_18_44,
                                      male_45_64,
                                      male_65_and_over,
                                      female_0_17,
                                      female_18_44,
                                      female_45_64,
                                      female_65_and_over, walked_to_work, worked_at_home))
summary(dataset)
head(dataset)
unique(dataset$county_name)
unique_county_names <- unique(dataset$state)
count_unique_county_names <- length(unique_county_names)
unique_county_names

 
colnames(dataset)
dataset_test = dataset

correlation_matrix <- cor(dataset[, !names(dataset) %in% c("county_name", "state")])

print(correlation_matrix)

library(corrplot)
corrplot(correlation_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

head(dataset)
library(knitr)


#Removing outliers

z_score_scale <- function(x) {
  (x - mean(x)) / sd(x)
}

outlier_features <- c("median_income", "worked_at_home", "walked_to_work", 
                      "male_0_17_per_1000", "male_18_44_per_1000", "male_45_64_per_1000", 
                      "male_65_and_over_per_1000", "female_0_17_per_1000", "female_18_44_per_1000", 
                      "female_45_64_per_1000", "female_65_and_over_per_1000", "confirmed_cases_per_1000", 
                      "deaths_per_1000", "low_income_per_1000", "medium_income_per_1000", "high_income_per_1000",
                      "employed_pop_per_1000", "unemployed_pop_per_1000", "children_per_1000", "poverty_per_1000", 
                      "high_travel_time_per_1000", "low_travel_time_per_1000", "commute_other_private_means_per_1000",
                      "commuters_by_public_transportation_per_1000", "worked_at_home_per_1000", "walked_to_work_per_1000")

# Apply min-max scaling to the columns
dataset[outlier_features] <- as.data.frame(lapply(dataset[outlier_features], z_score_scale))
colnames(dataset)

head(dataset)
colnames(dataset)

#Outlier Identification

length(dataset)
nrow(dataset)

nrow(dataset)

library(dbscan)

lof <- lof(dataset[outlier_features], minPts = 10)

threshold <- 1.8

outliers <- which(lof > threshold)
length(outliers)

# Remove outliers from the dataset
dataset <- dataset[-outliers, ]

nrow(dataset)
summary(dataset)

summary(lof)

hist(lof, breaks = 20, main = "Distribution of LOF Values", xlab = "LOF")

colnames(dataset)
nrow(dataset)

# Define quartiles for confirmed cases and deaths per 1000 for Class Split
confirmed_cases_percentiles <- quantile(dataset$confirmed_cases_per_1000, probs = c(0.65))
deaths_percentiles <- quantile(dataset$deaths_per_1000, probs = c(0.65))

# Create a function to classify counties based on thresholds
classify_county <- function(confirmed_cases_per_1000, deaths_per_1000) {
  if (confirmed_cases_per_1000 <= confirmed_cases_percentiles[1] && deaths_per_1000 <= deaths_percentiles[1]) {
    return("Low")}
 else {
    return("High")
  }
}

# Apply the classification function to the dataset
dataset$class <- mapply(classify_county, dataset$confirmed_cases_per_1000, dataset$deaths_per_1000)
class_balance <- table(dataset$class)
print(class_balance)
head(dataset)

# View the dataset with the class variable
head(dataset)
ggplot(dataset, aes(x = class)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Class Balance",
       x = "Risk Category",
       y = "Count")

colnames(dataset)


# Convert character variables to factors
dataset$county_name <- as.factor(dataset$county_name)
dataset$state <- as.factor(dataset$state)

#Splitting into train and test

colnames(dataset)
dataset1 <- dataset
dataset1 <- dataset1[, !(names(dataset1) %in% c("confirmed_cases_per_1000", "deaths_per_1000"))]
summary(dataset)

#random split

set.seed(123)
dataset <- dataset[, !(names(dataset) %in% c("confirmed_cases_per_1000", "deaths_per_1000"))]

unique_states <- unique(dataset$state)

num_training_states <- round(length(unique_states) * 0.8)
training_states <- sample(unique_states, num_training_states)
testing_states <- setdiff(unique_states, training_states)

train_data <- subset(dataset, state %in% training_states)
test_data <- subset(dataset, state %in% testing_states)

unique_states_train <- unique(train_data$state)
unique_states_test <- unique(test_data$state)

print("Unique states in training data:")
print(unique_states_train)

print("Unique states in testing data:")
print(unique_states_test)

class_balance_train <- table(train_data$class)

print(class_balance_train)
head(dataset)
ggplot(train_data, aes(x = class)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Class Balance",
       x = "Risk Category",
       y = "Count")

class_balance_test <- table(test_data$class)
print(class_balance_test)

colnames(train_data)

colnames(dataset)

#Map Plots

counties <- as_tibble(map_data("county"))
counties <- counties %>% 
  rename(c(county = subregion, state = region)) %>%
  mutate(state = state.abb[match(state, tolower(state.name))]) %>%
  select(state, county, long, lat, group)
counties 

counties_all <- counties %>% left_join(train_data %>% 
                                         mutate(county = county_name %>% str_to_lower() %>% 
                                                  str_replace('\\s+county\\s*$', '')))

ggplot(counties_all, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = class), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('High' = 'red', 'Low' = 'grey'))

#For Test

counties_all_test <- counties %>% left_join(test_data %>% 
                                         mutate(county = county_name %>% str_to_lower() %>% 
                                                  str_replace('\\s+county\\s*$', '')))

ggplot(counties_all_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = class), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('High' = 'red', 'Low' = 'grey'))



train_data <- train_data[, !(names(train_data) %in% c("class_predicted"))] 
test_data <- test_data[, !(names(test_data) %in% c("class_predicted"))] 

colnames(dataset)

#Classification Models

#1 Random Forest

library(caret)

set.seed(123)

library(caret)

# Train the random forest model with different values of mtry
fit_rf <- train(class ~ . - county_name - state,
                data = train_data,
                method = "rf",
                trControl = trainControl(method = "cv", number = 10))
                # tuneGrid = mtry_grid)

colnames(train_data)

# Print the model
fit_rf

train_data$class_predicted <- predict(fit_rf, train_data)
train_data

test_data$class_predicted <- predict(fit_rf, test_data)
test_data

counties_all <- counties %>% left_join(train_data %>% 
                                         mutate(county = county_name %>% str_to_lower() %>% 
                                                  str_replace('\\s+county\\s*$', '')))

ggplot(counties_all, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = class_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('High' = 'red', 'Low' = 'grey'))



counties_all_test <- counties %>% left_join(test_data %>% 
                                              mutate(county = county_name %>% str_to_lower() %>% 
                                                       str_replace('\\s+county\\s*$', '')))

ggplot(counties_all_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = class_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('High' = 'red', 'Low' = 'grey'))

train_data <- train_data[, !(names(train_data) %in% c("class_predicted"))]
test_data <- test_data[, !(names(test_data) %in% c("class_predicted"))]


nrow(test_data)
nrow(train_data)

# Get variable importance
importance = varImp(fit_rf)
predictions <- predict(fit_rf, newdata = test_data)
predictions
confusion_matrix <- table(predictions, test_data$class)
 
print(confusion_matrix)
kappa_score <- kappa(confusion_matrix)
print(kappa_score)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(accuracy)

TP <- confusion_matrix["High", "High"]  # True Positives
FP <- confusion_matrix["High", "Low"]   # False Positives

TN <- confusion_matrix["Low", "Low"]    # True Negatives
FN <- confusion_matrix["Low", "High"]   # False Negatives

# Calculate sensitivity (true positive rate)
sensitivity <- TP / (TP + FN)
print(sensitivity)
# Calculate specificity (true negative rate)
specificity <- TN / (TN + FP)
print(specificity)


precision <- TP / (TP + FP)
print(precision)
# Calculate recall
recall <- TP / (TP + FN)

# Calculate F1 score
f1_score <- 2 * (precision * recall) / (precision + recall)
print(f1_score)


importance
str(importance)

# Plot variable importance
plot(importance)
print(importance)

#2 SVM

train_data <- train_data[, !(names(train_data) %in% c("class_predicted"))]
test_data <- test_data[, !(names(test_data) %in% c("class_predicted"))]

c_values <- 10^seq(-2, 1, by = 0.5)  

sigma_values <- 10^seq(-2, 1, by = 0.5)  

tuning_grid <- expand.grid(C = c_values, sigma = sigma_values)

# Train SVM model with different values for C and sigma
fit_svm <- train(class ~ . - county_name - state,
                 data = train_data,
                 method = "svmRadial",
                 trControl = trainControl(method = "cv", number = 10),
                 tuneGrid = tuning_grid)

fit_svm

predictions <- predict(fit_svm, newdata = test_data)
predictions
confusion_matrix <- table(predictions, test_data$class)
print(confusion_matrix)
kappa_score <- kappa(confusion_matrix)
print(kappa_score)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(accuracy)


train_data$class_predicted <- predict(fit_svm, train_data)
train_data

test_data$class_predicted <- predict(fit_svm, test_data)
test_data

counties_all <- counties %>% left_join(train_data %>% 
                                         mutate(county = county_name %>% str_to_lower() %>% 
                                                  str_replace('\\s+county\\s*$', '')))

ggplot(counties_all, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = class_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('High' = 'red', 'Low' = 'grey'))



counties_all_test <- counties %>% left_join(test_data %>% 
                                              mutate(county = county_name %>% str_to_lower() %>% 
                                                       str_replace('\\s+county\\s*$', '')))

ggplot(counties_all_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = class_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('High' = 'red', 'Low' = 'grey'))


TP <- confusion_matrix["High", "High"]  # True Positives
FP <- confusion_matrix["High", "Low"]   # False Positives

TN <- confusion_matrix["Low", "Low"]    # True Negatives
FN <- confusion_matrix["Low", "High"]   # False Negatives

sensitivity <- TP / (TP + FN)
print(sensitivity)
specificity <- TN / (TN + FP)
print(specificity)
actual <- ifelse(test_data$class == "High", 1, 0)

precision <- TP / (TP + FP)
print(precision)
recall <- TP / (TP + FN)

f1_score <- 2 * (precision * recall) / (precision + recall)
print(f1_score)

train_data 

train_data <- train_data[, !(names(train_data) %in% c("class_predicted"))]
test_data <- test_data[, !(names(test_data) %in% c("class_predicted"))]


library(ggplot2)

accuracy_values <- c()

#3 KNN K values

# Loop through different values of k
for (k in 1:10) {
  # Train KNN model
  knn_model <- train(class ~ . - county_name - state,
                     data = train_data,
                     method = "knn",
                     trControl = trainControl(method = "cv", number = 10),
                     tuneGrid = data.frame(k = k))
  
  predictions <- predict(knn_model, newdata = test_data)
  confusion_matrix <- table(predictions, test_data$class)
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  accuracy_values <- c(accuracy_values, accuracy)
}

# Plot elbow graph
ggplot(data.frame(k = 1:10, accuracy = accuracy_values), aes(x = k, y = accuracy)) +
  geom_line() +
  geom_point() +
  labs(x = "Number of Neighbors (k)", y = "Accuracy", title = "Elbow Graph for KNN Model")


knn_model <- train(class ~ . - county_name - state,  # Exclude county_name and state from predictors
                   data = train_data,
                   method = "knn",
                   trControl = trainControl(method = "cv", number = 10))

knn_model
predictions <- predict(knn_model, newdata = test_data)
predictions
confusion_matrix <- table(predictions, test_data$class)
print(confusion_matrix)
kappa_score <- kappa(confusion_matrix)
print(kappa_score)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(accuracy)


TP <- confusion_matrix["High", "High"]  # True Positives
FP <- confusion_matrix["High", "Low"]   # False Positives

# True Negative (TN) and False Negative (FN) counts (not used in ROC)
TN <- confusion_matrix["Low", "Low"]    # True Negatives
FN <- confusion_matrix["Low", "High"]   # False Negatives

sensitivity <- TP / (TP + FN)
print(sensitivity)
specificity <- TN / (TN + FP)
print(specificity)

precision <- TP / (TP + FP)
print(precision)
recall <- TP / (TP + FN)

# Calculate F1 score
f1_score <- 2 * (precision * recall) / (precision + recall)
print(f1_score)

train_data$class_predicted_knn <- predict(knn_model, train_data)
train_data

test_data$class_predicted_knn <- predict(knn_model, test_data)
test_data

counties_all <- counties %>% left_join(train_data %>% 
                                         mutate(county = county_name %>% str_to_lower() %>% 
                                                  str_replace('\\s+county\\s*$', '')))

colnames(counties_all)

ggplot(counties_all, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = class_predicted_knn), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('High' = 'red', 'Low' = 'grey'))



counties_all_test <- counties %>% left_join(test_data %>% 
                                              mutate(county = county_name %>% str_to_lower() %>% 
                                                       str_replace('\\s+county\\s*$', '')))

ggplot(counties_all_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = class_predicted_knn), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('High' = 'red', 'Low' = 'grey'))

train_data <- train_data[, !(names(train_data) %in% c("class_predicted_knn"))]
test_data <- test_data[, !(names(test_data) %in% c("class_predicted_knn"))]

#4 GBM

library(gbm)
library(caret)

# Define parameters
interaction_depth <- c(1, 2, 3)
n_trees <- c(50, 100, 150)

library(gbm)

train_data <- train_data[, !(names(train_data) %in% c("class_binary"))]
test_data <- test_data[, !(names(test_data) %in% c("class_binary"))]

train_data
test_data

fit_gbm <- train(class ~ . - county_name - state,
                data = train_data,
                method = "gbm",  # Gradient Boosting method
                trControl = trainControl(method = "cv", number = 10))
print(fit_gbm)
predictions <- predict(fit_gbm, newdata = test_data)
predictions
confusion_matrix <- table(predictions, test_data$class)
confusion_matrix
kappa_score <- kappa(confusion_matrix)
print(kappa_score)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(accuracy)


TP <- confusion_matrix["High", "High"]  # True Positives
FP <- confusion_matrix["High", "Low"]   # False Positives

TN <- confusion_matrix["Low", "Low"]    # True Negatives
FN <- confusion_matrix["Low", "High"]   # False Negatives

# Calculate sensitivity (true positive rate)
sensitivity <- TP / (TP + FN)
print(sensitivity)
# Calculate specificity (true negative rate)
specificity <- TN / (TN + FP)
print(specificity)

# Calculate precision
precision <- TP / (TP + FP)
print(precision)
# Calculate recall
recall <- TP / (TP + FN)

# Calculate F1 score
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print F1 score
print(f1_score)

train_data$class_predicted_gbm <- predict(fit_gbm, train_data)
train_data

test_data$class_predicted_gbm <- predict(fit_gbm, test_data)
test_data

counties_all <- counties %>% left_join(train_data %>% 
                                         mutate(county = county_name %>% str_to_lower() %>% 
                                                  str_replace('\\s+county\\s*$', '')))

colnames(counties_all)

ggplot(counties_all, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = class_predicted_gbm), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('High' = 'red', 'Low' = 'grey'))



counties_all_test <- counties %>% left_join(test_data %>% 
                                              mutate(county = county_name %>% str_to_lower() %>% 
                                                       str_replace('\\s+county\\s*$', '')))

ggplot(counties_all_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = class_predicted_gbm), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('High' = 'red', 'Low' = 'grey'))

train_data <- train_data[, !(names(train_data) %in% c("class_predicted_gbm"))]
test_data <- test_data[, !(names(test_data) %in% c("class_predicted_gbm"))]


#5 TreeBAg

train_data <- train_data[, !(names(train_data) %in% c("class_predicted", "class_predicted_treebag"))]
test_data <- test_data[, !(names(test_data) %in% c("class_predicted", "class_predicted_treebag"))]
train_data
test_data
library(caret)

# Train treebag model
fit_treebag <- train(class ~ . - county_name - state,
                     data = train_data,
                     method = "treebag",
                     trControl = trainControl(method = "cv", number = 10))

print(fit_treebag)

predictions <- predict(fit_treebag, newdata = test_data)

confusion_matrix <- table(predictions, test_data$class)

# Print confusion matrix
print(confusion_matrix)
kappa_score <- kappa(confusion_matrix)
print(kappa_score)
# Calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(accuracy)

TP <- confusion_matrix["High", "High"]  # True Positives
FP <- confusion_matrix["High", "Low"]   # False Positives

TN <- confusion_matrix["Low", "Low"]    # True Negatives
FN <- confusion_matrix["Low", "High"]   # False Negatives

# Calculate sensitivity (true positive rate)
sensitivity <- TP / (TP + FN)
print(sensitivity)
# Calculate specificity (true negative rate)
specificity <- TN / (TN + FP)
print(specificity)

# Calculate precision
precision <- TP / (TP + FP)
print(precision)

# Calculate recall
recall <- TP / (TP + FN)

# Calculate F1 score
f1_score <- 2 * (precision * recall) / (precision + recall)
print(f1_score)

train_data$class_predicted_treebag <- predict(fit_treebag, train_data)
train_data

test_data$class_predicted_treebag <- predict(fit_treebag, test_data)
test_data

counties_all <- counties %>% left_join(train_data %>% 
                                         mutate(county = county_name %>% str_to_lower() %>% 
                                                  str_replace('\\s+county\\s*$', '')))

colnames(counties_all)

ggplot(counties_all, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = class_predicted_treebag), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('High' = 'red', 'Low' = 'grey'))



counties_all_test <- counties %>% left_join(test_data %>% 
                                              mutate(county = county_name %>% str_to_lower() %>% 
                                                       str_replace('\\s+county\\s*$', '')))

ggplot(counties_all_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = class_predicted_treebag), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('High' = 'red', 'Low' = 'grey'))




