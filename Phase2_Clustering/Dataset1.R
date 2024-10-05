library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)
library(tidyr)
library(ggplot2)
library(reshape2)
library(cluster)


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
selected_df 

selected_df <- selected_df %>%
  filter(state == "TX")

selected_df


dataset1_cols <- c("income_less_25000" ,"income_25000_49999"                                       
                   ,"income_50000_99999"                                       
                   ,"income_100000_199999",
                   "income_200000_or_more",
                   "employed_pop",
                   "unemployed_pop",
                   "children",
                   "male_pop",
                   "female_pop",
                   "poverty",
                   "total_pop")
dataset1 = selected_df[dataset1_cols]

correlation_matrix <- cor(dataset1)
correlation_matrix

# Convert correlation matrix to a data frame for visualization
correlation_df <- as.data.frame(as.table(correlation_matrix))
names(correlation_df) <- c("feature1", "feature2", "correlation")

# Plot correlation matrix
ggplot(data = correlation_df, aes(x = feature1, y = feature2, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Correlation Matrix")





dataset1
dataset1$low_income <- rowSums(dataset1[, c("income_less_25000", "income_25000_49999")])
dataset1$medium_income <- rowSums(dataset1[, c("income_50000_99999", "income_100000_199999")])
dataset1$high_income <- dataset1$income_200000_or_more
dataset1 <- dataset1[, !grepl("^income_", names(dataset1))]
dataset1


colnames(dataset1)

correlation_matrix <- cor(dataset1)
correlation_matrix

correlation_df <- as.data.frame(as.table(correlation_matrix))
names(correlation_df) <- c("feature1", "feature2", "correlation")

ggplot(data = correlation_df, aes(x = feature1, y = feature2, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Correlation Matrix")


dataset1 <- dataset1 %>%
  mutate(
    low_income_per_1000 = low_income / total_pop * 1000,
    medium_income_per_1000 = medium_income / total_pop * 1000,
    high_income_per_1000 = high_income / total_pop * 1000,
    employed_pop_per_1000 = employed_pop / total_pop * 1000,
    unemployed_pop_per_1000 = unemployed_pop / total_pop * 1000,
    children_per_1000 = children / total_pop * 1000,
    male_pop_per_1000 = male_pop / total_pop * 1000,
    female_pop_per_1000 = female_pop / total_pop * 1000,
    poverty_per_1000 = poverty / total_pop * 1000
  )

print(colnames(dataset1))
head(dataset1)

dataset1 <- subset(dataset1, select = -c(low_income, medium_income, high_income,
                                         employed_pop,
                                         unemployed_pop, children, male_pop, female_pop,
                                         poverty, total_pop))
colnames(dataset1)


lof <- lof(dataset1, minPts = 10)

threshold <- 1.3

outliers_ext <- which(lof > threshold)


dataset1_cleaned <- scaled_data[-outliers, ]


correlation_matrix <- cor(dataset1)
correlation_matrix

correlation_df <- as.data.frame(as.table(correlation_matrix))
names(correlation_df) <- c("feature1", "feature2", "correlation")

ggplot(data = correlation_df, aes(x = feature1, y = feature2, fill = correlation)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Correlation Matrix")


#Other process
min_max_scale <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

z_score_scale <- function(x) {
  (x - mean(x)) / sd(x)
}

# Apply min-max scaling to the columns
scaled_data <- as.data.frame(lapply(dataset1, z_score_scale))
#scaled_data <- as.data.frame(lapply(dataset1, min_max_scale))

#scaled_data <- scale(dataset1)

head(scaled_data)
colnames(scaled_data)

outlier_features <- c("employed_pop_per_1000", 
                      "unemployed_pop_per_1000",                    
                      "commuters_by_public_transportation_per_1000",
                      "worked_at_home_per_1000",                    
                      "walked_to_work_per_1000",                   
                      "high_travel_time_per_1000",                  
                      "low_travel_time_per_1000",                    
                      "commute_other_private_means_per_1000")



#Outlier Identification

length(scaled_data)
nrow(scaled_data)


nrow(scaled_data)

library(dbscan)


lof <- lof(scaled_data, minPts = 10)

threshold <- 1.3

outliers <- which(lof > threshold)

# Remove outliers from the dataset
dataset1_cleaned <- scaled_data[-outliers, ]

nrow(dataset1_cleaned)
nrow(scaled_data)

summary(dataset1_cleaned)

summary(lof)

hist(lof, breaks = 20, main = "Distribution of LOF Values", xlab = "LOF")

# Characteristics of outliers (if applicable)
outlier_data <- scaled_data[which(lof > 2.5), ]
outlier_data


colnames(dataset1_cleaned)

#Kmeans
col
set.seed(123)
ks <- 2:10

WCSS <- sapply(ks, FUN = function(k) {
  kmeans(dataset1_cleaned, centers = k, nstart = 20)$tot.withinss
})
ggplot(tibble(ks, WCSS), aes(ks, WCSS)) + geom_line() +
  geom_vline(xintercept = 3, color = "red", linetype = 2)

library(ggplot2)

wcss_df <- data.frame(ks = ks, WCSS = WCSS)

# Plot the scree plot
ggplot(wcss_df, aes(x = ks, y = WCSS)) +
  geom_line() +
  geom_point() +
  labs(x = "Number of clusters (k)", y = "Within-Cluster Sum of Squares (WCSS)") +
  geom_vline(xintercept = 4, color = "red", linetype = 2) +  # Highlight chosen number of clusters
  theme_minimal()


colnames(dataset1_cleaned)

km_dataset1 <- kmeans(dataset1_cleaned, centers = 4, nstart = 100)
km_dataset1


sil_width <- silhouette(km_dataset1$cluster, dist(dataset1_cleaned))

# Average silhouette width
avg_sil_width <- mean(sil_width[, "sil_width"])

# Print the average silhouette width
print(avg_sil_width)

cluster_profiles <- aggregate(dataset1_cleaned, by = list(cluster = km_dataset1$cluster), mean) # You can use median() instead of mean() if you prefer

# Plot the cluster profiles
library(tidyr)
library(ggplot2)

# Convert cluster_profiles to long format for easier plotting
# Convert cluster_profiles to long format for easier plotting
cluster_profiles_long <- gather(cluster_profiles, key = "feature", value = "value", -cluster)
cluster_profiles_long
# Plot cluster profiles
ggplot(cluster_profiles_long, aes(x = value, y = feature, fill = factor(cluster))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ cluster, scales = "free", dir = "v") +  # Rotate facet labels
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +  # Rotate x-axis labels
  labs(y = "Feature", x = "Mean Value", fill = "Cluster") +
  theme_minimal()
colnames(dataset1_cleaned)

library(cluster)

# Assuming you have the kmeans results stored in km_dataset1

# Calculate silhouette scores
library(cluster)

# Assuming you have the kmeans results stored in km_cluster2

# Calculate silhouette scores with cluster labels
silhouette_scores <- silhouette(km_dataset1$cluster, dist(dataset1_cleaned), cluster.label = as.character(1:5))  # Adjust labels if you have different cluster numbers

# Create the silhouette plot
d <- dist(dataset1_cleaned)
plot(silhouette_scores)
fviz_silhouette(silhouette(km_dataset1$cluster, d))

# Customize the plot (optional)
labs(title = "Silhouette Plot", x = "Silhouette Width", y = "Cluster", color = "Cluster")


# View the results
print(outlier_results)

# 7. Domain Knowledge
# Incorporate domain-specific knowledge or expert input to assess the reasonableness of values based on the context of your data.


dataset1_clustered <- dataset1_cleaned %>%
  add_column(cluster = factor(km_dataset1$cluster))

dataset1_clustered <- dataset1_cleaned |> add_column(cluster = factor(km_dataset1$cluster))
dataset1_clustered
colnames(dataset1_clustered)
colnames(dataset1_cleaned)

library(ggplot2)
library(GGally)

# Remove "_per_1000" from column names for better visualization
colnames(dataset1_clustered) <- gsub("_per_1000", "", colnames(dataset1_clustered))

# Plot Parallel Coordinates
ggparcoord(dataset1_clustered, columns = 1:8, groupColumn = "cluster",
           scale = "globalminmax", alphaLines = 0.5)

centroids <- as_tibble(km_dataset1$centers, rownames = "cluster")
head(centroids)
colnames(centroids)

#install.packages("factoextra")
library("factoextra")


#Hierarchical Clustering
hc <- hclust(dist(dataset1_cleaned))

# Visualize the optimal number of clusters
fviz_nbclust(dataset1_cleaned, hcut, method = "silhouette")


d <- dist(dataset1_cleaned)
hc <- hclust(d, method = "complete")
plot(hc)

fviz_cluster(list(data = dataset1_cleaned, cluster = cutree(hc, k = 3)), geom = "point")


fviz_dend(hc, k = 3)

library(cluster)
library(factoextra)

# Calculate the hierarchical clustering
d <- dist(dataset1_cleaned)
hc <- hclust(d, method = "complete")
hc.cut <- hcut(dataset1_cleaned, k = 3, hc_method = "complete")
fviz_silhouette(hc.cut)
hc_result <- hcut(dataset1_cleaned, k = 3, hc_method = "complete")

# Get the cluster assignments
cluster_assignments <- cutree(hc, k = 3)  # Adjust the number of clusters as needed
cluster_assignments_hc <- cutree(hc, k = 3)  # Adjust the number of clusters as needed

# Calculate silhouette scores
silhouette_scores <- silhouette(cluster_assignments, d)

# Create the silhouette plot
plot(silhouette_scores)
fviz_silhouette(list(silhouette(cluster_assignments, d)))


#PAM

#############
d <- dist(dataset1_cleaned)

library(cluster)

# Assuming 'd' is your data matrix or dataframe

# Function to calculate average silhouette width for a given number of clusters
calculate_avg_sil_width <- function(k, data) {
  pam_res <- pam(data, k = k)
  sil_width <- silhouette(pam_res)
  return(mean(sil_width[, "sil_width"]))
}

# Calculate silhouette widths for different values of k
k_values <- 3:10
silhouette_widths <- sapply(k_values, function(k) calculate_avg_sil_width(k, d))

# Find the optimal number of clusters
optimal_k <- k_values[which.max(silhouette_widths)]
cat("Optimal number of clusters:", optimal_k, "\n")

# Plot silhouette widths
plot(k_values, silhouette_widths, type = "b", 
     xlab = "Number of Clusters (k)", 
     ylab = "Average Silhouette Width", 
     main = "Average Silhouette Width for Different Numbers of Clusters")

# Add a vertical line at the optimal k
abline(v = optimal_k, col = "red", lty = 2)


library(cluster)

# Assuming 'd' is your data matrix or dataframe
pam_result <- pam(d, k = 4)
cluster_assignments_pam <- pam_result$clustering


print(pam_result)

# Plot the clustering result
plot(pam_result, main = "PAM Clustering with 4 Clusters")
fviz_silhouette(pam_result)

medoids <- pam_result$medoids

dist_matrix <- dist(medoids)
dist_matrix_square <- as.matrix(dist_matrix)
heatmap_data <- as.data.frame(as.table(dist_matrix_square))
heatmap_plot <- ggplot(data = heatmap_data, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Heatmap of Cluster Medoids Distance", x = "Cluster", y = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

print(heatmap_plot)

#Gaussian Mixture Model

library(cluster)
library(factoextra)

silhouette_scores <- numeric(10)
for (k in 2:10) {
  model <- Mclust(dataset1_cleaned, G = k)
  clusters <- model$classification
  silhouette_scores[k] <- mean(silhouette(clusters, dist(dataset1_cleaned))[, "sil_width"])
}

print(silhouette_scores)



# Extract the optimal number of clusters
optimal_num_clusters <- which.max(silhouette_scores) + 1  # Adding 1 to convert from index to actual number

# Plot silhouette scores
plot(2:(1+length(silhouette_scores)), silhouette_scores, type = "b", 
     xlab = "Number of clusters", ylab = "Average silhouette width")

abline(v = optimal_num_clusters, col = "red")

#install.packages("mclust")
library(mclust)
m <- Mclust(dataset1_cleaned, G = 3)
cluster_assignments_gmm <- m$classification

cluster_assignments_gmm
summary(m)

sil_width <- silhouette(m$classification, dist(dataset1_cleaned))

# Plot the silhouette plot
plot(sil_width, border = NA)
colnames((dataset1_cleaned))
library(cluster)
library(ggplot2)

# Fit the Mclust model
m <- Mclust(dataset1_cleaned, G = 3)

clusters <- m$classification


centroids <- t(sapply(unique(clusters), function(cluster) colMeans(dataset1_cleaned[clusters == cluster, ])))

heatmap(centroids, Rowv = NA, Colv = NA, col = cm.colors(max(clusters)), scale = "none", main = "Cluster Centroids", margins = c(5, 10))

# Add cluster labels to the heatmap
text(1:nrow(centroids), 1:ncol(centroids), labels = unique(clusters), col = "white")

plot(m, what = "classification")


#External evluations

truth_cols = c("confirmed_cases", 
                     "total_pop")
truth <- data[truth_cols]
truth

truth <- truth %>%
  mutate(
    confirmed_cases_per_1000 = confirmed_cases / total_pop * 1000)
truth
colnames(truth)

# Define the thresholds
threshold_low <- 60    
threshold_medium <- 100

# Create labels based on the thresholds
labels <- cut(truth$confirmed_cases_per_1000,
              breaks = c(-Inf, threshold_low, threshold_medium, Inf),
              labels = c("low", "medium", "high"),
              include.lowest = TRUE)

# Convert labels to numeric values
true_labels_numeric <- as.numeric(labels) - 1
unique(true_labels_numeric)

length(true_labels_numeric)


cluster_assignments <- km_dataset1$cluster
cluster_assignments = as.numeric(cluster_assignments)
length(cluster_assignments)

cluster_assignments_hc = as.numeric(cluster_assignments_hc)
length(cluster_assignments_hc)

cluster_assignments_pam = as.numeric(cluster_assignments_pam)
length(cluster_assignments_pam)

cluster_assignments_gmm = as.numeric(cluster_assignments_gmm)
length(cluster_assignments_gmm)


set.seed(123)

true_labels_shuffled <- sample(true_labels_numeric)

true_labels_subset <- true_labels_shuffled[1:length(cluster_assignments)]



d <- dist(dataset1_cleaned)




#####

entropy <- function(cluster_labels) {
  # Calculate the proportion of data points in each cluster
  prop <- table(cluster_labels) / length(cluster_labels)
  prop <- prop[prop > 0]
  entropy_value <- -sum(prop * log2(prop))
  return(entropy_value)
}
entropy_value <- entropy(cluster_assignments)
entropy_value

purity <- function(cluster_labels, true_labels) {
  n <- length(cluster_labels)
  table <- table(cluster_labels, true_labels)
  purity_value <- sum(apply(table, 1, max)) / n
  return(purity_value)
}
purity_value <- purity(cluster_assignments_hc, true_labels_subset)
purity_value

install.packages("flexclust")
library(flexclust)

library(infotheo)

vi <- function(cluster_labels, true_labels) {
  # Calculate joint distribution
  joint_distribution <- table(cluster_labels, true_labels)
  
  # Calculate marginal distributions
  marginal_cluster <- margin.table(joint_distribution, margin = 1)
  marginal_true <- margin.table(joint_distribution, margin = 2)
  
  # Calculate conditional entropies
  H_cluster_given_true <- entropy(joint_distribution) - entropy(marginal_cluster)
  H_true_given_cluster <- entropy(joint_distribution) - entropy(marginal_true)
  
  # Calculate VI
  vi_value <- H_cluster_given_true + H_true_given_cluster
  
  return(vi_value)
}

# Calculate VI with the given cluster assignments and true labels
vi_value <- vi(cluster_assignments, true_labels_subset)
vi_value

###



cluster_results <- list(
  cluster_assignments = cluster_assignments,
  cluster_assignments_hc = cluster_assignments_hc,
  cluster_assignments_pam = cluster_assignments_pam,
  cluster_assignments_gmm = cluster_assignments_gmm
)

true_labels_subset

metrics_results <- list()

compute_metrics <- function(cluster_assignments, true_labels) {
  entropy_value <- entropy(cluster_assignments)
  purity_value <- purity(cluster_assignments, true_labels)
  vi_value <- vi(cluster_assignments, true_labels)
  return(list(entropy = entropy_value, purity = purity_value, vi = vi_value))
}

for (name in names(cluster_results)) {
  cluster_assignments <- cluster_results[[name]]
  metrics <- compute_metrics(cluster_assignments, true_labels_subset)
  metrics_results[[name]] <- metrics
}

# Print results
for (name in names(metrics_results)) {
  cat("Metrics for", name, ":\n")
  print(metrics_results[[name]])
}


