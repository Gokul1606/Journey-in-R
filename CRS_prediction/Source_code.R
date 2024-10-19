# Installing packages
install.packages("pacman")
install.packages("ggplot2")
install.packages("ISLR2")
install.packages("dplyr")
install.packages("magrittr")
install.packages("lubridate")
install.packages("corrplot")
install.packages("caret")

#Loading the required libraries
pacman::p_load(pacman,ggplot2,ISLR2,dplyr,magrittr,lubridate,corrplot,caret)

# Reading the Express entry file which we scrapped from the browser
data <- read.csv("C:/Users/Owner/Documents/Multivariate_Assignment_1/Express_entry_data_Rselenium.csv")

# Selecting the required data for the analysis and cleaning the Round_type column

data %<>% 
  select(Date, Round.type, Invitations.issued, CRS.score.of.lowest.ranked.candidate.invited) %>%
  rename(Round_type = Round.type,
         No_of_invitations = Invitations.issued,
         CRS_score = CRS.score.of.lowest.ranked.candidate.invited) %>%
  mutate(Round_type = gsub("\\(Version 1\\)", "", Round_type)) %>%
  mutate(Round_type = trimws(Round_type)) %>%
  mutate(Round_type = as.factor(Round_type))

# Data cleaning on No_of_invitations column

data %<>% mutate(No_of_invitations = gsub("\\,","", No_of_invitations)) %>%
  mutate(No_of_invitations = as.integer(No_of_invitations))

# Data cleaning on Date column

data %<>%
  mutate(Date = as.Date(Date, format = "%B %d, %Y"))

data %<>%
  mutate(Month = month(Date),
         Year = year(Date)) %>%
  select(-Date) %>%
  select(Month, Year, everything())

data %<>% mutate(Month = as.factor(Month))
  
summary(data)

#Box plots for outliers

boxplot(data$No_of_invitations)
boxplot(data$CRS_score)
boxplot(data$Month)
boxplot(data$Year)
boxplot(data$Round_type)

# Removing outliers

# Calculate the first (Q1) and third (Q3) quartiles
Q1 <- quantile(data$No_of_invitations, 0.25, na.rm = TRUE)
Q3 <- quantile(data$No_of_invitations, 0.75, na.rm = TRUE)

# Calculate the IQR
IQR_value <- Q3 - Q1

# Define the lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value
lower_bound
upper_bound

# Remove outliers from the dataset
data_cleaned <- data %>%
  filter(No_of_invitations >= lower_bound & No_of_invitations <= upper_bound)

# Check the new summary of the cleaned data
summary(data_cleaned)

# Create a boxplot to visualize the cleaned data
boxplot(data_cleaned$No_of_invitations, main = "Boxplot of No_of_invitations (Cleaned)", ylab = "No_of_invitations")
boxplot(data_cleaned$CRS_score)
data_cleaned

# Histogram for Year
ggplot(data_cleaned, aes(x = Year)) +
  geom_histogram(bins = 10, fill = "lightgreen", color = "black") +
  scale_x_continuous(breaks = seq(2014, 2024, by = 1)) +  # Set breaks for years 2014 to 2024
  labs(title = "Histogram of Year",
       x = "Year",
       y = "No of times of draws in a year") +
  theme_minimal()

# Bar graph for Round_types
ggplot(data_cleaned, aes(x = Round_type)) +
  geom_bar(fill = "lightcoral", color = "black") +
  labs(title = "Frequency of Round Types",
       x = "Round Type",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# Correlation between variables

numeric_data <- data_cleaned[, c("Year", "No_of_invitations", "CRS_score")]
correlation_matrix <- cor(numeric_data)

# Create the correlation plot
corrplot(correlation_matrix, 
         method = "circle",  
         type = "full",      
         order = "original", 
         addCoef.col = "black", 
         tl.col = "black",   
         tl.srt = 45,        
         title = "Correlation Plot of Numerical Variables",
         mar = c(0, 0, 1, 0)
         ) # Adjust margins

# Boxplot of CRS Score by Month
ggplot(data_cleaned, aes(x = Month, y = CRS_score)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "CRS Score by Month",
       x = "Month",
       y = "CRS Score") +
  theme_minimal()

# Scatter plots

# Scatter plot of Year vs CRS_score with LM line
ggplot(data_cleaned, aes(x = Year, y = CRS_score)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Scatter plot of Year vs CRS Score with Linear Regression Line", 
       x = "Year", y = "CRS Score") +
  theme_minimal()

# Scatter plot of No_of_invitations vs CRS_score with LM line
ggplot(data_cleaned, aes(x = No_of_invitations, y = CRS_score)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Scatter plot of No of Invitations vs CRS Score with Linear Regression Line", 
       x = "No of Invitations", y = "CRS Score") +
  theme_minimal()

# Scatter plot of Month vs CRS_score with LM line
ggplot(data_cleaned, aes(x = as.numeric(Month), y = CRS_score)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Scatter plot of Month vs CRS Score with Linear Regression Line", 
       x = "Month", y = "CRS Score") +
  theme_minimal()

# Splitting the dataset into training and testing sets
set.seed(123)
train_indices <- createDataPartition(data_cleaned$CRS_score, p = 0.7, list = FALSE)
train_data <- data_cleaned[train_indices, ]
test_data <- data_cleaned[-train_indices, ]
       
# Linear Regression - Simple Model
simple_model <- lm(CRS_score ~ No_of_invitations, data = train_data)
summary(simple_model)

# Linear Regression - Small Model 
small_model <- lm(CRS_score ~ No_of_invitations + Round_type + Month, data = train_data)
summary(small_model)

# Linear Regression - large Model
large_model <- lm(CRS_score ~ No_of_invitations + Year + Month + Round_type, data = train_data)
summary(large_model)

# Linear Regression - Interaction Model
interaction_model <- lm(CRS_score ~ No_of_invitations * Round_type + Year * Round_type +
                        Month * Round_type + Round_type, data = train_data)
summary(interaction_model)

# Polynomial + Interaction Model
poly_interaction_model <- lm(CRS_score ~ poly(No_of_invitations, 2) + No_of_invitations * Round_type +
                               Year + Month + Round_type, data = train_data)
summary(poly_interaction_model)

# Ensure Round_type is a factor in train_data
train_data$Round_type <- factor(train_data$Round_type)

# Ensure Round_type is a factor in test_data
test_data$Round_type <- factor(test_data$Round_type, levels = levels(train_data$Round_type))


# Model evaluation on test data
simple_model_pred <- predict(simple_model, newdata = test_data)
small_model_pred <- predict(small_model, newdata = test_data)
large_model_pred <- predict(large_model, newdata = test_data)
interaction_model_pred <- predict(interaction_model, newdata = test_data)
poly_interaction_model_pred <- predict(poly_interaction_model, newdata = test_data)

valid_indices <- !is.na(test_data$CRS_score) & !is.na(poly_interaction_model_pred)
rmse_value <- rmse(test_data$CRS_score[valid_indices], poly_interaction_model_pred[valid_indices])
mae_value <- mae(test_data$CRS_score[valid_indices], poly_interaction_model_pred[valid_indices])

# Define RMSE and MAE functions
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2, na.rm = TRUE))
}

mae <- function(actual, predicted) {
  mean(abs(actual - predicted), na.rm = TRUE)
}



# Calculate RMSE and MAE for each model separately

simple_model_metrics <- data.frame(
  Model = "Simple Model",
  RMSE = rmse(test_data$CRS_score, simple_model_pred),
  R_squared = summary(simple_model)$r.squared,
  MAE = mae(test_data$CRS_score, simple_model_pred)
)
small_model_metrics <- data.frame(
  Model = "Small Model",
  RMSE = rmse(test_data$CRS_score, small_model_pred),
  R_squared = summary(small_model)$r.squared,
  MAE = mae(test_data$CRS_score, small_model_pred)
)

large_model_metrics <- data.frame(
  Model = "Large Model",
  RMSE = rmse(test_data$CRS_score, large_model_pred),
  R_squared = summary(large_model)$r.squared,
  MAE = mae(test_data$CRS_score, large_model_pred)
)

interaction_model_metrics <- data.frame(
  Model = "Interaction Model",
  RMSE = rmse(test_data$CRS_score, interaction_model_pred),
  R_squared = summary(interaction_model)$r.squared,
  MAE = mae(test_data$CRS_score, interaction_model_pred)
)

poly_interaction_model_metrics <- data.frame(
  Model = "Polynomial + Interaction Model",
  RMSE = rmse(test_data$CRS_score, poly_interaction_model_pred),
  R_squared = summary(poly_interaction_model)$r.squared,
  MAE = mae(test_data$CRS_score, poly_interaction_model_pred)
)

# Combine all model metrics
model_metrics <- rbind(simple_model_metrics, small_model_metrics, large_model_metrics, interaction_model_metrics, poly_interaction_model_metrics)
print(model_metrics)

set.seed(123)
train_control <- trainControl(method = "cv", number = 10)

# Train model with k-fold cross-validation
subset_model <- train(CRS_score ~ Round_type + No_of_invitations + Year  + Month, 
                      data = train_data, 
                      method = "lm", 
                      trControl = train_control)

print(subset_model)
subset_model
model_comparison <- rbind(
  model_metrics,
  data.frame(Model = "Subset Model", 
             RMSE = subset_model$results$RMSE, 
             R_squared = subset_model$results$Rsquared, 
             MAE = NA)
)
print(model_comparison)


# Create a new data frame with the input values
new_data <- data.frame(
  Month = factor(12, levels = levels(train_data$Month)),  # Ensure Month is a factor with appropriate levels
  Round_type = factor("STEM occupations", levels = levels(train_data$Round_type)),  # Ensure Round_type is a factor
  Year = 2024,
  No_of_invitations = 4500
)

# Use the predict function with the new data
prediction <- predict(interaction_model, newdata = new_data)
print(prediction)


