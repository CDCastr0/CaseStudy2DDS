# Load necessary libraries
library(readr)
library(dplyr)
library(tidyr)

# Load the data
data <- read.csv(choose.files())


# Convert Attrition and other categorical variables to factors
data$Attrition <- factor(data$Attrition, levels = c("No", "Yes"), labels = c(0, 1))
data$BusinessTravel <- factor(data$BusinessTravel)
data$Department <- factor(data$Department)
data$EducationField <- factor(data$EducationField)
data$Gender <- factor(data$Gender)
data$JobRole <- factor(data$JobRole)
data$MaritalStatus <- factor(data$MaritalStatus)
data$Over18 <- factor(data$Over18)
data$OverTime <- factor(data$OverTime)

# Convert factors to numeric for correlation analysis
data_numeric <- data %>%
  mutate(across(where(is.factor), as.numeric))

# Remove constant variables
data_numeric <- data_numeric %>% select_if(~sd(.) != 0)

# Perform correlation analysis
correlation <- cor(data_numeric, use = "complete.obs")

# Sort the correlation with Attrition and find the top factors
sorted_correlation <- sort(correlation['Attrition',], decreasing = TRUE)

# Display the top factors (excluding Attrition itself)
sorted_correlation[sorted_correlation != 1][1:3]



ggplot(data, aes(x = JobRole, fill = Attrition)) +
  geom_bar(position = "dodge") +
  labs(title = "Job Role vs Attrition", x = "Job Role", y = "Count") +
  theme_minimal()


ggplot(data, aes(x = OverTime, fill = Attrition)) +
  geom_bar(position = "dodge") +
  labs(title = "Overtime vs Attrition", x = "Overtime", y = "Count") +
  theme_minimal()

ggplot(data, aes(x = MaritalStatus, fill = Attrition)) +
  geom_bar(position = "dodge") +
  labs(title = "Marital Status vs Attrition", x = "Marital Status", y = "Count") +
  theme_minimal()


data$Attrition
# Load necessary libraries
library(readr)
library(dplyr)

# Load the data
data <- read.csv(choose.files())

# Check the unique values in Attrition column
unique(data$Attrition)

# Ensure Attrition is converted to a factor with correct levels
data$Attrition <- factor(data$Attrition, levels = c("No", "Yes"))

# Convert other variables to factors
data$OverTime <- factor(data$OverTime)
data$JobRole <- factor(data$JobRole)
data$MaritalStatus <- factor(data$MaritalStatus)

# Function to calculate attrition rates
calculate_attrition_rate <- function(data, column_name) {
  data %>%
    group_by(!!sym(column_name)) %>%
    summarise(Attrition_Count = sum(Attrition == "Yes", na.rm = TRUE),
              Total_Count = n(),
              Attrition_Rate = Attrition_Count / Total_Count) %>%
    arrange(desc(Attrition_Rate))
}

# Calculate attrition rates for Job Role, Overtime, and Marital Status
job_role_attrition <- calculate_attrition_rate(data, "JobRole")
overtime_attrition <- calculate_attrition_rate(data, "OverTime")
marital_status_attrition <- calculate_attrition_rate(data, "MaritalStatus")

# Display the results
job_role_attrition
overtime_attrition
marital_status_attrition




# Load necessary libraries
library(readr)
library(dplyr)
library(caret)
library(e1071)
library(class)

# Load the data
train_data <- read.csv(choose.files())
test_data <- read.csv(choose.files())

str(train_data)

# Select relevant columns and convert factors (modify as per your top factors)
train_data <- train_data %>%
  select(Attrition, OverTime, JobRole, MaritalStatus) %>%
  mutate(OverTime = factor(OverTime),
         Attrition = factor(Attrition, levels = c("No", "Yes")))

test_data <- test_data %>%
  select(OverTime, JobRole, MaritalStatus) %>%
  mutate(OverTime = factor(OverTime))

# Splitting the training data
set.seed(123)  # for reproducibility
training_indices <- createDataPartition(train_data$Attrition, p = 0.8, list = FALSE)
training_set <- train_data[training_indices, ]
validation_set <- train_data[-training_indices, ]

# Training KNN model
knn_model <- knn3(Attrition ~ ., data = training_set, k = 5)

# Training Naive Bayes model
nb_model <- naiveBayes(Attrition ~ ., data = training_set)

# Ensure the validation set has the same structure as the training set
validation_set_formatted <- validation_set %>% select(-Attrition)

# Predicting on the validation set
knn_pred <- predict(knn_model, validation_set_formatted, type = "class")

# Check if the number of predictions matches the number of observations
if(length(knn_pred) != nrow(validation_set)) {
  stop("Number of predictions does not match the number of observations in the validation set.")
}

# Calculate confusion matrix and sensitivity, specificity
knn_conf_matrix <- confusionMatrix(knn_pred, validation_set$Attrition)
knn_sensitivity <- knn_conf_matrix$byClass["Sensitivity"]
knn_specificity <- knn_conf_matrix$byClass["Specificity"]

# Display KNN Sensitivity and Specificity
list(KNN_Sensitivity = knn_sensitivity, KNN_Specificity = knn_specificity)

nb_pred <- predict(nb_model, validation_set)

# Calculate sensitivity and specificity
knn_conf_matrix <- confusionMatrix(knn_pred, validation_set$Attrition)
nb_conf_matrix <- confusionMatrix(nb_pred, validation_set$Attrition)

# Sensitivity and Specificity for KNN
knn_sensitivity <- knn_conf_matrix$byClass["Sensitivity"]
knn_specificity <- knn_conf_matrix$byClass["Specificity"]

# Sensitivity and Specificity for Naive Bayes
nb_sensitivity <- nb_conf_matrix$byClass["Sensitivity"]
nb_specificity <- nb_conf_matrix$byClass["Specificity"]

# Display results
list(KNN_Sensitivity = knn_sensitivity, KNN_Specificity = knn_specificity,
     NB_Sensitivity = nb_sensitivity, NB_Specificity = nb_specificity)

# Predicting on test data (example with KNN)
test_predictions <- predict(knn_model, test_data)

# Output predictions
test_predictions


