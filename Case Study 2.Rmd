---
title: "Case Study 2"
author: "Christian"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# # Project Presentation 
https://smu.zoom.us/rec/share/_PzRRtfdhT2ePWzXH0Lx2sIZMq8GwgeKTA2HAVasKvkCtHDZZRhChZdbOfmoVHVZ.ew57SCDdjbvTRiCs?startTime=1702429652000
Passcode: rdV076=w


## Preparing the Environment and Loading the Data

This is loading the necessary libraries like readr, dplyr, tidyr, ggplot2, caret, e1071, and class. 

```{r}
# Load libraries
library(rmarkdown)
library(readr)
library(dplyr)
library(tidyr)
library(caret)
library(e1071)
library(class)


# Load data
data <- read.csv(choose.files())


# Convert categorical variables to factors
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
```

# # Exploratory Data Analysis

To begin, we started by analyzing the correlation between all variables. This produced a correlation matrix used to reference different variables in the selection process. 

```{r}

# Perform correlation analysis
correlation <- cor(data_numeric, use = "complete.obs")

# Sort the correlation with Attrition and find the top factors
sorted_correlation <- sort(correlation['Attrition',], decreasing = TRUE)

# Display the top factors (excluding Attrition itself)
sorted_correlation[sorted_correlation != 1][1:3]
```

We next plotted different graphs in order to understand how the variables related to each other. 

```{r}

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

# Visualizing correlations
correlation_plot <- cor(data %>% select_if(is.numeric))
ggplot(data = as.data.frame(correlation_plot), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile()

# Identifying top factors for attrition
attrition_factors <- data %>% 
  group_by(attrition) %>% 
  summarise_all(funs(mean(., na.rm = TRUE)))
#Analyzing the distribution of attrition across different departments
ggplot(data, aes(x = Department, fill = Attrition)) +
  geom_bar(position = "fill") +
  labs(title = "Attrition Distribution Across Departments", x = "Department", y = "Proportion")


# Exploring the relationship between job satisfaction and attrition
ggplot(data, aes(x = JobSatisfaction, fill = Attrition)) +
  geom_bar(position = "fill") +
  labs(title = "Job Satisfaction vs Attrition", x = "Job Satisfaction Level", y = "Proportion")

# Relationship between total working years and attrition
ggplot(data, aes(x = TotalWorkingYears, y = after_stat(count), fill = Attrition)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(title = "Total Working Years vs Attrition", x = "Total Working Years", y = "Count")

# Plotting histogram of a variable (example: Age)
ggplot(data, aes(x = Age)) + geom_histogram(bins = 30) + theme_minimal()

# Exploring relationship between two variables (example: Age and Attrition)
ggplot(data, aes(x = Age, fill = Attrition)) + geom_histogram(bins = 30) + theme_minimal()

# Analyzing the distribution of attrition across different departments
ggplot(data, aes(x = Department, fill = Attrition)) +
  geom_bar(position = "fill") +
  labs(title = "Attrition Distribution Across Departments", x = "Department", y = "Proportion")

# Exploring the relationship between job satisfaction and attrition
ggplot(data, aes(x = JobSatisfaction, fill = Attrition)) +
  geom_bar(position = "fill") +
  labs(title = "Job Satisfaction vs Attrition", x = "Job Satisfaction Level", y = "Proportion")

# Relationship between total working years and attrition
ggplot(data, aes(x = TotalWorkingYears, y = after_stat(count), fill = Attrition)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(title = "Total Working Years vs Attrition", x = "Total Working Years", y = "Count")

# Analyzing job role specific trends in job satisfaction
ggplot(data, aes(x = JobRole, y = JobSatisfaction)) +
  geom_boxplot() +
  labs(title = "Job Satisfaction Across Different Job Roles", x = "Job Role", y = "Job Satisfaction Level") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Job role specific attrition trends
ggplot(data, aes(x = JobRole, fill = Attrition)) +
  geom_bar(position = "fill") +
  labs(title = "Attrition Rates Across Different Job Roles", x = "Job Role", y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Age distribution with respect to attrition
ggplot(data, aes(x = Age, fill = Attrition)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(title = "Age Distribution vs Attrition", x = "Age", y = "Count")

# Exploring the impact of marital status on attrition
ggplot(data, aes(x = MaritalStatus, fill = Attrition)) +
  geom_bar(position = "fill") +
  labs(title = "Marital Status vs Attrition", x = "Marital Status", y = "Proportion")

```

Parts of the above EDA were generated by AI and were useful in coming up with creative graphs.

# # Analysis of Important Variables

Given the most highly correlated variables, we decided to proceed with a model that includes them in order to test it's accuracy. 

```{r}

data$Attrition

# Load the data
data <- read.csv(choose.files())

# Check the unique values in Attrition column
unique(data$Attrition)

# Ensure Attrition is converted to a factor with correct levels
#data$Attrition <- factor(data$Attrition, levels = c("No", "Yes"))

# Convert other variables to factors
#data$OverTime <- factor(data$OverTime)
#data$JobRole <- factor(data$JobRole)
#data$MaritalStatus <- factor(data$MaritalStatus)

# Function to calculate attrition rates
#calculate_attrition_rate <- function(data, column_name) {
#  data %>%
#    group_by(!!sym(column_name)) %>%
#    summarise(Attrition_Count = sum(Attrition == "Yes", na.rm = TRUE),
#              Total_Count = n(),
#              Attrition_Rate = Attrition_Count / Total_Count) %>%
#    arrange(desc(Attrition_Rate))
#}

# Calculate attrition rates for Job Role, Overtime, and Marital Status
#job_role_attrition <- calculate_attrition_rate(data, "JobRole")
#overtime_attrition <- calculate_attrition_rate(data, "OverTime")
#marital_status_attrition <- calculate_attrition_rate(data, "MaritalStatus")

# Display the results
#job_role_attrition
#overtime_attrition
#marital_status_attrition

```

This did not yield a reliable or accurate model, so we began to search elsewhere for better variables. 


# # T-tests and Variables 

```{r}

# Load the data
data <- read.csv(choose.files())

# Ensure data$Attrition is a factor and check its levels
data$Attrition <- as.factor(data$Attrition)
print(levels(data$Attrition))  # Should output something like '0' and '1' or 'Yes' and 'No'

# Identify numeric variables
numeric_vars <- sapply(data, is.numeric)
numeric_vars_names <- names(data)[numeric_vars]

# Initialize an empty data frame to store t-test results
t_test_results <- data.frame(Variable = character(), 
                             p_value = numeric(), 
                             mean_diff = numeric(), 
                             stringsAsFactors = FALSE)

# Corrected code for handling variables with little or no variance within groups
for (var in numeric_vars_names) {
  # Perform t-test if the variable has more than one unique value in both groups
  if (length(unique(data[data$Attrition == levels(data$Attrition)[1], var])) > 1 &&
      length(unique(data[data$Attrition == levels(data$Attrition)[2], var])) > 1) {
    
    # Perform t-test
    t_test <- t.test(data[[var]] ~ data$Attrition)
    
    # Calculate mean difference
    mean_diff <- abs(diff(tapply(data[[var]], data$Attrition, mean, na.rm=TRUE)))
    
    # Append results to the data frame
    t_test_results <- rbind(t_test_results, 
                            data.frame(Variable = var, 
                                       p_value = t_test$p.value, 
                                       mean_diff = mean_diff))
  }
}

# Order results by p-value
t_test_results <- t_test_results[order(t_test_results$p_value),]

# Display the results
print(t_test_results)

```

Given these results, we began to proceed with the new model. 


```{r}
# Load the data
train_data <- read.csv(choose.files())
test_data <- read.csv(choose.files())

str(train_data)

# Select relevant columns and convert factors (modify as per your top factors)
#train_data <- train_data %>%
#  select(Attrition, OverTime, JobRole, MaritalStatus) %>%
#  mutate(OverTime = factor(OverTime),
#         Attrition = factor(Attrition, levels = c("No", "Yes")))

# Select relevant columns and convert factors (modify as per your top factors)
#train_data <- train_data %>%
#  select(Attrition, MonthlyIncome, JobLevel, TotalWorkingYears) %>%
#  mutate(MonthlyIncome = factor(MonthlyIncome),
#         Attrition = factor(Attrition, levels = c("No", "Yes")))


# Assuming MonthlyIncome is already loaded in your data frame
# First, check the range of MonthlyIncome
range(train_data$MonthlyIncome)

# Now, create the breaks for the intervals
min_income <- min(train_data$MonthlyIncome, na.rm = TRUE)
max_income <- max(train_data$MonthlyIncome, na.rm = TRUE)
breaks <- seq(from = min_income, to = max_income, by = 100)

# Use cut function to create a new factor variable
train_data$IncomeGroup <- cut(train_data$MonthlyIncome, breaks = breaks, include.lowest = TRUE, right = FALSE)

# Display the structure to see the changes
str(train_data$IncomeGroup)

# Optional: Convert IncomeGroup to a numeric code if needed
train_data$IncomeGroupCode <- as.numeric(train_data$IncomeGroup)

# Display the first few rows of the dataset to verify
head(train_data)

train_data <- train_data %>%
  select(Attrition, MonthlyIncome, JobLevel, TotalWorkingYears) %>%
  mutate(MonthlyIncome = factor(MonthlyIncome),
         Attrition = factor(Attrition, levels = c("No", "Yes")))


#Previous test with correlation-determined variables
#test_data <- test_data %>%
#  select(OverTime, JobRole, MaritalStatus) %>%
#  mutate(OverTime = factor(OverTime))

test_data <- test_data %>%
  select(MonthlyIncome, JobLevel, TotalWorkingYears) %>%
  mutate(MonthlyIncome = factor(MonthlyIncome))

# Splitting the training data
set.seed(123)  # for reproducibility
training_indices <- createDataPartition(train_data$Attrition, p = 0.8, list = FALSE)
training_set <- train_data[training_indices, ]
validation_set <- train_data[-training_indices, ]

# Trying different values of k
k_values <- c(3, 5, 7, 9)
sensitivities <- c()
specificities <- c()

for (k in k_values) {
  knn_model <- knn3(Attrition ~ ., data = train_data_balanced, k = k)
  knn_pred <- predict(knn_model, validation_set, type = "class")
  conf_matrix <- confusionMatrix(knn_pred, validation_set$Attrition)
  sensitivities <- c(sensitivities, conf_matrix$byClass["Sensitivity"])
  specificities <- c(specificities, conf_matrix$byClass["Specificity"])
}

# Find the best k value
best_k <- k_values[which.max(sensitivities + specificities)]

# Training KNN model
knn_model <- knn3(Attrition ~ ., data = training_set, k = 15)

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

data$OverTime

# Assuming 'data' is your dataframe
data$OverTime <- as.factor(data$OverTime)  # Convert to factor if it's not
data$JobRole <- as.factor(data$JobRole)  # Convert to factor if it's not
data$MaritalStatus <- as.factor(data$MaritalStatus)  # Convert to factor if it's not

# Building the model
model <- lm(MonthlyIncome ~ OverTime + JobRole + MaritalStatus, data = data)

# Summary of the model to see coefficients and statistics
summary(model)

```
# # Conclusion

Given the results of the t-test determined variables, the results were still disappointing. With hours of work put into researching models, and each of these models being no-good, there were no leads towards a better method, the deadline came and went without a good solution. This is not how I wanted this project to come out, nor how I imagined it would come out. This took hours upon hours, and even one all-nighter attempting to find a better way, but in the end, I was unable to do so. 

