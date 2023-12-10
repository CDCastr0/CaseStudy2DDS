# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(class)
library(tidyr)
install.packages("caret")
library(caret)


# Load the datasets
main_data <- read.csv(choose.files()) #caseStudy2-data
compset_no_attrition <- read.csv(choose.files()) #CaseStudy2CompSet No Attrition
compset_no_salary <- read.csv(choose.files()) #CaseStudy2COmpSet No Salary

# Inspect the main dataset
head(main_data)
summary(main_data)



###################################################
#removing missing values 
# Removing rows with missing values in the Attrition column
main_data <- main_data[!is.na(main_data$Attrition), ]

# Now, try creating the data partition again
set.seed(123)
splitIndex <- createDataPartition(main_data$Attrition, p = 0.70, list = FALSE, times = 1)


###################################################

# EDA: Summary statistics, distributions, correlations, etc.

# Plotting histogram of a variable (example: Age)
ggplot(main_data, aes(x = Age)) + geom_histogram(bins = 30) + theme_minimal()

# Exploring relationship between two variables (example: Age and Attrition)
ggplot(main_data, aes(x = Age, fill = Attrition)) + geom_histogram(bins = 30) + theme_minimal()



# this didn't work, so chatGPT made the next block 
# Correlation matrix
#correlation_matrix <- cor(select(main_data, -c(Attrition))) # excluding non-numeric variable for correlation
#print(correlation_matrix)

###################################################
#this didn't work, so chatGPT made the next block
# First, ensure that all non-numeric columns are excluded
#numeric_data <- main_data %>% select_if(is.numeric)

# Now compute the correlation matrix
#correlation_matrix <- cor(numeric_data)

# View the correlation matrix
#print(correlation_matrix)

###################################################

# Selecting only numeric columns
#numeric_data <- main_data %>% select_if(is.numeric)

# Removing constant variables
#non_constant_numeric_data <- numeric_data %>% select_if(function(x) sd(x) != 0)

# Now compute the correlation matrix
#correlation_matrix <- cor(non_constant_numeric_data)

# View the correlation matrix
#print(correlation_matrix)
#this worked to produce a correlation matrix


###################################################


# Converting categorical variables to numeric
main_data <- main_data %>%
  mutate(
    Attrition = as.numeric(factor(Attrition, levels = c("No", "Yes"))),
    Overtime = as.numeric(factor(OverTime, levels = c("No", "Yes"))),
    Gender = as.numeric(factor(Gender, levels = c("Female", "Male"))),
    MaritalStatus = as.numeric(factor(MaritalStatus, levels = c("Single", "Married", "Divorced")))
  )

# Now you can compute the correlation matrix including these variables
numeric_data <- main_data %>% select_if(is.numeric)
non_constant_numeric_data <- numeric_data %>% select_if(function(x) sd(x) != 0)
correlation_matrix <- cor(non_constant_numeric_data)

# View the correlation matrix
print(correlation_matrix)



##################################################

# Preparing data for modeling
set.seed(123) # for reproducibility
splitIndex <- createDataPartition(main_data$Attrition, p = .70, list = FALSE, times = 1)
train_data <- main_data[splitIndex,]
test_data <- main_data[-splitIndex,]

# Fitting a Naive Bayes model for Attrition
model_attrition <- naiveBayes(Attrition ~ ., data = train_data)
predictions <- predict(model_attrition, test_data)

# Checking accuracy
confusionMatrix(predictions, test_data$Attrition)

###################################################

# Assuming 'Salary' is a variable in the dataset for prediction
# Fitting a Linear Regression Model for Salary prediction
model_salary <- lm(Salary ~ ., data = train_data)
salary_predictions <- predict(model_salary, test_data)

# Checking RMSE
rmse_value <- sqrt(mean((test_data$Salary - salary_predictions)^2))
print(rmse_value)

###################################################

# Evaluating the models (example for the attrition model)
conf_matrix <- confusionMatrix(predictions, test_data$Attrition)
sensitivity <- conf_matrix$byClass["Sensitivity"]
specificity <- conf_matrix$byClass["Specificity"]
accuracy <- conf_matrix$overall['Accuracy']
cat("Sensitivity:", sensitivity, "\nSpecificity:", specificity, "\nAccuracy:", accuracy)

###################################################

###################################################

###################################################