#  Classification

# Import libraries:
library(mice)
library(dplyr)
library(caret)
library(ggpubr)
library(visdat)
library(lattice)
library(tidyverse)
library(DataExplorer)

theme_set(theme_pubr())

# import data:
credit <- read.csv("credit_training.csv", header = TRUE)

# Drop the first column:
credit <- credit[, -1]
attach(credit)

# Data structure:
str(credit)

# missing values:
introduce(credit) # Noted missing values

# Columns with missing values:
# Data Summary:
summary(credit)

# The MonthlyIncome and the NumberofDependents columns have missing data
# Fill the missing values of MonthlyIncome with mean of the present values:
credit$MonthlyIncome[is.na(credit$MonthlyIncome)] <- mean(credit$MonthlyIncome, na.rm = TRUE)

# Fill NumberOfDependents missing values with their median:
credit$NumberOfDependents[is.na(credit$NumberOfDependents)] <- median(credit$NumberOfDependents, na.rm = TRUE)

# Overview of data distribution
plot_histogram(credit)

# Note the points in the plots that could be outliers
# From summary vars below represent the values with possible outliers 
# Check for extreme outliers and drop:

vars = c("RevolvingUtilizationOfUnsecuredLines","NumberOfTime30to59DaysPastDueNotWorse","DebtRatio","MonthlyIncome","NumberOfTimes90DaysLate","NumberRealEstateLoansOrLines","NumberOfTime60to89DaysPastDueNotWorse")
credit_outliers <- credit %>%
  gather(all_of(vars), key = "factor", value = "value") %>% 
  group_by(factor)
  
# mutate(value = remove_outliers(value)) %>% ungroup()

# Plot
p_credit_outliers <-
  credit_outliers %>%
  ggboxplot(x = "factor", y = "value", palette = "aaas")
suppressMessages(p_credit_outliers)

# The "RevolvingUtilizationOfUnsecuredLines", "DebtRatio" and "MonthlyIncome" are the variables that seem to have extreme outliers.
# Remove the significant outliers:

#___________ RevolvingUtilizationOfUnsecuredLines
summary <- summary(credit$RevolvingUtilizationOfUnsecuredLines)

# Estimate interquartile range
# (3rd quartile minus 1st quartile)

iqr <- summary[[5]] - summary[[2]]

# Identify bounds for outliers
lower_bound <- summary[[2]] - (1.5 * iqr)
upper_bound <- summary[[5]] + (1.5 * iqr)

# Remove outliers from dataframe
# Keep only the points within the bounds
credit <- credit %>%
  filter(RevolvingUtilizationOfUnsecuredLines < upper_bound & RevolvingUtilizationOfUnsecuredLines > lower_bound)

#______________ DebtRatio
summary <- summary(credit$DebtRatio)

# Estimate interquartile range
# (3rd quartile minus 1st quartile)

iqr <- summary[[5]] - summary[[2]]

# Identify bounds for outliers
lower_bound <- summary[[2]] - (1.5 * iqr)
upper_bound <- summary[[5]] + (1.5 * iqr)

# Remove outliers from dataframe
# Keep only the points within the bounds
credit <- credit %>%
  filter(DebtRatio < upper_bound & DebtRatio > lower_bound)

# ______________ MonthlyIncome
summary <- summary(credit$MonthlyIncome)

# Estimate interquartile range
# (3rd quartile minus 1st quartile)

iqr <- summary[[5]] - summary[[2]]

# Identify bounds for outliers
lower_bound <- summary[[2]] - (1.5 * iqr)
upper_bound <- summary[[5]] + (1.5 * iqr)

# Remove outliers from dataframe
# Keep only the points within the bounds
credit <- credit %>%
  filter(MonthlyIncome < upper_bound & MonthlyIncome > lower_bound)

# View boxplot having removed outliers:
credit_outliers <- credit %>%
  gather(all_of(vars), key = "factor", value = "value") %>% 
  group_by(factor)

# Plot
p_credit_outliers <-
  credit_outliers %>%
  ggboxplot(x = "factor", y = "value", palette = "aaas")
suppressMessages(p_credit_outliers)


# Correlation: Only happens to numerical columns.
library(ggcorrplot)
ggcorrplot(round(cor(credit[, sapply(credit, is.factor) == FALSE]), 1), hc.order = TRUE, type = "lower",
           outline.col = "white",
           color = c("#00AFBB", "white", "#FC4E07"),
           lab = TRUE)

# Drop NumberOfOpenCreditLinesAndLoans & NumberRealEstatesLoansOrLines having no relationship with SeriousDlqin2yrs.
credit <- select(credit, -c(NumberOfOpenCreditLinesAndLoans, NumberRealEstateLoansOrLines))

# Check SeriousDlqin2yrs:
unique(credit$SeriousDlqin2yrs)
# Set as factor:
credit$SeriousDlqin2yrs <- as.factor(credit$SeriousDlqin2yrs)

#_______________ Logistic regression
# Split the data into training (75%) and test set (25%)
set.seed(123)
sample <- credit$SeriousDlqin2yrs %>%
  createDataPartition(p = 0.75, list = FALSE)

train.data <- credit[sample, ]
test.data <- credit[-sample, ]

glm_fit <- glm(SeriousDlqin2yrs~., data = train.data, family = binomial(link = "logit") )
summary(glm_fit)
# Note that in spite of the low correlations all variable contribute significantly to predicting Default in payment.


# Predictions:
probabilities  <- glm_fit %>% predict(test.data, type = "response")
pred_target <- ifelse(probabilities > 0.5, 1, 0) 

# Model Prediction Accuracy:___________________________
mean(pred_target == test.data$SeriousDlqin2yrs)
# The classification prediction accuracy is about 93%, which is good. The misclassification error rate is 7% i.e:
# Error rate = 1 - accuracy
mean(pred_target != test.data$SeriousDlqin2yrs)

# Confusion Matrix

table(test.data$SeriousDlqin2yrs, pred_target)


# _______________K-Nearest Neighbors
set.seed(123)
knnmodel <- train(
  SeriousDlqin2yrs~., data = train.data, method = "knn",
  trControl = trainControl("cv", number = 13), 
  preProcess = c("center", "scale"),
  tuneLength = 31
)

plot(knnmodel)
knnmodel$bestTune

#Predictions:
knnpred <- knnmodel %>% predict(test.data)

# Model Accuracy
mean(knnpred == test.data$SeriousDlqin2yrs)