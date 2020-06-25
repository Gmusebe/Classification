#  Classsification
# A set of machine learning methods for predicting the class (or category) of individuals on the basis of one or multiple predictor variables.
# The dependent variable is qualitative  unlike the regression where it is qauntitative.

# Import libraries:
library(tidyverse)
library(visdat)
library(caret)
library(ggpubr)
library(dplyr)


theme_set(theme_pubr())
# Data source: https://archive.ics.uci.edu/ml/datasets/Heart+Disease
class_data <- read.csv("HeartICU.csv")
attach(class_data)

# Data structure:
str(class_data)


cols <- c(2, 3, 6, 7, 9, 14)
class_data[, cols] <- lapply(class_data[, cols], factor)

# ** Set a number of columns as facctors??????

# Check for missing values
class_data[which(is.na(class_data) == TRUE), ] #There are no rows with misssing data
# visualize there is no missing data:
visdat::vis_dat(class_data)


# Correlation: Only happens to numerical columns.
cor(class_data[, sapply(class_data, is.factor) == FALSE])

# ** cass out numerical columns alone and perfomrn a correlation

#_______________ Logistic regression
# Predict wether an individual has heart disease or not based on chest pain type(cp), resting electrocardiographic results(restecg) and resting blood preasure(trestbps).
# i.e target(1 = Yes(has a heart disease), 0 = No(No heart disease))

# # Split the data into training (80%) and test set (20%)
set.seed(123)
train <- class_data$target %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data <- class_data[train,]
test.data <- class_data[-train, ]

glm_fit <- glm(target~., data = class_data, family = binomial(link = "logit") )
summary(glm_fit)

# Model coefficients
coef(glm_fit)


# First of all, we can see that age, restecg and trestbps, chol, fbs and slope are not statistically significant.
# cp i.e chest pain is statistically significant cp2 having the lowest p-value suggesting a strong association of the chest pain
# of the patient with the probability of having heart diesease.
# The positive coefficient for this predictor suggests that all other variables being equal, the patient with chest pain level 2 is more likely to have a heart disease. 


# Predictions:
probabilities  <- glm_fit %>% predict(test.data, type = "response")
contrasts(class_data$target)
pred_target <- ifelse(probabilities > 0.5, 1, 0) 
test.data$Pred_target = pred_target

# Model Prediction Accuracy:___________________________
# A measure how good the model is in predicting the outcome of new observations test data that have been not used to train the model.
# : the performance of the predictive model can be assessed by comparing the predicted outcome values against the known outcome values.
# The overall classification accuracy rate corresponds to the proportion of observations that have been correctly classified.
# The classification error rate is defined as the proportion of observations that have been misclassified:
# Error rate = 1 - accuracy
mean(pred_target == test.data$target)
# The classification prediction accuracy is about 85%, which is good. The misclassification error rate is 15% i.e:
mean(pred_target != test.data$target)

# Confusion Matrix
library(epiDisplay)
table(test.data$target, pred_target)


# In this chapter, we have described how logistic regression works and we have provided R codes to compute logistic regression.
# Additionally, we demonstrated how to make predictions and to assess the model accuracy. Logistic regression model output is 
# very easy to interpret compared to other classification methods. Additionally, because of its simplicity it is less prone to 
# overfitting than flexible methods such as decision trees.


# Discriminant Analysis
# Discriminant analysis is used to predict the probability of belonging to a given class (or category) based on one or multiple predictor variables.
# Compared to logistic regression, the discriminant analysis is more suitable for predicting the category of an observation in the situation where the outcome variable contains more than two classes.
# it's more stable than the logistic regression for multi-class classification problems.


# Estimating preprocessing parameters
Pre_param <- train.data %>% 
  preProcess(method = c("center", "scale"))

# # Transform the data using the estimated parameters
train.trans <- Pre_param %>% 
  predict(train.data)

test.trans <- Pre_param %>% 
  predict(test.data)

# _______________Linear Discriminant Analysis
# The LDA algorithm starts by finding directions that maximize the separation between classes, then use these directions to predict the class of individuals.
# LDA assumes that predictors are normally distributed (Gaussian distribution)

library(MASS)
lda.fit <- lda(target~., data = train.trans)
lda.fit

# LDA determines group means and computes, for each individual, the probability of belonging to the different groups. The individual is then affected to the group with the highest probability score.

plot(lda.fit)

# Predictions:
lda_pred <- lda.fit %>% predict(test.trans)
names(lda_pred)

# Model Accuracy
mean(lda_pred$class == test.trans$target)
# The classification prediction accuracy is about 82%, which is good. The misclassification error rate is 18%.



# _______________Quadratic Discriminant Analysis
# QDA is little bit more flexible than LDA, in the sense that it does not assumes the equality of variance/covariance. 
# In other words, for QDA the covariance matrix can be different for each class.

# LDA tends to be a better than QDA when you have a small training set.
qda.fit <- qda(target~., data = train.trans)
qda.fit

# LDA determines group means and computes, for each individual, the probability of belonging to the different groups. The individual is then affected to the group with the highest probability score.

# Predictions:
qda_pred <- qda.fit %>% predict(test.trans)

# Model Accuracy
mean(qda_pred$class == test.trans$target)
# The classification prediction accuracy is about 82%, which is good. The misclassification error rate is 18%.

# _______________K-Nearest Neighbors
# The k-nearest neighbors (KNN) algorithm is a simple machine learning method used for both classification and regression.
# The kNN algorithm predicts the outcome of a new observation by comparing it to k similar cases in the training data set, 
# where k is defined by the analyst.

# To classify a given new observation (new_obs), the k-nearest neighbors method starts by identifying the k most similar 
# training observations (i.e. neighbors) to our new_obs, and then assigns new_obs to the class containing the majority of its neighbors.

set.seed(123)
knnmodel <- train(
  target~., data = train.data, method = "knn",
  trControl = trainControl("cv", number = 13), 
  preProcess = c("center", "scale"),
  tuneLength = 31
)

plot(knnmodel)
knnmodel$bestTune

#Predictions:
knnpred <- knnmodel %>% predict(test.data)

# Model Accuracy
mean(knnpred == test.data$target)