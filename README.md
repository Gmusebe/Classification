# Classification
Predictive Models for Qualitative Variable(s)

**Classification** refers to a set of machine learning methods for predicting the class (or category) of individuals on the basis of one or multiple predictor variables.

Most of the classification algorithms computes the probability of belonging to a given class. Observations are then assigned to the class that have the highest probability score.

The methods used in this project are:

  a. Logistic regression
  
  b. Dicriminant Analysis;
  
      i)  Linear
      
      ii) Quadratic &
      
  c. K-Nearest Neigbors
  
 The data used in the exercising the above methods are extracted from:
 
    1. Kaggle: The **Heart Disease UCI**
       Link: https://archive.ics.uci.edu/ml/datasets/Heart+Disease
 The data is used to detect wether or not a patient has a heart disease or not. 
       
    2. Github: **Credit Default**
        Link: https://github.com/Apress/pro-machine-learning-algorithms/blob/master/Logistic%20regression/credit_training.csv
In an attempt to re-write the python analysis into R, visualisation and feature engineering are the major part in the R analysis. 


## Model Accuracy
Having a variety of classification methods, to distinguish and choose the best method to use in analysis of data, model accuracy is required: that is need to estimate the model prediction accuracy and prediction errors using a new test data set.

You need to evaluate the performance of the model, that is estimate the model prediction accuracy and prediction errors using a new test data set. 

The methods for assessing the performance of predictive classification methods  include:

   **Average classification accuracy** , representing the proportion of correctly classified observations.
  
   **Confusion matrix** , which is 2x2 table showing four parameters, including the number of true positives, true negatives, false negatives and false positives.
  
   **Precision, Recall and Specificity**, which are three major performance metrics describing a predictive classification model
  
   **ROC curve**, which is a graphical summary of the overall performance of the model, showing the proportion of true positives and false positives at all possible values of probability cutoff. **The Area Under the Curve (AUC)** summarizes the overall performance of the classifier.
