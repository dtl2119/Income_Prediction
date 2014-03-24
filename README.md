********************************************************************************************
* Drew Limm (dtl2119)
* STAT4249 - Data Science
* Income_Prediction: Machine Learning and Classification Methods to predict Person's Income
*
********************************************************************************************

Given a data set extracted from the census bureau database, our job was to predict whether or not a person makes over $50K a year.  The data set contains the following variables: age, workclass, fnlwgt, education, marital-status, occupation, relationship, race, sex, capital-gain, capital-loss, hours-per-week, native-country, and a binary value indicating if the subject made over $50K a year. We separated the data set, using the first half as training data, and the second half to test.  We each explored different classification and regression methods, but kept our data manipulation and dummy variable construction consistent.


***Data Loading and Manipulation***
We first wrote a script to manipulate the data into a manageable form.  This involved loading the data from the provided URL and then splitting the data into two groups, test and train.  Each group held half of the data.  Next we converted categorical data into matrices of dummy variables so that we could run a selection of classification techniques.  Here's how we broke down our data set:
                        Train             Test
Rows(Observations):     16281             16280
Columns (Variables):    15                15




***Personal Contribution***
I explored the Ridge and Lasso Regression models using the glmnet package in R.  I was able to find the lambda value giving the least error on the train data, and then used the model on our test data.  Using the ggplot2 package I was able to create visualizations of coefficient profiles as well as the importance of certain variables (i.e. which variables were the highest indicators of predicting whether a subject made over $50K/yr)


***Team Contribution***
My teammates explored several other regresion methods: Naive Bayes, Linear Discriminant Analysis, OLS, SVM, KNN, and Random Forest.  Because our test data already indicated if the subject made over $50K/yr (we already knew the answer), we were able to gather test misclassification rates for each method and conclude which one was best.
