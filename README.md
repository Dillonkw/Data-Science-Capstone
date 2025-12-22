# Predicting Healthy BMI
### Kyle Dillon  

## Overview  
The goal of this project is to use various datasets from the National Health and Nutrition Examination Survey to identify adults who have a healthy BMI. Also, to understand which nutrients are the most important in making this decision? Data from 2017-2020 and 2021-2023 have been used in conjunction to help achieve this goal. A Principal Component Analysis has been used for dimensionality reduction, and Logistic Regression, Random Forest, and XGBoost models were used to analyze patterns and make predictions to try and predict healthy BMI. By the end, we should have a clear vision that helps us to answer the research question, hypothesis, and prediction.  

## Defined Research Question  
Can we predict which adults have a healthy BMI primarily based on their nutrient intake, along with other demographic, laboratory, and questionnaire variables from the National Health and Nutrition Examination Survey? Which nutrients play the biggest role in making this decision?

## Hypothesis  
Nutrition intake, along with demographic, questionnaire, and laboratory variables, will be able to predict if an adult has a healthy BMI because these variables are indicators of dietary behaviors, lifestyle patterns, and medical history, which affect a person's weight.

## Prediction  
Adults who have a balanced nutritional intake, a strong lifestyle, and a stable medical history will have a healthy BMI. We predict that a combination of macronutrients, including fat, protein, and carbs, will emerge as some of the variables that have the strongest effect on BMI, as exemplified by the machine learning model’s feature importance.
  

## Data Sets  
National Health and Nutrition Examination Survey (NHANES)  
(https://wwwn.cdc.gov/nchs/nhanes/)  

Observations: 17,721 | Variables: 108  

### Outcome variable  
BMI_Binary  
Healthy >= 18.5 & <= 24.9  
Unhealthy = Everything else

## Stakeholder  
Department of Public Health (DPH)

## Custom Function  
Sum_func <- function(a,b) {  
  ifelse(is.na(a) & is.na(b), NA, rowSums(cbind(a,b), na.rm = TRUE))  
  }  
### Apply Function  
Total = Sum_func(Var1, Var2)    

- Function to add the features together     
- If one has a value and the other has a NA, then the one value will be used    
- If both have value it will add the value together   
- If both are NA, it keeps the total value as NA for that row

### Identify missing values function  
num_missing <- sapply(CleanedData, function(x) sum(is.na(x)))  


## How to run code  
To run the code first, go into the code folder and select which file you want to run. Then go into the Raw data folder and download the necessary raw data files. After, go back to the file you selected and install the packages provided. Then set the working directory and run the code.     






