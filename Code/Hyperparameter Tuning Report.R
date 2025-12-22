#Kyle Dillon
#Capstone Project (Hyperparameter Tuning and Model Evaluation Report)
#Predicting Healthy BMI


install.packages("tidyverse")
install.packages("haven")
install.packages("tidymodels")
install.packages("mice")
install.packages("glmnet")
install.packages("fastDummies")
install.packages("xgboost")
install.packages("ggplot2")
install.packages("MESS")
install.packages("xgboost")
install.packages("tibble")
install.packages("themis")
install.packages("caret")
install.packages("corrplot")
install.packages("shapviz")
install.packages("parsnip")
install.packages("probably")
install.packages("dials")



library(tidyverse)
library(haven)
library(tidymodels)
library(mice)
library(glmnet)
library(fastDummies)
library(xgboost)
library(ggplot2)
library(xgboost)
library(tibble)
library(themis)
library(caret)
library(corrplot)
library(shapviz)
library(parsnip)
library(probably)
library(dials)


setwd("/Users/kyledillon/Documents/GitHub/Data-Science-Capstone") #Working directory for laptop
setwd("C:/Users/kboom/OneDrive/Documents/GitHub/Data-Science-Capstone") #Working directory for desktop computer


Seed <- 50


#2021-2023 Data
BMXData_21_23 <- read_xpt("RawData/BMX_L.xpt") #Body Measurements Data
BPQData_21_23 <- read_xpt("RawData/BPQ_L.xpt") #Blood Pressure and Cholesterol Data
DemoData_21_23 <- read_xpt("RawData/DEMO_L.xpt") #Demographic Data
DR1TOTData_21_23 <- read_xpt("RawData/DR1TOT_L.xpt") #Dietary Interview - Total Nutrient Intakes, First Day
DSQTOTData_21_23 <- read_xpt("RawData/DSQTOT_L.xpt") #Dietary Supplement Use 30-Day - Total Dietary Supplements
GHBData_21_23 <- read_xpt("RawData/GHB_L.xpt") #Glycohemoglobin
GLUData_21_23 <- read_xpt("RawData/GLU_L.xpt") #Plasma Fasting Glucose
HIQData_21_23 <- read_xpt("RawData/HIQ_L.xpt") #Health Insurance
INSData_21_23 <- read_xpt("RawData/INS_L.xpt") #Insulin
MCQData_21_23 <- read_xpt("RawData/MCQ_L.xpt") #Medical Conditions
PAQData_21_23 <- read_xpt("RawData/PAQ_L.xpt") #Physical Activity
SLQData_21_23 <- read_xpt("RawData/SLQ_L.xpt") #Sleep Disorders
UCPREGData_21_23 <- read_xpt("RawData/UCPREG_L.xpt") #Urine Pregnancy Test
WHQData_21_23 <- read_xpt("RawData/WHQ_L.xpt") #Weight History	


#Puts all the individual datasets into one list called datasets_21_23
datasets_21_23 <- list( 
  BMXData_21_23,
  BPQData_21_23,
  DemoData_21_23,
  DR1TOTData_21_23,
  DSQTOTData_21_23,
  GHBData_21_23,
  GLUData_21_23,
  HIQData_21_23,
  INSData_21_23,
  MCQData_21_23,
  PAQData_21_23,
  SLQData_21_23,
  UCPREGData_21_23,
  WHQData_21_23
)


FullData_21_23 <- reduce(datasets_21_23, full_join, by = "SEQN") #uses the reduce function to full join the 2021-2023 datasets list by the SEQN variable


FullData_21_23$Survey_Year <- "2021-2023" 


#2017-2020 Data
BMXData_17_20 <- read_xpt("RawData/P_BMX.xpt") #Body Measurements Data
BPQData_17_20 <- read_xpt("RawData/P_BPQ.xpt") #Blood Pressure and Cholesterol Data
DemoData_17_20 <- read_xpt("RawData/P_DEMO.xpt") #Demographic Data
DR1TOTData_17_20 <- read_xpt("RawData/P_DR1TOT.xpt") #Dietary Interview - Total Nutrient Intakes, First Day
DSQTOTData_17_20 <- read_xpt("RawData/P_DSQTOT.xpt") #Dietary Supplement Use 30-Day - Total Dietary Supplements
GHBData_17_20 <- read_xpt("RawData/P_GHB.xpt") #Glycohemoglobin
GLUData_17_20 <- read_xpt("RawData/P_GLU.xpt") #Plasma Fasting Glucose
HIQData_17_20 <- read_xpt("RawData/P_HIQ.xpt") #Health Insurance
INSData_17_20 <- read_xpt("RawData/P_INS.xpt") #Insulin
MCQData_17_20 <- read_xpt("RawData/P_MCQ.xpt") #Medical Conditions
PAQData_17_20 <- read_xpt("RawData/P_PAQ.xpt") #Physical Activity
SLQData_17_20 <- read_xpt("RawData/P_SLQ.xpt") #Sleep Disorders
UCPREGData_17_20 <- read_xpt("RawData/P_UCPREG.xpt") #Urine Pregnancy Test
WHQData_17_20 <- read_xpt("RawData/P_WHQ.xpt") #Weight History



#Puts all the individual datasets into one list called datasets_17_20
datasets_17_20 <- list( 
  BMXData_17_20,
  BPQData_17_20,
  DemoData_17_20,
  DR1TOTData_17_20,
  DSQTOTData_17_20,
  GHBData_17_20,
  GLUData_17_20,
  HIQData_17_20,
  INSData_17_20,
  MCQData_17_20,
  PAQData_17_20,
  SLQData_17_20,
  UCPREGData_17_20,
  WHQData_17_20
)


#uses the reduce function to full join the 2017-2020 datasets list by the SEQN variable
FullData_17_20 <- reduce(datasets_17_20, full_join, by = "SEQN")


FullData_17_20$Survey_Year <- "2017-2020" 


ComData <- bind_rows(FullData_17_20, FullData_21_23)


CleanedData <- ComData %>% #Storing the full data into a new dataset called CleanedData
  filter(RIDAGEYR >= 18) %>% #Only includes people that are above the age of 18
  filter(URXPREG != 1 | is.na(URXPREG)) #


CleanedData <- CleanedData %>% #Selecting all the variable that are relevant to the research question
  select(BMXBMI, RIDAGEYR, RIAGENDR, RIDRETH1, DMDBORN4, DMDEDUC2, DMDMARTZ, DMDHHSIZ, 
         INDFMPIR,DR1TKCAL, DR1TPROT, DR1TCARB, DR1TFIBE, DR1TTFAT, DR1TCHOL, DR1TATOC, 
         DR1TVARA, DR1TVB1, DR1TVB2, DR1TNIAC, DR1TVB6, DR1TFDFE, DR1TCHL, DR1TVB12, 
         DR1TVC, DR1TVD, DR1TVK, DR1TCALC, DR1TPHOS, DR1TMAGN, DR1TIRON, DR1TZINC, 
         DR1TFOLA, DR1TB12A, DR1TFA, DR1TCOPP, DR1TSODI, DR1TPOTA, DR1TSELE, DR1TCAFF, 
         DR1TALCO, DR1TMOIS, DR1_320Z, DR1TSUGR, DR1TSFAT, DR1TMFAT, DR1TPFAT, DR1TATOA, 
         DR1TRET, DR1TACAR, DR1TBCAR, DR1TCRYP, DR1TLYCO, DR1TLZ, DR1TFA, DR1TFF, DR1TTHEO, 
         DSQTKCAL, DSQTPROT, DSQTCARB, DSQTFIBE, DSQTTFAT, DSQTCHOL, DSQTVB1, DSQTVB2, DSQTNIAC, 
         DSQTVB6, DSQTFDFE, DSQTCHL, DSQTVB12, DSQTVC, DSQTVK, DSQTVD, DSQTCALC, DSQTPHOS, 
         DSQTMAGN, DSQTIRON, DSQTZINC, DSQTCOPP, DSQTSODI, DSQTPOTA, DSQTSELE, DSQTCAFF, DSQTSUGR, 
         DSQTSFAT, DSQTMFAT, DSQTPFAT, DSQTLYCO, DSQTLZ, DSQTFA, DSQTIODI, 
         LBXGLU, LBXIN, LBXGH, HIQ011, MCQ160M, MCQ160L, MCQ053, BPQ101D, 
         BPQ020, BPQ030, SLD012, SLD013, PAD800, PAD820, PAD680, WHQ070, MCQ550, Survey_Year)


sum(duplicated(CleanedData)) #Check for duplicate rows
#After running this, we can see there are no duplicate rows 


#Converting 7, 9, 77, 99, 7777, 9999, which mean don't know and refused to answer, to NA.
CleanedData <- CleanedData %>%
  mutate(across(c( DMDBORN4, DMDEDUC2, DMDMARTZ,HIQ011, MCQ160M, MCQ550, MCQ160L, MCQ053, BPQ101D, BPQ020, BPQ030, PAD800, PAD820, PAD680, WHQ070),
                ~ replace(., .%in% c(7,9,77,99,7777,9999), NA)))


#Renaming columns
CleanedData <- CleanedData %>%
  rename(
    Age = RIDAGEYR, #Demographic Features
    Gender = RIAGENDR,
    Race = RIDRETH1,
    Country_of_Origin = DMDBORN4,
    Education_Level = DMDEDUC2,
    Marital_Status = DMDMARTZ,
    Household_Size = DMDHHSIZ,
    Family_Income_to_Poverty_Ratio = INDFMPIR,
    Vitamin_E_Alpha_Tocopherol = DR1TATOC, #Total Nutrition Intake Features
    Added_Alpha_Tocopherol = DR1TATOA,
    Retinol = DR1TRET,
    Vitamin_A = DR1TVARA,
    Alpha_Carotene = DR1TACAR,
    Beta_Carotene = DR1TBCAR,
    Beta_Cryptoxanthin = DR1TCRYP,
    Total_Folate = DR1TFOLA,
    Food_Folate = DR1TFF,
    Added_Vitamin_B12 = DR1TB12A,
    Theobromine = DR1TTHEO,
    Alcohol = DR1TALCO,
    Moisture = DR1TMOIS,
    Total_Plain_Water = DR1_320Z,
    Iodine = DSQTIODI,
    Plasma_Fasting_Glucose = LBXGLU, # Laboratory Features
    Insulin = LBXIN,
    Glycohemoglobin = LBXGH,
    Health_Insurance = HIQ011, # Questionnaire Features
    Thyroid_Problem = MCQ160M,
    Liver_Condition = MCQ160L,
    Taking_Treatment_Anemia = MCQ053,
    Gallstones = MCQ550,
    Blood_Cholesterol_Meds = BPQ101D,
    Told_High_BP = BPQ020,
    Told_High_BP_2 = BPQ030,
    Sleep_Hours_Weekdays = SLD012,
    Sleep_Hours_Weekends = SLD013,
    Minutes_Moderate_LTPA = PAD800,
    Minutes_Vigorous_LTPA = PAD820,
    Minutes_Sedentary_Activity = PAD680,
    Attempted_Weight_Loss_Past_Year = WHQ070
  )


#Change features that are labeled as numeric but are actually categorical to factor 
CleanedData <- CleanedData %>% 
  mutate(
    Gender = factor(Gender, 
                    levels = c(1,2), 
                    labels = c("Male", "Female")),
    Race = factor(Race, 
                  levels = c(1,2,3,4,5), 
                  labels = c("Mexican American", "Other Hispanic", "Non-Hispanic White", 
                             "Non-Hispanic Black", "Other Race")),
    Country_of_Origin = factor(Country_of_Origin, 
                               levels = c(1,2), 
                               labels = c("Born in United States", "Others")),
    Education_Level = factor(Education_Level, 
                             levels = c(1,2,3,4,5), 
                             labels = c("Less than 9th grade", "9-11th grade(12th with no diploma)", "High school graduate/GED", 
                                        "Some college or AA Degree", "College graduate or above")),
    Marital_Status = factor(Marital_Status, 
                            levels = c(1,2,3), 
                            labels = c("Married/Living with partner", "Widowed/Divorced/Seperated", "Never married")),
    Health_Insurance = factor(Health_Insurance, 
                              levels = c(1,2), 
                              labels = c("Yes", "No")),
    Thyroid_Problem = factor(Thyroid_Problem, 
                             levels = c(1,2), 
                             labels = c("Yes", "No")),
    Gallstones = factor(Gallstones, 
                        levels = c(1,2), 
                        labels = c("Yes", "No")),
    Liver_Condition = factor(Liver_Condition, 
                             levels = c(1,2), 
                             labels = c("Yes", "No")),
    Taking_Treatment_Anemia = factor(Taking_Treatment_Anemia, 
                                     levels = c(1,2), 
                                     labels = c("Yes", "No")),
    Blood_Cholesterol_Meds = factor(Blood_Cholesterol_Meds, 
                                    levels = c(1,2), 
                                    labels = c("Yes", "No")),
    Told_High_BP = factor(Told_High_BP, 
                          levels = c(1,2), 
                          labels = c("Yes", "No")),
    Told_High_BP_2 = factor(Told_High_BP_2, 
                            levels = c(1,2), 
                            labels = c("Yes", "No")),
    Attempted_Weight_Loss_Past_Year = factor(Attempted_Weight_Loss_Past_Year, 
                                             levels = c(1,2), 
                                             labels = c("Yes", "No"))
  )





#Function to add the features together. 
#If one has a value and the other has a NA, then the one value will be used. 
#If both have value it will add the value together. 
#If both are NA, it keeps the total value as NA for that row. 
Sum_func <- function(a,b) {
  ifelse(is.na(a) & is.na(b), NA, rowSums(cbind(a,b), na.rm = TRUE))
}


CleanedData <- CleanedData %>%
  mutate(
    Total_Energy = Sum_func(DR1TKCAL, DSQTKCAL),
    Total_Protein = Sum_func(DR1TPROT, DSQTPROT),
    Total_Carbs = Sum_func(DR1TCARB, DSQTCARB),
    Total_Sugar = Sum_func(DR1TSUGR, DSQTSUGR),
    Total_Fiber = Sum_func(DR1TFIBE, DSQTFIBE),
    Total_Fat = Sum_func(DR1TTFAT, DSQTTFAT),
    Total_Sat_Fat = Sum_func(DR1TSFAT, DSQTSFAT),
    Total_Mono_Sat_Fat = Sum_func(DR1TMFAT, DSQTMFAT),
    Total_Poly_Sat_Fat = Sum_func(DR1TPFAT, DSQTPFAT),
    Total_Cholesterol = Sum_func(DR1TCHOL, DSQTCHOL),
    Total_Lycopene = Sum_func(DR1TLYCO, DSQTLYCO),
    Total_Lutein_Zeaxanthin = Sum_func(DR1TLZ, DSQTLZ),
    Total_Thiamin = Sum_func(DR1TVB1, DSQTVB1),
    Total_Riboflavin = Sum_func(DR1TVB2, DSQTVB2),
    Total_Niacin = Sum_func(DR1TNIAC, DSQTNIAC),
    Total_VitaminB6 = Sum_func(DR1TVB6, DSQTVB6),
    Total_Folic_Acid = Sum_func(DR1TFA, DSQTFA),
    Total_Folate_DFE = Sum_func(DR1TFDFE, DSQTFDFE),
    Total_Choline = Sum_func(DR1TCHL, DSQTCHL),
    Total_VitaminB12 = Sum_func(DR1TVB12, DSQTVB12),
    Total_VitaminC = Sum_func(DR1TVC, DSQTVC),
    Total_VitaminK = Sum_func(DR1TVK, DSQTVK),
    Total_VitaminD = Sum_func(DR1TVD, DSQTVD),
    Total_Calcium = Sum_func(DR1TCALC, DSQTCALC),
    Total_Phosphorus = Sum_func(DR1TPHOS, DSQTPHOS),
    Total_Magnesium = Sum_func(DR1TMAGN, DSQTMAGN),
    Total_Iron = Sum_func(DR1TIRON, DSQTIRON),
    Total_Zinc = Sum_func(DR1TZINC, DSQTZINC),
    Total_Copper = Sum_func(DR1TCOPP, DSQTCOPP),
    Total_Sodium = Sum_func(DR1TSODI, DSQTSODI),
    Total_Potassium = Sum_func(DR1TPOTA, DSQTPOTA),
    Total_Selenium = Sum_func(DR1TSELE, DSQTSELE),
    Total_Caffeine = Sum_func(DR1TCAFF, DSQTCAFF)
  ) %>% 
  select( 
    - starts_with("DR1T"), # Removes original features and only keeps the total feature column 
    - starts_with("DSQ")
  )


#Creating Target Variable
CleanedData$BMXBMI <- as.numeric(CleanedData$BMXBMI)

CleanedData <- CleanedData %>%
  mutate(BMI = ifelse(BMXBMI >= 18.5 & BMXBMI <= 24.9, "Healthy", "Unhealthy"),
         BMI_Binary = ifelse(BMI == "Healthy", 1,0))

table(CleanedData$BMI, useNA = "ifany")



saveRDS(CleanedData, "CleanedDataNew1.rds")



#Removes missing values from the target variable
CleanedData <- CleanedData %>% 
  filter(!is.na(BMI))

table(CleanedData$BMI, useNA = "ifany")


#Identify total missing values for each varibale
num_missing <- sapply(CleanedData, function(x) sum(is.na(x)))
num_missing


#Initial feature removal (Removal of redundant variables and variables with substantial rows missing)
CleanedData <- CleanedData %>% 
  select(- BMXBMI, -BMI, -Country_of_Origin, -Household_Size, -Iodine, -Insulin, 
         -Plasma_Fasting_Glucose, -Minutes_Moderate_LTPA, -Minutes_Vigorous_LTPA, 
         -Blood_Cholesterol_Meds, -Told_High_BP_2, -Attempted_Weight_Loss_Past_Year)



#Calculating the train/ test split 
#Using the calculation provided by Dr. Geist to find optimal train/ test split
calcSplitRatio <- function(p = NA, df) {
  ## @p  = the number of parameters. by default, if none are provided, the number of columns (predictors) in the dataset are used
  ## @df = the dataframe that will be used for the analysis
  
  ## If the number of parameters isn't supplied, set it to the number of features minus 1 for the target
  if(is.na(p)) {
    p <- ncol(df) -1   
  }
  
  ## Calculate the ideal number of testing set
  test_N <- (1/sqrt(p))*nrow(df)
  ## Turn that into a testing proportion
  test_prop <- round((1/sqrt(p))*nrow(df)/nrow(df), 2)
  ## And find the training proportion
  train_prop <- 1-test_prop
  
  ## Tell us the results!
  print(paste0("The ideal split ratio is ", train_prop, ":", test_prop, " training:testing"))
  
  ## Return the size of the training set
  return(train_prop)
}


train_prop <- calcSplitRatio(p = 65, df = CleanedData)


#Setting the train/ test split with a 80/20 split
set.seed(Seed)
split <- initial_split(CleanedData, strata = BMI_Binary, prop = 0.80)

train_data <- training(split)
test_data <- testing(split)


#Converting target variable (BMI_Binary) to a factor variable
train_data$BMI_Binary <- factor(train_data$BMI_Binary,
                                levels = c(1, 0),
                                labels = c("Healthy", "Unhealthy"))

test_data$BMI_Binary <- factor(test_data$BMI_Binary,
                               levels = c(1, 0),
                               labels = c("Healthy", "Unhealthy"))




#fastDummies to encode categorical features
cat_variables <- setdiff(names(Filter(is.factor, train_data)), "BMI_Binary")

train_dummy <- dummy_cols(train_data,
                          select_columns = cat_variables,
                          remove_first_dummy = TRUE,
                          ignore_na = TRUE,
                          remove_selected_columns = TRUE
)

test_dummy <- dummy_cols(test_data,
                         select_columns = cat_variables,
                         remove_first_dummy = TRUE,
                         ignore_na = TRUE,
                         remove_selected_columns = TRUE
)


#Mice imputation for Na values
set.seed(Seed)
mice_train <- mice(train_dummy, m = 1, method = "pmm")
train_imputed <- complete(mice_train)

set.seed(Seed)
mice_test <- mice(test_dummy, m = 1, method = "pmm", seed = 22)
test_imputed <- complete(mice_test)


#Correlation matrix 
numeric_data <- train_imputed %>% 
  select(where(is.numeric))

corr_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

top_correlation <- corr_matrix %>%
  as.data.frame() %>%
  rownames_to_column(var = "Feature1") %>%
  pivot_longer(-Feature1, names_to = "Feature2", values_to = "Correlation") %>%
  filter(Feature1 != Feature2) %>%
  filter(abs(Correlation) > 0.75) %>%
  arrange(desc(abs(Correlation))) 


#Correlation plot
variables_to_keep <- unique(c(top_correlation$Feature1, top_correlation$Feature2))
strong_corr_matrix <- corr_matrix[variables_to_keep, variables_to_keep]


corrplot(strong_corr_matrix, 
         method = "color",
         type = "upper",   
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         diag = FALSE,
         is.corr = TRUE)   



#Removing highly correlated features
train_data_clean <- train_imputed %>% 
  select(-Total_Folic_Acid, -Total_Energy, -Total_Fat, 
         -Total_Carbs, -Total_Choline, -Total_Phosphorus, -Food_Folate)

test_data_clean <- test_imputed %>% 
  select(names(train_data_clean))


#PCA Test
numeric_train <- train_data_clean %>% 
  select(where(is.numeric))

numeric_test <- test_data_clean %>% 
  select(where(is.numeric))

variances <- sapply(numeric_train, var)
variances

variances[variances == 0]


PCA <- prcomp(numeric_train, center = TRUE, scale. = TRUE)

summary(PCA)

plot(PCA, type = "lines", main = "Scree Plot")



#Checking for skewed varibales
train_data_clean %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), ~ e1071::skewness(.)))



log_columns <- c(
  "Total_VitaminC", "Total_VitaminK", "Total_Lutein_Zeaxanthin", "Retinol", 
  "Total_Riboflavin", "Total_Niacin", "Total_VitaminB6", "Beta_Cryptoxanthin", 
  "Added_Vitamin_B12", "Vitamin_A", "Added_Alpha_Tocopherol", "Alpha_Carotene",
  "Alcohol", "Theobromine", "Vitamin_E_Alpha_Tocopherol", "Total_Lycopene")


#Set the recipe
BMI_recipe <- recipe(BMI_Binary ~ ., data = train_data_clean) %>%
  step_rm(Survey_Year) %>% 
  step_log(any_of(log_columns), base = 10, offset = 1) %>%
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors())



#Creating cross validation folds and stratifying it by the outcome variable
set.seed(Seed) 
cv_folds <- vfold_cv(train_data_clean, v= 5, strata = BMI_Binary)


#Establishing metrics to apply to the models
my_metrics <- metric_set(
  yardstick::bal_accuracy,
  yardstick::sensitivity,
  yardstick::specificity,
  yardstick::accuracy, 
  yardstick::precision, 
  yardstick::recall, 
  yardstick::f_meas, 
  yardstick::pr_auc)


#Supervised Models

#Logistic Regression
#Create the model
logreg_conf <- 
  logistic_reg(
    penalty = tune(),
    mixture = tune(),
  ) %>%
  set_engine("glmnet") %>% 
  set_mode("classification")


#Workflow
logreg_workflow <- 
  workflow() %>% 
  add_model(logreg_conf) %>% 
  add_recipe(BMI_recipe)


#Grid space filling
logreg_grid <- 
  grid_regular(
    penalty(),
    mixture(),
    levels = 10
  )


#Tune the model
set.seed(Seed)
logreg_tune <- tune_grid(
  logreg_workflow,
  resamples = cv_folds,
  grid = logreg_grid,
  metrics = my_metrics,
  control = control_resamples(save_pred = TRUE)
)

collect_metrics(logreg_tune)
logreg_preds_cv <- collect_predictions(logreg_tune)

logreg_best_metrics <- logreg_tune %>% 
  show_best(metric = "pr_auc", n = 1)


#Select the best Hyperparameters
logreg_best <- select_best(logreg_tune, metric = "pr_auc")
logreg_best


#Fit on the whole training data
logreg_workflow_final <- finalize_workflow(logreg_workflow, logreg_best)

logreg_train_full <- fit(
  logreg_workflow_final,
  data = train_data_clean
)


#Models on the test set
logreg_test_results <- predict(logreg_train_full, test_data_clean, type = "prob") %>% 
  bind_cols(predict(logreg_train_full, test_data_clean, type = "class")) %>% 
  bind_cols(test_data_clean %>%
              select(BMI_Binary))


#Test set metrics
logreg_test_metrics <- my_metrics(
  data = logreg_test_results,
  truth = BMI_Binary,
  estimate = .pred_class,
  .pred_Healthy
)
logreg_test_metrics


#Confusion Matrix
logreg_conf_matrix <- conf_mat(logreg_test_results, truth = BMI_Binary, estimate = .pred_class)
autoplot(logreg_conf_matrix, type = "heatmap")+
  theme_minimal()



#ROC Curve test set 
roc_logreg <- data.frame(threshold = seq(1,0,-0.01), fpr = 0, tpr = 0)
for (i in roc_logreg$threshold){
  
  over_threshold <- logreg_test_results[logreg_test_results$.pred_Healthy >= i, ]
  
  fpr <- sum(over_threshold$BMI_Binary == "Unhealthy")/sum(logreg_test_results$BMI_Binary == "Unhealthy")
  roc_logreg[roc_logreg$threshold==i, "fpr"] <- fpr
  
  tpr <- sum(over_threshold$BMI_Binary == "Healthy")/sum(logreg_test_results$BMI_Binary == "Healthy")
  roc_logreg[roc_logreg$threshold==i, "tpr"] <- tpr
  
}

ggplot()+
  geom_line(data = roc_logreg, aes(x = fpr, y = tpr, color = threshold), linewidth = 2)+
  scale_color_gradientn(colors = rainbow(3))+
  geom_abline(intercept = 0, slope = 1, lty = 2)+
  geom_point(data = roc_logreg[seq(1, 101, 10), ], aes(x = fpr, y = tpr))+
  geom_text(data = roc_logreg[seq(1, 101, 10), ],
            aes(x = fpr, y = tpr, label = threshold, hjust = 1.2, vjust = -0.2))+
  labs(x = "False Positive Rate",
       y = "True Positive Rate",
       title = "Logistic Regression ROC Curve",
       color = "Threshold")+
  theme_minimal()




#Random Forest

#Create the model
rf_conf <- 
  rand_forest(
    trees = tune(),
    mtry = tune(),
    min_n = tune()
  ) %>% 
  set_engine("ranger", 
             importance = "impurity",
             seed = Seed)%>% 
  set_mode("classification")


#Workflow
rf_workflow <- 
  workflow() %>% 
  add_model(rf_conf) %>% 
  add_recipe(BMI_recipe)


#Grid space filling
rf_grid <- 
  grid_space_filling(
    trees(range = c(500, 1000)),
    mtry(range = c(2, ncol(train_data_clean) -1)),
    min_n(range = c(5, 30)),
    size = 15
  )


#Tune the model
set.seed(Seed)
rf_tune <- tune_grid(
  rf_workflow,
  resamples = cv_folds,
  grid = rf_grid,
  metrics = my_metrics,
  control = control_resamples(save_pred = TRUE)
)


collect_metrics(rf_tune)
rf_preds_cv <- collect_predictions(rf_tune)

rf_best_metrics <- rf_tune %>%
  show_best(metric = "pr_auc", n = 1)


#Select the best hyperparameters
rf_best <- select_best(rf_tune, metric = "pr_auc")
rf_best


#Fit on the whole training data
rf_workflow_final <- finalize_workflow(rf_workflow, rf_best)

rf_train_full <- fit(
  rf_workflow_final,
  data = train_data_clean
)


#Model on the test set
rf_test_results <- predict(rf_train_full, test_data_clean, type = "prob") %>% 
  bind_cols(predict(rf_train_full, test_data_clean, type = "class")) %>% 
  bind_cols(test_data_clean %>%
              select(BMI_Binary))


#Test set metrics
rf_test_metrics <- my_metrics(
  data = rf_test_results,
  truth = BMI_Binary,
  estimate = .pred_class,
  .pred_Healthy
)
rf_test_metrics


#Confusion Matrix
rf_conf_matrix <- conf_mat(rf_test_results, truth = BMI_Binary, estimate = .pred_class)
autoplot(rf_conf_matrix, type = "heatmap")+
  theme_minimal()


#ROC Curve test set 
roc_rf <- data.frame(threshold = seq(1,0,-0.01), fpr = 0, tpr = 0)
for (i in roc_rf$threshold){
  
  over_threshold <- rf_test_results[rf_test_results$.pred_Healthy >= i, ]
  
  fpr <- sum(over_threshold$BMI_Binary == "Unhealthy")/sum(rf_test_results$BMI_Binary == "Unhealthy")
  roc_rf[roc_rf$threshold==i, "fpr"] <- fpr
  
  tpr <- sum(over_threshold$BMI_Binary == "Healthy")/sum(rf_test_results$BMI_Binary == "Healthy")
  roc_rf[roc_rf$threshold==i, "tpr"] <- tpr
  
}

ggplot()+
  geom_line(data = roc_rf, aes(x = fpr, y = tpr, color = threshold), linewidth = 2)+
  scale_color_gradientn(colors = rainbow(3))+
  geom_abline(intercept = 0, slope = 1, lty = 2)+
  geom_point(data = roc_rf[seq(1, 101, 10), ], aes(x = fpr, y = tpr))+
  geom_text(data = roc_rf[seq(1, 101, 10), ],
            aes(x = fpr, y = tpr, label = threshold, hjust = 1.2, vjust = -0.2))+
  labs(x = "False Positive Rate",
       y = "True Positive Rate",
       title = "Random Forest ROC Curve",
       color = "Threshold")+
  theme_minimal()


#Feature importance

rf_ext <- extract_fit_parsnip(rf_train_full)$fit


rf_feat_importance <- rf_ext$variable.importance %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "Features") %>%
  rename(Importance = ".") %>% 
  arrange(desc(Importance))
rf_feat_importance

top_20_features <- rf_feat_importance %>%
  slice_max(order_by = Importance, n = 20)
top_20_features


ggplot(top_20_features, aes(x = reorder(Features, Importance), y = Importance)) +
  geom_col(fill = "skyblue")+
  coord_flip()+
  labs(
    title = "Feature Importance Random Forest",
    x = "Features",
    y = "Importance"
  )+
  theme_minimal()




#XGBoost

#Creating the model
xg_conf <- 
  boost_tree(
    trees = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune(),
    mtry = tune(),
    sample_size = tune()
  ) %>% 
  set_engine("xgboost",
             seed = Seed) %>% 
  set_mode("classification")


#Workflow
#XGBoost 
xg_workflow <- 
  workflow() %>% 
  add_model(xg_conf) %>% 
  add_recipe(BMI_recipe)


#Grid space filling
xg_grid <- 
  grid_space_filling(
    trees(range = c(400, 1200)),
    tree_depth(range = c(2, 6)),
    learn_rate(range = c(-4, -1)),
    loss_reduction(range = c(0, 2)),
    mtry(range = c(2, ncol(train_data_clean) -1)),
    sample_prop(range = c(0.6, 0.9)),
    size = 40
  )


#Tune the model
set.seed(Seed)
xg_tune <- tune_grid(
  xg_workflow,
  resamples = cv_folds,
  grid = xg_grid,
  metrics = my_metrics,
  control = control_resamples(save_pred = TRUE)
)

collect_metrics(xg_tune)
xg_preds_cv <- collect_predictions(xg_tune)

xg_best_metrics <- xg_tune %>%
  show_best(metric = "pr_auc", n = 1)


#Select best hyperparamer
xg_best <- select_best(xg_tune, metric = "pr_auc")
xg_best


#Fit on the whole training data
xg_workflow_final <- finalize_workflow(xg_workflow, xg_best)

xg_train_full <- fit(
  xg_workflow_final,
  data = train_data_clean
)


#Model on the test set
xg_test_results <- predict(xg_train_full, test_data_clean, type = "prob") %>% 
  bind_cols(predict(xg_train_full, new_data = test_data_clean, type = "class")) %>% 
  bind_cols(test_data_clean %>%
              select(BMI_Binary))


#Test set metrics
xg_test_metrics <- my_metrics(
  data = xg_test_results,
  truth = BMI_Binary,
  estimate = .pred_class,
  .pred_Healthy
)
xg_test_metrics


#Confusion Matrix
xg_conf_matrix <- conf_mat(xg_test_results, truth = BMI_Binary, estimate = .pred_class)
autoplot(xg_conf_matrix, type = "heatmap")+
  theme_minimal()


#ROC Curve test set 
roc_xg <- data.frame(threshold = seq(1,0,-0.01), fpr = 0, tpr = 0)
for (i in roc_xg$threshold){
  
  over_threshold <- xg_test_results[xg_test_results$.pred_Healthy >= i, ]
  
  fpr <- sum(over_threshold$BMI_Binary == "Unhealthy")/sum(xg_test_results$BMI_Binary == "Unhealthy")
  roc_xg[roc_xg$threshold==i, "fpr"] <- fpr
  
  tpr <- sum(over_threshold$BMI_Binary == "Healthy")/sum(xg_test_results$BMI_Binary == "Healthy")
  roc_xg[roc_xg$threshold==i, "tpr"] <- tpr
  
}

ggplot()+
  geom_line(data = roc_xg, aes(x = fpr, y = tpr, color = threshold), linewidth = 2)+
  scale_color_gradientn(colors = rainbow(3))+
  geom_abline(intercept = 0, slope = 1, lty = 2)+
  geom_point(data = roc_xg[seq(1, 101, 10), ], aes(x = fpr, y = tpr))+
  geom_text(data = roc_xg[seq(1, 101, 10), ],
            aes(x = fpr, y = tpr, label = threshold, hjust = 1.2, vjust = -0.2))+
  labs(x = "False Positive Rate",
       y = "True Positive Rate",
       title = "XGBoost Curve",
       color = "Threshold")+
  theme_minimal()


#Feature importance
trained_recipe <- extract_recipe(xg_train_full)

x_test <- bake(trained_recipe, new_data = test_data_clean)

x_test_matrix <- x_test %>%
  select(-BMI_Binary) %>%
  as.matrix()

xg_model <- extract_fit_parsnip(xg_train_full)$fit

shap_values <- shapviz(object = xg_model, X_pred = x_test_matrix)

xg_importance_plot <- sv_importance(shap_values)

xg_importance_plot+
  labs(x = "Feature Inportance",
       title = "Top Features")+
  theme_minimal()
  



#Calculating the optimal threshold
#Logistic Regression
roc_logreg <- roc_logreg %>%
  mutate(youden_j = tpr - fpr)

logreg_best_threshold <- roc_logreg %>%
  arrange(desc(youden_j)) %>%
  slice(1)

logreg_best_threshold


#Applying the new threshold
logreg_test_results <- logreg_test_results %>% 
  mutate(.pred_class = factor(if_else(.pred_Healthy >= 0.2, "Healthy", "Unhealthy"), levels = c("Healthy", "Unhealthy")))

#Test metrics with the new threshold
logreg_test_metrics <- my_metrics(
  data = logreg_test_results,
  truth = BMI_Binary,
  estimate = .pred_class,
  .pred_Healthy
)
logreg_test_metrics


#Confusion Matrix for new threshold
logreg_conf_matrix_new_thres <- conf_mat(logreg_test_results, truth = BMI_Binary, estimate = .pred_class)
autoplot(logreg_conf_matrix_new_thres, type = "heatmap")+
  theme_minimal()



#Calculating the optimal threshold
#Random Forest 
roc_rf <- roc_rf %>%
  mutate(youden_j = tpr - fpr)

rf_best_threshold <- roc_rf %>%
  arrange(desc(youden_j)) %>%
  slice(1)

rf_best_threshold


#Applying the new threshold
rf_test_results <- rf_test_results %>% 
  mutate(.pred_class = factor(if_else(.pred_Healthy >= 0.33, "Healthy", "Unhealthy"), levels = c("Healthy", "Unhealthy")))

#Test metrics with the new threshold
rf_test_metrics <- my_metrics(
  data = rf_test_results,
  truth = BMI_Binary,
  estimate = .pred_class,
  .pred_Healthy
)
rf_test_metrics


#Confusion Matrix for new threshold
rf_conf_matrix_new_thres <- conf_mat(rf_test_results, truth = BMI_Binary, estimate = .pred_class)
autoplot(rf_conf_matrix_new_thres, type = "heatmap")+
  theme_minimal()



#Calculating the optimal threshold
#XGBoost 
roc_xg <- roc_xg %>%
  mutate(youden_j = tpr - fpr)

xg_best_threshold <- roc_xg %>%
  arrange(desc(youden_j)) %>%
  slice(1)

xg_best_threshold


#Applying the new threshold
xg_test_results <- xg_test_results %>% 
  mutate(.pred_class = factor(if_else(.pred_Healthy >= 0.5, "Healthy", "Unhelathy"), levels = c("Healthy", "Unhealthy")))

#Test metrics with the new threshold
xg_test_metrics <- my_metrics(
  data = xg_test_results,
  truth = BMI_Binary,
  estimate = .pred_class,
  .pred_Healthy
)
xg_test_metrics


#Confusion Matrix for new threshold
xg_conf_matrix_new_thres <- conf_mat(xg_test_results, truth = BMI_Binary, estimate = .pred_class)
autoplot(xg_conf_matrix_new_thres, type = "heatmap")+
  theme_minimal()
