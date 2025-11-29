#Kyle Dillon
#Capstone Project (Preprocessing and Feature Engineering Report)
#Predicting Healthy BMI


install.packages("tidyverse")
install.packages("haven")
install.packages("tidymodels")
install.packages("mice")
install.packages("glmnet")


library(tidyverse)
library(haven)
library(tidymodels)
library(mice)
library(glmnet)


setwd("/Users/kyledillon/Documents/GitHub/Data-Science-Capstone") #Working directory for laptop
setwd("C:/Users/kboom/OneDrive/Documents/GitHub/Data-Science-Capstone") #Working directory for desktop computer


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
datasets_21_13 <- list( 
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


FullData_21_23 <- reduce(datasets_21_13, full_join, by = "SEQN") #uses the reduce function to full join the 2021-2023 datasets list by the SEQN variable


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
         LBXGLU, LBXIN, LBXGH, HIQ011, MCQ160M, MCQ550, MCQ160L, MCQ053, BPQ101D, 
         BPQ020, BPQ030, SLD012, SLD013, PAD800, PAD820, PAD680, WHQ070, Survey_Year)


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
    Gallstones = MCQ550,
    Liver_Condition = MCQ160L,
    Taking_Treatment_Anemia = MCQ053,
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
  mutate(BMI = ifelse(BMXBMI >= 18.5 & BMXBMI <= 27, "Healthy", "Unhealthy"),
         BMI_Binary = ifelse(BMI == "Healthy", 1,0))

table(CleanedData$BMI, useNA = "ifany")



#Removes missing values from the target variable
CleanedData <- CleanedData %>% 
  filter(!is.na(BMI))

table(CleanedData$BMI, useNA = "ifany")

saveRDS(CleanedData, "CleanedDataNew.rds")

num_missing <- sapply(CleanedData, function(x) sum(is.na(x)))
num_missing


#Initial feature removal (Removal of redundant variables and variables with substantial rows missing)
CleanedData <- CleanedData %>% 
  select(- BMXBMI, -BMI, -Country_of_Origin, -Household_Size, -Iodine, -Insulin, 
         -Plasma_Fasting_Glucose, -Minutes_Moderate_LTPA, -Minutes_Vigorous_LTPA, 
         -Blood_Cholesterol_Meds, -Told_High_BP_2)


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


#Setting the train/ test split with a 90/10 split
set.seed(22)

split <- initial_split(CleanedData, strata = BMI_Binary, prop = 0.9)

train_data <- training(split)
test_data <- testing(split)


#Mice imputation for Na values
mice_train <- mice(train_data, m = 1, method = "pmm", seed = 22)
train_imputed <- complete(mice_train, 1)

mice_test <- mice(test_data, m = 1, method = "pmm", seed = 22)
test_imputed <- complete(mice_test, 1)


#Correlation matrix 
numeric_data <- train_imputed %>% 
  select(where(is.numeric))

cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

top_correlation <- cor_matrix %>%
  as.data.frame() %>%
  rownames_to_column(var = "Feature1") %>%
  pivot_longer(-Feature1, names_to = "Feature2", values_to = "Correlation") %>%
  filter(Feature1 != Feature2) %>%
  filter(abs(Correlation) > 0.75) %>%
  arrange(desc(abs(Correlation)))


#Removing highly correlated features
train_data_clean <- train_imputed %>% 
  select(-Total_Folic_Acid, -Total_Energy, -Total_Fat, 
         -Total_Carbs, -Total_Choline, -Total_Phosphorus, -Food_Folate)

test_data_clean <- test_imputed %>% 
  select(names(train_data_clean))



#Set the recipe
BMI_recipe <- recipe(BMI_Binary ~ ., data = train_imputed) %>%
  step_rm(Survey_Year) %>% 
  step_log(all_numeric_predictors(), offset = 1) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  prep(training = train_imputed, retain = TRUE)

train_baked <- bake(BMI_recipe, new_data = NULL)
  
test_baked <- bake(BMI_recipe, new_data = test_imputed)


#Lasso
x_train <- as.matrix(train_baked %>% select(-BMI_Binary))
y_train <- train_baked$BMI_Binary

x_test <- as.matrix(test_baked %>% select(-BMI_Binary))
y_test <- test_baked$BMI_Binary

lasso <- cv.glmnet(x_train, y_train, alpha = 1, family = "binomial")

lasso_predict <- predict(lasso, s = "lambda.min", newx = x_test, type = "response")

coef(lasso, s = "lambda.min")


#PCA
num_features <- train_baked %>% 
  select(where(is.numeric))

pca <- prcomp(num_features, center = FALSE, scale. = FALSE)

train_pca <- predict(pca, newdata = num_features)

test_pca  <- predict(pca, newdata = test_baked
                     %>% select(where(is.numeric)))

summary(pca)


#Test code on machine 1 (Desktop) - successful!
#Test code on machine 2 (Laptop) - successful!
