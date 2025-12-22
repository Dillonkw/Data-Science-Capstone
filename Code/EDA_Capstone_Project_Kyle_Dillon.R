#Kyle Dillon
#Capstone Project (EDA)
#Predicting Healthy BMI


install.packages("tidyverse")
install.packages("haven")

library(tidyverse)
library(haven)

setwd("/Users/kyledillon/Documents/GitHub/Data-Science-Capstone") #Working directory for laptop
setwd("C:/Users/kboom/OneDrive/Documents/GitHub/Data-Science-Capstone") #Working directory for desktop computer

BMXData <- read_xpt("RawData/BMX_L.xpt") #Body Measurements Data
BPQData <- read_xpt("RawData/BPQ_L.xpt") #Blood Pressure and Cholesterol Data
DemoData <- read_xpt("RawData/DEMO_L.xpt") #Demographic Data
DR1TOTData <- read_xpt("RawData/DR1TOT_L.xpt") #Dietary Interview - Total Nutrient Intakes, First Day
DSQTOTData <- read_xpt("RawData/DSQTOT_L.xpt") #Dietary Supplement Use 30-Day - Total Dietary Supplements
GHBData <- read_xpt("RawData/GHB_L.xpt") #Glycohemoglobin
GLUData <- read_xpt("RawData/GLU_L.xpt") #Plasma Fasting Glucose
HIQData <- read_xpt("RawData/HIQ_L.xpt") #Health Insurance
INSData <- read_xpt("RawData/INS_L.xpt") #Insulin
MCQData <- read_xpt("RawData/MCQ_L.xpt") #Medical Conditions
PAQData <- read_xpt("RawData/PAQ_L.xpt") #Physical Activity
SLQData <- read_xpt("RawData/SLQ_L.xpt") #Sleep Disorders	
TCHOLData <- read_xpt("RawData/TCHOL_L.xpt") #Cholesterol - Total
UCPREGData <- read_xpt("RawData/UCPREG_L.xpt") #Urine Pregnancy Test
WHQData <- read_xpt("RawData/WHQ_L.xpt") #Weight History	

#Puts all the individual datasets into one list called datasets
datasets <- list( 
  BMXData,
  BPQData,
  DemoData,
  DR1TOTData,
  DSQTOTData,
  GHBData,
  GLUData,
  HIQData,
  INSData,
  MCQData,
  PAQData,
  SLQData,
  TCHOLData,
  UCPREGData,
  WHQData
)


FullData <- reduce(datasets, full_join, by = "SEQN") #uses the reduce function to full join the datasets list by the SEQN variable


CleanedData <- FullData %>% #Storing the full data into a new dataset called CleanedData
  filter(RIDAGEYR >= 18) %>% #Only includes people that are above the age of 18
  filter(URXPREG != 1 | is.na(URXPREG)) #

CleanedData <- CleanedData %>% #Selecting all the variable that are relevant to the research question
  select(BMXBMI, RIDAGEYR, RIAGENDR, RIDRETH1, DMDBORN4, DMDEDUC2, DMDMARTZ, 
         DMDHHSIZ, INDFMPIR,DR1TKCAL, DR1TPROT, DR1TCARB, DR1TFIBE, DR1TTFAT, 
         DR1TCHOL, DR1TATOC, DR1TVARA, DR1TVB1, DR1TVB2, DR1TNIAC, DR1TVB6, 
         DR1TFDFE, DR1TCHL, DR1TVB12, DR1TVC, DR1TVD, DR1TVK, DR1TCALC, DR1TPHOS, 
         DR1TMAGN, DR1TIRON, DR1TZINC, DR1TFOLA, DR1TB12A, DR1TFA, 
         DR1TCOPP, DR1TSODI, DR1TPOTA, DR1TSELE, DR1TCAFF, DR1TALCO, 
         DR1TMOIS, DR1_320Z, DR1TSUGR, DR1TSFAT, DR1TMFAT, DR1TPFAT, DR1TATOA, 
         DR1TRET, DR1TACAR, DR1TBCAR, DR1TCRYP, DR1TLYCO, DR1TLZ, DR1TFA, 
         DR1TFF, DR1TTHEO, DSQTKCAL, DSQTPROT, DSQTCARB, DSQTFIBE, DSQTTFAT, 
         DSQTCHOL, DSQTVB1, DSQTVB2, DSQTNIAC, DSQTVB6, DSQTFDFE, DSQTCHL, 
         DSQTVB12, DSQTVC, DSQTVK, DSQTVD, DSQTCALC, DSQTPHOS, DSQTMAGN, 
         DSQTIRON, DSQTZINC, DSQTCOPP, DSQTSODI, DSQTPOTA, DSQTSELE, DSQTCAFF, DSQTSUGR, 
         DSQTSFAT, DSQTMFAT, DSQTPFAT, DSQTLYCO, DSQTLZ, DSQTFA, DSQTIODI, 
         LBXGLU, LBXIN, LBXGH, HIQ011, MCQ160M, MCQ550, MCQ160L, MCQ053, 
         BPQ101D, BPQ020, BPQ030, SLD012, SLD013, PAD800, PAD820, PAD680, WHQ070)


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
 
#Change features that are labeled as numeric but are actually categorical
CleanedData <- CleanedData %>% 
  mutate(across(c("Gender","Race","Country_of_Origin","Education_Level","Marital_Status", "Health_Insurance",
                  "Thyroid_Problem","Gallstones","Liver_Condition","Taking_Treatment_Anemia", "Blood_Cholesterol_Meds",
                  "Told_High_BP", "Told_High_BP_2", "Attempted_Weight_Loss_Past_Year"), as.factor))

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
  mutate(BMI = ifelse(BMXBMI >= 18.5 & BMXBMI <= 24.9, "Healthy", "Unhealthy"))

table(CleanedData$BMI)

saveRDS(CleanedData, "CleanedData.rds")

names(CleanedData)