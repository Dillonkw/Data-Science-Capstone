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

datasets <- list( #Puts all the individual datasets into one list called datasets 
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


CleanedData <- FullData %>% 
  filter(RIDAGEYR >= 18) %>% 
  filter(URXPREG != 1 | is.na(URXPREG))

CleanedData <- CleanedData %>%
  select(RIDAGEYR, RIAGENDR, RIDRETH1, DMDBORN4, DMDEDUC2, DMDMARTZ, DMDHHSIZ, INDFMPIR,DR1TKCAL, DR1TPROT, 
         DR1TCARB, DR1TFIBE, DR1TTFAT, DR1TCHOL, DR1TATOC, DR1TVARA, DR1TVB1, DR1TVB2, DR1TNIAC, DR1TVB6, 
         DR1TFDFE, DR1TCHL, DR1TVB12, DR1TVC, DR1TVD, DR1TVK, DR1TCALC, DR1TPHOS, DR1TMAGN, DR1TIRON, DR1TZINC, 
         DR1TCOPP, DR1TSODI, DR1TPOTA, DR1TSELE, DR1TCAFF, DR1TALCO, DR1TMOIS, DR1_320Z,DSQTKCAL, DSQTPROT, DSQTCARB, 
         DSQTFIBE, DSQTTFAT, DSQTCHOL, DSQTVB1, DSQTVB2, DSQTNIAC, DSQTVB6, DSQTFDFE, DSQTCHL, DSQTVB12, DSQTVC, DSQTVK, 
         DSQTVD, DSQTCALC, DSQTPHOS, DSQTMAGN, DSQTIRON, DSQTZINC, DSQTCOPP, DSQTSODI, DSQTPOTA, DSQTSELE, DSQTCAFF, 
         URXPREG, LBXGLU, LBXIN, LBXTC, LBXGH, HIQ011, MCQ160m, MCQ550, MCQ160l, MCQ053, BPQ101D, BPQ020, BPQ030, SLQ300, 
         SLQ310, SLD012, SLQ320, SLQ330, SLD013, PAD800, PAD820, PAD680, PAD810Q, PAD790Q, WHQ070)

