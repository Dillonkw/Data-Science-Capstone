#Kyle Dillon
#Capstone Project (EDA)
#Predicting Healthy BMI


install.packages("tidyverse")
install.packages("haven")

library(tidyverse)
library(haven)

setwd("/Users/kyledillon/Documents/GitHub/Data-Science-Capstone")


BMXData <- read_xpt("RawData/BMX_L.xpt") #Body Measurements Data
BPQData <- read_xpt("RawData/BPQ_L.xpt") #Blood Pressure and Cholesterol Data
DemoData <- read_xpt("RawData/DEMO_L.xpt") #Demographic Data
DR1TOTData <- read_xpt("RawData/DR1TOT_L.xpt") #Dietary Interview - Total Nutrient Intakes, First Day
DSQTOTData <- read_xpt("RawData/DSQTOT_L.xpt") #Dietary Supplement Use 30-Day - Total Dietary Supplements

