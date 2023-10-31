######## --------- SART analysis ----------- ############# 


# ------ 1. Clear work space and set path variables --------------------- 
rm(list=ls()) # clear environment 
graphics.off() # clear plots 

# ------ 2. Set path variables -------------------------------------------  
path_clean_data <- "/Users/japneetbhatia/desktop/thesis/10.CLEAN_DATA" 
path_results <- "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS" 
path_analysis_scripts <- "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_ANALYSIS/scripts"
path_plots <- "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS/plots" 

setwd(path_clean_data) 

# ------ 3. Load all libraries needed in the scripts later on ------------- 
library(readxl) 
library(dplyr) 
library(AMR)
library(ggplot2) 
library(tidyverse) 
library(psychometric)
library(Gifi)
library(mirt) 
library(stringi)
library(data.table) 
library(openxlsx) 
library(reshape2)
library(ggpubr) 

# -----  4. loaded data vars as data.frame -----------------------------------------  
SART_subid <- as.data.frame(read_excel("clean_data.xlsx", sheet = 9)) # polytomous = 0, 1, 2 = indep, minor dependence, dependent

# Missing data 
SART_na <- SART_subid[is.na(SART_subid$com_err),] # SART has participants who didn't do the tests 

# remove missing values 
SART_clean <- SART_subid[c(-7,-9,-19),c(-1)] 


# ------ 5. Plot a scatterplot 

plot(SART_clean$om_err, SART_clean$com_err, main = "Scatterplot of om_err vs com_err",
     xlab = "om_err", ylab = "com_err") 

