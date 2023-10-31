##################################################################################################### 
############## Rasch model - CBSF_nonmotor Fluid intellegence/geometric pattern recognition  ################# 
##################################################################################################### 

# ------ start new worksheet, libraries, load data ------------- # 
## Clear work space and set path variables 
rm(list=ls()) # clear environment 
graphics.off() # clear plots 

## libraries 
library(readxl) 
library(dplyr) 
library(eRm) # load package for 
library(readr) # To import the data
library(plyr) # For plot the Item characteristic curves
library(WrightMap)# For plot the variable map

## LOAD DATA without any variance 
mainpath <- "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS" 
setwd(mainpath) 
CBSF <- as.data.frame(read_excel("data_with_variance.xlsx", sheet = 2))  
CBSF_grp <- as.data.frame(read_excel("group_data.xlsx", sheet = 2))  

CBSF_nonmotor <- CBSF[,c(15:31)]

CBSF_nonmotor_age <- cbind(CBSF_nonmotor, age = CBSF_grp$age) 
CBSF_nonmotor_sex <- cbind(CBSF_nonmotor, gender = CBSF_grp$gender)

## start saving all the plots created in the script below in a pdf 
pdf(file = "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS/plots/CBSF_nonmotor_pcm.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8) # The height of the plot in inches

# ------- Apply the partial credit model to the data -------------------- # 


# 1. fit the rasch model to your data 
fitpcm <- PCM(CBSF_nonmotor) 

# 2. person parameter 
ppar <- person.parameter(fitpcm)

# 3. chi square statistics, to assess model fit 
ifit <- eRm::itemfit(ppar) 
# Food-preparation - p = 0.003, infit = 1.725, outfit = 1.962 

# 4. LR test to assess the model fit 

# - divide the data on the basis of AGE var to do this 
age_cat <- factor(CBSF_nonmotor_age$age <= median(CBSF_nonmotor_age$age), 
                  labels = c("50-60", "<60")) 

# - trial code to divide data into separate age groups 
age_cat2 <- CBSF_nonmotor_age %>% 
  mutate(
    # Create categories
    age_group = dplyr::case_when 
    ( age <= 60 ~ "50-60",
      age > 60  ~ ">60" ) , 
    # Convert to factor
    age_group = factor(
      age_group,
      level = c("50-60", ">60")
    )
  ) 

fitLR_age <- LRtest(fitpcm, age_cat) 

# The following items were excluded due to inappropriate response patterns within
# subgroups: Using-telephone Medications Housekeeping

#? we will accept the null hypothesis because the p is above 0.05, which means the two models 
# fit equally, which means the rasch model fit's the data well 

# - divide the data on the basis of GENDER var to do this 
sex_cat <- factor(CBSF_nonmotor_sex$gender, levels = c("M", "F"), 
                  labels = c("Male", "Female"))  

# # another method to check if the division is correct 
# CBSF_nonmotor_sex %>% group_by(gender) %>% summarise(n = n()) 

fitLR_sex <- LRtest(fitpcm, sex_cat) 

# 5. Wald test - to find which item is responsible for misfit 
waldt <-  Waldtest(fitpcm, age_cat)
waldt <-  Waldtest(fitpcm, sex_cat)

# 6. thresholds 
th <- thresholds(fitpcm)

# 7. plot ICCs 
plotICC(fitpcm, ask = FALSE)

# 8. plot person item map  
plotPImap(fitpcm, latdim = "Independent living trait", main = "Person-Item map CEAQ")

#------ 
dev.off() 



