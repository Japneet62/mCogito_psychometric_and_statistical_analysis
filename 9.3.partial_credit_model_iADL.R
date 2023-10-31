#####################################################################################################
############## Rasch model - iADL Fluid intellegence/geometric pattern recognition  ##################
#####################################################################################################

# ------ start new worksheet, libraries, load data ------------- # 
## Clear work space and set path variables 
rm(list=ls()) # clear environment 
graphics.off() # clear plots 

## libraries 
library(mirt) 
library(readxl) 
library(dplyr) 

## LOAD DATA without any variance 
mainpath <- "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS" 
setwd(mainpath) 
iADL <- as.data.frame(read_excel("data_with_variance.xlsx", sheet = 5))  
iADL_grp <- as.data.frame(read_excel("group_data.xlsx", sheet = 5))  

iADL_age <- cbind(iADL, age = iADL_grp$age) 
iADL_sex <- cbind(iADL, gender = iADL_grp$gender)

## start saving all the plots created in the script below in a pdf 
# pdf(file = "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS/plots/iADL_pcm.pdf",   # The directory you want to save the file in
#     width = 8, # The width of the plot in inches
#     height = 8) # The height of the plot in inches

# ------- Apply the partial credit model to the data -------------------- # 
library(eRm) # load package for 
library(readr) # To import the data
library(plyr) # For plot the Item characteristic curves
library(WrightMap)# For plot the variable map

# 1. fit the partial credit model to your data 
fitpcm <- PCM(iADL) 

# 2. person parameter 
ppar <- person.parameter(fitpcm)

# 3. chi square statistics, to assess model fit 
ifit <- eRm::itemfit(ppar) 
  # Food-preparation - p = 0.003, infit = 1.725, outfit = 1.962 

# 4. LR test to assess the model fit 

  # - divide the data on the basis of AGE var to do this 
  age_cat <- factor(iADL_age$age <= median(iADL_age$age), 
                    labels = c("50-60", "<60")) 
  
  # - trial code to divide data into separate age groups 
  age_cat2 <- iADL_age %>% 
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
  sex_cat <- factor(iADL_sex$gender, levels = c("M", "F"), 
                    labels = c("Male", "Female"))  
  
  # # another method to check if the division is correct 
  # iADL_sex %>% group_by(gender) %>% summarise(n = n()) 
  
fitLR_sex <- LRtest(fitpcm, sex_cat) 
  
# 5. Wald test - to find which item is responsible for misfit 
waldt <-  Waldtest(fitpcm, age_cat)
waldt <-  Waldtest(fitpcm, sex_cat)

# 6. thresholds 
th <- thresholds(fitpcm)

# 7. plot ICCs 
par(mfrow=c(3,3))
plotICC(fitpcm, ask = FALSE) 
legend(195, 95, legend=c("Item 1", "Category 2", "Category 3"),
       col=c("red", "blue", "green"), lty=1) 

# 8. plot person item map  
plotPImap(fitpcm, latdim = "Independent living trait", main = "Person-Item map CEAQ")


#----- 
# dev.off() 


