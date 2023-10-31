#####################################################################################
############## Rasch model - DRT False positives / discrimination  ##################
#####################################################################################

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
DRT <- as.data.frame(read_excel("data_with_variance.xlsx", sheet = 3))  
DRT_grp <- as.data.frame(read_excel("group_data.xlsx", sheet = 3))  

DRT_np <- DRT[,c(1, 2, 4, 6, 7, 9, 11, 13, 15, 17, 18)] # our main data table 
DRT_np_age <- cbind(DRT_np, age = DRT_grp$age) 
DRT_np_sex <- cbind(DRT_np, gender = DRT_grp$gender)

# rm(list = c("DRT", "DRT_grp")) # clear unnecessary variables 

# ------- Apply the rasch model to the data -------------------- # 
library(eRm) # load package for 

# 1. fit the rasch model to your data 
fitrasch <- RM(DRT_np[,c(-9,-10)]) 

# 2. easiness parameters 
easiness <- round(fitrasch$betapar, 3)

# 3. difficulty parameters 
difficulty <- round(sort(-fitrasch$betapar), 3)

# 4. test if the rasch model fits
  # 4.1. LR test !!!! 
  
    # - divide the data on the basis of AGE var to do this 
    age_cat <- factor(DRT_np_age$age <= median(DRT_np_age$age), 
                      labels = c("50-60", "<60")) 
    
      # trial code to divide data into separate age groups 
      age_cat2 <- DRT_np_age %>% 
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
      
      # check age groups division again 
      DRT_np_age %>% group_by(age) %>% summarise(n = n())
      
fitLR_age <- LRtest(fitrasch, age_cat) 

      # we will accept the null hypothesis because the p is above 0.05, which means the two models 
      # fit equally, which means the rasch model fit's the data well 
      
    # - divide the data on the basis of GENDER var to do this 
      sex_cat <- factor(DRT_np_sex$gender, levels = c("M", "F"), 
                        labels = c("Male", "Female"))  
      
      # another method to check if the division is correct 
      DRT_np_sex %>% group_by(gender) %>% summarise(n = n()) # 31, 31 total 62
      
fitLR_sex <- LRtest(fitrasch, sex_cat) 

# 5. Wald test - to find which item is responsible for misfit 
waldt <- Waldtest(fitrasch, age_cat)
coef <- data.matrix(waldt$coef.table) 

# Item 5 is slightly misfitting but not soo much so we will leave it. 

# 6. Graphically illustrate misfit 
plotGOF(fitLR_age, ctrline = list(col = "grey"), conf = list())

# 7. Item specific local dependence 
set.seed(123)
T1 <- NPtest(as.matrix(DRT_np), n = 1000, method = "T1")

# 8. local dependence at global level 
T11 <- NPtest(as.matrix(DRT_np), n = 1000, method = "T11")

# 9. parallel ICCs 

# 10. ICCs
plotjointICC(fitrasch, xlab = "Discrimination Trait", main = "ICCs discrimination trait")

# 11. person parameter 
DRT_p_par <- person.parameter(fitrasch)










