#########################################################################
############## Rasch model - DRT Hits / Image present  ##################
#########################################################################

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

DRT_p <- DRT[,c(3, 5, 8, 10, 12, 14, 16)] # our main data table 
DRT_p_age <- cbind(DRT_p, age = DRT_grp$age) 
DRT_p_sex <- cbind(DRT_p, gender = DRT_grp$gender)

# rm(list = c("DRT", "DRT_grp")) # clear unnecessary variables 

# ------- Apply the rasch model to the data -------------------- # 
library(eRm) # load package for 

# 1. fit the rasch model to your data 
fitrasch <- RM(DRT_p)

# 2. easiness parameters 
easiness <- round(fitrasch$betapar, 3)

# 3. difficulty parameters 
difficulty <- round(sort(-fitrasch$betapar), 3)

# 4. test if the rasch model fits
  # 4.1. LR test !!!! 

    # - divide the data on the basis of AGE var to do this 
        age_cat <- factor(DRT_p_age$age <= median(DRT_p_age$age), 
                          labels = c("50-60", "<60")) 
        
        # trial code to divide data into separate age groups 
        age_cat2 <- DRT_p_age %>% 
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
        DRT_p_age %>% group_by(age) %>% summarise(n = n())
        
    fitLR_age <- LRtest(fitrasch, age_cat) 
    # we will accept the null hypothesis because the p is above 0.05, which means the two models 
    # fit equally, which means the rasch model fit's the data well 
    

    # - divide the data on the basis of GENDER var to do this 
        sex_cat <- factor(DRT_p_sex$gender, levels = c("M", "F"), 
                          labels = c("Male", "Female"))  
  
        # another method to check if the division is correct 
        DRT_p_sex %>% group_by(gender) %>% summarise(n = n()) # 31, 31 total 62
    
    fitLR_sex <- LRtest(fitrasch, sex_cat) 
    
# 5. Wald test - to find which item is responsible for misfit 
     Waldtest(fitrasch, age_cat)
    
    # Item 5 is slightly misfitting but not soo much so we will leave it. 
     
# 6. Graphically illustrate misfit 
     plotGOF(fitLR_age, ctrline = list(col = "grey"), conf = list())
     
# 7. Item specific local dependence 
     set.seed(123)
     T1 <- NPtest(as.matrix(DRT_p), n = 1000, method = "T1")
     
# 8. local dependence at global level 
     T11 <- NPtest(as.matrix(DRT_p), n = 1000, method = "T11")
     
# 9. parallel ICCs 
     
# 10. ICCs
     plotjointICC(fitrasch, xlab = "Recognition Trait", main = "ICCs recognition trait")

# 11. person parameter 
     DRT_p_par <- person.parameter(fitrasch)


  
    
    
    
    
    
    
    
    
