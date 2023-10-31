#####################################################################################
############## Rasch model - False positives / Discrimination  ##################
#####################################################################################

# ------ start new worksheet, libraries, load data ------------- # 
## Clear work space and set path variables -------
rm(list=ls()) # clear environment 
graphics.off() # clear plots 

## libraries
library(readxl) 
library(dplyr) 
library(gridExtra) 

## LOAD DATA without any variance -------
mainpath <- "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS" 
setwd(mainpath) 
DRT <- as.data.frame(read_excel("data_with_variance.xlsx", sheet = 3))  
DRT_grp <- as.data.frame(read_excel("group_data.xlsx", sheet = 3))  

DRT_p<- DRT[,c(1, 4, 6, 9, 11, 13, 18)] # our main data table 

colnames(DRT_p) <- gsub("_r","",colnames(DRT_p)) # improve column names 
DRT_p_age <- cbind(DRT_p, age = DRT_grp$age) 
DRT_p_sex <- cbind(DRT_p, sex_M1_F0 = DRT_grp$sex_M1_F0)

## start saving all the plots created in the script below in a pdf -------
pdf(file = "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS/plots/DRT_rm_discrimination.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8) # The height of the plot in inches

# ------- Apply the rasch model to the data -------------------- # -------
library(eRm) # load package for 

# 1. fit the rasch model to your data -------
fitrasch <- RM(DRT_p) 

# --2. easiness parameters -------
easiness <- round(fitrasch$betapar, 3)

# --3. difficulty parameters -------
difficulty <- data.frame(difficulty = round(sort(-fitrasch$betapar), 3))
grid.table(difficulty)

# 4. test if the rasch model fits -------
# 4.1. LR test !!!! -------

# - divide the data on the basis of AGE var to do this -------
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

# # check age groups division again 
# DRT_p_age %>% group_by(age) %>% summarise(n = n())
# 
fitLR_age <- LRtest(fitrasch, age_cat) 
# we will accept the null hypothesis because the p is above 0.05, which means the two models 
# fit equally, which means the rasch model fit's the data well 

# - divide the data on the basis of GENDER var to do this -------
sex_cat <- factor(DRT_p_sex$sex_M1_F0, levels = c("M", "F"), 
                  labels = c("Male", "Female"))  

# # another method to check if the division is correct 
# DRT_p_sex %>% group_by(gender) %>% summarise(n = n()) # 31, 31 total 62
# 
fitLR_sex <- LRtest(fitrasch, sex_cat) 

# 5. Wald test - to find which item is responsible for misfit -------
waldt <-  Waldtest(fitrasch, age_cat)  
waldt <- waldt["coef.table"]

coef <- data.matrix(waldt$coef.table) 

# Item 5 is slightly misfitting but not soo much so we will leave it. 

# 6. Graphically illustrate misfit -------
plotGOF(fitLR_age, ctrline = list(col = "grey"), conf = list())

# 7. Item specific local dependence -------
set.seed(123)
T1 <- NPtest(as.matrix(DRT_p), n = 1000, method = "T1")

# 8. local dependence at global level -------
T11 <- NPtest(as.matrix(DRT_p), n = 1000, method = "T11")

# 9. parallel ICCs -------

# 10. ICCs -------
plotjointICC(fitrasch, xlab = "Discrimination Trait", main = "ICCs for Discrimination trait", legpos = "bottomright")

# 11. person parameter -------
DRT_np_p.par <- person.parameter(fitrasch)

# add person parameters to the data matrix and use them in further analyses -------
DRT_D_p.par <- data.frame(PersonParameter = DRT_np_p.par$theta.table[,1])
DRT_D_p.par <- cbind(DRT_D_p.par,SubID = DRT_grp$StudyID)


# ---- 
dev.off() 









