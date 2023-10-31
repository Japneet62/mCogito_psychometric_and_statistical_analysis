#####################################################################################################
############## Rasch model - FRT Fluid intellegence/geometric pattern recognition  ##################
#####################################################################################################

# ------ start new worksheet, libraries, load data ------------- # 
## Clear work space and set path variables 
rm(list=ls()) # clear environment 
graphics.off() # clear plots 

## libraries 
library(mirt) 
library(readxl) 
library(dplyr) 

## LOAD DATA without any variance --------
mainpath <- "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS" 
setwd(mainpath) 
FRT <- as.data.frame(read_excel("data_with_variance.xlsx", sheet = 4))  
FRT <- FRT[,c(-7,-24,-28)]
# FRT <- FRT[,c(-1,-2)]
# FRT <- FRT[,c(-1,-12)]

# FRT <- FRT[,c(-1, -2, -3, -7, -12, -24,-28)]

# path_clean_data <- "/Users/japneetbhatia/desktop/thesis/10.CLEAN_DATA" 
# setwd(path_clean_data) 
# FRT_full <- as.data.frame(read_excel("clean_data.xlsx", sheet = 5)) 
# FRT_full <- FRT_full[,c(-1,-2,-3)]
# FRT  <- FRT[,c(-24,-28)] 

## Load group data ------- 
setwd(mainpath) 

FRT_grp <- as.data.frame(read_excel("group_data.xlsx", sheet = 4))  
colnames(FRT) <- gsub("_ans","",colnames(FRT)) # improve column names 
FRT_age <- cbind(FRT, age = FRT_grp$age) 
FRT_sex <- cbind(FRT, sex = FRT_grp$sex_M1_F0)

# rm(list = c("FRT_grp")) # clear unnecessary variables

# ## start saving all the plots created in the script below in a pdf 
# pdf(file = "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS/plots/FRT_rm_fluidIntel.pdf",   # The directory you want to save the file in
#     width = 8, # The width of the plot in inches
#     height = 8) # The height of the plot in inches

# ------- Apply the Rasch model to the data -------------------- # --------
library(eRm) # load package for 

# 1. fit the Rasch model to your data --------
fitrasch <- RM(FRT) # all items except the first 2 with no variance, ceiling effect 

# 2. easiness parameters --------
easiness <- round(fitrasch$betapar, 3)

# 3. difficulty parameters --------
difficulty <- data.frame(difficulty = round(sort(-fitrasch$betapar), 3))

# 4. test if the Rasch model fits --------
# 4.1. LR test !!!! --------

# - divide the data on the basis of AGE var to do this 
age_cat <- factor(FRT_age$age <= median(FRT_age$age), 
                  labels = c("50-60", "<60")) 
 
# trial code to divide data into separate age groups --------
age_cat2 <- FRT_age %>% 
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
# FRT_age %>% group_by(age) %>% summarise(n = n())

fitLR_age <- LRtest(fitrasch, age_cat) 

# we will accept the null hypothesis because the p is above 0.05, which means the two models 
# fit equally, which means the rasch model fit's the data well 

# 4.2. divide the data on the basis of GENDER var to do this --------
sex_cat <- factor(FRT_sex$sex, levels = c("M", "F"), 
                  labels = c("Male", "Female"))  

# # another method to check if the division is correct 
# FRT_sex %>% group_by(gender) %>% summarise(n = n()) # 31, 31 total 62

fitLR_sex <- LRtest(fitrasch, sex_cat) 

# 5. Wald test - to find which item is responsible for misfit --------
waldt <-  Waldtest(fitrasch, age_cat)
waldt <- waldt["coef.table"]

coef <- data.frame(waldt$coef.table) 
coef  <- coef%>% arrange(z.statistic) 

# Item 5 is slightly misfitting but not soo much so we will leave it. 

# 6. Graphically illustrate misfit --------
par(mar = c(1, 1, 1, 1))
plotGOF(fitLR_age, ctrline = list(col = "grey"), conf = list(), main = "Item misfit - FRT")
plotGOF(fitLR_sex, ctrline = list(col = "grey"), conf = list(), main = "Item misfit - FRT")

# 7. Item specific local dependence --------
set.seed(123)
T1 <- NPtest(as.matrix(FRT), n = 1000, method = "T1")

# 8. local dependence at global level --------
T11 <- NPtest(as.matrix(FRT), n = 1000, method = "T11")

# 9. parallel ICCs --------
plotjointICC(fitrasch, xlab = "Fluid Intelligence Trait", main = 
               "ICCs for Fluid Intelligence Trait", legpos = "right")

# 10. person parameter --------
FRT_p_par <- person.parameter(fitrasch) 
  # add person parameters to the data matrix and use them in further analyses
  FRT_p_par <- data.frame(person.parameter = FRT_p_par$theta.table[,1])
  names(FRT_grp)[31] <- "Study_ID"
  FRT_p_par <- cbind(FRT_p_par,SubID = FRT_grp$StudyID)
  










   