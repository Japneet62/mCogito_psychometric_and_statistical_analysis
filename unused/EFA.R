## -------- DIMENSIONALITY ASSESSMENT - EXPLORATORY FACTOR ANALYSIS WITH POLYCHORIC CORRELATIONS ---------- # 

# clear workspace 
rm(list=ls()) # clear environment 
graphics.off() # clear plots 

# library for EFA 
library(psych)

# Load 2 questionnaires and 3 neuropsycological tests 
library(readxl) 
path_clean_data <- "/Users/japneetbhatia/desktop/thesis/10.CLEAN_DATA/main"
setwd(path_clean_data) 

# loaded as data.frame
AMT_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 1)) # dichotomous = 0, 1 = incorrect, correct 
CBSF_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 2)) # polytomous = 0, 1, 2, 3 = independent, minor dependence, dependent, majorly dependent 
DRT_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 4)) # dichotomous = 0, 1 = incorrect, correct 
FRT_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 5)) # dichotomous = 0, 1 = incorrect, correct 
iADL_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 6)) # polytomous = 0, 1, 2 = indep, minor dependence, dependent

# Data extraction suitable to fit the model  # extract the scores only, remove subjectID 
AMT_sub <- AMT_data[, grep("AMT", colnames(AMT_data))] 
CBSF_sub <- CBSF_data[, grep("CBSF", colnames(CBSF_data))]
CBSF_A <- CBSF_sub[, grep("CBSF_A", colnames(CBSF_sub))]
CBSF_B <- CBSF_sub[, grep("CBSF_B", colnames(CBSF_sub))]
DRT_sub <- DRT_data[, grep("DRT", colnames(DRT_data))]
FRT_sub <- FRT_data[, grep("F", colnames(FRT_data))]
iADL_sub <- iADL_data[,grep("iADL", colnames(iADL_data))]

# delete unwanted vars 
rm(CBSF_data)
rm(iADL_data)
rm(FRT_data)
rm(AMT_data)
rm(DRT_data)
rm(CBSF_sub)

# STEP 1. Calculate polychoric correlations for all the tests and questionnaires 

# 2.AMT 

# 3. DRT 
tch_DRT_sub <- tetrachoric(DRT_sub, smooth = TRUE)       # DRT_r15 and DRT_r17 had no variance and was deleted
DRT_tr <-  tch_DRT_sub$rho

# check if there are any negative eigen values 
tail(round(eigen(tch_DRT_sub$rho)$values, 3)) # 4 -ve vals 

# factor analysis 
DRT_FA <- fa(DRT_tr, nfactors = 16, rotate = "none", fm = "ml")
print(DRT_FA$loadings, cutoff = 0.3)

# communality 
round(DRT_FA$communality, 2)  

# sort communality - represent sum of squared loadings and represent proportion of variance 
sort(DRT_FA$communality)

plot(x = DRT_FA$loadings)

