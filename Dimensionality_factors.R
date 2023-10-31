#####################################################################
############## Dimensionality assessment - IFA ######################
#####################################################################

## 1. Clear work space and set path variables 
rm(list=ls()) # clear environment 
graphics.off() # clear plots 
pathin <- "/Users/japneetbhatia/desktop/thesis/10.CLEAN_DATA" 
all_data <- "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS" 
scripts <- "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_ANALYSIS/scripts"
refined_data <- "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS" 

setwd(refined_data) 

## 2. Libraries 
library(readxl) 
library(dplyr) 
library(AMR) 
library(ggplot2) 
library(tidyverse) 
library(psychometric) 
library(mirt) 
library(stringi)

# 3. loaded as data.frame
CBSF <- as.data.frame(read_excel("data_with_variance.xlsx", sheet = 4)) 
CBSF_A <- as.data.frame(read_excel("data_with_variance.xlsx", sheet = 2)) 
CBSF_B <- as.data.frame(read_excel("data_with_variance.xlsx", sheet = 3)) 
DRT <- as.data.frame(read_excel("data_with_variance.xlsx", sheet = 5)) 
FRT <- as.data.frame(read_excel("data_with_variance.xlsx", sheet = 6)) 
iADL <- as.data.frame(read_excel("data_with_variance.xlsx", sheet = 7)) 

######## 1. DIMENSIONALITY ASSESSMENT (IFA) with mirt ############################# 
## Models with different number of factors
# 
# CBSF1d <- mirt::mirt(CBSF, 1, itemtype = "graded") # terminated after 500 
# CBSF2d <- mirt::mirt(CBSF, 2, itemtype = "graded") # terminated after 500 
# CBSF3d <- mirt::mirt(CBSF, 3, itemtype = "graded") # terminated after 500 
# CBSF4d <- mirt::mirt(CBSF, 4, itemtype = "graded") # terminated after 500 
# 
# CBSF_A_1d <- mirt::mirt(CBSF_A, 1, itemtype = "graded") # terminated after 500 
# CBSF_A_2d <- mirt::mirt(CBSF_A, 2, itemtype = "graded") # terminated after 500 
# CBSF_A_3d <- mirt::mirt(CBSF_A, 3, itemtype = "graded")
# 
# CBSF_B_1d <- mirt::mirt(CBSF_B, 1, itemtype = "graded")
# CBSF_B_2d <- mirt::mirt(CBSF_B, 2, itemtype = "graded")
# CBSF_B_3d <- mirt::mirt(CBSF_B, 3, itemtype = "graded")

# "CBSF_B_8r" re-mapped to ensure all categories have a distance of 1
# "CBSF_B_11r" re-mapped to ensure all categories have a distance of 1 

DRT1d <- mirt::mirt(DRT, 1, itemtype = "2PL") 
DRT2d <- mirt::mirt(DRT, 2, itemtype = "2PL") # terminated after 500 
DRT3d <- mirt::mirt(DRT, 3, itemtype = "2PL") # terminated after 500 
DRT4d <- mirt::mirt(DRT, 4, itemtype = "2PL") # EM quadrature for high dimensional models are better handled 
                                              # with the "QMCEM" or "MCEM" method

DRT5d <- mirt::mirt(DRT, 5, itemtype = "2PL") # EM quadrature for high dimensional models are better handled
                                              # with the "QMCEM" or "MCEM" method
# 
# iADL1d <- mirt::mirt(iADL, 1, itemtype = "graded")
# iADL2d <- mirt::mirt(iADL, 2, itemtype = "graded")
# iADL3d <- mirt::mirt(iADL, 3, itemtype = "graded")
# iADL4d <- mirt::mirt(iADL, 4, itemtype = "graded") 
# iADL5d <- mirt::mirt(iADL, 5, itemtype = "graded") 
# iADL6d <- mirt::mirt(iADL, 6, itemtype = "graded") 

FRT1d <- mirt::mirt(FRT, 1, itemtype = "2PL") # terminated after 500 
FRT2d <- mirt::mirt(FRT, 2, itemtype = "2PL") # terminated after 500 
FRT3d <- mirt::mirt(FRT, 3, itemtype = "2PL") # terminated after 500 
FRT4d <- mirt::mirt(FRT, 4, itemtype = "2PL") # EM quadrature for high dimensional models are better handled
                                              # with the "QMCEM" or "MCEM" method
FRT5d <- mirt::mirt(FRT, 5, itemtype = "2PL") # EM quadrature for high dimensional models are better handled
                                              # with the "QMCEM" or "MCEM" method
FRT6d <- mirt::mirt(FRT, 6, itemtype = "2PL") # terminated after 500 

######## 2. LR TEST (ANOVA) #################################################### 

# LR_CBSF <- anova(CBSF1d, CBSF2d, CBSF3d, CBSF4d)
# LR_CBSF_A <- anova(CBSF_A_1d, CBSF_A_2d, CBSF_A_3d) 
# LR_CBSF_B <- anova(CBSF_B_1d, CBSF_B_2d, CBSF_B_3d) 
LR_DRT <- anova(DRT1d, DRT2d, DRT3d, DRT4d, DRT5d) 
# LR_iADL <- anova(iADL1d, iADL2d, iADL3d, iADL4d) 
LR_FRT <- anova(FRT1d, FRT2d, FRT3d, FRT4d, FRT5d, FRT6d) 

######## 3. M2 statistic #######################################################

# M2(CBSF1d)
# M2(CBSF2d)
# M2(CBSF3d)
# 
# M2(CBSF_A_3d)  
# M2(CBSF_A_2d) #### 
# M2(CBSF_A_1d)
# 
# M2(CBSF_B_1d)
# M2(CBSF_B_2d)
# M2(CBSF_B_3d) #### 

M2_DRT1d <- M2(DRT1d) 
M2_DRT2d <- M2(DRT2d) 
M2_DRT3d <- M2(DRT3d) 

M2_DRT <- rbind(M2_DRT1d, M2_DRT2d, M2_DRT3d)
rownames(M2_DRT) <- c("DRT1d", "DRT2d", "DRT3d")

M2_FRT1d <- M2(FRT1d) 
M2_FRT2d <- M2(FRT2d)
M2_FRT3d <- M2(FRT3d) 

M2_FRT <- rbind(M2_FRT1d, M2_FRT2d, M2_FRT3d)
rownames(M2_FRT) <- c("FRT1d", "FRT2d", "FRT3d")

# M2(iADL1d)
# M2(iADL2d) 

###### 4. Item statistics - to find out misfitting items 
# 
# ifit2D_CBSF_A <- mirt::itemfit(CBSF_A_2d) 
# ifit2D_CBSF_A[ifit2D_CBSF_A[, 4] < 0.05, ]  ## misfitting items
# 
# ifit3D_CBSF_B <- mirt::itemfit(CBSF_B_3d)
# ifit2D_CBSF_B[ifit2D_CBSF_B[, 4] < 0.05, ]  ##CBSF_B_3r

ifit2D_DRT <- mirt::itemfit(DRT2d)
ifit2D_DRT[ifit2D_DRT[, 4] < 0.05, ]  ## misfitting items 

ifit4D_FRT <- mirt::itemfit(FRT4d) 
ifit4D_FRT[ifit4D_FRT[, 4] < 0.05, ]  ## misfitting items
# 
# ifit_iADL <- mirt::itemfit(iADL1d)
# ifit_iADL[ifit_iADL[, 4] < 0.05, ]  ## misfitting items

######## 5. FACTOR EXTRACTION WITH OBLIQUE ROTATION ############################ 
# 
# fa_CBSF_A_2d <- mirt::summary(CBSF_A_2d, rotate = "oblimin") 
# fa_CBSF_B_2d <- mirt::summary(CBSF_B_2d, rotate = "oblimin")  
fa_DRT2d <- mirt::summary(DRT2d, rotate = "oblimin")

# fa_iADL2d <- mirt::summary(iADL2d, rotate = "oblimin") 
fa_FRT4d <- mirt::summary(FRT4d, rotate = "oblimin") 
fa_FRT5d <- mirt::summary(FRT5d, rotate = "oblimin") 

### Arrange all the factors accoring to the item number with the highest loading at the top 
# 
# fa_CBSF_A_2d <- as.data.frame(fa_CBSF_A_2d$rotF)
# f1_CBSF_A_2d <-  fa_CBSF_A_2d%>% arrange(-F1) 
# f1_CBSF_A_2d[,3] <- rownames(f1_CBSF_A_2d)
# f2_CBSF_A_2d <- fa_CBSF_A_2d%>% arrange(-F2) 
# f2_CBSF_A_2d[,3] <- rownames(f2_CBSF_A_2d)
# rm(fa_CBSF_A_2d) 
# 
# fa_CBSF_B_2d <- as.data.frame(fa_CBSF_B_2d$rotF) 
# f1_CBSF_B_2d <-  fa_CBSF_B_2d%>% arrange(-F1) 
# f1_CBSF_B_2d[,3] <- rownames(f1_CBSF_B_2d)
# f2_CBSF_B_2d <- fa_CBSF_B_2d%>% arrange(-F2) 
# f2_CBSF_B_2d[,3] <- rownames(f2_CBSF_B_2d)
# rm(fa_CBSF_B_2d) 

fa_DRT2d <- as.data.frame(fa_DRT2d$rotF)
f1_DRT2d <- fa_DRT2d%>% arrange(-F1) 
f2_DRT2d <- fa_DRT2d%>% arrange(-F2)
f1_DRT2d[,3] <- rownames(f1_DRT2d)
f2_DRT2d[,3] <- rownames(f2_DRT2d)
rm(fa_DRT2d)  

# fa_iADL2d <- as.data.frame(fa_iADL2d$rotF) 
# f1_iADL_2d <-  fa_iADL2d%>% arrange(-F1) 
# f1_iADL_2d[,3] <- rownames(f1_iADL_2d)
# f2_iADL_2d <- fa_iADL2d%>% arrange(-F2) 
# f2_iADL_2d[,3] <- rownames(f2_iADL_2d) 
# rm(fa_iADL2d) 

fa_FRT5d <- as.data.frame(fa_FRT5d$rotF) 
f1_FRT_5d <-  fa_FRT5d%>% arrange(-F1)
f1_FRT_5d[,6] <- rownames(f1_FRT_5d)
f2_FRT_5d <- fa_FRT5d%>% arrange(-F2) 
f2_FRT_5d[,6] <- rownames(f2_FRT_5d)
f3_FRT_5d <- fa_FRT5d%>% arrange(-F3) 
f3_FRT_5d[,6] <- rownames(f3_FRT_5d)
f4_FRT_5d <- fa_FRT5d%>% arrange(-F4)
f4_FRT_5d[,6] <- rownames(f4_FRT_5d) 
f5_FRT_5d <- fa_FRT5d%>% arrange(-F5)
f5_FRT_5d[,6] <- rownames(f5_FRT_5d)
rm(fa_FRT5d)


###################################################################################
#                           WRITE RESULTS OF THIS SCRIPT                          #
###################################################################################

# Remove tables you don't want to write 

rm(list= c("FRT", "DRT", "M2_DRT1d", "M2_DRT2d", "M2_DRT3d"))

dfs<- ls()[sapply(ls(), function(x) class(get(x))) == 'data.frame']   # extract all the df in R and save as a char vec
dfs <- Filter(function(x) is(x, "data.frame"), mget(ls())) 

LR_test <- dfs[grepl("LR", names(dfs))] # get the dfs that start with "LR"
fa <- dfs[grepl("f", names(dfs))] # factors extracted 
M2 <- dfs[grepl("M2", names(dfs))] 

# stores data in multiple sheets, in one excel file 
# 1. LR test for dimensionality assessment. 
# 2. factor extraction 

library(openxlsx) 
write.xlsx(LR_test, file = 'dimensions_LRtest.xlsx') 
write.xlsx(fa, file = 'factor_extraction.xlsx')
write.xlsx(M2, file = 'M2.xlsx', rowNames = TRUE) 
