#####################################################################
############## Delayed Object Recognition Task ######################
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
library(ggplot2) 
library(tidyverse) 
library(mirt) 
library(ltm) # 2pl
library(eRm) # rasch 

## 3. loaded as data.frame
DRT <- as.data.frame(read_excel("data_with_variance.xlsx", sheet = 5)) 

######## 1. DIMENSIONALITY ASSESSMENT (IFA) with mirt ##########################
DRT1d <- mirt::mirt(DRT, 1, itemtype = "2PL") 
DRT2d <- mirt::mirt(DRT, 2, itemtype = "2PL") # terminated after 500 
DRT3d <- mirt::mirt(DRT, 3, itemtype = "2PL") # terminated after 500 
DRT4d <- mirt::mirt(DRT, 4, itemtype = "2PL") # EM quadrature for high dimensional models are better handled 
# with the "QMCEM" or "MCEM" method

######## 2. LR TEST ############################################################ 
LR_DRT <- anova(DRT1d, DRT2d, DRT3d, DRT4d) 

M2_DRT1d <- M2(DRT1d) 
M2_DRT2d <- M2(DRT2d) 
M2_DRT3d <- M2(DRT3d) 

######## 3. M2 statistic #######################################################
M2_DRT <- rbind(M2_DRT1d, M2_DRT2d, M2_DRT3d)
rownames(M2_DRT) <- c("DRT1d", "DRT2d", "DRT3d")

###### 4. Item statistics - to find out misfitting items 
ifit2D_DRT <- mirt::itemfit(DRT2d)
ifit2D_DRT[ifit2D_DRT[, 4] < 0.05, ]  ## misfitting items 

######## 5. FACTOR EXTRACTION WITH OBLIQUE ROTATION ############################ 
fa_DRT2d <- mirt::summary(DRT2d, rotate = "oblimin")

### Arrange all the factors accoring to the item number with the highest loading at the top 
fa_DRT2d <- as.data.frame(fa_DRT2d$rotF)
f1_DRT2d <- fa_DRT2d%>% arrange(-F1) 
f2_DRT2d <- fa_DRT2d%>% arrange(-F2)
f1_DRT2d[,3] <- rownames(f1_DRT2d)
f2_DRT2d[,3] <- rownames(f2_DRT2d)
f1_DRT2d <- f1_DRT2d[,-2]
f2_DRT2d <- f2_DRT2d[,-1]


####### 6. Rasch model #########################################################


