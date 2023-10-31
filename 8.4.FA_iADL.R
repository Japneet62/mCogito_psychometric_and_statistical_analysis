#########################################################################
############## Factor extraction with mirt package - iADL  ###############
#########################################################################

## 1. Clear work space and set path variables 
rm(list=ls()) # clear environment 
graphics.off() # clear plots 

## libraries 
library(mirt)
library(plyr)
library(readxl)

## LOAD DATA without any variance 
mainpath <- "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS" 
setwd(mainpath) 
iADL <- as.data.frame(read_excel("data_with_variance.xlsx", sheet = 5)) 

## MIRT - dimensionality assessment 
iADL1d <- mirt::mirt(iADL, 1, itemtype = "graded")  
iADL2d <- mirt::mirt(iADL, 2, itemtype = "graded")  
iADL3d <- mirt::mirt(iADL, 3, itemtype = "graded") 
iADL4d <- mirt::mirt(iADL, 4, method = 'QMCEM', itemtype = "graded") 

## LR test - likelihood ratio test 
LR_iADL <- anova(iADL1d, iADL2d, iADL3d, iADL4d) 

## M2 statistic 
M2_iADL1D <- M2(iADL1d)
M2_iADL2D <- M2(iADL2d)
M2_iADL3D <- M2(iADL3d) 

M2_iADL <- rbind(M2_iADL1D, M2_iADL2D, M2_iADL3D) 
rownames(M2_iADL) <- c("iADL1d", "iADL2d", "iADL3d")

## Item misfit 
ifit1D_iADL <- mirt::itemfit(iADL1d)  

ifit1D_iADL <- ifit1D_iADL[ifit1D_iADL[, 4] < 0.05, ]  ## misfitting items 

ifit1D_iADL <- ifit1D_iADL%>% arrange(RMSEA.S_X2) 
ifit1D_iADL <- ifit1D_iADL%>% arrange(p.S_X2) 

## Factor extraction with oblique rotations - 2D 
fa_iADL1d <- mirt::summary(iADL1d, rotate = "oblimin") # oblique - factors are allowed to correlate with each other 

### Arrange all the factors accoring to the item number with the highest loading at the top 
fa_iADL1d <- as.data.frame(fa_iADL1d$rotF)
f1_iADL1d <- fa_iADL1d%>% arrange(-F1) 
f1_iADL1d[,2] <- rownames(fa_iADL1d) 

# ## Write results is
# write.xlsx(LR_iADL file = 'LR_iADL.xlsx') 
# write.xlsx(M2_iADL, file = 'M2_iADL.xlsx')


