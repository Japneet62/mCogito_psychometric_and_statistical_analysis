#########################################################################
############## Factor extraction with mirt package - CBSF  ###############
#########################################################################

## 1. Clear work space and set path variables ----
rm(list=ls()) # clear environment 
graphics.off() # clear plots 

## libraries  ----
library(mirt)
library(readxl)

## LOAD DATA without any variance  ----
mainpath <- "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS" 
setwd(mainpath) 
CBSF <- as.data.frame(read_excel("data_with_variance.xlsx", sheet = 2)) 

## MIRT - dimensionality assessment  ----
CBSF1D <- mirt::mirt(CBSF, 1, itemtype = "graded")  
CBSF2D <- mirt::mirt(CBSF, 2, itemtype = "graded")  
CBSF3D <- mirt::mirt(CBSF, 3, itemtype = "graded") 
CBSF4D <- mirt::mirt(CBSF, 4, method = 'QMCEM', itemtype = "graded") 

## LR test - likelihood ratio test  ----
LR_CBSF <- anova(CBSF1D, CBSF2D, CBSF3D, CBSF4D) 

## M2 statistic  ----
M2_CBSF1D <- M2(CBSF1D)
M2_CBSF2D <- M2(CBSF2D)
M2_CBSF3D <- M2(CBSF3D)
M2_CBSF4D <- M2(CBSF4D, QMC=TRUE) 

M2_CBSF <- rbind(M2_CBSF1D, M2_CBSF2D, M2_CBSF3D, M2_CBSF4D) 
rownames(M2_CBSF) <- c("M2_CBSF1D", "M2_CBSF2D", "M2_CBSF3D", "M2_CBSF4D")

## Item misfit 2D ----
ifit2D_CBSF <- mirt::itemfit(CBSF2D)  

ifit2D_CBSF <- ifit2D_CBSF[ifit2D_CBSF[, 4] < 0.05, ]  ## misfitting items 

library(dplyr) 

ifit2D_CBSF <- ifit2D_CBSF%>% arrange(RMSEA.S_X2) 
ifit2D_CBSF <- ifit2D_CBSF%>% arrange(p.S_X2) 

## Item misfit 4D ----
ifit4D_CBSF <- mirt::itemfit(CBSF4D)  
ifit4D_CBSF <- ifit4D_CBSF[ifit4D_CBSF[, 4] < 0.05, ]  ## misfitting items 

ifit4D_CBSF <- ifit4D_CBSF%>% arrange(RMSEA.S_X2) 
ifit4D_CBSF <- ifit4D_CBSF%>% arrange(p.S_X2) 

## Factor extraction with oblique rotations - 2D  ----
fa_CBSF2d <- mirt::summary(CBSF2D, rotate = "oblimin") # oblique - factors are allowed to correlate with each other 

## Factor extraction with oblique rotations - 4D  ----
fa_CBSF4d <- mirt::summary(CBSF4D, rotate = "oblimin") # oblique - factors are allowed to correlate with each other 

a <- fa_CBSF4d$rotF
h <- fa_CBSF4d$h2

# Communality - h2  ----
h2_CBSF <- fa_CBSF2d$h2 

### Arrange all the factors in descending order  ----
fa_CBSF2d <- as.data.frame(fa_CBSF2d$rotF)
f1_CBSF2d <- fa_CBSF2d%>% arrange(-F1) 
f1_CBSF2d[,2] <- rownames(fa_CBSF2d) 
f2_CBSF2d <- fa_CBSF2d%>% arrange(-F2) 
f2_CBSF2d[,1] <- rownames(fa_CBSF2d) 

### Arrange all the factors in descending order for 4D ----
fa_CBSF4d <- as.data.frame(fa_CBSF4d$rotF)
f1_CBSF4d <- fa_CBSF4d%>% arrange(F1) 
f2_CBSF4d <- fa_CBSF4d%>% arrange(F2) 
f3_CBSF4d <- fa_CBSF4d%>% arrange(F3) 
f4_CBSF4d <- fa_CBSF4d%>% arrange(F4) 

## Write results  ----
# write.xlsx(LR_CBSF, file = 'LR_CBSF.xlsx') 
# write.xlsx(M2_CBSF, file = 'M2_CBSF.xlsx') 




 

 