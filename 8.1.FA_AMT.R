#########################################################################
############## Factor extraction with mirt package - AMT  ###############
#########################################################################

## Clear work space and set path variables ----
rm(list=ls()) # clear environment 
graphics.off() # clear plots 

## libraries ---- 
library(mirt)
library(readxl)

## LOAD DATA without any variance ---- 
mainpath <- "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS" 
setwd(mainpath) 
AMT <- as.data.frame(read_excel("data_with_variance.xlsx", sheet = 1)) 

## MIRT - dimensionality assessment ---- 

AMT1d <- mirt::mirt(AMT, 1, itemtype = "2PL")  
AMT2d <- mirt::mirt(AMT, 2, itemtype = "2PL")  
AMT3d <- mirt::mirt(AMT, 3, itemtype = "2PL")  
AMT4d <- mirt::mirt(AMT, 4, itemtype = "2PL")  

## LR test - likelihood ratio test ---- 
LR_AMT <- anova(AMT1d, AMT2d, AMT3d, AMT4d) 

## M2 statistic ---- 
M2_AMT1d <- M2(AMT1d) 
M2_AMT2d <- M2(AMT2d) 
M2_AMT3d <- M2(AMT3d) 

M2_AMT <- rbind(M2_AMT1d, M2_AMT2d, M2_AMT3d)
rownames(M2_AMT) <- c("AMT1d", "AMT2d", "AMT3d")

## Write results ---- 
write.xlsx(LR_AMT, file = 'LR_AMT.xlsx') 
write.xlsx(M2_AMT, file = 'M2_AMT.xlsx') 


