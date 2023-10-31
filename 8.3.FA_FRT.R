#########################################################################
############## Factor extraction with mirt package - AMT  ###############
#########################################################################

## Clear work space and set path variables ----
rm(list=ls()) # clear environment 
graphics.off() # clear plots 

## libraries 
library(mirt)
library(readxl)

## LOAD DATA without any variance  ----
mainpath <- "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS" 
setwd(mainpath) 
FRT <- as.data.frame(read_excel("data_with_variance.xlsx", sheet = 4)) 

## remove item 9, 26 and 30 and see if you find any difference in the results  ----
FRT <- FRT[,c(-7, -24,-28)] # item 9,26 and 30
# FRT <- FRT[,c(-24)] # item 26 

## MIRT - dimensionality assessment  ----

FRT1d <- mirt::mirt(FRT, 1, itemtype = "2PL")  
FRT2d <- mirt::mirt(FRT, 2, itemtype = "2PL")  
FRT3d <- mirt::mirt(FRT, 3, itemtype = "2PL") 
# FRT4d <- mirt::mirt(FRT, 4, itemtype = "2PL", method = "QMCEM")
# FRT4d <- mirt::mirt(FRT, 5, itemtype = "2PL", method = "QMCEM")

## LR test - likelihood ratio test  ----
LR_FRT <- anova(FRT1d, FRT2d, FRT3d) 

## M2 statistic  ----
M2_FRT1d <- M2(FRT1d) 
M2_FRT2d <- M2(FRT2d) 
M2_FRT3d <- M2(FRT3d) 

M2_FRT <- rbind(M2_FRT1d, M2_FRT2d, M2_FRT3d) 
rownames(M2_FRT) <- c("FRT1d", "FRT2d", "FRT3d") 
 
## Item misfit 1D  ----
ifit1D_FRT <- mirt::itemfit(FRT1d) 
    # ifit1D_FRT <- ifit1D_FRT[ifit1D_FRT[, 4] < 0.05, ]  ## misfitting items 
    # 
    # ifit1D_FRT <- ifit1D_FRT%>% arrange(-RMSEA.S_X2) 
    # ifit1D_FRT <- ifit1D_FRT%>% arrange(p.S_X2) 
 
## Item misfit 2D  ----
ifit2D_FRT <- mirt::itemfit(FRT2d) 
# ifit2D_FRT <- ifit2D_FRT[ifit2D_FRT[, 4] < 0.05, ]  ## misfitting items 
# 
# ifit2D_FRT <- ifit2D_FRT%>% arrange(-RMSEA.S_X2) 
# ifit2D_FRT <- ifit2D_FRT%>% arrange(p.S_X2) 

 
## Item misfit 3D  ----
ifit3D_FRT <- mirt::itemfit(FRT3d) 
# ifit3D_FRT <- ifit3D_FRT[ifit2D_FRT[, 4] < 0.05, ]  ## misfitting items 
# 
# ifit3D_FRT <- ifit3D_FRT%>% arrange(-RMSEA.S_X2) 
# ifit3D_FRT <- ifit3D_FRT%>% arrange(p.S_X2) 

  
## --- Factor extraction with oblique rotations - 1D ----
  fa_FRT1d <- mirt::summary(FRT1d, rotate = "oblimin")
  
  ### Arrange all the factors accoring to the item number with the highest loading at the top 
    fa_FRT1d <- as.data.frame(fa_FRT1d$rotF)
    f1_FRT1d <- fa_FRT1d%>% arrange(-F1) 

## ---- Factor extraction with oblique rotations - 2D  ----
  fa_FRT2d <- mirt::summary(FRT2d, rotate = "oblimin") # oblique - factors are allowed to correlate with each other 
  
  ### Arrange all the factors accoring to the item number with the highest loading at the top 
  fa_FRT2d <- as.data.frame(fa_FRT2d$rotF) 
  f1_FRT2d <- fa_FRT2d%>% arrange(-F1) 
  f2_FRT2d <- fa_FRT2d%>% arrange(-F2)
  f1_FRT2d[,3] <- rownames(f1_FRT2d) 
  f2_FRT2d[,3] <- rownames(f2_FRT2d)
  f1_FRT2d <- f1_FRT2d[,-2] 
  f2_FRT2d <- f2_FRT2d[,-1] 

## Factor extraction with oblique rotations - 3D  ----
  fa_FRT3d <- mirt::summary(FRT3d, rotate = "oblimin") # oblique - factors are allowed to correlate with each other 
  
  ### Arrange all the factors accoring to the item number with the highest loading at the top  ----
  fa_FRT3d <- as.data.frame(fa_FRT3d$rotF)
  f1_FRT3d <- fa_FRT3d%>% arrange(-F1) # factor 1
  f2_FRT3d <- fa_FRT3d%>% arrange(-F2) # factor 2
  f3_FRT3d <- fa_FRT3d%>% arrange(-F3) # factor 3
  f1_FRT3d[,4] <- rownames(f1_FRT3d) 
  f1_FRT3d <- f1_FRT3d[,1] 
  f2_FRT3d <- f2_FRT3d[,-1] 
  
## Factor extraction with oblique rotations - 3D  ----
fa_FRT1d <- mirt::summary(FRT1d, rotate = "oblimin") # oblique - factors are allowed to correlate with each other 
fa_FRT1d <- as.data.frame(fa_FRT1d$rotF)
f1_FRT1d <- fa_FRT1d%>% arrange(-F1) 


# ## Write results   ----
write.xlsx(LR_FRT, file = 'LR_FRT.xlsx')
write.xlsx(M2_FRT, file = 'M2_FRT.xlsx')
write.xlsx(M2_FRT, file = 'M2_FRT.xlsx')




  