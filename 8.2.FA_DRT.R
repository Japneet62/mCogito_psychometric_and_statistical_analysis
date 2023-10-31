#########################################################################
############## Factor extraction with mirt package - DRT  ###############
#########################################################################

## 1. Clear work space and set path variables  ----
rm(list=ls()) # clear environment 
graphics.off() # clear plots 

## libraries 
library(mirt)
library(readxl)

## LOAD DATA without any variance  ----
mainpath <- "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS" 
setwd(mainpath) 
DRT <- as.data.frame(read_excel("data_with_variance.xlsx", sheet = 3))  

## Remove items with high ceiling effects 
DRT <- DRT[,c(-15,-17)] 

### ------------ STEP 1. DIMENSIONALITY ASSESSMENT FOR ALL DATA -------- 

## ------ Dimensionality assessment and factorial extraction ----- 
  
## MIRT - dimensionality assessment - 2PL ----
DRT1d <- mirt::mirt(DRT, 1, itemtype = "2PL", SE = TRUE)  
DRT2d <- mirt::mirt(DRT, 2, itemtype = "2PL", SE = TRUE)  
DRT3d <- mirt::mirt(DRT, 3, itemtype = "2PL", SE = TRUE) 

## LR test - likelihood ratio test ----
LR_DRT <- anova(DRT1d,DRT2d, DRT3d) 

## M2 statistic ---- 
M2_DRT1d <- M2(DRT1d) 
M2_DRT2d <- M2(DRT2d) 
M2_DRT3d <- M2(DRT3d) 

M2_DRT <- rbind(M2_DRT1d, M2_DRT2d, M2_DRT3d)
rownames(M2_DRT) <- c("DRT1d", "DRT2d", "DRT3d") 

## Item misfit ---- 
ifit2D_DRT <- mirt::itemfit(DRT2d) 
ifit2D_DRT <- ifit2D_DRT[ifit2D_DRT[, 4] < 0.05, ]  ## misfitting items 

ifit2D_DRT%>% arrange(RMSEA.S_X2) 
ifit2D_DRT%>% arrange(p.S_X2) 

## Factor extraction with oblique rotations (correlation allowed) ---- 
fa_DRT2d <- mirt::summary(DRT2d, rotate = "oblimin") # oblique - factors are allowed to correlate with each other 

  ### Arrange all the factors accoring to the item number with the highest loading at the top 
  fa_DRT2d <- as.data.frame(fa_DRT2d$rotF)
  f1_DRT2d <- fa_DRT2d%>% arrange(-F1) 
  f2_DRT2d <- fa_DRT2d%>% arrange(-F2)
  f1_DRT2d[,3] <- rownames(f1_DRT2d) 
  f2_DRT2d[,3] <- rownames(f2_DRT2d)
  f1_DRT2d <- f1_DRT2d[,-2] 
  f2_DRT2d <- f2_DRT2d[,-1] 

## Intercept-slope parameterization ---- 
coef_DRT2d <- coef(DRT2d) 

## Plot item plots ---- 
itemplot(DRT2d, 1, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
itemplot(DRT2d, 2, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
itemplot(DRT2d, 3, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
itemplot(DRT2d, 4, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
itemplot(DRT2d, 5, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
itemplot(DRT2d, 6, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
itemplot(DRT2d, 7, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
itemplot(DRT2d, 8, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
itemplot(DRT2d, 9, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
itemplot(DRT2d, 10, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
itemplot(DRT2d, 11, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
itemplot(DRT2d, 12, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
itemplot(DRT2d, 13, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
itemplot(DRT2d, 14, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
itemplot(DRT2d, 15, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
itemplot(DRT2d, 16, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
itemplot(DRT2d, 17, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
itemplot(DRT2d, 18, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 


 
### ----------- STEP 2. DIMENSIONALITY ASSESSMENT FOR TWO sub-factorial structure items (recognition, discrimination)  ----

## Divide data into two halves and fit two separate models on the data  ----
DRT_recognition <- DRT[,c(3, 5, 8, 10, 12, 14, 16)]
DRT_discrimination <- DRT[,c(1, 2, 4, 6, 7, 9, 11, 13, 15, 17, 18)]
DRT_sub_discrimination <- DRT[,c(1, 4, 6, 9, 11, 13, 18)]

# --------- Check the factorial struture after dividing DRT into hits and false positive items --------- 
DRT_recognition <- DRT[,c(3, 5, 8, 10, 12, 14, 16)] # our main data table 

        ## MIRT - dimensionality assessment  ----
        DRT1d_p <- mirt::mirt(DRT_recognition, 1, itemtype = "2PL", SE = TRUE)  
        DRT2d_p <- mirt::mirt(DRT_recognition, 2, itemtype = "2PL", SE = TRUE)  
        
        ## LR test - likelihood ratio test  ----
        LR_DRT_recognition <- anova(DRT1d_p, DRT2d_p) 
        
        ## M2 statistic  ----
        M2_DRT1d_p <- M2(DRT1d_p) 
        M2_DRT2d_p <- M2(DRT2d_p) 
        
        M2_DRT_recognition <- rbind(M2_DRT1d_p, M2_DRT2d_p)
        rownames(M2_DRT_recognition) <- c("DRT1d_p", "DRT2d_p") 
        
        ## Item misfit  ----
        ifit1D_DRT_recognition <- mirt::itemfit(DRT1d_p) 
        ifit2D_DRT_recognition <- mirt::itemfit(DRT2d_p) 
        ifit1D_DRT_recognition <- ifit1D_DRT_recognition[ifit1D_DRT_recognition[, 4] < 0.05, ]  ## misfitting items 
        
        ifit1D_DRT_recognition%>% arrange(RMSEA.S_X2) 
        ifit1D_DRT_recognition%>% arrange(p.S_X2) 
        
        ## Factor extraction with oblique rotations  ----
        fa_DRT1d_p <- mirt::summary(DRT1d_p, rotate = "oblimin") # oblique - factors are allowed to correlate with each other 
        
        ### Arrange all the factors accoring to the item number with the highest loading at the top  ----
        fa_DRT1d_p <- as.data.frame(fa_DRT1d_p$rotF)
        f1_DRT1d_p <- fa_DRT1d%>% arrange(-F1) 
        f1_DRT1d_p[,2] <- rownames(f1_DRT1d_p) 
        
        ## Intercept-slope parameterization  ----
        
        coef(DRT1d_p) 

## --------- factor extraction for discrimination - false positives ---------------- ##  ----
DRT_discrimination <- DRT[,c(1, 2, 4, 6, 7, 9, 11, 13, 15, 17, 18)] # our main data table 

        ## MIRT - dimensionality assessment  ----
        DRT1d_np <- mirt::mirt(DRT_discrimination, 1, itemtype = "2PL", SE = TRUE)  
        DRT2d_np <- mirt::mirt(DRT_discrimination, 2, itemtype = "2PL", SE = TRUE)  
        
        ## LR test - likelihood ratio test  ----
        LR_DRT_discrimination <- anova(DRT1d_np, DRT2d_np) 
        
        ## M2 statistic  ----
        M2_DRT1d_np <- M2(DRT1d_np) 
        M2_DRT2d_np <- M2(DRT2d_np) 
        
        M2_DRT_discrimination <- rbind(M2_DRT1d_np, M2_DRT2d_np)
        rownames(M2_DRT_discrimination) <- c("DRT1d_np", "DRT2d_np") 
        
        ## Item misfit  ----
        ifit1D_DRT_discrimination <- mirt::itemfit(DRT1d_np) 
        # ifit1D_DRT_discrimination <- ifit1D_DRT_discrimination[ifit1D_DRT_discrimination[, 4] < 0.05, ]  ## misfitting items 
        
        ifit1D_DRT_discrimination%>% arrange(RMSEA.S_X2) 
        ifit1D_DRT_discrimination%>% arrange(p.S_X2) 
        
        ## Factor extraction with oblique rotations  ----
        fa_DRT1d_np <- mirt::summary(DRT1d_np, rotate = "oblimin") # oblique - factors are allowed to correlate with each other 
         
        ### Arrange all the factors accoring to the item number with the highest loading at the top  ----
        fa_DRT1d_np <- as.data.frame(fa_DRT1d_np$rotF)
        f1_DRT1d_np <- fa_DRT1d_np%>% arrange(-F1) 
        f1_DRT1d_np[,2] <- rownames(f1_DRT1d_np) 
        
        ## Intercept-slope parameterization  ----
        coef(DRT1d_np) 
        

## ----------- STEP 3. REMOVE MISFITTING ITEMS AND REFIT THE MODEL  ----

## ---------- factor extraction for false positives after removing misfitting items (2,7,15,17) ---------- ###  ----

## Remove item 17 and 15 because of ceiling effects  ----
DRT_csub <- DRT[,c(-15,-17)]

## Remove item 2 and 7 because of misfitting in factorial loading structure ----
DRT_cfsub <- DRT_csub[,c(-2,-7)]

          ## MIRT - dimensionality assessment - 2PL  ----
          DRT1d <- mirt::mirt(DRT_cfsub, 1, itemtype = "2PL", SE = TRUE)  
          DRT2d <- mirt::mirt(DRT_cfsub, 2, itemtype = "2PL", SE = TRUE)  
          DRT3d <- mirt::mirt(DRT_cfsub, 3, itemtype = "2PL", SE = TRUE) 
          
          ## LR test - likelihood ratio test  ----
          LR_DRT <- anova(DRT1d,DRT2d, DRT3d) 
          
          ## M2 statistic  ----
          M2_DRT1d <- M2(DRT1d) 
          M2_DRT2d <- M2(DRT2d) 
          M2_DRT3d <- M2(DRT3d) 
          
          M2_DRT <- rbind(M2_DRT1d, M2_DRT2d, M2_DRT3d)
          rownames(M2_DRT) <- c("DRT1d", "DRT2d", "DRT3d") 
          
          ## Item misfit  ----
          ifit2D_DRT <- mirt::itemfit(DRT2d) 
          ifit2D_DRT <- ifit2D_DRT[ifit2D_DRT[, 4] < 0.05, ]  ## misfitting items 
          
          ifit2D_DRT <- ifit2D_DRT%>% arrange(-RMSEA.S_X2) 
          ifit2D_DRT%>% arrange(p.S_X2) 
          
          ## Factor extraction with oblique rotations  ----
          fa_DRT2d <- mirt::summary(DRT2d, rotate = "oblimin") # oblique - factors are allowed to correlate with each other 
          
          ### Arrange all the factors accoring to the item number with the highest loading at the top  ----
          fa_DRT2d <- as.data.frame(fa_DRT2d$rotF)
          f1_DRT2d <- fa_DRT2d%>% arrange(-F1) 
          f2_DRT2d <- fa_DRT2d%>% arrange(-F2)
          f1_DRT2d[,3] <- rownames(f1_DRT2d) 
          f2_DRT2d[,3] <- rownames(f2_DRT2d)
          f1_DRT2d <- f1_DRT2d[,-2] 
          f2_DRT2d <- f2_DRT2d[,-1] 
          
          ## Intercept-slope parameterization  ----
          coef_DRT2d <- coef(DRT2d) 
          
          ## Plot item plots  ----
          itemplot(DRT2d, 1, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
          itemplot(DRT2d, 2, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
          itemplot(DRT2d, 3, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
          itemplot(DRT2d, 4, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
          itemplot(DRT2d, 5, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
          itemplot(DRT2d, 6, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
          itemplot(DRT2d, 7, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
          itemplot(DRT2d, 8, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
          itemplot(DRT2d, 9, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
          itemplot(DRT2d, 10, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
          itemplot(DRT2d, 11, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
          itemplot(DRT2d, 12, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
          itemplot(DRT2d, 13, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
          itemplot(DRT2d, 14, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
          itemplot(DRT2d, 15, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
          itemplot(DRT2d, 16, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
          itemplot(DRT2d, 17, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
          itemplot(DRT2d, 18, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 




# then run the codes above again to see the factorial structure 

## Write results  ----
write.xlsx(LR_DRT, file = 'LR_DRT.xlsx') 
write.xlsx(M2_DRT, file = 'M2_DRT.xlsx') 
write.xlsx(f1_DRT2d, file = 'f1_DRT2d.xlsx')
write.xlsx(f2_DRT2d, file = 'f2_DRT2d.xlsx') 
write.xlsx(ifit2D_DRT, file = 'itemfit_DRT.xlsx')



