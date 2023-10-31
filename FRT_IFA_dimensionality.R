####### -----------------
# clear workspace 
rm(list=ls()) # clear environment 
graphics.off() # clear plots 

# loaded as data.frame 
FRT_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 5)) # dichotomous = 0, 1 = incorrect, correct 
FRT_sub <- FRT_data[, grep("F", colnames(FRT_data))]
rm(FRT_data)
FRT_sub <- FRT_sub[,-1] 
FRT_sub <- FRT_sub[,-1]
FRT_sub <- FRT_sub[,-1]

## explore the number of factors - princals  
pzar_FRT <- Gifi::princals(FRT_sub)
plot(pzar_FRT)

## plot individual scores along princals  
plot(pzar_FRT, 
     plot.type = "biplot", 
     col.scores = "black", 
     col.loadings = "red", 
     col.lines = "blue")

plot(pzar_FRT, "screeplot", 
     col.scores = "black", 
     col.loadings = "red", 
     col.lines = "blue")
# shows 2 dimensionality should be sufficient

## exploratory multidimensional 2PL model fit 
FRT1d <- mirt(FRT_sub, 1, itemtype = "2PL", TOL = 0.001) # unidimensional
FRT2d <- mirt(FRT_sub, 2, itemtype = "2PL", TOL = 0.001) # two dimensional
FRT3d <- mirt(FRT_sub, 3, itemtype = "2PL", TOL = 0.001) # three dimensional
FRT4d <- mirt(FRT_sub, 4, itemtype = "2PL", TOL = 0.001) # three dimensional

## LR test
anova(FRT1d,FRT2d, FRT3d) 

## M2 test 
M2(FRT4d)

## itemfit 
ifit1Dpl <- mirt::itemfit(FRT1d)

## itemfit 
ifit2Dpl <- mirt::itemfit(FRT2d)
ifit2Dpl[ifit2Dpl[,4]<0.05,]  # 1,7,8,9 items show misfit, need to be removed and data needs to be refitted. 

ifit3Dpl <- mirt::itemfit(FRT3d)
ifit3Dpl[ifit3Dpl[,4]<0.05,]  #book
ifit3Dpl[order(ifit3Dpl$p.S_X2),]  #video 

## remove items that do not fit ??? 


## rotations 
summary(FRT3d, rotate = "varimax") # orthogonal - 90% - independent, uncorrelated

itemplot(FRT2d, 1, main = "FRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 

coef(DRT3d) #intercept-slope parameterization 

#multidimensional difficulty index 
#multidimensional item location 
head(MDIFF(DRT3d)) 

# person parameter for each dimension
head(fscores(DRT3d)) 

# plot individual estmated factor values
plot( 
  mirt::fscores(
    object = DRT3d),  
  col = demog_data$StudyID,  
  pch = 17
)
