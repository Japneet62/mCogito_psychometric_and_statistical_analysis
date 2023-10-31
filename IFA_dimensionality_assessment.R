## Dimensionality assessment - Item Factor Analysis ## 

# clear workspace 
rm(list=ls()) # clear environment 
graphics.off() # clear plots 

## mirt attempt 1 ------------------------------------------------ ### 

library(mirt) # multidimensional IRT models 

# Load data 

  # 1. DRT 
    library(readxl) 
    path_clean_data <- "/Users/japneetbhatia/desktop/thesis/10.CLEAN_DATA/main"
    setwd(path_clean_data) 
    
    DRT_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 4)) # dichotomous = 0, 1 = incorrect, correct 
    DRT_sub <- DRT_data[, grep("DRT", colnames(DRT_data))]
    rm(DRT_data)

    
# IFA 
    mirt_DRT1D <- mirt(DRT_sub, model = 1, verbose = FALSE, itemtype = '2PL') 
    mirt_DRT1.1 <- mirt(DRT_sub, model = 1, verbose = FALSE, TOL = 0.001) 
    mirt_DRT1.1.1 <- mirt::mirt(DRT_sub, model = 1, technical = list(NCYCLES = 1e5), verbose = FALSE)     
    mirt_DRT2D <- mirt::mirt(DRT_sub, model = 2, verbose = FALSE, itemtype = '2PL') # EM cycles terminated after 500 iterations.
    mirt_DRT2.1 <- mirt::mirt(DRT_sub, model = 2, verbose = FALSE, TOL = 0.001) # EM cycles terminated after 500 iterations.
    mirt_DRT3 <- mirt::mirt(DRT_sub, model = 3, verbose = FALSE) 
    
        # mirt_DRT3 - plotting error
          # Error in cloud.formula(x = score ~ Theta1 + Theta2 | Theta3, data = list( : 
          # formal argument "ylim" matched by multiple actual arguments
    
    # summary and plot 
    mirt:summary(object = mirt_DRT1) 
    mirt::plot(x = mirt_DRT2D) 
    
    # anova 
    mirt::anova(mirt_DRT1D, mirt_DRT2D, mirt_DRT3, verbose = FALSE)

  # learn about logistic regression? 

    
    
library(ltm)
# irtpar_DRT <- ltm(DRT_sub ~ z1) 
# fapar_DRT <- ltm(DRT_sub ~ z1, IRT.param = FALSE)
# round(head(cbind(coef(irtpar_DRT), coef(fapar_DRT))), 3)
# 
# irtppar_DRT <- factor.scores(irtpar_DRT)$scores.sat$z1
# faapar_DRT <- factor.scores(fapar_DRT)$scores.dat$z1
# identical(irtppar_DRT, faapar_DRT)

## attempt 2 ------ 4.7.2 book - exploratory multidimensional IRT models (mirt) --------- ## 
    
    library(Gifi)
    
    ## Load data 
    
    # 1. DRT 
    library(readxl) 
    path_clean_data <- "/Users/japneetbhatia/desktop/thesis/10.CLEAN_DATA/main"
    setwd(path_clean_data) 

    demog_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 3)) # deographics
    DRT_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 4)) # dichotomous = 0, 1 = incorrect, correct 
    DRT_sub <- DRT_data[, grep("DRT", colnames(DRT_data))]
    rm(DRT_data)
    
    ## explore the number of factors - princals  
    pzar_DRT <- Gifi::princals(DRT_sub)
    plot(pzar_DRT)
    
    ## plot individual scores along princals  
    plot(pzar_DRT, 
         plot.type = "biplot", 
         col.scores = "black", 
         col.loadings = "red", 
         col.lines = "blue")
    
    plot(pzar_DRT, "screeplot", 
         col.scores = "black", 
         col.loadings = "red", 
         col.lines = "blue")
      # shows 2 dimensionality should be sufficient
    
    ## exploratory multidimensional 2PL model fit 
    DRT1d <- mirt(DRT_sub, 1, itemtype = "2PL") # unidimensional
    DRT2d <- mirt(DRT_sub, 2, itemtype = "2PL") # two dimensional
    DRT3d <- mirt(DRT_sub, 3, itemtype = "2PL") # two dimensional
    DRT3.3d <- mirt(DRT_sub, 3, itemtype = "3PL") # two dimensional
    
    
      ## LR test
      anova(DRT1d,DRT2d, DRT3d, DRT3.3d) 
      
      ## M2 test 
      M2(DRT1d)
      
    
    ## itemfit 
    ifit2Dpl <- mirt::itemfit(DRT2d)
    ifit2Dpl[ifit2Dpl[,4]<0.05,]  # 1,7,8,9 items show misfit, need to be removed and data needs to be refitted. 
    
    ifit3Dpl <- mirt::itemfit(DRT3d)
    ifit3Dpl[ifit3Dpl[,4]<0.05,]  #book
    ifit3Dpl[order(ifit3Dpl$p.S_X2),]  #video
    
    ## remove items that do not fit ??? 
    DRT_sub <- DRT_sub[,-8]
 
    
    ## rotations 
    mirt::summary(DRT3d, rotate = "varimax") # orthogonal - 90% - independent, uncorrelated
     
    itemplot(DRT2d, 1, main = "DRT", rot = list(xaxis = -70, yaxis = 50, zaxis = 10)) 
    
    coef(DRT2d) #intercept-slope parameterization 
    
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
    
    # itemplot 
    itemplot(DRT3d, 3, main = "D", 
             rot = list(xaxis = -70, yaxis = 50, zaxis = 10))
    

