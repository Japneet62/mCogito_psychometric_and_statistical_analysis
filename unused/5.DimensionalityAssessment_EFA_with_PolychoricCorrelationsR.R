## -------- DIMENSIONALITY ASSESSMENT - EXPLORATORY FACTOR ANALYSIS WITH POLYCHORIC CORRELATIONS ---------- # 

# clear workspace 
rm(list=ls()) # clear environment 
graphics.off() # clear plots 

# library for EFA 
# library(psych)

# Load 2 questionnaires and 3 neuropsycological tests 
library(readxl) 
path_clean_data <- "/Users/japneetbhatia/desktop/thesis/10.CLEAN_DATA"
setwd(path_clean_data) 

  # loaded as data.frame 
  AMT_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 1)) # dichotomous = 0, 1 = incorrect, correct 
  CBSF_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 2)) # polytomous = 0, 1, 2, 3 = independent, minor dependence, dependent, majorly dependent 
  DRT_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 4)) # dichotomous = 0, 1 = incorrect, correct 
  FRT_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 5)) # dichotomous = 0, 1 = incorrect, correct 
  iADL_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 6)) # polytomous = 0, 1, 2 = indep, minor dependence, dependent
  demog_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 3)) # dichotomous = 0, 1 = incorrect, correct 
  
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
  
  # 1. CBSF - PART A - questionnaire 

    poly_CBSF_A <- polychoric(CBSF_A)
    # CBSF_A_3r had no variance and was deleted
    # too many other errors
    
    poly_CBSF_A <- polychoric(CBSF_A)
    
    
  
  # 2. CBSF - PART B 
  
  # poly_CBSF_B <- polychoric(CBSF_B)
  
  # 2.AMT 
  
  # 3. DRT 
    tch_DRT_sub <- tetrachoric(DRT_sub, smooth = TRUE)       # DRT_r15 and DRT_r17 had no variance and was deleted
    DRT_tr <-  tch_DRT_sub$rho
  
    # check if there are any negative eigen values 
    tail(round(eigen(tch_DRT_sub$rho)$values, 3)) # 4 -ve vals 
        
  # 4. FRT
    tch_FRT_sub <- tetrachoric(FRT_sub, smooth = TRUE) # item 1 - 4 
    FRT_tr <- tch_FRT_sub$rho
    
    # check if there are any negative eigen values 
    tail(round(eigen(tch_FRT_sub$rho)$values, 3)) # 4 -ve vals 
    
  # 5. iADL 
    poly_iADL <- polychoric(iADL_sub, smooth = TRUE) # item 6 
    iADL_pr <- poly_iADL$rho 
    
    # check if there are any negative eigen values 
    tail(round(eigen(poly_iADL$rho)$values, 3)) # 4 -ve vals 
    
## STEP 2. there could be two problems in this step: 
    # 1. correlations that are not high enough  - Barlett's test of sphericity 
        # compares an observed correlation matrix to identity matrix 
    
        # from polychoric and tetrachoric correlations 
        cortest.bartlett(DRT_tr, n = 39) 
        cortest.bartlett(FRT_tr, n = 39) 
        cortest.bartlett(iADL_pr, n = 39) 
    
    # 2. correlations are too high - 
        
        # Calculate Determinant of the R matrix should be greater than 0.00001
          det(DRT_tr)
          # 7.202302e-44
          
          det(FRT_tr)
          # 5.146085e-82
          
          det(iADL_pr)
          # 2.525562e-30
          
    # 3. KMO - check if the sample size is appropriate (between 0 and 1. 0 = factor analysis not appropriate, 1 = appropriate)
          a <- KMO(DRT_tr)
          a <- KMO(FRT_tr)
          KMO(iADL_pr)
          
          b<- data.frame(a$MSAi)
    
  ## STEP 3. PCA 
          DRT_PCA <- principal(DRT_tr, nfactors = 16, rotate = "none") 
          FRT_PCA <- principal(FRT_tr, nfactors = 24, rotate = "none") 
          iADL_PCA <- principal(iADL_pr, nfactors = 6, rotate = "none") 
          

  ## STEP 3. Variation - FA with MLF 
          DRT_FA <- fa(DRT_tr, nfactors = 16, rotate = "none", fm = "ml")
          FRT_FA <- fa(FRT_tr, nfactors = 24, rotate = "none", fm = "ml")
          iADL_FA <- fa(iADL_pr, nfactors = 6, rotate = "none", fm = "ml")
          
  ## STEP 4.Determine the number of factors with eignevalue above 1 
    
    # 1. Kaiser’s criterion = eigen value > 1
    # 2. Kaiser’s criterion - eigen value > 0.7
          
          
    DRT_e<- eigen(DRT_tr)
    DRT_eig <- data.frame(round(DRT_PCA$values,2)) # 1. 5 factors # 2. 6 factors 

    FRT_e <- eigen(FRT_tr)
    FRT_eig <- data.frame(round(FRT_e$values,2)) # 1. 9 factors # 2. 10 factors 
    
    iADL_e <- eigen(iADL_pr)
    i_eig <- data.frame(round(iADL_e$values,2)) # 1. 2 factors # 2. 3 factors 
    
    # 3. Scree plot 
    round(DRT_PCA$values,2) # eigen values 
    plot(round(DRT_PCA$values), type = "b")
    
    round(FRT_PCA$values,2) # eigen values 
    plot(round(FRT_PCA$values), type = "b")
    
    round(iADL_PCA$values,2) # eigen values 
    plot(round(iADL_PCA$values), type = "b")
    
    # scree plot method 2 
    
    DRT_Rdep <- tetrachoric(DRT_sub)$rho
    evals <- eigen(DRT_Rdep)$values 
    scree(DRT_Rdep, factors = FALSE) 
    
## 5. Factor rotation - orthogonal - done before CFA 
    
    # varimax 
    DRT_PCA_r <- principal(DRT_tr, nfactors = 5, rotate = "varimax") 
    print.psych(DRT_PCA_r, cut = 0.3, sort = TRUE) 
    
    FRT_PCA_r <- principal(FRT_tr, nfactors = 10, rotate = "varimax") 
    print.psych(FRT_PCA_r, cut = 0.3, sort = TRUE) 
    
    iADL_PCA_r <- principal(iADL_pr, nfactors = 3, rotate = "varimax") 
    print.psych(iADL_PCA_r, cut = 0.3, sort = TRUE) 
    
## 6. Factor rotation - oblique - when theory supports that factors are correlated 
    DRT_PCA_r <- principal(DRT_tr, nfactors = 5, rotate = "oblimin") 
    print.psych(DRT_PCA_r, cut = 0.3, sort = TRUE) 
    
    FRT_PCA_r <- principal(FRT_tr, nfactors = 10, rotate = "oblimin") 
    print.psych(FRT_PCA_r, cut = 0.3, sort = TRUE) 
    
    iADL_PCA_r <- principal(DRT_tr, nfactors = 3, rotate = "oblimin") 
    print.psych(iADL_PCA_r, cut = 0.3, sort = TRUE) 
    

    
    
    
