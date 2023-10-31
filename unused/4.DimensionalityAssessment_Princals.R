## -------- ITEM RESPONSE THEORY - STEP 1.  DIMENSIONALITY ASSESSMENT - PRINCALS -------- ## 

# clear workspace 

rm(list=ls()) # clear environment 
graphics.off() # clear plots 

# Install packages 
library(readxl) 

# Load clean data for all tasks and questionnaires 
path_clean_data <- "/Users/japneetbhatia/desktop/thesis/10.CLEAN_DATA/main"
setwd(path_clean_data) 

# it is important to load data as a data frame, for pricals to run properly. 

AMT_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 1)) # dichotomous = 0, 1 = incorrect, correct 
CBSF_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 2)) # polytomous = 0, 1, 2, 3 = independent, minor dependence, dependent, majorly dependent 
DRT_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 4)) # dichotomous = 0, 1 = incorrect, correct 
FRT_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 5)) # dichotomous = 0, 1 = incorrect, correct 
iADL_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 6)) # polytomous = 0, 1, 2 = indep, minor dependence, dependent

# --------------------------------------------------------------------------- # 
## Dimensionality assessment 
# --------------------------------------------------------------------------- # 

  ## 1. PRINCALS - Categorical PCA 

    # install.packages("mirt", "MPsychoR", "Gifi")   
    
    library("mirt") 
    library("MPsychoR")
    
    # Data extraction suitable to fit the model  # extract the scores, remove subjectID 
      
      AMT_sub <- AMT_data[, grep("AMT", colnames(AMT_data))] 
      CBSF_sub <- CBSF_data[, grep("CBSF", colnames(CBSF_data))]
      DRT_sub <- DRT_data[, grep("DRT", colnames(DRT_data))]
      FRT_sub <- FRT_data[, grep("F", colnames(FRT_data))]
      iADL_sub <- iADL_data[,grep("iADL", colnames(iADL_data))]
    
  # Attempt 1 -> 2 dimensional Princals 
    # categorical PCA
    # data should be in the form of data.frame, convert it. 
    # ordinal or nominal data? 
    
    library("Gifi") 
      
      # 1. AMT 
        # prinzar <- Gifi::princals(AMT_sub, ordinal = FALSE) # error bcz one whole column has value 0
        # 
        # plot(prinzar, main = "AMT loadings") 
        # 
        # AMT_dims <- as.character(AMT_dims) 
        # 
        # AMT_dims <- as.double(unlist(AMT_dims)) # convert list to double type
        # AMT_dims<- relist(AMT_dims, skeleton = AMT)  # relist to get the structure back
        
      
        # 2. CBSF 
        CBSF_sub <- CBSF_sub[,-3] # remove the column with all zeros and then try to fit in the data 
        prinzar_CBSF <- Gifi::princals(CBSF_sub, ordinal = FALSE) # works!! 
        plot(prinzar_CBSF, main = "CBSF loadings")  
        AMT_dims <- as.character(AMT_dims) 
        
              # Call:
              #   Gifi::princals(data = CBSF_sub, ordinal = FALSE)
              # 
              # Loss value: 0.739 
              # Number of iterations: 73 - model structure and data 
              # 
              # Eigenvalues: 9.797 5.876 
        
        
        # 3. DRT 
        # 4. FRT 
        # 5. iADL 
        iADL_sub <- iADL_sub[,-6]
        prinzar <- Gifi::princals(iADL_sub, ordinal = FALSE) 
        
        
    library("Gifi") 
    prinzar <- Gifi::princals(AMT_dims) 
    plot(prinzar, main = "AMT loadings") 
    
    # check if there is any voilations of dimensionality, by looking into the plot 
    # and searching for a var that is not in a similar direction as the rest of the vars 
