### --------- Dimensionality assessment - Item factor analysis (mirt) ------------- #### 

# clear workspace 
rm(list=ls()) # clear environment 
graphics.off() # clear plots 

### ------ 1. Load libraries ----------------- ### 
library(dplyr)
library(tidyr)
library(ggplot2)
library(mirt) 
library(Gifi)
library(readxl) 

### ------ 2. set path----------------- ### 
pathin <- "/Users/japneetbhatia/desktop/thesis/10.CLEAN_DATA" 
pathout <- "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS" 
scripts <- "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_ANALYSIS/scripts"

setwd(pathin)
##---- 3. LOAD REFINED DATA ----## 
                      
# loaded as data.frame 
demog_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 3)) # dichotomous = 0, 1 = incorrect, correct 
medical <- as.data.frame(read_excel("clean_data.xlsx", sheet = 7)) # dichotomous = 0, 1 = incorrect, correct 
AMT_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 1)) # dichotomous = 0, 1 = incorrect, correct 
CBSF_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 2)) # polytomous = 0, 1, 2, 3 = independent, minor dependence, dependent, majorly dependent 
DRT_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 4)) # dichotomous = 0, 1 = incorrect, correct 
FRT_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 5)) # dichotomous = 0, 1 = incorrect, correct 
iADL_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 6)) # polytomous = 0, 1, 2 = indep, minor dependence, dependent

# Data extraction suitable to fit the model  # extract the scores only, remove subjectID 
AMT_sub <- AMT_data[, grep("AMT", colnames(AMT_data))] 
CBSF_sub <- CBSF_data[, grep("CBSF", colnames(CBSF_data))]
CBSF_A <- CBSF_sub[, grep("CBSF_A", colnames(CBSF_sub))]
CBSF_B <- CBSF_sub[, grep("CBSF_B", colnames(CBSF_sub))]
DRT_sub <- DRT_data[, grep("DRT", colnames(DRT_data))]
FRT_sub <- FRT_data[, grep("F", colnames(FRT_data))]
iADL_sub <- iADL_data[,grep("iADL", colnames(iADL_data))]

# Correct errors in the stored data 
names(AMT_sub)[18] <- "AMT_12r"
names(FRT_sub)[30] <- "F_ans29"

# delete unwanted vars 
rm(CBSF_data)
rm(iADL_data)
rm(FRT_data)
rm(AMT_data)
rm(DRT_data)
rm(CBSF_sub)



### -------- 3. Conduct IFA for all the 2 tests (DRT, FRT) and 2 questionnaires (CBSF, iADL) ------- ### 

## ------ 1. Data cleaning, summarizing and filtering 



# Associative Match Task  




## ----- 2. check the item response distribution for each item by plotting 

demog_data <- demog_data[-15,]


# 1. Demographics 

library(ggplot2)

age_plot <- ggplot(demog_data, aes(x = age)) +
  geom_histogram(fill = "chocolate4",
                 color = "white",
                 bins = 30) +
  labs(title="Participants by age",
       subtitle = "number of bins = 30",
       x = "Age")

gender_plot <- ggplot(demog_data, aes(x = gender)) +
  geom_bar(fill = "brown") +
  labs(x = "Gender",
       y = "Population",
       title = "Participants by gender")

profession_plot <- ggplot(demog_data, aes(x = profession)) +
  geom_bar(fill = "orange") +
  labs(x = "Profession",
       y = "Population",
       )

# 2. Medical

med_dgn_plot <- ggplot(medical, aes(x = Med_daignosis)) +
  geom_bar(fill = "cornflowerblue") +
  labs(x = "Psychaitric daignosis",
       y = "Population",
       title = "Psychaitric daignosis")

meds_plot <- ggplot(medical, aes(x = Medicine)) +
  geom_bar(fill = "cornflowerblue") +
  labs(x = "Medicinal intake",
       y = "Population",
       title = "Participants by Medicinal intake")

# 3. Associative Match Task 


# ## ----- 2. correlations on item responses - tetrachoric (binary), polychoric (polytomous)
# library(Gifi)
# tetra_DRT <- tetrachoric(DRT_sub, smooth = TRUE)       
# tetra_FRT <- tetrachoric(FRT_sub, smooth = TRUE) # item 1,2,3 no variation 
# poly_iADL <- polychoric(iADL_sub, smooth = TRUE) # item 6 
# poly_CBSF_A <- polychoric(CBSF_A) 
# poly_CBSF_B <- polychoric(CBSF_B) 

## ------ 3. Remove items with no variation in responses 

# function to remove variance 
removeZeroVar1 <- function(df){
  df[, sapply(df, var) != 0]
}

AMT_sub <- removeZeroVar1(AMT_sub)  # 4 items left 
DRT_sub <- removeZeroVar1(DRT_sub) # 0 items were removed
FRT_sub <- removeZeroVar1(FRT_sub) # 3 items were removed
CBSF_A <- removeZeroVar1(CBSF_A) # 0 items were removed
CBSF_B <- removeZeroVar1(CBSF_B) # 0 items were removed
iADL_sub <- removeZeroVar1(iADL_sub) # 0 items were removed

## ----- 4. princals - to see the number of factors 

setwd(pathout) 

pdf(file = "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS/plots/pcals_scree_plots.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8) # The height of the plot in inches

library(Gifi)

pcals_DRT <- Gifi::princals(DRT_sub)
pcal_FRT <- Gifi::princals(FRT_sub)
pcals_iADL <- Gifi::princals(iADL_sub) # error 
pcals_CBSF_A <- Gifi::princals(CBSF_A)
pcals_CBSF_B <- Gifi::princals(CBSF_B)

  # Plot princals 
  plot(pcals_DRT, main = "pcals_DRT")
  plot(pcal_FRT, main = "pcal_FRT")
  plot(pcals_iADL, main = "pcals_iADL") # error 
  plot(pcals_CBSF_A, main = "pcals_CBSF_A")
  plot(pcals_CBSF_B, main = "pcals_CBSF_B")
  
## ----- 5. Scree plot
  plot(pcals_DRT, "screeplot", main = "Scree Plot: DRT" )
  plot(pcal_FRT, "screeplot", main = "Scree Plot: FRT")
  plot(pcals_iADL, "screeplot", main = "Scree Plot: iADL")
  plot(pcals_CBSF_A, "screeplot", main = "Scree Plot: CBSF_A")
  plot(pcals_CBSF_B, "screeplot", main = "Scree Plot: CBSF_B")
  
## ----- 6. DIMENSIONALITY ASSESSMENT WITH mirt - factor scores 
  
  # DRT 
  library(mirt)
  
  DRT1d <- mirt::mirt(DRT_sub, 1, itemtype = "2PL")  
  DRT2d <- mirt::mirt(DRT_sub, 2, itemtype = "2PL")  
  DRT3d <- mirt::mirt(DRT_sub, 3, itemtype = "2PL") 
  
  # FRT 
  
  FRT1d <- mirt::mirt(FRT_sub, 1, itemtype = "2PL")  
  FRT2d <- mirt::mirt(FRT_sub, 2, itemtype = "2PL")  
  FRT3d <- mirt::mirt(FRT_sub, 3, itemtype = "2PL") 
  FRT4d <- mirt::mirt(FRT_sub, 4, itemtype = "2PL")
  
  # iADL
  
  # CBSF
  
## ----- 6. LR test with anova
  LR_DRT <- anova(DRT1d,DRT2d, DRT3d) 
  LR_FRT <- anova(FRT1d, FRT2d, FRT3d) 
  
## ----- 7. M2 statistic
## ----- 8. Itemfit statistics
## ----- 9. Factor rotation
  fa_DRT <- mirt::summary(DRT2d, rotate = "oblimin") # orthogonal - 90% - independent, uncorrelated
  
  fa <- data.frame(fa_DRT$rotF)
  fa %>% arrange(f1)
  
  fa %>% arrange(F2)
  
  
  
## ----- 10. Orthogonal
## ----- 11. Oblique
## ----- 12. Item-plot
## ----- 13. Mean difference MDIFF 
## ----- 14. Person parameters

