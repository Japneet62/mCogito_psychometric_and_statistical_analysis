#########################################################################
############## Summary statistics - Item analysis  ######################
#########################################################################

## 1. Clear work space and set path variables 
rm(list=ls()) # clear environment 
graphics.off() # clear plots 
pathin <- "/Users/japneetbhatia/desktop/thesis/10.CLEAN_DATA" 
pathout <- "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS/summary_stats" 
scripts <- "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_ANALYSIS/scripts"
pathout_plots <- "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS/plots" 

setwd(pathin) 

## 2. Libraries 
library(readxl) 
library(dplyr)
library(AMR)
library(ggplot2)
library(tidyverse)
library(psychometric)
library(Gifi)
library(mirt) 
library(stringi)

# 3. loaded as data.frame

demog_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 3)) # dichotomous = 0, 1 = incorrect, correct 
medical <- as.data.frame(read_excel("clean_data.xlsx", sheet = 7)) # dichotomous = 0, 1 = incorrect, correct 
demog_med <- merge(demog_data, medical)

AMT_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 1)) # dichotomous = 0, 1 = incorrect, correct 
CBSF_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 2)) # polytomous = 0, 1, 2, 3 = independent, minor dependence, dependent, majorly dependent 
DRT_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 4)) # dichotomous = 0, 1 = incorrect, correct 
FRT_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 5)) # dichotomous = 0, 1 = incorrect, correct 
iADL_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 6)) # polytomous = 0, 1, 2 = indep, minor dependence, dependent

# Data extraction suitable to fit the model  # extract the scores only, remove subjectID 
AMT <- AMT_data[, grep("AMT", colnames(AMT_data))] 
names(AMT)[18] <- "AMT_12r" # correct name so all col names match 
CBSF <- CBSF_data[, grep("CBSF", colnames(CBSF_data))]
CBSF_A <- CBSF[, grep("CBSF_A", colnames(CBSF))]
CBSF_B <- CBSF[, grep("CBSF_B", colnames(CBSF))]
DRT <- DRT_data[, grep("DRT", colnames(DRT_data))]
FRT <- FRT_data[, grep("F", colnames(FRT_data))]
names(FRT)[30] <- "F_ans29"
iADL <- iADL_data[,grep("iADL", colnames(iADL_data))]

# delete unwanted vars 
rm(CBSF_data)
rm(iADL_data)
rm(FRT_data)
rm(AMT_data)
rm(DRT_data)

###################################################################################
#                                     PLOTTING                                    #
###################################################################################

setwd(pathout_plots)

pdf(file = "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS/plots/plots.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8) # The height of the plot in inches


####################################################################
############## 1. Demographics and medical status ##################
####################################################################

## ----- 4. SUMMARIZE DEMOGRAPHICS 
age_groups <- demog_data %>% group_by(AMR::age_groups(age, split_at = c(50, 55, 60, 65, 70, 75, 80))) %>% summarize(count=n())
colnames(age_groups) <- c("Age groups", "Total")
gender <- demog_data %>% group_by(gender) %>% summarize(count=n())
profession <- demog_data %>% group_by(profession) %>% summarize(count=n())
edu <- demog_data %>% group_by(Edu) %>% summarize(count=n())

## --- list profession meanings in a seperate table 
professions_list <- data_frame(c("a", "b", "c", "d", "e", "f", "g", "h"))
professions_list[,2] <- data.frame(c("Full-time employee", "Part-time employee" , "Business owner", "Self-employed/Freelancer", "Retired", "Housewife / househusband", "Unemployed", "Independent income"))
colnames(professions_list) <- c("responses", "professions")

## ----- 5. SUMMARIZE MEDICAL STATUS 
# calculate the number of people diagnosed with psychiatric illnesses 
psych_dng_total <- medical %>% group_by(Med_daignosis) %>% summarize(count=n())
psych_dng_total[,1] <- c("No", "Yes")

# Extract the data of the participants diagnosed with psychiatric disorders. 
psych_dgn_partic <- filter(demog_med, Med_daignosis == "1") 
psych_daignosis_partic <- psych_dgn_partic[,c(1,7)]

# calculate the number of people taking some kind of medications
meds_total <- medical %>% group_by(Medicine) %>% summarize(count=n()) 
meds_total[,1] <- c("No", "Yes") 

###################################################################################
############## 2. Associative Match Task ##########################################
###################################################################################
## Dichotomous data - 0/1 (incorrect/correct)
## 

## ---- 1. ITEM RESPONSE VARIANCE (remove items with no variance) ---- # 

# function to remove variance 
removeZeroVar1 <- function(df){
  df[, sapply(df, var) != 0]
}

AMT_sub <- removeZeroVar1(AMT)

## ---- 2. ITEM CATEGORIES (create a table) -------------------------- # 
AMT_item_categories <- data.frame(c("Vehicals", "Kitchen objects", "Food items", "Cloths", "Animals", "Stationary", "Mechanical tools"))
AMT_item_categories[1,2] <- "1" 
AMT_item_categories[2,2] <- c("6, 11, 12, 14, 18")
AMT_item_categories[3,2] <- c("8, 9, 13")
AMT_item_categories[4,2] <- c("5, 17")
AMT_item_categories[5,2] <- c("3, 10")
AMT_item_categories[6,2] <- c("4, 15")
AMT_item_categories[7,2] <- c("2, 7, 16")
colnames(AMT_item_categories) <- c("Semantic category", "Items")

## ---- 3. ITEM RESPONSE DISTRIBUTION ------------------------------- #   
AMT_summary_scores <- data.frame(apply(AMT_sub, 2, table))

## ---- 4. ITEM DIFFICULTY ------------------------------------------ #   
AMT_dif <- data.frame(round(colMeans(AMT_sub, na.rm = T), 2))
AMT_dif[,2] <- data.frame(colnames(AMT_sub)) # write item numbers along in next column 
colnames(AMT_dif) <- c("difficulty", "items") # table headings 

# arrange items with increasing levels of difficulty 
AMT_dif <- AMT_dif %>% arrange(-difficulty)
AMT_dif[,2] <- rownames(AMT_dif)

#########################################################################################
############## 3. CBSF ##################################################################
#########################################################################################
## Scores: 0, 1, 2, 3, 4 (Normal (No), Light, Mild, Moderate, Major (Assistance needed))

## ---- 1. ITEM RESPONSE VARIANCE (remove items with no variance) ---- # 
CBSF_A <- removeZeroVar1(CBSF_A) # 0 items were removed
CBSF_B <- removeZeroVar1(CBSF_B) # 0 items were removed

## ---- 2. ITEM CATEGORIES (predivided -> motor and non-motor scale) -------------------------- # 

## ---- 3. ITEM RESPONSE DISTRIBUTION (all items) ------------------------------- #   
CBFS_A_summary_scores <- apply(CBSF_A, 2, table) # summary scores 
CBFS_B_summary_scores <- apply(CBSF_B, 2, table) 
CBFS_A_summary_scores <- stri_list2matrix(CBFS_A_summary_scores, byrow=TRUE) # convert to matrix
CBFS_B_summary_scores <- stri_list2matrix(CBFS_B_summary_scores, byrow=TRUE) 
colnames(CBFS_A_summary_scores) <- c("0", "1", "2") #column headings 
colnames(CBFS_B_summary_scores) <- c("0", "1", "2","3","4")
CBFS_A_summary_scores <- as.data.frame(unlist(CBFS_A_summary_scores))
CBFS_B_summary_scores <- as.data.frame(unlist(CBFS_B_summary_scores))

## Plot 

## ---- 4. ITEM DIFFICULTY ------------------------------------------ #   

MDIFF()

# ## 2nd method 
# CBSF_A_dif_temp <- psychometric::item.exam(CBSF_A, y = NULL, discrim = TRUE)
# CBSF_B_dif_temp <- psychometric::item.exam(CBSF_B, y = NULL, discrim = TRUE)
# 
# mirt::gen.difficulty(CBSF_A_dif_temp, type = "IRF", interval = c(-30, 30))
# 
# 
# # arrange items with decreasing levels of difficulty 
# CBSF_A_dif <- as.data.frame(CBSF_A_dif_temp %>% arrange(-Difficulty))
# CBSF_B_dif <- as.data.frame(CBSF_B_dif_temp %>% arrange(-Difficulty))
# 
# CBSF_A_dif <- as.data.frame(CBSF_A_dif_temp$Difficulty)
# CBSF_B_dif <- as.data.frame(CBSF_B_dif_temp$Difficulty) 
# 
# # Add item numbers 
# CBSF_A_dif[,2] <- rownames(CBSF_A_dif_temp)
# CBSF_B_dif[,2] <- rownames(CBSF_B_dif_temp)
# 
# rm(CBSF_A_dif_temp)
# rm(CBSF_B_dif_temp)

## ---- 5. PRINCALS -------------------------------------------------- # 
pcals_CBSF_A <- Gifi::princals(CBSF_A)
pcals_CBSF_B <- Gifi::princals(CBSF_B)

plot(pcals_CBSF_A, main = "pcals_CBSF_A")
plot(pcals_CBSF_B, main = "pcals_CBSF_B")

## Plot individual scores along with the princals 
plot(pcals_CBSF_A, 
     plot.type = "biplot", 
     col.scores = "black", 
     col.loadings = "red", 
     col.lines = "blue")

plot(pcals_CBSF_B, 
     plot.type = "biplot", 
     col.scores = "black", 
     col.loadings = "red", 
     col.lines = "blue")

## ---- 6. SCREE PLOT -------------------------------------------------- # 
plot(pcals_CBSF_A, "screeplot", main = "Scree Plot: CBSF_A")
plot(pcals_CBSF_B, "screeplot", main = "Scree Plot: CBSF_B")

## ---- 7. DIMENSIONALITY ASSESSMENT (IFA) WITH MIRT PACKAGE ----------- # 

# ## CBSF - A - Motor scale 
CBSF_A_1d <- mirt::mirt(CBSF_A, 1, itemtype = "graded")
CBSF_A_2d <- mirt::mirt(CBSF_A, 2, itemtype = "graded")
CBSF_A_3d <- mirt::mirt(CBSF_A, 3, itemtype = "graded")

coef(CBSF_A_1d)
summary(CBSF_A_1d)

coef(CBSF_A_2d)
summary(CBSF_A_2d)

# ## CBSF - B - Non-Motor scale 
CBSF_B_1d <- mirt::mirt(CBSF_B, 1, itemtype = "graded")
CBSF_B_2d <- mirt::mirt(CBSF_B, 2, itemtype = "graded")
CBSF_B_3d <- mirt::mirt(CBSF_B, 3, itemtype = "graded")

# # LR test with anova (AIC, BIC, p-value)
LR_CBSF_A <- anova(CBSF_A_1d,CBSF_A_2d, CBSF_A_3d) 
LR_CBSF_B <- anova(CBSF_B_1d,CBSF_B_2d, CBSF_B_3d) 

## ---- 8. FACTOR EXTRACTION -------------------------------------------------- # 
fa_CBSF_A <- mirt::summary(CBSF_A_2d, rotate = "oblimin") # factor extraction and oblique rotation
fa_CBSF_A <- data.frame(fa_CBSF_A$rotF) #get factors in another table 
f1_CBSF_A <- fa_CBSF_A %>% arrange(-F1) # arrange factor 1 in descending order
f2_CBSF_A <- fa_CBSF_A %>% arrange(-F2) # arrange factor 2 in descending order

f1_CBSF_A[,3] <- rownames(f1_CBSF_A)
f2_CBSF_A[,3] <- rownames(f2_CBSF_A)

fa_CBSF_B <- mirt::summary(CBSF_B_2d, rotate = "oblimin") 
fa_CBSF_B <- data.frame(fa_CBSF_B$rotF)
f1_CBSF_B <- fa_CBSF_B %>% arrange(-F1)
f2_CBSF_B <- fa_CBSF_B %>% arrange(-F2)

f1_CBSF_B[,3] <- rownames(f1_CBSF_B)
f2_CBSF_B[,3] <- rownames(f2_CBSF_B)

## ----- Item difficulty with mirt results 


###############################################################
############## 4. Delayed Object Recognition Task #############
###############################################################

## ---- 1. ITEM RESPONSE VARIANCE (remove items with no variance) ---------------- # 
DRT <- removeZeroVar1(DRT) # 0 items were removed

## ---- 2. ITEM CATEGORIES (divided into image present and image not present) ---- # 
cols_DRT <- colnames(DRT) 
p_DRT <- DRT[,c(3, 5, 8, 10, 12, 14, 16)] # image present
np_DRT <- DRT[,c(1, 2, 4, 6, 7, 9, 11, 13, 15, 17, 18)] # image not present:

## ---- 3. ITEM RESPONSE DISTRIBUTION ------------------------------- #   
p_DRT_summary_scores <- data.frame(apply(p_DRT, 2, table))
np_DRT_summary_scores <- data.frame(apply(np_DRT, 2, table))

## ---- 4. ITEM DIFFICULTY ------------------------------------------ #   
# image present 
p_DRT_dif <- data.frame(round(colMeans(p_DRT, na.rm = T), 2))
p_DRT_dif[,2] <- colnames(p_DRT)

# image not present 
np_DRT_dif <- data.frame(round(colMeans(np_DRT, na.rm = T), 2)) 
np_DRT_dif[,2] <- colnames(np_DRT)

# columns headings 
colnames(p_DRT_dif) <- c("Difficulty", "Items")
colnames(np_DRT_dif) <- c("Difficulty", "Items") 

# order data acc to item difficulty 
p_DRT_dif <- p_DRT_dif %>% arrange(-Difficulty)
np_DRT_dif <- np_DRT_dif %>% arrange(-Difficulty)

## another method to measure difficulty 
p_DRT_dif2 <- psychometric::item.exam(p_DRT, y = NULL, discrim = FALSE)
np_DRT_dif2 <- psychometric::item.exam(np_DRT, y = NULL, discrim = FALSE)

## ---- 5. PRINCALS -------------------------------------------------- # 
pcals_DRT <- Gifi::princals(DRT)
plot(pcals_DRT, main = "pcals_DRT")

pcals_p_DRT <- Gifi::princals(p_DRT)
plot(pcals_p_DRT, main = "pcals_p_DRT")

pcals_np_DRT <- Gifi::princals(np_DRT)
plot(pcals_np_DRT, main = "pcals_np_DRT")

## Plot individual scores along with the princals 
plot(pcals_p_DRT, 
     plot.type = "biplot", 
     col.scores = "black", 
     col.loadings = "red", 
     col.lines = "blue")

## ---- 6. SCREE PLOT -------------------------------------------------- # 
plot(pcals_DRT, "screeplot", main = "Scree Plot: DRT" )
plot(pcals_p_DRT, "screeplot", main = "Scree Plot: p_DRT" )
plot(pcals_np_DRT, "screeplot", main = "Scree Plot: np_DRT" )

## ---- 7. DIMENSIONALITY ASSESSMENT (IFA) WITH MIRT PACKAGE ----------- # 

## 1. -- DRT GLOBAL ----  
DRT1d <- mirt::mirt(DRT, 1, itemtype = "2PL")
DRT2d <- mirt::mirt(DRT, 2, itemtype = "2PL")
DRT3d <- mirt::mirt(DRT, 3, itemtype = "2PL")

##LR TEST
anova(DRT1d, DRT2d, DRT3d)

## ---- 8. FACTOR EXTRACTION -------------------------------------------------- # 
fa_DRT <- mirt::summary(DRT2d, rotate = "oblimin") # factor extraction and oblique rotation
fa_DRT <- data.frame(fa_DRT$rotF) #get factors in another table 
f1_DRT <- fa_DRT %>% arrange(-F1) # arrange factor 1 in descending order
f2_DRT <- fa_DRT %>% arrange(-F2) # arrange factor 1 in descending order

f1_DRT[,3] <- rownames(f1_DRT)
f2_DRT[,3] <- rownames(f2_DRT)

###########################################################################################
############## 5. iADL ####################################################################
###########################################################################################
## Scores: 0, 1, 2 (Independent, Minor dependence, Majorly dependent)
## Polytomous data; likert scale. 

## ---- 1. ITEM RESPONSE VARIANCE (remove items with no variance) ----------------- # 
iADL <- removeZeroVar1(iADL) # 1 item was removed

## ---- 2. ITEM CATEGORIES (no separate category - skip) -------------------------- # 

## ---- 3. ITEM RESPONSE DISTRIBUTION --------------------------------------------- #   
iADL_summary_scores <- apply(iADL, 2, table)

iADL_summary_scores <- stri_list2matrix(iADL_summary_scores, byrow=TRUE)
colnames(iADL_summary_scores) <- c("0", "1", "2")

## ---- 4. ITEM DIFFICULTY -------------------------------------------------------- #   
## 2nd method 
iADL_dif <- psychometric::item.exam(iADL, y = NULL, discrim = FALSE) # calculate difficulty 
iADL_dif <- iADL_dif %>% arrange(-Difficulty) # arrange items with decreasing levels of difficulty 
iADL_dif[,10] <- rownames(iADL_dif) # Add item numbers 

## ---- 5. PRINCALS --------------------------------------------------------------- # 
pcals_iADL <- Gifi::princals(iADL)
plot(pcals_iADL, main = "pcals_iADL")

## Plot individual scores along with the princals 
plot(pcals_iADL, 
     plot.type = "biplot", 
     col.scores = "black", 
     col.loadings = "red", 
     col.lines = "blue")

## ---- 6. SCREE PLOT ------------------------------------------------------------- # 
plot(pcals_iADL, "screeplot", main = "Scree Plot: iADL" )

## ---- 7. DIMENSIONALITY ASSESSMENT (IFA) WITH MIRT PACKAGE ----------- # 

iADL1d <- mirt::mirt(iADL, 1, itemtype = "graded")
iADL2d <- mirt::mirt(iADL, 2, itemtype = "graded")
iADL3d <- mirt::mirt(iADL, 3, itemtype = "graded")

## ---- 8. FACTOR EXTRACTION -------------------------------------------------- # 
fa_iADL <- mirt::summary(iADL1d, rotate = "oblimin") # factor extraction and oblique rotation
fa_iADL <- data.frame(fa_iADL$rotF) #get factors in another table 
f1_iADL <- fa_iADL %>% arrange(-F1) # arrange factor 1 in descending order
f1_iADL[,2] <- rownames(f1_iADL)

fa_iADL2d <- mirt::summary(iADL2d, rotate = "oblimin") # factor extraction and oblique rotation
fa_iADL2d <- data.frame(fa_iADL2d$rotF) #get factors in another table 
f1_iADL2d <- fa_iADL2d %>% arrange(-F1) # arrange factor 1 in descending order
f2_iADL2d <- fa_iADL2d %>% arrange(-F2) # arrange factor 1 in descending order

f1_iADL2d[,3] <- rownames(f1_iADL2d)
f2_iADL2d[,3] <- rownames(f2_iADL2d)


###########################################################################################
############## 6. Figural Reasoning Task ##################################################
###########################################################################################

## Dichotomous data: 0/1 
## 

## ---- 1. ITEM RESPONSE VARIANCE (remove items with no variance) ----------------- # 
FRT <- removeZeroVar1(FRT) # 3 items were removed (1,2,3) 

## ---- 2. ITEM CATEGORIES (???revise after factor extraction????) ---------------- # ??????

## ---- 3. ITEM RESPONSE DISTRIBUTION --------------------------------------------- #   
FRT_summary_scores <- data.frame(apply(FRT, 2, table))

## ---- 4. ITEM DIFFICULTY -------------------------------------------------------- #   
FRT_dif <- data.frame(round(colMeans(FRT, na.rm = T), 2))
FRT_dif[,2] <- colnames(FRT)
colnames(FRT_dif) <- c("Difficulty", "Items") 

## arrange items in increasing difficulty 
FRT_dif <- FRT_dif %>% arrange(-Difficulty)

## ---- 5. PRINCALS ---------------------------------------------------------------- # 
pcals_FRT <- Gifi::princals(FRT)
plot(pcals_FRT, main = "pcals_FRT")

## Plot individual scores along with the princals 
plot(pcals_FRT, 
     plot.type = "biplot", 
     col.scores = "black", 
     col.loadings = "red", 
     col.lines = "blue")

## ---- 6. SCREE PLOT -------------------------------------------------------------- # 
plot(pcals_FRT, "screeplot", main = "Scree Plot: FRT" )

## ---- 7. DIMENSIONALITY ASSESSMENT (IFA) WITH MIRT PACKAGE ----------- # 

FRT1d <- mirt::mirt(DRT, itemtype = "2PL")  
FRT2d <- mirt::mirt(DRT, 2, itemtype = "2PL")  
FRT3d <- mirt::mirt(DRT, 3, itemtype = "2PL") 

# LR test with anova (AIC, BIC, p-value)
LR_DRT <- anova(FRT1d,FRT2d) 

## ---- 8. FACTOR EXTRACTION -------------------------------------------------- # 
fa_FRT2d <- mirt::summary(FRT2d, rotate = "oblimin") # factor extraction and oblique rotation
fa_FRT2d <- data.frame(fa_FRT2d$rotF) #get factors in another table 
f1_FRT2d <- fa_FRT2d %>% arrange(-F1) # arrange factor 1 in descending order
f2_FRT2d <- fa_FRT2d %>% arrange(-F2) # arrange factor 1 in descending order

f1_FRT2d[,3] <- rownames(f1_FRT2d) 
f2_FRT2d[,3] <- rownames(f2_FRT2d) 

###################################################################################
#                                     PLOTTING                                    #
###################################################################################
dev.off()


###################################################################################
#                           WRITE RESULTS OF THIS SCRIPT                          #
###################################################################################

# Remove tables you don't want to write 

rm(list= c("AMT", "demog_data", "demog_med", "psych_dgn_partic", "medical", 
           "CBSF_A", "CBSF_B", "FRT", "iADL", "p_DRT", "np_DRT", "DRT"))

setwd(pathout) 
dfs<- ls()[sapply(ls(), function(x) class(get(x))) == 'data.frame']   # extract all the df in R and save as a char vec
dfs <- Filter(function(x) is(x, "data.frame"), mget(ls())) 

# stores data in multiple sheets, in one excel file 

library(openxlsx) 
write.xlsx(dfs, file = 'summary_stats.xlsx') 












