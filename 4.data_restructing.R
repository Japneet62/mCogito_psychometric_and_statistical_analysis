######################################################################################################
########################### ------ DATA RESTRUCTURING --------- ######################################
######################################################################################################

# ------ 1. Clear work space and set path variables --------------------- 
rm(list=ls()) # clear environment 
graphics.off() # clear plots 

# ------ 2. Set path variables -------------------------------------------  
path_clean_data <- "/Users/japneetbhatia/desktop/thesis/10.CLEAN_DATA" 
path_results <- "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS" 
path_analysis_scripts <- "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_ANALYSIS/scripts"
path_plots <- "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS/plots" 

setwd(path_clean_data) 

# ------ 3. Load all libraries needed in the scripts later on ------------- 
library(readxl) 
library(dplyr) 
library(AMR)
library(ggplot2) 
library(tidyverse) 
library(psychometric)
library(Gifi)
library(mirt) 
library(stringi)
library(data.table) 
library(openxlsx) 
library(reshape2)
library(ggpubr) 
 
# -----  4. loaded data vars as data.frame -----------------------------------------  
demog_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 3)) # dichotomous = 0, 1 = incorrect, correct 
medical <- as.data.frame(read_excel("clean_data.xlsx", sheet = 7)) # dichotomous = 0, 1 = incorrect, correct 
demog_med <- merge(demog_data, medical)
AMT_subid <- as.data.frame(read_excel("clean_data.xlsx", sheet = 1)) # dichotomous = 0, 1 = incorrect, correct 
CBSF_subid <- as.data.frame(read_excel("clean_data.xlsx", sheet = 2)) # polytomous = 0, 1, 2, 3 = independent, minor dependence, dependent, majorly dependent 
DRT_subid <- as.data.frame(read_excel("clean_data.xlsx", sheet = 4)) # dichotomous = 0, 1 = incorrect, correct 
FRT_subid <- as.data.frame(read_excel("clean_data.xlsx", sheet = 5)) # dichotomous = 0, 1 = incorrect, correct 
iADL_subid <- as.data.frame(read_excel("clean_data.xlsx", sheet = 6)) # polytomous = 0, 1, 2 = indep, minor dependence, dependent
SART_subid <- as.data.frame(read_excel("clean_data.xlsx", sheet = 9)) # polytomous = 0, 1, 2 = indep, minor dependence, dependent

setwd(path_results)
edu <- as.data.frame(read_excel("edu.xlsx", sheet = 1)) 

# ------ 5. Correct column headings ------------------- 
names(demog_data)[4] <- "sex_M1_F0"  # change gender column name
names(demog_med)[4] <- "sex_M1_F0"  # change gender column name
names(AMT_subid)[19] <- "AMT_12r"
names(FRT_subid)[31] <- "F_ans29"
colnames(iADL_subid) <- c("SubjectID", "Using-telephone", "Travelling", "Shopping", 
                    "Food-preparation", "Housekeeping", "Medications", "Finances")

colnames(CBSF_subid) <- c("SubID", "Speaking", "Saliva & drooling", "Eating", "Chewing and swallowing", 
                    "Dressing", "Hygiene", "Handwriting or typing", "hobbies and leisure activities", 
                    "Turning in bed", "Getting in or out of bed, car, or deep chair", "Walking and balancing",
                    "Spontaneous involuntary movements", "Urinary control", "Visual problems", 
                    "Understanding what I read", "Thinking clearly", "Remembering", "Managing finances",  
                    "Performing complex or multiple tasks ", "Acting appropriately around others", 
                    "Doing things repetitively", "Diet and food preferences", "Mood", "Feeling anxious or panicky", 
                    "Feeling motivated", "Caring about others", "Agitation", "Sleeping at night", 
                    "Staying awake during the day", "Feeling physically or mentally fatigued", 
                    "Finding your way around")

colnames_CBSF <- c("SubID", "Spk", "SD", "Eat", "CS", "Drs", "Hyg", "HT", "HLA", "TB", 
                  "GBCDc", "WB", "SIM", "UCP", "VP", "UWR", "TC","RemT",
                  "MF", "PCMT","AAAO","DTR", "DFP", "FAP","FM", "CO", "Ag",
                  "SN", "SADD",  "FPMF","FWA")

# ------ 6. Correct column placement because of incorrect labeling ------------------- 
AMT_subid <- AMT_subid %>% relocate(AMT_12r, .before = AMT_13r) 
FRT_subid <- FRT_subid %>% relocate(F_ans29, .before = F_ans30)

# ------ 7. Add the empty rows for day2missing participants ---- 
# day2 <- data.frame("FRT_subid", "iADL_subid", "SART_subid")

Day2missingIDs <- as.data.frame(setdiff(demog_data$StudyID, FRT_subid$StudyID)) # find the subjectIDs for which day 2 is missing 

# FRT 
df_NA <- as.data.frame(matrix(NA, nrow = 4, ncol = 30)) # create an empty df with NAs
df_NA <- cbind(StudyID = Day2missingIDs,df_NA)# combine the subjectIDs with NA values 
names(df_NA)[1] <- "StudyID"
colnames(df_NA) <- colnames(FRT_subid)
FRT_subid_NA <- rbind(FRT_subid, df_NA) # add the NA cols with all the data for day 2 missing partic

# iADL 
df_NA <- as.data.frame(matrix(NA, nrow = 4, ncol = 7))
df_NA <- cbind(StudyID = Day2missingIDs,df_NA)# create an empty df with NAs
names(df_NA)[1] <- "StudyID"
colnames(df_NA) <- colnames(iADL_subid)
iADL_subid_NA <- rbind(iADL_subid, df_NA) 

# SART 
df_NA <- as.data.frame(matrix(NA, nrow = 4, ncol = 4))
df_NA <- cbind(StudyID = Day2missingIDs, df_NA) # create an empty df with NAs
names(df_NA)[1] <- "StudyID" 
colnames(df_NA) <- colnames(SART_subid) 
SART_subid_NA <- rbind(SART_subid, df_NA) 
SART_subid_NA <- transform(SART_subid_NA, om_err = as.numeric(om_err)) # change "canceled true" char values to "NA" and transform to numeric var
SART_subid_NA <- as.data.frame(transform(SART_subid_NA, com_err = as.numeric(com_err))) # change "canceled true" char values to "NA" and transform to numeric var

# ------ 8. Extract data without subID columns  ------------------- 
AMT <- AMT_subid[, grep("AMT", colnames(AMT_subid))] 
CBSF <- CBSF_subid[,c(-1)]
DRT <- DRT_subid[, grep("DRT", colnames(DRT_subid))]
FRT <- FRT_subid[, grep("F", colnames(FRT_subid))]
iADL <- iADL_subid[,c(-1)]
SART <- SART_subid[,grep("r", colnames(SART_subid))]

# ------ 9. Dummy code the values in the data frames  ------------------- 
# demog_data$sex_M1_F0<-ifelse(demog_data$sex_M1_F0=="M",1,0) # convert M = 1, F = 0

# ------ 10. Extract participant demog_data who completed both day 1 and day 2  ------------------- 
demog_data_day2missing <- demog_data[c(-10,-21,-25,-57),]
medical_data_day2mssing <- medical[c(-10, -21, -25, -57),]

demog_med_day2missing <- cbind(demog_data_day2missing,medical_data_day2mssing )

# ------ 11. Add studyID, age and gender info along with  the tasks  ------------------- 
AMT_grp <- cbind(AMT,demog_med)
CBSF_grp <- cbind(CBSF,demog_med)
DRT_grp <- cbind(DRT,demog_med)
SART_grp <- cbind(SART,demog_med_day2missing)
FRT_grp <- cbind(FRT,demog_med_day2missing)
iADL_grp <- cbind(iADL, demog_med_day2missing)

# ------ 12. Convert data into long format ----------------------------------------- 
AMT_long <- melt(AMT_grp, 
                 id.vars = c("StudyID", "age", "Edu", "sex_M1_F0", "profession"))

DRT_long <- melt(DRT_grp, 
                 id.vars = c("StudyID", "age", "Edu", "sex_M1_F0", "profession")) 

FRT_long <- melt(FRT_grp, 
                 id.vars = c("StudyID", "age", "Edu", "sex_M1_F0", "profession")) 

CBSF_long <- melt(CBSF_grp, 
                  id.vars = c("StudyID", "age", "Edu", "sex_M1_F0", "profession"))

iADL_long <- melt(iADL_grp, 
                  id.vars = c("StudyID", "age", "Edu", "sex_M1_F0", "profession")) 

SART_long <- melt(SART_grp, 
                  c("StudyID", "age", "Edu", "sex_M1_F0", "profession"))

# ------ 13. Remove data with no variance ----------------------------------------- 

# function to remove variance 
removeZeroVar1 <- function(df){ 
  df[, sapply(df, var) != 0]
}

# extract only the data which has some variance 
AMT_var <- removeZeroVar1(AMT) 
CBSF_var <- removeZeroVar1(CBSF) 
DRT_var <- removeZeroVar1(DRT) # 0 items were removed 
iADL_var<- removeZeroVar1(iADL) # 1 item was removed
FRT_var<- removeZeroVar1(FRT) # 3 items were removed (1,2,3) 

# ------ 14. Missing data  ------------------------------------------------------ 
SART_na <- SART_subid[is.na(SART_subid$com_err),] # SART has participants who didn't do the tests 

# ------ 14. Diving drt into recog and discrimination items 
DRT_recognition <- DRT[,c(3, 5, 8, 10, 12, 14, 16)] 
DRT_discrimination<- DRT[,c(1, 4, 6, 9, 11, 13, 18)] # our main data table 

# ------ 15. Sum scores - Tasks -------------------------------------------- 
AMT_sum <- data.frame(cbind(SumScores = rowSums(AMT), SubID = AMT_grp$SubID))
DRT_sum <- data.frame(cbind(SumScores = rowSums(DRT), SubID = DRT_grp$SubID))
DRT_r_sum <- data.frame(cbind(SumScores = rowSums(DRT_recognition), SubID = DRT_grp$SubID))
DRT_d_sum <- data.frame(cbind(SumScores = rowSums(DRT_discrimination), SubID = DRT_grp$SubID))
FRT_sum <- data.frame(cbind(SumScores = rowSums(FRT), SubID = FRT_grp$SubID))

# ------ 16. Sum scores - Questionnaires (sum per category) ------- 
CBSF_sum <- data.frame(cbind(SumScores = rowSums(CBSF), SubID = CBSF_subid$SubID))
iADL_sum <- data.frame(cbind(SumScores = rowSums(iADL), SubID = iADL_subid$SubjectID))

# ------ 17. Factorize the data  -------------------------------------------- 
demog_data$sex_M1_F0 <- factor(demog_data$sex_M1_F0, levels =c("M","F"),
                               labels=c("Male", "Female"))

demog_data_day2missing$sex_M1_F0 <-factor(demog_data_day2missing$sex_M1_F0,
                                          levels=c("M","F"),
                                          labels=c("Male", "Female"))

demog_med$Med_daignosis <- factor(demog_med$Med_daignosis, 
                                  levels = c("0", "1"), 
                                  labels = c("No psychological daignosis", 
                                             "Psychological disorder daignosed"))

demog_data$profession <- factor(demog_data$profession, levels =c("a","b","d","e","f","g","h"),
                                labels=c("Full-time employee", "Part-time employee", 
                                         "Self-employed/Freelancer", "Retired", "Housewife / househusband", 
                                         "Unemployed", "Independent income"))
demog_data_day2missing$profession <- factor(demog_data_day2missing$profession, levels =c("a","b","d","e","f","g","h"),
                                            labels=c("Full-time employee", "Part-time employee", 
                                                     "Self-employed/Freelancer", "Retired", "Housewife / househusband", 
                                                     "Unemployed", "Independent income"))

edu$EduYears <- factor(edu$EduYears, levels = c("10", "11", "12", "13", "14", "15", "16", "17", "18"))


# ------ 18. WRITE THE NAME OF ALL THE TASKS AND QUESTIONNAIRES ----------------- 


setwd(path_results)

filenames <- as.data.frame(c("AMT_subid", "DRT_subid", "CBSF_subid", "iADL_subid", "FRT_subid", "SART_subid",
                             "demog_med", "demog_med_day2missing"),)
colnames(filenames) <- "filenames" 
data_list <- mget(pull(filenames, 1)) 

dfs<- ls()[sapply(ls(), function(x) class(get(x))) == 'data.frame']   # extract all the df in R and save as a char vec
dfs <- Filter(function(x) is(x, "data.frame"), mget(ls())) # covert it to data frame

data_with_variance <- dfs[grepl("var", names(dfs))] # extract the data showing some variance 
grp_data <- dfs[grepl("grp", names(dfs))]

# data <- dfs[grepl(filenames, names(dfs))]


write.xlsx(data_with_variance, file = 'data_with_variance.xlsx') 
write.xlsx(grp_data, file = 'group_data.xlsx') 
write.xlsx(data_list, file = 'clean_data.xlsx') 

write.xlsx(SART_subid_NA, file = 'SART.xlsx')






# ------ 19. sum scores for all data ---- 
AMTsum <- rowSums(AMT)
DRTsum <- rowSums(DRT)
FRTsum <- rowSums(FRT)
iADLsum <- rowSums(iADL)
CBSFsum <- rowSums(CBSF)

# ------ 20. plot sum scores --------- 
par(mar = c(1, 1, 1, 1)) # fix fig margins 

plot(AMTsum)
plot(DRTsum)
plot(FRTsum)
plot(iADLsum)
plot(CBSFsum)
plot(SART$corr)
plot(SART$sum_err)
plot(SART$om_err) # skip error, just a warning 
plot(SART$com_err)

dev.off() 

