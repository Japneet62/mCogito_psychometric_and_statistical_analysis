#########################################################################
############## -------- DESCRIPTIVE STATISTICS ------------------  ######
#########################################################################

## ---- 1. Clear work space and set path variables  -------- 
rm(list=ls()) # clear environment 
graphics.off() # clear plots 
pathin <- "/Users/japneetbhatia/desktop/thesis/13.STATISTICAL_ANALYSIS/scripts" 

setwd(pathin) 
source("4.data_restructing.R")  

## ---- 2. SUMMARIZE DEMOGRAPHICS - age, gender, profession, edu -------- 
# 1. AGE ------- 
age_groups <- demog_data %>% 
  group_by(AMR::age_groups(age, split_at = c(50, 55, 60, 65, 70, 75, 80))) %>% 
  dplyr::summarize(count=n()) 

colnames(age_groups) <- c("Age groups", "Total")

# 2. GENDER ------ 
gender_d1 <- demog_data_day2missing %>%
  group_by(sex_M1_F0) %>%
  dplyr::summarize(count = n()) %>%
  mutate(gender = ifelse(sex_M1_F0 == 1, "male", "female"))

gender_d1d2 <- demog_data %>%
  group_by(sex_M1_F0) %>%
  dplyr::summarize(count = n()) %>%
  mutate(gender = ifelse(sex_M1_F0 == 1, "male", "female"))

# 3. PROFESSION -------- 
profession <- demog_data %>% group_by(profession) %>% dplyr::summarize(count=n())

# List the meanings of professions 
professions_list <- data_frame(c("a", "b", "c", "d", "e", "f", "g", "h"))
professions_list[,2] <- data.frame(c("Full-time employee", "Part-time employee", "Buisness owner", 
                                     "Self-employed/Freelancer", "Retired", "Housewife / househusband", "Unemployed", "Independent income"))
colnames(professions_list) <- c("responses", "professions")

# 4. EDUCATION ------- 
# edu <- demog_data %>% group_by(Edu) %>% dplyr::summarize(count=n())
# 
# # Group the education data together to find how many people belong to each group 
# realschule <- edu[edu$Edu %like% "Real", ] 
# realschule[4,] <- edu[edu$Edu %like% "Mittlere", ]
# berufschule <- edu[edu$Edu %like% "Beruf", ]
# fachschule <- edu[edu$Edu %like% "Fachhoch", ]
# fachschule[5,] <- edu[edu$Edu %like% "Fachschulabsch", ]        # Extract matching rows with Fachschule
# hauptschule <- edu[edu$Edu %like% "Haupt", ]        # Extract matching rows with haputschu;e 
# gymnasium <- edu[edu$Edu %like% "Abitur",]
# uni  <- edu[edu$Edu %like% c("Uni"), ]        # Extract matching rows with uni 
# bachelors <- edu[edu$Edu %like% "Bachelor", ]        # Extract matching rows with bachelors 
# bachelors <- rbind(uni,bachelors)
# master <- edu[edu$Edu %like% "Master", ]        # Extract matching rows with Realschule 
# phd <- edu[edu$Edu %like% "Promotion", ]        # Extract matching rows with Realschule 
# ausbildung <- edu[edu$Edu %like% "Ausbildung", ] 
# diploma <- edu[edu$Edu %like% "Diplom", ] 
# diploma[4,] <- edu[edu$Edu %like% "DIP", ] 
# Staatsexamen <- edu[edu$Edu %like% "Staatsexamen", ] 
# 
# ## SUM ## 
# realschule_sum <- sum(realschule$count)
# berufschule_sum <- sum(berufschule$count)
# fachschule_sum <- sum(fachschule$count)
# hauptschule_sum <- sum(hauptschule$count)
# gymnasium_sum <- sum(gymnasium$count)
# bachelors_sum <- sum(bachelors$count)
# masters_sum <- sum(master$count)
# PhD_sum <- sum(phd$count)
# ausbildung_sum <- sum(ausbildung$count)
# diploma_sum <- sum(diploma$count)
# Staatsexamen_sum <- sum(Staatsexamen$count)
# 
# edu_summary <- as.data.frame(rbind(hauptschule_sum, realschule_sum, berufschule_sum, fachschule_sum,
#                                    gymnasium_sum, bachelors_sum, ausbildung_sum, diploma_sum, 
#                                    masters_sum, PhD_sum, Staatsexamen_sum)) 

# # Change the education data 
# demog_data$Edu <- gsub("Mittlere Reife", "Realschule", demog_data$Edu) # Realschule
# demog_data$Edu <- gsub("Realschulabschluss", "Realschule", demog_data$Edu)
# demog_data$Edu <- gsub("Realschule und Facharbeiter", "Realschule", demog_data$Edu)
# demog_data$Edu <- gsub("Abgeschlossene Berufsausbildung", "Berufsausbildung", demog_data$Edu) # Berufsausbildung
# demog_data$Edu <- gsub("Fachhochschulabschluss", "Fachhochschule", demog_data$Edu) # Fachhochschule
# demog_data$Edu <- gsub("Fachhochschuldiplom", "Fachhochschule", demog_data$Edu)
# demog_data$Edu <- gsub("Fachhochschulreife", "Fachhochschule", demog_data$Edu)
# demog_data$Edu <- gsub("Fachschulabschluss", "Fachhochschule", demog_data$Edu)
# demog_data$Edu <- gsub("Bachelor degree", "Bachelor", demog_data$Edu) # Bachelor
# demog_data$Edu <- gsub("Bachelor Uni", "Bachelor", demog_data$Edu)
# demog_data$Edu <- gsub("Uni", "Bachelor", demog_data$Edu)
# demog_data$Edu <- gsub("Uni Staatsexamen + Diplom + Promotion", "Bachelor", demog_data$Edu) 
# demog_data$Edu <- gsub("Diplom", "Diploma", demog_data$Edu) # diploma 
# demog_data$Edu <- gsub("Diplom FH", "Diploma", demog_data$Edu) 
# demog_data$Edu <- gsub("Diploma FH", "Diploma", demog_data$Edu) 
# demog_data$Edu <- gsub("Diplomaa", "Diploma", demog_data$Edu) 
# demog_data$Edu <- gsub("Diplomaa FH", "Diploma", demog_data$Edu) 
# demog_data$Edu <- gsub("DIPLOM", "Diploma", demog_data$Edu) 
# demog_data$Edu <- gsub("DIPLOM", "Diploma", demog_data$Edu) 
# demog_data$Edu <- gsub("Bachelor Staatsexamen + Diploma + Promotion", "Promotion", demog_data$Edu) 
# 
# edu <- demog_data %>% group_by(Edu) %>% dplyr::summarize(count=n()) 

# -- summarize edu years data 
demog_data <- cbind(demog_data, EduYears = edu$EduYears) 
demog_med <- cbind(demog_med, EduYears = edu$EduYears )

demog_data %>%
  group_by(EduYears) 

edu %>%
  group_by(EduYears) 

# 
eduYears <- edu %>% group_by(EduYears) %>% dplyr::summarize(count=n())


# ----- Plot eduYears for males and females 


# 5. PSYCHOLOGICAL DAIGNOSIS  ------- 
psych_dng_total <- medical %>% group_by(Med_daignosis) %>% dplyr::summarize(count=n())
psych_dng_total[,1] <- c("No", "Yes") 

# Extract the data of the participants diagnosed with psychiatric disorders. 
psych_dgn_partic <- filter(demog_med, Med_daignosis == "1") 
psych_daignosis_partic <- psych_dgn_partic[,c(1,7)]

# 6. MEDICATIONS ------- 
meds_total <- medical %>% group_by(Medicine) %>% dplyr::summarize(count=n()) 
meds_total[,1] <- c("No", "Yes") 


## ---- 3. PLOTS for single vars ----- 
# 1. age    -----
age_plot <- ggplot(demog_data, aes(x = age)) +
  geom_histogram(fill = "cornflowerblue",
                 color = "white",
                 bins = 30) +
  labs(title="Participants by age",
       subtitle = "number of bins = 30",
       x = "Age")

age_plot 

# 2. gender   -----

gender_plot <- ggplot(demog_data, aes(x = sex_M1_F0)) +
  geom_bar(fill = "cornflowerblue") +
  labs(x = "Gender", 
       y = "Population", 
       title = "Participants by gender") 
gender_plot 

# 3. profession  -----
profession_plot <- ggplot(demog_data, aes(x = profession)) +
  geom_bar(fill = "orange") +
  labs(x = "Profession",
       y = "Populatsion") +
  scale_x_discrete(labels= professions_list$professions)+
  coord_flip() 

profession_plot 


# 4. psychiatric_diagnosis  -----
psy_dgn_plot <- ggplot(medical, aes(x = Med_daignosis)) +
  geom_bar(fill = "cornflowerblue") +
  labs(x = "Psychaitric daignosis",
       y = "Population",
       title = "Psychaitric daignosis")

psy_dgn_plot

# 5. medicinal intake  -----
meds_plot <- ggplot(medical, aes(x = Medicine)) +
  geom_bar(fill = "cornflowerblue") +
  labs(x = "Medicinal intake",
       y = "Population",
       title = "Participants by Medicinal intake")

meds_plot 


#### -------  4. SUM SCORES FOR EACH SCORE TYPE (correct, incorrect, etc) FOR ALL PARTICIPANTS  ---------- ##### 
#1. AMT ----
sum.sc.AMT2 <- data.frame(apply(AMT, 2, table)) # sum the total responses for each res category    
sum.sc.AMT2 <- data.frame(t(sum.sc.AMT2)) # change table structure 
rownames(sum.sc.AMT2) -> sum.sc.AMT2[,3] # print 
colnames(sum.sc.AMT2) <- c("R0", "R1", "Items") # change table names 
sum.sc.AMT3 <- sum.sc.AMT2 %>% filter(grepl('Freq', Items)) # extract columns which only shows the frequencies 
sum.sc.AMT3[,-3] -> sum.sc.AMT4 # remove items column 

# Initialize a result vector 
result <- logical(nrow(sum.sc.AMT4) - 1)

# Loop through the matrix to change the same adjacent value for R0 to 0
for (i in 1:length(sum.sc.AMT4$R0)) {
  if (sum.sc.AMT4$R0[i] == sum.sc.AMT4$R1[i]) {
    result[i] <- TRUE
    if (result[i] == TRUE){
      sum.sc.AMT4$R0[i] = "0"
    }
  }
}


rownames(sum.sc.AMT4) -> sum.sc.AMT4[,3] # get item numbers in the 3rd col 
colnames(sum.sc.AMT4) <- c("R0", "R1", "Items") # correct headings 
sum.sc.AMT4[,3] <- gsub(".Freq","",sum.sc.AMT4$Items) # Item column remove ".Freq"
sum.sc.AMT4[,3] <- gsub("r","",sum.sc.AMT4$Items) # Item column remove "r"
sum.sc.AMT4[,3] <- gsub("_","",sum.sc.AMT4$Items) # # Item column remove "_"

sum.sc.AMT <- sum.sc.AMT4 # save processed data in final var 

rm(list = "sum.sc.AMT4", "sum.sc.AMT3", "sum.sc.AMT2") # remove unnecessary vars 

#2. CBFS  ----
sum.sc.CBFS <- apply(CBSF, 2, table) 
sum.sc.CBFS <- stri_list2matrix(sum.sc.CBFS, byrow=TRUE) # convert to matrix
sum.sc.CBFS <- as.data.frame(unlist(sum.sc.CBFS))
colnames(sum.sc.CBFS) <- c("No_assistance", "Slight_assistance", 
                           "Mild_assistance", "Moderate_sssistance", "Major_assistance")

#3. DRT  ----
sum.sc.DRT <- data.frame(apply(DRT, 2, table)) 
sum.sc.DRT <- data.frame(t(sum.sc.DRT)) # change table structure 

#4. iADL  ----
sum.sc.iADL <- apply(iADL, 2, table) # sum scores 
sum.sc.iADL <- stri_list2matrix(sum.sc.iADL, byrow=TRUE) # convert to matrix 
sum.sc.iADL <- as.data.frame(unlist(sum.sc.iADL)) # change to df 
colnames(sum.sc.iADL) <- c("Independent", "Minor dependence", "Major dependence") # add colnames 

#5. FRT  ----
sum.sc.FRT2 <- data.frame(apply(FRT, 2, table)) # sum the total responses for each res category    
sum.sc.FRT2 <- data.frame(t(sum.sc.FRT2)) # change table structure 
rownames(sum.sc.FRT2) -> sum.sc.FRT2[,3] # print 
colnames(sum.sc.FRT2) <- c("R0", "R1", "Items") # change table names 
sum.sc.FRT3 <- sum.sc.FRT2 %>% filter(grepl('Freq', Items)) # extract columns which only shows the frequencies 
sum.sc.FRT3[,-3] -> sum.sc.FRT4 # remove items column 

# Initialize a result vector
result <- logical(nrow(sum.sc.FRT4) - 1)

# Loop through the matrix to change the same adjacent value for R0 to 0
for (i in 1:length(sum.sc.FRT4$R0)) {
  if (sum.sc.FRT4$R0[i] == sum.sc.FRT4$R1[i]) {
    result[i] <- TRUE
    if (result[i] == TRUE){
      sum.sc.FRT4$R0[i] = "0"
    }
  }
}

rownames(sum.sc.FRT4) -> sum.sc.FRT4[,3] # get item numbers in the 3rd col 
colnames(sum.sc.FRT4) <- c("R0", "R1", "Items") # correct headings 
sum.sc.FRT4[,3] <- gsub(".Freq","",sum.sc.FRT4$Items) # Item column remove ".Freq"
sum.sc.FRT4[,3] <- gsub("_ans","",sum.sc.FRT4$Items) # # Item column remove "_"

sum.sc.FRT <- sum.sc.FRT4 # save processed data in final var 

rm(list = "sum.sc.FRT4", "sum.sc.FRT3", "sum.sc.FRT2")


## ---- 5. PLOTS comparing vars -----
# 1. Age against gender Day 1 ----- 

  with(demog_data_day2missing, summary(sex_M1_F0)) # total males and females 
  with(demog_data_day2missing, summary(age)) 
  GenderMeans = with(demog_data_day2missing, by(age, sex_M1_F0, mean)) 
  
  # plot gender against age 
  GenderPlot1 = ggplot(demog_data_day2missing, aes(x = sex_M1_F0, y = age, fill = sex_M1_F0)) + 
    geom_boxplot() +
    labs( x = "Sex (m = 28, f = 30)",
          y = "Age (mean = 60.76)", 
          title = "Age/sex distribution for particiapants - Day 1") + 
    theme(legend.position = "none", axis.title = element_text(size = 15), 
          axis.text.y = element_text(size = 12), 
          axis.text.x = element_text(size = 12), 
          plot.title = element_text(size = 15))
  
  GenderPlot1 
  


# 2. Age against gender Day 1, 2 ----

  with(demog_data, summary(sex_M1_F0)) # total males and females 
  with(demog_data, summary(age)) 
  
  GenderMeans = with(demog_data, by(age, sex_M1_F0, mean)) 
  
  
  GenderPlot2 = ggplot(demog_data, aes(x = sex_M1_F0, y = age, fill = sex_M1_F0)) + 
    geom_boxplot() +
    labs( x = "Sex (m = 31, f = 31)",
          y = "Age (mean = 60.76)", 
          title = "Age/sex distribution for particiapants - Day 1 and 2") + 
    theme(legend.position = "none", axis.title = element_text(size = 15), 
          axis.text.y = element_text(size = 12), 
          axis.text.x = element_text(size = 12), 
          plot.title = element_text(size = 15))
  
  GenderPlot2
  

# 3. Years of education against gender ----
  with(demog_data, summary(EduYears)) # summarize eduYears 
  with(demog_data, summary(sex_M1_F0)) 
  
  eduYears_mean = with(demog_data, by(EduYears, sex_M1_F0, mean)) 
  
  
## ----- 6. BOXPLOTS BY KEEPING PEOPLE WITH PSYCHAITRIC DAIGNOSIS IN ONE GROUP AND WITHOUT PSYCHAITRIC DAIGNOSIS IN ANOTHER-------

  sum.sc.AMT <- cbind(sum.sc.AMT, AMT_grp$Med_daignosis)
  
  PsyD_mean_AMT = with(cbind(sum.sc.AMT, AMT_grp$Med_daignosis), by(age, sex_M1_F0, mean)) 
  
  AMTsum <- cbind(AMTsum, demog_med)
  DRTsum <- cbind(DRTsum, demog_med)
  FRTsum <- cbind(FRTsum, demog_med_day2missing)
  iADLsum <- cbind(iADLsum, demog_med_day2missing)
  CBSFsum <- cbind(CBSFsum, demog_med)
  
  AMTmeans = with(AMTsum, by(AMTsum, Med_daignosis, mean)) 
  DRTmeans = with(DRTsum, by(DRTsum, Med_daignosis, mean))
  FRTmeans = with(FRTsum, by(FRTsum, Med_daignosis, mean))
  iADLmeans = with(FRTsum, by(iADLsum, Med_daignosis, mean))
  CBSFmeans = with(FRTsum, by(CBSFsum, Med_daignosis, mean))
  
  

ggplot(AMTsum$AMTsum, aes(x = AMTsum, y = Med_daignosis, fill = AMTsum))+
  geom_boxplot() +
  labs( x = "Psychaitric daignosis",
        y = "Sum scores", 
        title = "AMT scores for participants with and without Psychaitric daignosis") +
  theme(legend.position = "none", axis.title = element_text(size = 15), 
        axis.text.y = element_text(size = 12), 
        axis.text.x = element_text(size = 12), 
        plot.title = element_text(size = 15))
  

## ----- 7. BOX PLOTS FOR DESCRIPTIVE STATISTICS ------
## 7.1. Age ----







