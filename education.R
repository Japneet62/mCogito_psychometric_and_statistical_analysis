### Education data ###### 

## 1. Clear work space and set path variables 
rm(list=ls()) # clear environment 
graphics.off() # clear plots 

## 2. path vars for scripts preprocessing and data analysis 
path_clean_data <- "/Users/japneetbhatia/desktop/thesis/10.CLEAN_DATA" 
setwd(path_clean_data) 

demog_data <- as.data.frame(read_excel("clean_data.xlsx", sheet = 3)) # dichotomous = 0, 1 = incorrect, correct 

edu <- demog_data %>% group_by(Edu) %>% dplyr::summarize(count=n()) # all types of categories

## Group the education data together to find how many people belong to each group -----
realschule <- edu[edu$Edu %like% "Real", ] 
realschule[4,] <- edu[edu$Edu %like% "Mittlere", ]
berufschule <- edu[edu$Edu %like% "Beruf", ]
fachschule <- edu[edu$Edu %like% "Fachhoch", ]
fachschule[5,] <- edu[edu$Edu %like% "Fachschulabsch", ]        # Extract matching rows with Fachschule
hauptschule <- edu[edu$Edu %like% "Haupt", ]        # Extract matching rows with haputschu;e 
gymnasium <- edu[edu$Edu %like% "Abitur",]
uni  <- edu[edu$Edu %like% c("Uni"), ]        # Extract matching rows with uni 
bachelors <- edu[edu$Edu %like% "Bachelor", ]        # Extract matching rows with bachelors 
bachelors <- rbind(uni,bachelors)
master <- edu[edu$Edu %like% "Master", ]        # Extract matching rows with Realschule 
phd <- edu[edu$Edu %like% "Promotion", ]        # Extract matching rows with Realschule 
ausbildung <- edu[edu$Edu %like% "Ausbildung", ] 
diploma <- edu[edu$Edu %like% "Diplom", ] 
diploma[4,] <- edu[edu$Edu %like% "DIP", ] 
Staatsexamen <- edu[edu$Edu %like% "Staatsexamen", ] 

## SUM ## -----
realschule_sum <- sum(realschule$count)
berufschule_sum <- sum(berufschule$count)
fachschule_sum <- sum(fachschule$count)
hauptschule_sum <- sum(hauptschule$count)
gymnasium_sum <- sum(gymnasium$count)
bachelors_sum <- sum(bachelors$count)
masters_sum <- sum(master$count)
PhD_sum <- sum(phd$count)
ausbildung_sum <- sum(ausbildung$count)
diploma_sum <- sum(diploma$count)
Staatsexamen_sum <- sum(Staatsexamen$count)

edu_summary <- as.data.frame(rbind(hauptschule_sum, realschule_sum, berufschule_sum, fachschule_sum,
                                   gymnasium_sum, bachelors_sum, ausbildung_sum, diploma_sum, 
                                   masters_sum, PhD_sum, Staatsexamen_sum)) 

## Change the education data -----
demog_data$Edu <- gsub("Mittlere Reife", "Realschule", demog_data$Edu) # Realschule
demog_data$Edu <- gsub("Realschulabschluss", "Realschule", demog_data$Edu)
demog_data$Edu <- gsub("Realschule und Facharbeiter", "Realschule", demog_data$Edu)
demog_data$Edu <- gsub("Abgeschlossene Berufsausbildung", "Berufsausbildung", demog_data$Edu) # Berufsausbildung
demog_data$Edu <- gsub("Fachhochschulabschluss", "Fachhochschule", demog_data$Edu) # Fachhochschule
demog_data$Edu <- gsub("Fachhochschuldiplom", "Fachhochschule", demog_data$Edu)
demog_data$Edu <- gsub("Fachhochschulreife", "Fachhochschule", demog_data$Edu)
demog_data$Edu <- gsub("Fachschulabschluss", "Fachhochschule", demog_data$Edu)
demog_data$Edu <- gsub("Bachelor degree", "Bachelor", demog_data$Edu) # Bachelor
demog_data$Edu <- gsub("Bachelor Uni", "Bachelor", demog_data$Edu)
demog_data$Edu <- gsub("Uni", "Bachelor", demog_data$Edu)
demog_data$Edu <- gsub("Uni Staatsexamen + Diplom + Promotion", "Bachelor", demog_data$Edu) 
demog_data$Edu <- gsub("Diplom", "Diploma", demog_data$Edu) # diploma 
demog_data$Edu <- gsub("Diplom FH", "Diploma", demog_data$Edu) 
demog_data$Edu <- gsub("Diploma FH", "Diploma", demog_data$Edu) 
demog_data$Edu <- gsub("Diplomaa", "Diploma", demog_data$Edu) 
demog_data$Edu <- gsub("Diplomaa FH", "Diploma", demog_data$Edu) 
demog_data$Edu <- gsub("DIPLOM", "Diploma", demog_data$Edu) 
demog_data$Edu <- gsub("DIPLOM", "Diploma", demog_data$Edu) 
demog_data$Edu <- gsub("Bachelor Staatsexamen + Diploma + Promotion", "Promotion", demog_data$Edu) 

edu2 <- demog_data %>% group_by(Edu) %>% dplyr::summarize(count=n()) 



### Missed out edu data  
