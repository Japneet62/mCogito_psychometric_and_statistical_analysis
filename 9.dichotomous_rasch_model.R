#####################################################################
############## DICHOTOMOUS RASCH MODEL ##############################
#####################################################################

## 1. Clear work space and set path variables 
rm(list=ls()) # clear environment 
graphics.off() # clear plots 

pathin <- "/Users/japneetbhatia/desktop/thesis/10.CLEAN_DATA" 
refined_data <- "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS" 

## 2. Libraries 
library(readxl) 
library(tidyr) 
library(eRm)  

# 3. loaded as data.frame
setwd(refined_data) 
DRT <- as.data.frame(read_excel("data_with_variance.xlsx", sheet = 5)) 
FRT <- as.data.frame(read_excel("data_with_variance.xlsx", sheet = 6)) 

  
# 4. convert the data to long format 

#     # give a char to row names for participants = "P"
#     rows<- as.data.frame(rownames(DRT))
#     colnames(rows) <- "rownames"
#     rows$rownames<- paste("P", rows$rownames, sep="_")
#     rownames(DRT) <- rows[,1]
#     
#     rows_frt <- as.data.frame(rownames(FRT))
#     colnames(rows_frt) <- "rownames"
#     rows_frt$rownames<- paste("P", rows_frt$rownames, sep="_")
#     rownames(FRT) <- rows_frt[,1]
#     
# tidyr::gather(
#   data = DRT,
#   key = "DRT",
#   value = "Score",
#   -P
# ) 
#  


# 4. Fit a rasch model on the dichotomous neuropsychological test data  
fitrasch_DRT <- RM(DRT)

# 6. difficulty parameter 
rasch_DRT_dif <- as.data.frame(round(sort(-fitrasch_DRT$betapar), 3))
colnames(rasch_DRT_dif) <- "Difficulty"
rasch_DRT_dif <- rasch_DRT_dif%>% arrange(-Difficulty) 
rasch_DRT_dif[,2] <- rownames(rasch_DRT_dif)
colnames(rasch_DRT_dif) <- c("Difficulty","Items") 

# model fit assessment 
DRT.num <- as.numeric(unlist(DRT))

modelsplit <- factor(DRT <= median(DRT.num), labels = c("one", "two"))

fitLR <- LRtest(fitrasch1, timecat)


###################################################################################
#                           WRITE RESULTS OF THIS SCRIPT                          #
###################################################################################

# Remove tables you don't want to write 

rm(list= c("DRT", "FRT"))

dfs<- ls()[sapply(ls(), function(x) class(get(x))) == 'data.frame']   # extract all the df in R and save as a char vec
dfs <- Filter(function(x) is(x, "data.frame"), mget(ls())) 

library(openxlsx) 
write.xlsx(dfs, file = 'Dichotomous_Rasch_Model_Results_DRT_FRT.xlsx') 


