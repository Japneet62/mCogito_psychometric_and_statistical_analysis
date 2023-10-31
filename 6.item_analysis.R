#########################################################################
############## Summary scores for test data  ############################
#########################################################################

## 1. Clear work space and set path variables ----
rm(list=ls()) # clear environment 
graphics.off() # clear plots 
pathin <- "/Users/japneetbhatia/desktop/thesis/13.STATISTICAL_ANALYSIS/scripts" 

setwd(pathin) 
source("4.data_restructing.R") 

#### -------  SUM FOR EACH SCORE TYPE FOR ALL PARTICIPANTS  ---------- ##### 
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


## 2. floor and ceiling effects in the data calculations ----- ##  ----

library(DACF) 

AMT_sub <- as.double(unlist(AMT_var))
AMT.f = induce.cfe(.2, 0, AMT_sub)
rec.mean.var(AMT_sub) 

DRT_sub <- as.double(unlist(DRT)) 
DRT.f=induce.cfe(.2,0, DRT_sub) 
rec.mean.var(DRT_sub) 

FRT_sub <- as.double(unlist(FRT)) 
FRT.f=induce.cfe(.2,0, FRT_sub) 
rec.mean.var(FRT_sub) 

CBSF_sub <- as.double(unlist(CBSF)) 
FRT.f=induce.cfe(.2,0, CBSF_sub) 
rec.mean.var(CBSF_sub) 

iADL_sub <- as.double(unlist(iADL)) 
FRT.f=induce.cfe(.2,0, iADL_sub) 
rec.mean.var(iADL_sub) 



## 3. Compare mean scores for all the tasks comparing participants education levels



##------- Writing output tables ------------ ###  ----
# remove unnecessary output variables 
rm(list=c("AMT_subid", "DRT_subid", "CBSF_subid", "iADL_subid")) 


## Write output tables 
  # 1. data_with_variance 
  # 2. summary_scores

  setwd(path_results)
  dfs<- ls()[sapply(ls(), function(x) class(get(x))) == 'data.frame']   # extract all the df in R and save as a char vec
  dfs <- Filter(function(x) is(x, "data.frame"), mget(ls())) # covert it to data frame
  
  data_with_variance <- dfs[grepl("var", names(dfs))] # extract the data showing some variance 
  sum.sc <- dfs[grepl("sum.sc", names(dfs))]

write.xlsx(data_with_variance, file = 'data_with_variance.xlsx') 
write.xlsx(sum.sc, file = 'summary_scores.xlsx') 
 