#####################################################################
############## Two parameteric logistic model #######################
#####################################################################

# 1. Clear work space and set path variables 
rm(list=ls()) # clear environment 
graphics.off() # clear plots 

pathin <- "/Users/japneetbhatia/desktop/thesis/10.CLEAN_DATA" 
refined_data <- "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS" 

## 2. Libraries 
library(readxl) 
library(tidyr) 
library(ltm) # 2pl
library(eRm) # rasch 

# 3. Loaded as data.frame 
setwd(refined_data) 
DRT <- as.data.frame(read_excel("data_with_variance.xlsx", sheet = 5)) 
FRT <- as.data.frame(read_excel("data_with_variance.xlsx", sheet = 6)) 
p_DRT <- DRT[c(3,5,8,10,12,14,16),]
np_DRT <- DRT[c(1,2,4,6,7,9,11,13,15,17,18),]

# 4. 2pl model fit 
fit2pl_DRT <- ltm(DRT ~ z1) # 2pl
DRT_2pl_dfdm <- as.data.frame(coef(fit2pl_DRT) )

DRT_2pl_df <- DRT_2pl_dfdm%>% arrange(-Dffclt) 
DRT_2pl_df[,2] <- rownames(DRT_2pl_df)

DRT_2pl_dm <- DRT_2pl_dfdm%>% arrange(Dscrmn) 
DRT_2pl_dm[,1] <- rownames(DRT_2pl_dm)

item.fit(fit2pl_DRT) 
summary(fit2pl_DRT) 

# 5. plotting ICC 
plot(fit2pl_DRT, type = "ICC") 
DRT <- DRT[,c(-1,-2,-4,-6,-7,-9,-11,-13,-15,-17,-18)] 
DRT <- DRT[,c(3,5,8,10,12,14,16)] 


### ##### #### FRT ##### ##### ####### 

fit2pl_FRT <- ltm(FRT ~ z1) # 2pl
FRT_2pl_dfdm <- as.data.frame(coef(fit2pl_FRT) )

FRT_2pl_df <- FRT_2pl_dfdm%>% arrange(-Dffclt) 
FRT_2pl_df[,2] <- rownames(FRT_2pl_df)

FRT_2pl_dm <- FRT_2pl_dfdm%>% arrange(-Dscrmn) 
FRT_2pl_dm[,1] <- rownames(FRT_2pl_dm)

misfit_FRT<- item.fit(fit2pl_FRT) 
misfit_FRT <- as.data.frame(misfit_FRT$p.values)
misfit_FRT <- misfit_FRT%>% arrange(p.values) 

summary(fit2pl_FRT) 

plot(fit2pl_FRT, type = "ICC", legend = TRUE) 
plot(fit2pl_FRT, type = "ICC") 

FRT <- FRT[,c(-1,-2,-6,-7,-15,-23, -26)]

# WRITING TABLES 

# Remove tables you don't want to write 

# rm(list= c("DRT", "FRT"))

dfs<- ls()[sapply(ls(), function(x) class(get(x))) == 'data.frame']   # extract all the df in R and save as a char vec
dfs <- Filter(function(x) is(x, "data.frame"), mget(ls())) 

library(openxlsx) 
write.xlsx(dfs, file = '2pl.xlsx') 


