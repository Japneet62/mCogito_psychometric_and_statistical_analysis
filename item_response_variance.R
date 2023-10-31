#########################################################################
############## Item response variance ###################################
#########################################################################

## 1. Clear work space and set path variables 
rm(list=ls()) # clear environment 
graphics.off() # clear plots 
pathin <- "/Users/japneetbhatia/desktop/thesis/13.STATISTICAL_ANALYSIS/scripts" 

setwd(pathin) 
source("6.item_analysis.R")  

library(reshape2)
## Convert the data into long format 
AMT_grp_long <- reshape(AMT_grp, 
                        direction = "long", 
                         varying = list(responses = 2:19, age = 20, gender = 21), 
                          v.names = c("responses", "age", "gender")
)


data_long <- gather(AMT_grp, AMT_1r, measurement, control:AMT2r, factor_key=TRUE)

AMT_grp_long <- AMT_grp %>%
  pivot_longer(cols = starts_with("AMT"), names_to = "gender", values_to = "value") 
