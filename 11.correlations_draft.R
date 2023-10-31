
################# ----------- CORRELATIONS - draft old attempt  ----------------- ################### 

## 1. Clear work space, set path variables, load libraries ------- 
rm(list=ls()) # clear environment 
graphics.off() # clear plots 
pathin <- "/Users/japneetbhatia/desktop/thesis/13.STATISTICAL_ANALYSIS/scripts" 
setwd(pathin) 
source("4.data_restructing.R")  
mainpath <- "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS" 

# libraries 
library("Hmisc")

## Function to flatten the correlation matrix  ------- 
# ++++++++++++++++++++++++++++
# flattenCorrMatrix - Function to format the correlation matrix in df format 
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

## ------ 1. Item level Correlations with age and gender ------- 
# i. DRT - recognition ------- 
DRT_grp_recognition <- DRT_grp[,c(3, 5, 8, 10, 12, 14, 16, 19)] # data + age 
  DRT_R_corr <- rcorr(as.matrix(DRT_grp_recognition), type = "pearson")   # find correlations 
  DRT_R_corr_f <- flattenCorrMatrix(DRT_R_corr$r, DRT_R_corr$P) # flatten the correlation matrix 
  DRT_R_corr_f %>% arrange(-p) 
  
# ii. DRT - discrimination  ------- 
DRT_grp_discrimination<- DRT_grp[,c(1, 4, 6, 9, 11, 13, 18, 19)] # our main data table 
  DRT_D_corr <- rcorr(as.matrix(DRT_grp_discrimination))    # find correlations 
  DRT_D_corr_f <- flattenCorrMatrix(DRT_D_corr$r, DRT_D_corr$P) # flatten the correlation matrix 

# iii. FRT - fluid intelligence  ------- 
FRT_grp <- FRT_grp[c(-26,-30),c(-32,-33,-34,-35)]
  FRT_corr <- rcorr(as.matrix(FRT_grp))    # find correlations 
  FRT_corr_f <- flattenCorrMatrix(FRT_corr$r, FRT_corr$P) # flatten the correlation matrix 
  FRT_corr_f %>% arrange(-cor) 
  
# iv. SART   ------- 
SART_clean <- SART_grp[c(-7,-9,-19),c(-6, -7, -8, -9)] # remove cmissing values 
  SART_corr <- rcorr(as.matrix(SART_clean))   # find correlations 
  SART_corr_f <- flattenCorrMatrix(SART_corr$r, SART_corr$P) # flatten the correlation matrix 
  SART_corr_f %>% arrange(-cor) 
  
### --- 2. Inter-correlation between all the tasks - DRT_R, DRT_D, FRT, with SART by taking the person.parameter 
  # -------- values for the item level dichotomous response tasks (D,R,FI) 

# # Extract recognition and discrim items   
DRT_recognition <- DRT_grp[,c(3, 5, 8, 10, 12, 14, 16)]
DRT_discrimination<- DRT_grp[,c(1, 4, 6, 9, 11, 13, 18)]

# Load the person parameter values for R, D, FRT
setwd(mainpath)
p.par <- as.data.frame(read_excel("Person_parameter.xlsx", sheet = 1, col_names = TRUE)) 
p.par[] <-as.data.frame(sapply(p.par, as.numeric)) #convert SART values to numeric

p.par<- p.par[,c(-1,-9)] # remove subid 

# calculate correlation by keeping SART as criterion variable 
cor.SART.corr <- cor(p.par$com_err, p.par[,c("Recognition", "Discrimination", 
                                             "FluidIntellegence")], use = "pairwise.complete.obs", 
                                                                    method = "pearson")

cor.SART.sum.err <- cor(p.par$sum_err, p.par[,c("Recognition", "Discrimination", 
                                                "FluidIntellegence")], use = "pairwise.complete.obs", 
                                                                       method = "pearson")

cor.SART.com.err <- cor(p.par$com_err, p.par[,c("Recognition", "Discrimination", 
                                                "FluidIntellegence")], use = "pairwise.complete.obs", 
                                                                       method = "pearson")

cor.SART.om.err <- cor(p.par$om_err, p.par[,c("Recognition", "Discrimination", 
                                              "FluidIntellegence")], use = "pairwise.complete.obs", 
                                                                     method = "pearson")

## ------ 2. Plot correlations ----------------------- 
  
# install.packages("psych") 
library(psych) 

# create a scatterplot for this first 
library("PerformanceAnalytics")

# library("corrplot")
# corrplot(cor.SART.corr, method = "num") #  correlogram 

# -- Final plot needed for correlation --- # 
chart.Correlation(as.matrix(p.par), histogram=TRUE, pch=19) 

##-- Another attempt at correlations to plot with chart.correlation function with SART as criterion var 

data1 <- data.frame(cbind(SART = p.par$corr, Recognition = p.par$Recognition, Discrimination=p.par$Discrimination, FluidIntellegence=p.par$FluidIntellegence))
data2 <- data.frame(cbind(SART = p.par$sum_err, Recognition = p.par$Recognition, Discrimination=p.par$Discrimination, FluidIntellegence=p.par$FluidIntellegence))
data3 <- data.frame(cbind(p.par$com_err, p.par$Recognition, p.par$Discrimination, p.par$FluidIntellegence))
data4 <- data.frame(cbind(p.par$om_err, p.par$Recognition, p.par$Discrimination, p.par$FluidIntellegence))

corr_matrix <- cor(data4, method = "pearson", use = "pairwise.complete.obs")
chart.Correlation(data1, histogram=TRUE, pch=19, method = "pearson") # correlation plot 

plot(SART$corr) 

## ANOTHER ATTEMPT 
library("ggplot2")                     
library("GGally")

ggpairs(
  data4, 
  title = "Correlation matrix for SART, Recognition, Discrimination and Fluid Intellegence") 

ggpairs(data2) 

### -- CORRELATION BETWEEN GENDER / AGE WITH  ITEM LEVEL DATA-- # 

# DRT - R 
library(corrplot)
DRT.R.cor.sex <- cor(DRT_recognition, DRT_grp$sex_M1_F0)
corrplot(DRT.R.cor.sex) 

DRT.R.cor.age <- cor(DRT_recognition, DRT_grp$age)
corrplot(DRT.R.cor.age) 
  
# DRT - D
DRT.D.cor.sex <- cor(DRT_discrimination, DRT_grp$sex_M1_F0)
corrplot(DRT.D.cor.sex) 

DRT.D.cor.age <- cor(DRT_discrimination, DRT_grp$age)
corrplot(DRT.D.cor.age) 


  







 











################# ----------- CORRELATIONS - draft 2 ----------------- ################### 
## 1. Clear work space, set path variables, load libraries ------- 
rm(list=ls()) # clear environment 
graphics.off() # clear plots 
pathin <- "/Users/japneetbhatia/desktop/thesis/13.STATISTICAL_ANALYSIS/scripts" 
setwd(pathin) 
source("4.data_restructing.R")  
mainpath <- "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS" 

# Libraries 
library("Hmisc")
library("GGally")

## 2. Function to flatten the correlation matrix  ------- 
# ++++++++++++++++++++++++++++
# flattenCorrMatrix - Function to format the correlation matrix in df format 
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}


## 3. Load person parameter values for all the tasks  -----
setwd(mainpath)
p.par <- as.data.frame(read_excel("Person_parameter.xlsx", sheet = 1, col_names = TRUE)) 
p.par[] <-as.data.frame(sapply(p.par, as.numeric)) #convert SART values to numeric
p.par<- p.par[,c(-1,-9)] # remove subid added twice to the df 


## 4. Correlate education years with task performance -----

ggpairs(as.data.frame(cbind(EduYears = edu$EduYears, 
                            SART_om_error = SART_subid_NA$om_err, 
                            SART_com_error = SART_subid_NA$com_err,  
                            p.par_Recognition = p.par$Recognition, 
                            p.par_Discrimination = p.par$Discrimination, 
                            p.par_FluidIntellegence = p.par$FluidIntellegence)), 
        title = "Correlation matrix for Education, Recognition, Discrimination, Fluid Intellegence, SART")

# Divide education into 2 factors - less than 14 and more than 14 and then plot correlations 
edu$EduYears <- cut(edu$EduYears, breaks = c(0, 14, 18),
                    labels = c("Factor 1", "Factor 2"))

ggpairs(as.data.frame(cbind(EduYears = edu$EduYears, 
                            SART_om_error = SART_subid_NA$om_err, 
                            SART_com_error = SART_subid_NA$com_err,  
                            p.par_Recognition = p.par$Recognition, 
                            p.par_Discrimination = p.par$Discrimination, 
                            p.par_FluidIntellegence = p.par$FluidIntellegence)), 
        aes(color = edu$EduYears), 
        title = "Correlation matrix for Education, Recognition, Discrimination, Fluid Intellegence, SART")


## 5. Correlate SART with other tasks  -----

# SART - omission & commission error
ggpairs(as.data.frame(cbind(SART_om_error = SART_subid_NA$om_err, 
                            SART_com_error = SART_subid_NA$com_err,  
                            p.par_Recognition = p.par$Recognition, 
                            p.par_Discrimination = p.par$Discrimination, 
                            p.par_FluidIntellegence = p.par$FluidIntellegence)), 
        title = "SART (omission error, comission error), Recognition, Discrimination, FluidIntellegence")

## 6. Correlate tasks with psychological diagnosis ------ 
demog_med$Med_daignosis <- factor(demog_med$Med_daignosis, 
                                  levels = c("0", "1"), 
                                  labels = c("No psychological daignosis", 
                                             "Psychological disorder daignosed"))

demog_med$Med_daignosis <- as.factor(demog_med$Med_daignosis) 

ggpairs(as.data.frame(cbind(Psych_Daignosis = demog_med$Med_daignosis,
                            SART_om_error = SART_subid_NA$om_err, 
                            SART_com_error = SART_subid_NA$com_err, 
                            p.par_Recognition = p.par$Recognition, 
                            p.par_Discrimination = p.par$Discrimination, 
                            p.par_FluidIntellegence = p.par$FluidIntellegence)),
        aes(color = demog_med$Med_daignosis), 
        title = "Correlation plot for SART (com_err), om error, Recognition, Discrimination, Fluid 
Intellegence grouped by Psychological daignosis, ")



## 7. Correlate everything together in 1 table ---- 
ggpairs(as.data.frame(cbind(SART_om_error = SART_subid_NA$om_err, 
                            SART_com_error = SART_subid_NA$com_err,  
                            p.par_Recognition = p.par$Recognition, 
                            p.par_Discrimination = p.par$Discrimination, 
                            p.par_FluidIntellegence = p.par$FluidIntellegence, 
                            Psych_Daignosis = demog_med$Med_daignosis,
                            EduYears = edu$EduYears)), 
        title = "SART (omission error, comission error), Recognition, Discrimination, FluidIntellegence")

## 9. correlate FEDL - mood and all tasks ----

for (i in 15:31)
{ 
  # plot 
  p <- ggpairs(as.data.frame(cbind(i = CBSF[,i],
                                   SART_om_error = SART_subid_NA$om_err, 
                                   SART_com_error = SART_subid_NA$com_err,  
                                   p.par_Recognition = p.par$Recognition, 
                                   p.par_Discrimination = p.par$Discrimination, 
                                   p.par_FluidIntellegence = p.par$FluidIntellegence)))
  
  # print plot
  print(p)
}




## 10.1. Find correlation matrix for dicrimination p.par and FEDL - non motor scores   ----
# > PEARSONS CORRELATION    ----
datan <- as.data.frame(cbind(p.par_Discrimination = p.par$Discrimination, 
                             CBSF[,c(15:31)]))

rcorr(as.matrix(datan), type = "pearson")

# > POINT BISERAL CORRELATION USED - categorical and continous var     ----

# Assumptions 
# 1. normality of cont var needed 
library("ggpubr")
ggdensity(p.par$Discrimination, 
          main = "Density plot of Discrimination",
          xlab = "Discrimination p.par") 

# 2. The regression Y on X is linear

# 3. The mean value of Y in the minor or smaller category as specified by X lies on 
#    the regression lines.

# calc pb cor 

cor.test()






























