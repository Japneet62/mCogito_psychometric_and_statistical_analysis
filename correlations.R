################# ----------- CORRELATIONS ----------------- ################### 
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



























