## 1. Clear work space and set path variables ----
rm(list=ls()) # clear environment 
graphics.off() # clear plots 

## 2. path vars for scripts preprocessing and data analysis -----
preprocessing_scripts <- "/Users/japneetbhatia/desktop/thesis/9.PREPROCESSING/scripts"
analysis_scripts <- "/Users/japneetbhatia/desktop/thesis/13.STATISTICAL_ANALYSIS/scripts" 

## 4. ANALYSIS FOR THESIS ------ 
setwd(analysis_scripts) 

source("4.data_restructing.R") # path vars, libraries, extract data, change column headings, dummy code data, 
                                # combine data with age, gender, professional & medical status
                               # convert data to long format, remove data with variance, missing values 
source("5.descriptive_stats.R") # summarize demo, med, psych diagnosis, profession, 
                                 # correct edu data and plotting 
source("6.item_analysis.R") # summarize item responses, plot item responses?, floor & ceiling effects 
source("FCeffects_barplots.R") # floor and ceiling effects and histograms for the same 
source("7.pcals_scree.R") # princals, scree plots - whole data 
source("8.1.FA_AMT.R") # dimensionality assessment with mirt, LRtest, M2 statistic, 
  # item misfit, Factor extraction with oblique rotations, Intercept-slope parameterization, 
  # item plots, coefficients 
source("8.2.FA_DRT.R") 
source("8.3.FA_FRT.R") 
source("8.5.FA_CBSF.R") # 4 factors extracted 
source("9.1.1.rasch_model_DRT_hits.R") # fit rasch model, easiness/dofficulty p, divide the data into factors. 
  # LRtest, item misfit graphs, wald test, item specific local dependence, global dependence, ICCs, Person parameters. 
source("9.1.2.rasch_model_DRT_falsepositives.R") 
source("9.2.rasch_model_FRT_fluid_intellegence.R") 
source("10.SART.R") 
source("12.correlations_main.R")  


########  next analysis steps ######################## 
# 1. create histograms for floor and ceiling effects in the data - 6. item analysis
# 2. analyse the person parameters for all the 3 latent vars - recog, discrim, log reasoning 
# 3. find correlation between SART (criterion variable), recognition, discrimination and logical reasoning 
# 4. create scatterplots to denote the 
# 5. create a table which shows the correlation values of the 4 tasks. 

###########  Not needed anymore ######################## 
# source("8.4.FA_iADL.R") # not needed anymore 

# source("9.3.partial_credit_model_iADL.R") # fit partial credit model, person parameter, 
#   # chi sq - model fit, LRtest, wald test, remove misfitting items, refit the model, thresholds, 
#   # ICCs, person item map  
# source("9.4.1.partial_credit_model_CBSF_motor.R")  
# source("9.4.2.partial_credit_model_CBSF_non_motor.R") 

#-----# 3. DO NOT RUN, if there's no change in rawdata structure, headings or new data #----####
setwd(preprocessing_scripts)
source("2.CleanRawDataForOneParticipant.R") # 1. clean the data for one participant 
source("3.CleanRawDataForMulParticipants.R") # get the data from all participants in one data file 







