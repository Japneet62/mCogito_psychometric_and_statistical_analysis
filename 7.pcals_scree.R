#########################################################################
############## Princals and scree plots   ###############################
#########################################################################

## 1. Clear work space and set path variables ----
rm(list=ls()) # clear environment 
graphics.off() # clear plots 
mainpath <- "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS" 
pathin <- "/Users/japneetbhatia/desktop/thesis/13.STATISTICAL_ANALYSIS/scripts" 

setwd(pathin) 
source("4.data_restructing.R") 

## libraries 
library(tidyverse) 
library(readxl)
library(dplyr) 
library(writexl)  
library(Gifi) 

# extract data needed ----
DRT_p <- DRT_var[,c(3, 5, 8, 10, 12, 14, 16)]
DRT_np <- DRT_var[,c(1, 2, 4, 6, 7, 9, 11, 13, 15, 17, 18)]
# DRT_var <- DRT_var[,c(-15,-17)]

FRT_var <- FRT_var[,c(-7,-24,-28)]

# start saving all the plots created in the script below in a pdf  ----
pdf(file = "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS/plots/pcals_scree.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8) # The height of the plot in inches

## Princals - principal component analysis - to understand the latent dimensionality  ----
## structure of the data 

  par(mar = c(1, 1, 1, 1)) # to avoid the error of figure margins too big 

  # 1. AMT  ----
  pcals_AMT <- Gifi::princals(AMT_var) 
  plot(pcals_AMT, main = "Princals AMT")  
  
  # 2. DRT  ----
  pcals_DRT <- Gifi::princals(DRT_var) 
  cols <- c("green", "blue")
  plot(pcals_DRT, main = "Princals DORT")  
   
  # 3. FRT  ----
  pcals_FRT <- Gifi::princals(FRT_var)
  plot(pcals_FRT, main = "Princals FRT")
  
  # 4. CBSF ----
  pcals_CBSF <- Gifi::princals(CBSF_var)
  plot(pcals_CBSF, main = "Princals CBSF") 
  
  # 5. iADL  ----
  pcals_iADL <- Gifi::princals(iADL_var)
  plot(pcals_iADL, main = "Princals iADL")
  
## Plot individual scores along with the princals to see the distribution of the scores  ----

  # 1. AMT  ----
  plot(pcals_AMT, 
       plot.type = "biplot", 
       col.scores = "black", 
       col.loadings = "red", 
       col.lines = "blue") 
  
  # 2. DRT  ----
  plot(pcals_DRT, 
       plot.type = "biplot", 
       col.scores = "black", 
       col.loadings = "red", 
       col.lines = "blue") 
   
  # 3. FRT  ----
  plot(pcals_FRT, 
       plot.type = "biplot", 
       col.scores = "black", 
       col.loadings = "red", 
       col.lines = "blue") 
  
  # 4. CBSF  ----
  plot(pcals_CBSF, 
       plot.type = "biplot", 
       col.scores = "black", 
       col.loadings = "red", 
       col.lines = "blue") 
  
  # 5. iADL  ----
  plot(pcals_iADL, 
       plot.type = "biplot", 
       col.scores = "black", 
       col.loadings = "red", 
       col.lines = "blue") 
  
  
## SCREE PLOTS - latent dimensionality structure  ----
  
  # 1. AMT  ----
  AMT_screeplot <- plot(pcals_AMT, "screeplot", main = "Scree Plot: AMT" )
   
  # 2. DRT  ----
  DRT_screeplot <- plot(pcals_DRT, "screeplot", main = "Scree Plot: DRT" )
  
  # 3. FRT ----
  FRT_screeplot <- plot(pcals_FRT, "screeplot", main = "Scree Plot: FRT" )
   
  # 4. CBSF  ----
  CBSF_screeplot <- plot(pcals_CBSF, "screeplot", main = "Scree Plot: CBSF" )
   
  # 5. iADL  ----
  iADL_screeplot <- plot(pcals_iADL, "screeplot", main = "Scree Plot: iADL" )
  
# ---- 
dev.off()


  

  
  
 