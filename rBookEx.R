
rm(list=ls()) # clear environment 
graphics.off() # clear plots 

library("mirt") # multidimensional IRT models 
library("MPsychoR") 

data("zareki") # dataset 
zarsub <- zareki[, grep("subtr", colnames(zareki))]

# -- STEP 1: Dimensionality assessment -- # 

# Attempt 1. Princals - 2 dimensional - no 

library("Gifi") 

prinzar <- princals(zarsub)
plot(prinzar, main = "zareki loadings") 

# check if there is any voilations of dimensionality, by looking into the plot 
# and searching for a var that is not in a similar direction as the rest of the vars 
graphics.off() # clear plots 

# Attempt 2. Exploratory factor analysis? no 

library(psych)
library(readxl)
library(REdaS) 

attach(zareki)
bart_spher(zareki) 

# Attempt 3. IFA 

  # fit data with 1 factor model, then two factor model
  # then check with LR test which fit is better 

# - LR test suggests if 2D fit is superior or not to 1D fit. 

fitifa1 <- mirt(zarsub, 1, verbose = FALSE)
fitifa2 <- mirt(zarsub, 2, verbose = FALSE, TOL = 0.001)
anova(fitifa1, fitifa2, verbose = FALSE)

# p = 0.132  - this means 2D fit is not better than 1D fit. 

