##### Plotting summary scores for the tests and questionnaires ###### 

rm(list=ls()) # clear environment 
graphics.off() # clear plots 
pathin <- "/Users/japneetbhatia/desktop/thesis/13.STATISTICAL_ANALYSIS/scripts" 

setwd(pathin) 
source("6.item_analysis.R") 

# DRT 
  score_DRT <- rowSums(DRT) # find total scores in dichotomous data 
  tab <- describe(score_DRT)[, c("n", "min", "max", "mean", "median", "sd", "skew", "kurtosis")]
  tab$kurtosis <- tab$kurtosis + 3
  tab 
  
  # histogram
  ggplot(DRT, aes(score_DRT)) +
    geom_histogram(binwidth = 1, col = "black") +
    xlab("Total score") + 
    ylab("Number of respondents") +
    theme_app() 
  
  # colors by cut-score
  cut <- median(score_DRT) # cut-score
  color <- c(rep("red", cut - min(score_DRT)), "gray", rep("blue", max(score_DRT) - cut))
  df <- data.frame(score_DRT)
  
  # histogram
  ggplot(df, aes(score_DRT)) +
    geom_histogram(binwidth = 1, fill = color, col = "black") +
    xlab("Total score") +
    ylab("Number of respondents") +
    theme_app()

# AMT 
  score_AMT <- rowSums(AMT_var) # find total scores in dichotomous data 
  tab <- describe(score_AMT)[, c("n", "min", "max", "mean", "median", "sd", "skew", "kurtosis")]
  tab$kurtosis <- tab$kurtosis + 3
  tab 
  
  # histogram
  ggplot(AMT_var, aes(score_AMT)) +
    geom_histogram(binwidth = 1, col = "black") +
    xlab("Total score") + 
    ylab("Number of respondents") +
    theme_app() 
  
  # colors by cut-score
  cut <- median(score_AMT) # cut-score
  color <- c(rep("red", cut - min(score_AMT)), "gray", rep("blue", max(score_AMT) - cut))
  df <- data.frame(score_AMT)
  
  # histogram
  ggplot(df, aes(score_AMT)) +
    geom_histogram(binwidth = 1, fill = color, col = "black") +
    xlab("Total score") +
    ylab("Number of respondents") +
    theme_app()
  
# FRT 
  score_FRT <- rowSums(FRT_var) # find total scores in dichotomous data 
  tab <- describe(score_FRT)[, c("n", "min", "max", "mean", "median", "sd", "skew", "kurtosis")]
  tab$kurtosis <- tab$kurtosis + 3
  tab 
  
  # histogram
  ggplot(FRT_var, aes(score_FRT)) +
    geom_histogram(binwidth = 1, col = "black") +
    xlab("Total score") + 
    ylab("Number of respondents") +
    theme_app() 
  
  # colors by cut-score
  cut <- median(score_FRT) # cut-score
  color <- c(rep("red", cut - min(score_FRT)), "gray", rep("blue", max(score_FRT) - cut))
  df <- data.frame(score_FRT)
  
  # histogram
  ggplot(df, aes(score_FRT)) +
    geom_histogram(binwidth = 1, fill = color, col = "black") + 
    xlab("Total score") +
    ylab("Number of respondents") +
    theme_app()
  

  

