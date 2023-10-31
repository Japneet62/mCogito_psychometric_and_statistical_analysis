#########################################################################
############## Summary statistics - Demographics and medical data  ######
#########################################################################
                                                           --- # 

  



## ---- 5. WRITE TABLE FOR SUMMARY STATS  -------- 
  
  # Remove tables you don't want to write 
  rm(list= c("AMT", "demog_med", "psych_dgn_partic", "medical", 
             "FRT", "iADL", "DRT", "CBSF", "AMT_subid", 
             "CBSF_subid", "FRT_subid", "SART_subid", "SART"))
  
  # create a list of dfs in the env 
  dfs<- ls()[sapply(ls(), function(x) class(get(x))) == 'data.frame']   # extract all the df in R and save as a char vec
  dfs <- Filter(function(x) is(x, "data.frame"), mget(ls())) # covert it to data frame
  
  # write summary statistics file 
  setwd(path_results) # set path where you want to save results
  write.xlsx(dfs, file = 'demog_med_summary.xlsx') 

## ---- 6. Descriptive statistics  -------- 
  
  ### DAY 1  -------- 
  # --- Plot gender against age groups 
  
  # extract sum of males and females who did only day 1
  gender_d1 <- demog_data_day2missing %>%
    group_by(sex_M1_F0) %>%
    summarize(count = n()) %>%
    mutate(gender = ifelse(sex_M1_F0 == 1, "male", "female"))
  
  # make variables into factors 
  demog_data_day2missing$sex_M1_F0 <-factor(demog_data_day2missing$sex_M1_F0,
                                     levels=c(1,0),
                                     labels=c("Male", "Female"))
  
  with(demog_data_day2missing, summary(sex_M1_F0)) # total males and females 
  with(demog_data_day2missing, summary(age)) 
  
  GenderMeans = with(demog_data_day2missing, by(age, sex_M1_F0, mean)) 
  
  # plot gender against age 
  GenderPlot1 = ggplot(demog_data_day2missing, aes(x = sex_M1_F0, y = age, fill = sex_M1_F0)) + 
    geom_boxplot() +
    labs( x = "Sex (m = 28, f = 30)",
          y = "Age (mean = 60.76)", 
          title = "Age/sex distribution for particiapants - Day 1") + 
    theme(legend.position = "none", axis.title = element_text(size = 15), 
                                    axis.text.y = element_text(size = 12), 
                                    axis.text.x = element_text(size = 12), 
                                    plot.title = element_text(size = 15))
             
  GenderPlot1 
  
  
  
  ### DAY 1 + DAY 2 -------- 
  demog_data$sex_M1_F0 <-factor(demog_data$sex_M1_F0,
                                            levels=c(1,0),
                                            labels=c("Male", "Female"))
  
  with(demog_data, summary(sex_M1_F0)) # total males and females 
  with(demog_data, summary(age)) 
  
  GenderMeans = with(demog_data, by(age, sex_M1_F0, mean)) 
  
  
  GenderPlot2 = ggplot(demog_data, aes(x = sex_M1_F0, y = age, fill = sex_M1_F0)) + 
    geom_boxplot() +
    labs( x = "Sex (m = 31, f = 31)",
          y = "Age (mean = 60.76)", 
          title = "Age/sex distribution for particiapants - Day 1 and 2") + 
    theme(legend.position = "none", axis.title = element_text(size = 15), 
          axis.text.y = element_text(size = 12), 
          axis.text.x = element_text(size = 12), 
          plot.title = element_text(size = 15))
  
  GenderPlot2
  
  
  
  demog_data$profession <-factor(demog_data$profession,
                                levels=c("a", "b","c", "d", "e", "f", "g", "h"),
                                labels=c("Full-time employee", "Part-time employee", "Buisness owner", 
                                         "Self-employed/Freelancer", "Retired", "Housewife / 
                                         househusband", "Unemployed", "Independent income")) 
  

