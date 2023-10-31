################# ----------- CORRELATIONS FINAL ----------------- ################### 

## 1. Clear work space, set path variables, load libraries -------
rm(list=ls()) # clear environment 
graphics.off() # clear plots 
pathin <- "/Users/japneetbhatia/desktop/thesis/13.STATISTICAL_ANALYSIS/scripts/" 
setwd(pathin) 
source("4.data_restructing.R")  
mainpath <- "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS" 

# Libraries 
library("Hmisc")
library("GGally")
library("ggcorrplot")
library("ggplot2") 

## 2. Load person parameter values for all the tasks  -----
setwd(mainpath)
p.par <- as.data.frame(read_excel("Person_parameter.xlsx", sheet = 1, col_names = TRUE)) 
p.par[] <-as.data.frame(sapply(p.par, as.numeric)) #convert SART values to numeric
p.par<- p.par[,c(-1,-9)] # remove subid added twice to the df 

## 3. Reorder correlation  matrix ----
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}


  
## 4. Redo headings 
colnames(CBSF) <- c("Spk", "SD", "Eat", "CS", "Drs", "Hyg", "HT", "Hob", "TB", 
                    "GIOB", "WaBa", "SpoM", "UriC", "Vis", "UndRead", "TC", "Rem", 
                    "MF", "PCMT", "AOOO", "DTR", "DFP", "Mood", "FAP", "FM", "CAO", 
                    "Ag", "Sleep", "SADD", "FPMF", "FYWA")

colnames(iADL_subid_NA) <- c("Sub_ID", "Tele", "Trvl", "Shop", "FoodP", "HsK", "Med", "Fin")

## - SAVE PLOTS IN A PDF  ----
# pdf(file = "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS/plots/correlations_demog_tasks_ques.pdf",   # The directory you want to save the file in
#     width = 13, # The width of the plot in inches
#     height = 13) # The height of the plot in inches 
 
## -------------- DEMOG - NEUROPSYCH TASKS CORR -----------  
## 1. Correlation plot of age with neuropsychological tasks ----
ggpairs(as.data.frame(cbind(Age = demog_data$age, 
                            SART_om_error = SART_subid_NA$om_err, 
                            SART_com_error = SART_subid_NA$com_err,  
                            p.par_Recognition = p.par$Recognition, 
                            p.par_Discrimination = p.par$Discrimination, 
                            p.par_FluidIntellegence = p.par$FluidIntellegence)), 
        title = "Correlation matrix of age and neuropsychological tasks")


## 2. Correlation plot of gender with neuropsychological tasks  ----

ggpairs(as.data.frame(cbind(gender = demog_data$sex_M1_F0, 
                            SART_om_error = SART_subid_NA$om_err, 
                            SART_com_error = SART_subid_NA$com_err,  
                            p.par_Recognition = p.par$Recognition, 
                            p.par_Discrimination = p.par$Discrimination, 
                            p.par_FluidIntellegence = p.par$FluidIntellegence)), 
        title = "Correlation matrix of gender and neuropsychological tasks")



## 3. Correlate plot of education with neuropsychological tasks -----

ggpairs(as.data.frame(cbind(EduYears = edu$EduYears, 
                            SART_om_error = SART_subid_NA$om_err, 
                            SART_com_error = SART_subid_NA$com_err,  
                            p.par_Recognition = p.par$Recognition, 
                            p.par_Discrimination = p.par$Discrimination, 
                            p.par_FluidIntellegence = p.par$FluidIntellegence)), 
        title = "Correlation matrix for education and neuropsychological tasks")
 
## 4. Correlate tasks of psychological diagnosis with neuropsychological tasks  ------ 

ggpairs(as.data.frame(cbind(Psych_Daignosis = demog_med$Med_daignosis,
                            SART_om_error = SART_subid_NA$om_err, 
                            SART_com_error = SART_subid_NA$com_err, 
                            p.par_Recognition = p.par$Recognition, 
                            p.par_Discrimination = p.par$Discrimination, 
                            p.par_FluidIntellegence = p.par$FluidIntellegence)),
        title = "Correlation plot depicting relationship between psychological illness and neuropsychological tasks")

## 5. Correlate plot of profession with neuropsychological tasks ---- 
ggpairs(as.data.frame(cbind(Profession = demog_data$profession,
                            SART_om_error = SART_subid_NA$om_err, 
                            SART_com_error = SART_subid_NA$com_err, 
                            p.par_Recognition = p.par$Recognition, 
                            p.par_Discrimination = p.par$Discrimination, 
                            p.par_FluidIntellegence = p.par$FluidIntellegence)),
        title = "Correlation matrix for psychological illness and neuropsychological tasks")


## -------------- DEMOG - fADL - NON-MOTOR ITEMS -----------  
## 1. Correlation plot of age with FADL ----
ggpairs(as.data.frame(cbind(Age = demog_data$age, 
                            CBSF[,c(15:31)])), 
        title = "Correlation matrix of age and FADL questionnaire - Non-motor items")
 
## 2. Correlation plot of gender with FADL ----
ggpairs(as.data.frame(cbind(gender = demog_data$sex_M1_F0, 
                            CBSF[,c(15:31)])), 
        title = "Correlation matrix of age and FADL questionnaire - Non-motor items")

## 3. Correlation plot of education with FADL ----
ggpairs(as.data.frame(cbind(Education = edu$EduYears, 
                            CBSF[,c(15:31)])), 
        title = "Correlation matrix of education and FADL questionnaire - Non-motor items")

## 4. Correlation plot of education with FADL ----
ggpairs(as.data.frame(cbind(PsychDaignosis = demog_med$Med_daignosis, 
                            CBSF[,c(15:31)])), 
        title = "Correlation matrix of psychological illness and FADL questionnaire - Non-motor items")
 
## 5. Correlate plot of profession with neuropsychological tasks ---- 
ggpairs(as.data.frame(cbind(Profession = demog_data$profession,
                            CBSF[,c(15:31)])),
        title = "Correlation matrix for psychological illness and neuropsychological tasks")

## -------------- TASKS -> FADL - NON-MOTOR ITEMS -----------  
## 1. correlate FADL non motor scores with discrimination p.par  ----
ggpairs(as.data.frame(cbind(p.par_Discrimination = p.par$Discrimination, 
                            CBSF[,c(15:31)])))

## 2. correlate FADL non motor scores with Fluid Intelligence p.par  ----
ggpairs(as.data.frame(cbind(p.par_Fluid_Intellegence = p.par$FluidIntellegence, 
                            CBSF[,c(15:31)])))



## -------------- ALL TASKS  -----------   
## 1. SART - omsission & commission error with LRT, DRT  -----------  
ggpairs(as.data.frame(cbind(SART_omission = SART_subid_NA$om_err, 
                            SART_comission = SART_subid_NA$com_err,  
                            p.par_Recognition = p.par$Recognition, 
                            p.par_Discrimination = p.par$Discrimination, 
                            p.par_FluidIntellegence = p.par$FluidIntellegence)), 
        title = "SART (omission error, comission error), Recognition, Discrimination, FluidIntellegence")


## ------------- FADL WITH TASKS ------------  
## 1. Discrimination and FADL -----------
colnames(CBSF) <- colnames_CBSF # use abbreviations in item names

# calculate correlation matrix  
Dis_FADL_cormat <- round(cor(as.data.frame(cbind(Discrimination = p.par$Discrimination, 
                                                 CBSF[,c(16:31)])))   ,2)
# reorder corr matrix 
cormat <- reorder_cormat(Dis_FADL_cormat)
upper_tri <- get_upper_tri(cormat)

Dis_FADL_cormat_melted <- melt(upper_tri, na.rm = TRUE)


# Create a ggheatmap
ggheatmap <- ggplot(Dis_FADL_cormat_melted, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 35, vjust = 1, 
                                   size = 12, hjust = 1), 
        axis.text.y = element_text(size = 12), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5)) +
  coord_fixed()

# Print the heatmap
print(ggheatmap)




## 2. FI and FADL -----------
# Omit the data for CBSF participants for which FI values are NA 
colnames(CBSF) <- colnames_CBSF # use abbreviations in item names

# calculate correlation matrix  
FI_FADL_cormat <- round(cor(na.omit(as.data.frame(cbind(FI = p.par$FluidIntellegence, 
                                                        CBSF[,c(16:31)]))))   ,2)
# reorder corr matrix 
cormat <- reorder_cormat(FI_FADL_cormat)
upper_tri <- get_upper_tri(cormat)

FI_FADL_cormat_melted <- melt(upper_tri, na.rm = TRUE)


# Create a ggheatmap
ggheatmap <- ggplot(FI_FADL_cormat_melted, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5)) +
  coord_fixed()

# Print the heatmap
print(ggheatmap)

 
## ------------- QUESTIONNAIRES --------- 
## 1. iADL and FADL Non-motor items ----- 

# calculate correlation matrix  
iADL_FADL_cormat <- round(cor(na.omit(as.data.frame(cbind(iADL_subid_NA[,c(2:8)], 
                                                        CBSF[,c(16:31)]))))   ,2)

# reorder corr matrix 
cormat <- reorder_cormat(iADL_FADL_cormat)
upper_tri <- get_upper_tri(cormat)

iADL_FADL_cormat_melted <- melt(upper_tri, na.rm = TRUE)


# Create a ggheatmap
ggheatmap <- ggplot(iADL_FADL_cormat_melted, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 35, vjust = 1, 
                                   size = 12, hjust = 1), 
        axis.text.y = element_text(size = 12), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5)) +
  coord_fixed()

# Print the heatmap
print(ggheatmap)

 
## 2. iADL and FADL motor items ----- 

# calculate correlation matrix  
iADL_FADL_cormat <- round(cor(na.omit(as.data.frame(cbind(iADL_subid_NA[,c(2:8)], 
                                                          CBSF[,c(1:15)]))))   ,2)

# reorder corr matrix 
cormat <- reorder_cormat(iADL_FADL_cormat)
upper_tri <- get_upper_tri(cormat)

iADL_FADL_cormat_melted <- melt(upper_tri, na.rm = TRUE)


# Create a ggheatmap
ggheatmap <- ggplot(iADL_FADL_cormat_melted, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 35, vjust = 1, 
                                   size = 12, hjust = 1), 
        axis.text.y = element_text(size = 12), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5)) +
  coord_fixed()

# Print the heatmap
print(ggheatmap)

## ------------ CBSF factors with neuropsychological tasks ----
## 1. Extract items for all the factors first -- 4 factors ----
CBSF_f1 <- CBSF[c(7,13,4,14,12,2,6,11,9,5,10,3,29)]
CBSF_f2 <- CBSF[c(22,25,24,23,30,28,27)]
CBSF_f3 <- CBSF[c(17,21,16,18,8,15,31,19)]
CBSF_f4 <- CBSF[c(26,20,21)] 

## 2.1 Correlated CBSF_F1 with FRT ---- 

# calculate correlation matrix 
CBSFf1_FRT_cormat <- round(cor(na.omit(as.data.frame(cbind(FI = p.par$FluidIntellegence, 
                                                           CBSF_f1))))   ,2)

# reorder corr matrix 
cormat <- reorder_cormat(CBSFf1_FRT_cormat)
upper_tri <- get_upper_tri(cormat)

CBSFf1_FRT_cormat_melted <- melt(upper_tri, na.rm = TRUE)

# Create a ggheatmap 
ggheatmap <- ggplot(CBSFf1_FRT_cormat_melted, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 35, vjust = 1, 
                                   size = 12, hjust = 1), 
        axis.text.y = element_text(size = 12), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5)) +
  coord_fixed()


# Print the heatmap
print(ggheatmap)



## 2.2 Correlated CBSF_F2 with FRT ---- 

# calculate correlation matrix 
CBSFf2_FRT_cormat <- round(cor(na.omit(as.data.frame(cbind(FI = p.par$FluidIntellegence, 
                                                           CBSF_f2))))   ,2)

# reorder corr matrix 
cormat <- reorder_cormat(CBSFf2_FRT_cormat)
upper_tri <- get_upper_tri(cormat)

CBSFf2_FRT_cormat_melted <- melt(upper_tri, na.rm = TRUE)

# Create a ggheatmap 
ggheatmap <- ggplot(CBSFf2_FRT_cormat_melted, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 35, vjust = 1, 
                                   size = 12, hjust = 1), 
        axis.text.y = element_text(size = 12), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5)) +
  coord_fixed()


# Print the heatmap
print(ggheatmap)



 
## 2.3 Correlated CBSF_F3 with FRT ---- 

# calculate correlation matrix 
CBSFf3_FRT_cormat <- round(cor(na.omit(as.data.frame(cbind(FI = p.par$FluidIntellegence, 
                                                           CBSF_f3))))   ,2)

# reorder corr matrix 
cormat <- reorder_cormat(CBSFf3_FRT_cormat)
upper_tri <- get_upper_tri(cormat)

CBSFf3_FRT_cormat_melted <- melt(upper_tri, na.rm = TRUE)

# Create a ggheatmap 
ggheatmap <- ggplot(CBSFf3_FRT_cormat_melted, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 35, vjust = 1, 
                                   size = 12, hjust = 1), 
        axis.text.y = element_text(size = 12), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5)) +
  coord_fixed()
 

# Print the heatmap
print(ggheatmap)


## 2.4 Correlated CBSF_F4 with FRT ---- 

# calculate correlation matrix 
CBSFf4_FRT_cormat <- round(cor(na.omit(as.data.frame(cbind(FI = p.par$FluidIntellegence, 
                                                           CBSF_f4))))   ,2)

# reorder corr matrix 
cormat <- reorder_cormat(CBSFf4_FRT_cormat)
upper_tri <- get_upper_tri(cormat)

CBSFf4_FRT_cormat_melted <- melt(upper_tri, na.rm = TRUE)

# Create a ggheatmap 
ggheatmap <- ggplot(CBSFf4_FRT_cormat_melted, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 35, vjust = 1, 
                                   size = 12, hjust = 1), 
        axis.text.y = element_text(size = 12), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5)) +
  coord_fixed()


# Print the heatmap
print(ggheatmap)



## 3.1 Correlate CBSF_F1 with SART ----

# calculate correlation matrix 
CBSFf1_SART_cormat <- round(cor(na.omit(as.data.frame(cbind(SART_OmEr = SART_subid_NA$om_err, SART_ComEr = SART_subid_NA$com_err,
                                                           CBSF_f1))))   ,2)

# reorder corr matrix 
cormat <- reorder_cormat(CBSFf1_SART_cormat)
upper_tri <- get_upper_tri(cormat)

CBSFf1_SART_cormat_melted <- melt(upper_tri, na.rm = TRUE)

# Create a ggheatmap 
ggheatmap <- ggplot(CBSFf1_SART_cormat_melted, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 35, vjust = 1, 
                                   size = 12, hjust = 1), 
        axis.text.y = element_text(size = 12), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5)) +
  coord_fixed()


# Print the heatmap
print(ggheatmap)


## 3.2 Correlate CBSF_F2 with SART ----

# calculate correlation matrix 
CBSFf2_SART_cormat <- round(cor(na.omit(as.data.frame(cbind(SART_OmEr = SART_subid_NA$om_err, SART_ComEr = SART_subid_NA$com_err,
                                                            CBSF_f2))))   ,2)

# reorder corr matrix 
cormat <- reorder_cormat(CBSFf2_SART_cormat)
upper_tri <- get_upper_tri(cormat)

CBSFf2_SART_cormat_melted <- melt(upper_tri, na.rm = TRUE)

# Create a ggheatmap 
ggheatmap <- ggplot(CBSFf2_SART_cormat_melted, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 35, vjust = 1, 
                                   size = 12, hjust = 1), 
        axis.text.y = element_text(size = 12), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5)) +
  coord_fixed()


# Print the heatmap
print(ggheatmap)



## 3.3 Correlate CBSF_F3 with SART ----

# calculate correlation matrix 
CBSFf3_SART_cormat <- round(cor(na.omit(as.data.frame(cbind(SART_OmEr = SART_subid_NA$om_err, SART_ComEr = SART_subid_NA$com_err,
                                                            CBSF_f3))))   ,2)

# reorder corr matrix 
cormat <- reorder_cormat(CBSFf3_SART_cormat)
upper_tri <- get_upper_tri(cormat)

CBSFf3_SART_cormat_melted <- melt(upper_tri, na.rm = TRUE)

# Create a ggheatmap 
ggheatmap <- ggplot(CBSFf3_SART_cormat_melted, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 35, vjust = 1, 
                                   size = 12, hjust = 1), 
        axis.text.y = element_text(size = 12), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5)) +
  coord_fixed()


# Print the heatmap
print(ggheatmap)





## 3.4 Correlate CBSF_F4 with SART ----

# calculate correlation matrix 
CBSFf4_SART_cormat <- round(cor(na.omit(as.data.frame(cbind(SART_OmEr = SART_subid_NA$om_err, SART_ComEr = SART_subid_NA$com_err,
                                                            CBSF_f4))))   ,2)

# reorder corr matrix 
cormat <- reorder_cormat(CBSFf4_SART_cormat)
upper_tri <- get_upper_tri(cormat)

CBSFf4_SART_cormat_melted <- melt(upper_tri, na.rm = TRUE)

# Create a ggheatmap 
ggheatmap <- ggplot(CBSFf4_SART_cormat_melted, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 35, vjust = 1, 
                                   size = 12, hjust = 1), 
        axis.text.y = element_text(size = 12), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "bottom", title.hjust = 0.5)) +
  coord_fixed()


# Print the heatmap
print(ggheatmap)







## ------ FACTOR SCORES CORRELATIONS FOR CBSF and FRT ------ 



## - EXIT PDF ----
# dev.off() 

 

## ------- HEAT MAPS -------------- ## 
 
## Correlate sart scores with sleep scores from FADL questionnaire ---- 
SART_subid_NA$sum_err <- as.numeric(SART_subid_NA$sum_err)
SART_subid_NA$corr <- as.numeric(SART_subid_NA$corr)

ggpairs(as.data.frame(cbind(SART_sum_err = SART_subid_NA$sum_err, 
                            sleep = CBSF$`Sleeping at night`)))













