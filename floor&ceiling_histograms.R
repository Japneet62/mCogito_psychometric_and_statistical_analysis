##### Create histograms to show the floor and ceiling effects in the data ########## 

## 1. Clear work space and set path variables 
rm(list=ls()) # clear environment 
graphics.off() # clear plots 
pathin <- "/Users/japneetbhatia/desktop/thesis/13.STATISTICAL_ANALYSIS/scripts" 

setwd(pathin) 
source("6.item_analysis.R") 

library(reshape2) 

# remove unnecessary vars 
rm(list = "AMT_grp", "AMT_long", "AMT_var", "AMT_item_categories", "DRT_grp", "DRT_long", "DRT_var", 
   "FRT_grp", "FRT_long", "FRT_var", "iADL_grp", "iADL_long", "iADL_var", "CBSF_grp", "CBSF_long", 
   "CBSF_var", "data_with_variance", "dfs", "demog_data", "demog_med", "medical_data_day2mssing", 
   "demog_data_day2missing", "filenames", "FRT_subid", "SART_subid", "SART_na", "grp_data" )

# ----- PLOT HISTOGRAMS FOR FLOOR AND CEING EFFECTS IN THE DATA ------- #

######## 1. AMT ######### 

#restructure the data to fit histograms 
AMTt <- data.frame(t(sum.sc.AMT)) 
rownames(AMTt) -> AMTt[,3]
colnames(AMTt) <- c("R0", "R1", "items") 
AMTt$items <-  gsub("_", "", AMTt$items)
AMTt$items <-  gsub("r", "", AMTt$items)
AMTlong<-melt(AMTt) # convert data to long format 

# lock in factor level order 
  AMTt$items <- factor(AMTt$items, levels = AMTt$items)

# Plot histogram
AMTplot <- ggplot(data = AMTt, aes(x = items, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge2(width=0.10, preserve = "total"), ) +
  geom_bar(aes(x = items, y = R0, fill = "lightgreen"),
           stat = "identity", position = position_dodge2(width=0.10), preserve = "total") +
  xlab("Taks Items") +
  ylab("Response frequencies") + 
  ggtitle("Associative Match Task") +
  scale_fill_manual(values = c("lightblue", "lightgreen"), 
                    labels = c("Correct", "Incorrect")) +
  theme_classic() + 
  guides(fill = guide_legend(title = "Response")) 

AMTplot

# another attempt 

library(reshape2) 
df.long<-melt(AMTt)
ggplot(df.long,aes(items,value,fill=variable))+
  geom_bar(stat="identity",position="dodge")

######### 2. DRT ######### 

#restructure the data to fit histograms 
DRTt <- data.frame(t(sum.sc.DRT)) 
rownames(DRTt) -> DRTt[,3]
colnames(DRTt) <- c("R0", "R1", "items") 
DRTt$items <-  gsub("_r", "", DRTt$items)

# lock in factor level order 
DRTt$items <- factor(DRTt$items, levels = DRTt$items)

# Plot histogram 
DRT_plot <- ggplot(data = DRTt, aes(x = items, y = R1, fill = "lightgreen")) +
  geom_bar(stat = "identity") +
  geom_bar(aes(x = items, y = R0, fill = "lightblue"),
           stat = "identity") +
  xlab("Taks Items") + 
  ylab("Response frequencies") + 
  ggtitle("Delayed object recognition Task") +
  scale_fill_manual(values = c("lightblue", "lightgreen"), 
                    labels = c("Correct", "Incorrect")) +
  theme_classic() +
  guides(fill = guide_legend(title = "Response")) 

DRT_plot

# ggplot(data = DRTt, aes(x = items, y = R1, fill = "green")) +
#   geom_bar(fill="#69b3a2", color="#e9ecef", alpha=0.9)

######### 3. FRT  ######### 
FRTt <- data.frame(t(sum.sc.FRT)) 
rownames(FRTt) -> FRTt[,3]
colnames(FRTt) <- c("R0", "R1", "items") 
FRTt$items <-  gsub("_ans", "", FRTt$items)

# lock in factor level order 
FRTt$items <- factor(FRTt$items, levels = FRTt$items)

# Plot histogram
FRT_plot <- ggplot(data = FRTt, aes(x = items, y = R1, fill = "lightgreen")) +
  geom_bar(stat = "identity") +
  geom_bar(aes(x = items, y = R0, fill = "lightblue"),
           stat = "identity") +
  xlab("Taks Items") + 
  ylab("Response frequencies") + 
  ggtitle("Figural Reasoning Task") +
  scale_fill_manual(values = c("lightblue", "lightgreen"), 
                    labels = c("Correct", "Incorrect")) +
  theme_classic() +
  guides(fill = guide_legend(title = "Response")) 

FRT_plot

######### 4. iADL  ######### 

# correct table structure and labels 
items <- colnames(iADL)    # get item names 
sum.sc.iADL <- cbind(sum.sc.iADL,items)     #combine item names with the frequency scores table  
colnames(sum.sc.iADL) <- c("Independent", "Minor_dependence", "Major_dependence", "items")  # correct headings 
sum.sc.iADL <- as.data.frame(sum.sc.iADL)   # change to df 

# replace NA values to 0 
sum.sc.iADL[is.na(sum.sc.iADL)] <- 0

# lock in factor level order 
sum.sc.iADL$items <- factor(sum.sc.iADL$items, levels = sum.sc.iADL$items)  # save items as fixed factor 

# Plot histogram 
iADL_plot <- ggplot(data = sum.sc.iADL, aes(x = items, y = Independent, fill = "lightgreen")) +
  geom_bar(stat = "identity") +
  geom_bar(aes(x = items, y = Minor_dependence, fill = "lightblue"),
           stat = "identity") +
  geom_bar(aes(x = items, y = Major_dependence, fill = "blue"),
           stat = "identity") +
  xlab("Taks Items") +
  ylab("Response frequencies") +
  ggtitle("Instrumental Activities of Daily Living Questionnaire") +
  scale_fill_manual(values = c("lightblue", "lightgreen", "blue"),
                    labels = c("Independent", "Minor_dependence", "Major_dependence")) +
  theme_classic() +
  guides(fill = guide_legend(title = "Responses"))

iADL_plot +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

######### 5. CBSF  ######### 

#correct structure and labels 
items <- colnames(CBSF)
sum.sc.CBFS <- cbind(sum.sc.CBFS,items) 

# lock in factor level order 
sum.sc.CBFS$items <- factor(sum.sc.CBFS$items, levels = sum.sc.CBFS$items)  # save items as fixed factor 

# convert NA values to 0 
sum.sc.CBFS[is.na(sum.sc.CBFS)] <- 0

# Plot histogram 
CBSF_plot <- ggplot(data = sum.sc.CBFS, aes(x = items, y = No_assistance, fill = "darkgreen")) +
  geom_bar(stat = "identity", position = position_dodge(width=0.5)) + 
  geom_bar(aes(x = items, y = Slight_assistance, fill = "green"),
           stat = "identity", position = position_dodge(width=0.5)) + 
  geom_bar(aes(x = items, y = Mild_assistance, fill = "lightgreen"),
           stat = "identity", position = position_dodge(width=0.5)) +
  geom_bar(aes(x = items, y = Moderate_sssistance, fill = "lightblue"),
           stat = "identity", position = position_dodge(width=0.5)) +
  geom_bar(aes(x = items, y = Major_assistance, fill = "blue"),
           stat = "identity", position = position_dodge(width=0.5)) +
  xlab("Taks Items") + 
  ylab("Response frequencies") + 
  ggtitle("Basal ganglion questionnaire") +
  scale_fill_manual(values = c("darkgreen", "green", "lightgreen", "lightblue", "blue"), 
                    labels = c("No_assistance", "Slight_assistance", "Mild_assistance", 
                               "Moderate_sssistance", "Major_assistance")) +
  theme_classic() +
  guides(fill = guide_legend(title = "Responses")) 

CBSF_plot+
theme(axis.text.x = element_text(angle = 45, hjust = 1))