##### Create histograms to show the floor and ceiling effects in the data ########## 

## 1. Clear work space and set path variables -----
rm(list=ls()) # clear environment 
graphics.off() # clear plots 
pathin <- "/Users/japneetbhatia/desktop/thesis/13.STATISTICAL_ANALYSIS/scripts" 

setwd(pathin) 
source("6.item_analysis.R") 

# remove unnecessary vars 
rm(list = "AMT_grp", "AMT_var", "DRT_grp", "DRT_var", "FRT_grp", 
   "FRT_var", "iADL_grp", "iADL_var", "CBSF_grp", "CBSF_var", "data_with_variance", 
   "dfs", "demog_data", "demog_med", "medical_data_day2mssing", "demog_data_day2missing", 
   "filenames", "FRT_subid", "SART_subid", "SART_na", "grp_data", "AMT", "DRT", "FRT", 
   "AMT_sub") 

# ----- PLOT HISTOGRAMS FOR FLOOR AND CEING EFFECTS IN THE DATA ------- # -----
## ----  1. AMT  ----- ## -----

# restructure the data to fit histograms -----
AMTlong<-melt(sum.sc.AMT, id.vars = "Items") # convert to long format 
AMTlong$value <- as.numeric(AMTlong$value)# convert the datatype to numeric 
AMTlong <- arrange(AMTlong, value) # arrange in ascending order 
AMTlong$value <- as.character(AMTlong$value)# convert the datatype to character 
AMTlong$value[AMTlong$value == 0] <- NA # change the 0 values to NA to avoid them from plotting 
xlabs <- 1:18 

# plot histograms -----
AMT_plot <- ggplot(data=subset(AMTlong, !is.na(value)),aes(x = factor(Items, level = sum.sc.AMT$Items), group = variable, fct_inorder(value),fill=variable))+
            geom_col(width = 1, position = position_dodge2(width = 0.10, preserve = "single"))+
            xlab("Taks Items") + 
            ylab("Response frequencies") + 
            scale_x_discrete(labels=xlabs) + 
            ggtitle("Associative Match Task") +
            scale_fill_manual(values = c("darkblue", "lightgreen"), 
                              labels = c("Incorrect", "Correct")) + 
            theme_classic() +
            guides(fill = guide_legend(title = "Responses")) +
            theme(text = element_text(family = "Times New Roman", size = 12))

AMT_plot


## ----  2. DRT  ----- ## -----

#restructure the data to fit histograms -----
DRTt <- data.frame(t(sum.sc.DRT)) # transpose and save as df 
rownames(DRTt) -> DRTt[,3] # get rownames in 3rd column 
colnames(DRTt) <- c("R0", "R1", "items")# add headings 
DRTt$items <-  gsub("_r", "", DRTt$items) # remove unnecessary chars 
DRTt$items <- factor(DRTt$items, levels = DRTt$items) # lock in factor level order 
DRTlong<-melt(DRTt, id.vars = "items") # convert to long format 
DRTlong$value <- as.numeric(DRTlong$value)# convert the datatype to numeric for next statement 
DRTlong <- arrange(DRTlong, value) # arrange in ascending order 
DRTlong$value <- as.character(DRTlong$value)# convert the datatype to character, requirement for setting a factor 
xlabs <- 1:18 

# plot bar graphs -----
DRT_plot <- ggplot(DRTlong,aes(x = factor(items, level = DRTt$items), group = variable, fct_inorder(value),fill=variable))+
            geom_col(width = 1, position = position_dodge2(width = 0.10, preserve = "single"))+
            xlab("Taks Items") + 
            ylab("Response frequencies") + 
            scale_x_discrete(labels=xlabs) + 
            ggtitle("Delayed object recognition Task") + 
            scale_fill_manual(values = c("darkblue", "lightgreen"), 
                              labels = c("Incorrect", "Correct")) +
            theme_classic() +
            guides(fill = guide_legend(title = "Responses"))+
            theme(text = element_text(family = "Times New Roman", size = 12))

DRTlong$value <- as.numeric(DRTlong$value) # convert the datatype to numeric for next statement 

DRT_plot <- DRT_plot + 
            scale_y_discrete(breaks = seq(0,max(DRTlong$value), by = 3))
  
DRT_plot 

## ----  3. FRT  ----- ## -----

# restructure the data to fit histograms -----
FRTlong<-melt(sum.sc.FRT, id.vars = "Items")  
FRTlong$value <- as.numeric(FRTlong$value)# convert the datatype to numeric 
FRTlong <- arrange(FRTlong, value) # arrange in ascending order 
FRTlong$value <- as.character(FRTlong$value)# convert the datatype to char 
FRTlong$value[FRTlong$value == 0] <- NA # change the 0 values to NA to avoid them from plotting 
xlabs <- 1:30 
  
# plot bar graphs -----
FRT_plot <- ggplot(data=subset(FRTlong, !is.na(value)), aes(x = factor(Items, level = sum.sc.FRT$Items), group = variable, fct_inorder(value),fill=variable))+
            geom_col(width = 1, position = position_dodge2(width = 0.10, preserve = "single"))+
            xlab("Taks Items") + 
            ylab("Response frequencies") + 
            scale_x_discrete(labels=xlabs) + 
            ggtitle("Logical Reasoning Task") +
            scale_fill_manual(values = c("darkblue", "lightgreen"), 
                              labels = c("Incorrect", "Correct")) +
            theme_classic() +
            guides(fill = guide_legend(title = "Responses")) + 
            theme(text = element_text(family = "Times New Roman", size = 12), 
                  legend.position = "bottom")


FRT_plot 

FRTlong$value <- as.numeric(FRTlong$value)# convert the datatype to numeric
FRT_plot <- FRT_plot + 
            scale_y_discrete(breaks = seq(0, max(FRTlong$value, na.rm = TRUE), by = 2))
FRT_plot 
  
# ----- 4. iADL ------ ## ----- 
# correct table structure and labels -----
items <- colnames(iADL)    # get item names 
sum.sc.iADL <- cbind(sum.sc.iADL,items)     #combine item names with the frequency scores table  
colnames(sum.sc.iADL) <- c("Independent", "Minor_dependence", "Major_dependence", "items")  # correct headings 
sum.sc.iADL <- as.data.frame(sum.sc.iADL)   # change to df 
  
# long format -----
iADLlong <- melt(sum.sc.iADL, measure = c("Independent", "Minor_dependence", "Major_dependence"))  
  
# plot -----
iADL_plot <- ggplot(subset(iADLlong, !is.na(value)), aes(items, value, fill = variable), 
       group = variable, fill = variable)+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  facet_grid(~variable)+
  theme_classic()+
  ylab("Frequencies")+
  xlab("Task items")+
  ggtitle("Instrumental Activities of Daily Living Questionnaire")+
  scale_y_discrete(breaks = seq(0,max(as.numeric(iADLlong$value), na.rm = TRUE), 
                                by = 1))+
  theme(axis.text.y = element_text(size = 13), 
        axis.title = element_text(size = 13), 
        plot.title = element_text(size = 16)) 

iADL_plot
 
# ----- 5. CBSF ------ ## -----
 
# correct table structure and labels -----
items <- colnames(CBSF) 
sum.sc.CBFS <- cbind(sum.sc.CBFS,items) 
colnames(sum.sc.CBFS) <- c("No_assistance", "Slight_assistance", "Mild_assistance", 
                           "Moderate_sssistance", "Major_assistance", "items")  # correct headings 
sum.sc.CBFS <- as.data.frame(sum.sc.CBFS)   # change to df 
 
# long format 
CBSFlong<-melt(sum.sc.CBFS, measure = c("No_assistance", "Slight_assistance", "Mild_assistance", 
                                        "Moderate_sssistance", "Major_assistance"))  

# # to fix the yaxis values to get in ascending order 
# CBSFlong$value <- as.numeric(CBSFlong$value)# convert the datatype to numeric 
# CBSFlong <- arrange(CBSFlong, value) # arrange in ascending order 
# CBSFlong$value <- as.character(CBSFlong$value)# convert the datatype to numeric 

 
# plot bar plot -----

CBSF_plot2 <- ggplot(subset(CBSFlong, !is.na(value)), aes(items, value, fill = variable), 
       group = variable, fill = variable)+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()+
  facet_grid(~variable)+
  ylab("Frequencies")+
  xlab("Task items")+
  ggtitle("Functional Experiences of Daily living Questionnaire")+
  scale_y_discrete(breaks = seq(0,max(as.numeric(CBSFlong$value), na.rm = TRUE), 
                                by = 5))+
  theme_classic()+ 
  guides(fill = guide_legend(title = "Responses"))+ 
  theme(axis.text.y = element_text(size = 13), 
        axis.title = element_text(size = 13), 
        plot.title = element_text(size = 16)) 

CBSF_plot2
 
# ------ 6. SART ------ ## -----

ggplot(SART_long, aes(StudyID, value, fill = value))+
  theme_classic()
  
  



 
# -------7.  Get the 3 tasks in one plot ---------- ##### -----

top_row <- ggarrange(AMT_plot,DRT_plot, 
                     ncol=2,nrow=1, 
                     common.legend = TRUE, legend = "top")

task_variance_bplots <- ggarrange(top_row, FRT_plot, 
                        ncol=1, nrow=2, 
                        common.legend = TRUE, legend = "right") 

task_variance_bplots




 

# 
 
# ---- SAVE PLOTS -------- 

pdf(file = "/Users/japneetbhatia/desktop/thesis/14.STATISTICAL_RESULTS/plots/floor.&.ceiling.effects.pdf",   # The directory you want to save the file in
    width = 13, # The width of the plot in inches
    height = 10) # The height of the plot in inches

CBSF_plot2
iADL_plot
task_variance_bplots 

dev.off()
 
# -------- CBSF further analysis ------ 
# # Get the items in which participants need mild assistance 
# CBSF_mild <- filter(CBSF_long, value == 3)
# unique(CBSF_mild$variable)
# CBSF_mod <- filter(CBSF_long, value == 4)
# unique(CBSF_mod$variable)
# 
# 
# # count how many participants need mild assistance in every item and plot them 
# ggplot(CBSF_mild$value, aes(x = unique(CBSF_mild$variable), y = CBSF_mild$variable)+ 
#          geom_bar(stat = "identity", position = "dodge"))







plot + theme(text = element_text(family = "Times New Roman", size = 12))  