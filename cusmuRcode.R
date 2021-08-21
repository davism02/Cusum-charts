# Input load. Please do not change # 
#`dataset` = read.csv('C:/Users/JRawls3/REditorWrapper_a46caf92-983e-40dd-82d7-b8029ba1820f/input_df_e690df8e-dba8-4842-b3f1-5c1098e90b64.csv', check.names = FALSE, encoding = "UTF-8", blank.lines.skip = FALSE); 
# Original Script. Please update your script content here and once completed copy below section back to the original editing window # 
# The following code to create a dataframe and remove duplicated rows is always executed and acts as a preamble for your script: 

# dataset <- data.frame(Date, Year Week, OEE) 
# dataset <- unique(dataset) 

# Paste or type your script code here: 

### This script assumes that a single ma 

### Load the necessary R packages ### 
library(qcc) 
library(tidyverse) 
library(matrixStats) 
library(SixSigma) 
library(ggplot2) 
library(scales)
### Rename column headers as necessary to remove spaces and commas.  
### If possible, rename in the source data instead. 
names(dataset) <- str_replace_all(names(dataset), c(" " = "." , "," = "")) 

### Reorganize the daily data into weekly subgroups 
OEE.subgroups <- with(dataset, qcc.groups(OEE, Year.Week)) 


### Count the number of subgroups (in this case, weeks) of data 
### Set the number of weeks included in the baseline calculations of 
### mean & standard deviation 
### Define the initial subgroup for the observations to be evaluated 
weeks.total <- nrow(OEE.subgroups) 
weeks.calibration <- 52 
weeks.observation <- weeks.calibration + 1 

### Initialize Zi, the normalized observation 
### Assign values for the decision interval and allowable slack in units of one sigma 
Zi.minus1 <- 0 
H <- 4 
k <- 0.5 

### Calculate the mean and standard deviation of the calibration data 
### Two methods of calculating the standard deviation are shown below. One 
### method calculates the standard deviation from the entire calibration 
### data set.  The second method calculates the estimated standard deviation 
### from the average moving range of the subgroups and uses an unbiasing 
### constant based on a subgroup size of 7 (7 days a week in this case). 
process.mean <- mean(OEE.subgroups[1:weeks.calibration,], na.rm = TRUE) 
process.sd <- sd(OEE.subgroups[1:weeks.calibration,], na.rm = TRUE) 
### ss.cc.getd2 function assigns a name to the variable.  Removing it here. 
unbiasing.constant.d2 <- unname(ss.cc.getd2(7)) 
process.MRSD <- mean( 
  rowMaxs(as.matrix(OEE.subgroups[1:weeks.calibration,]), na.rm = TRUE) 
  - 
    rowMins(as.matrix(OEE.subgroups[1:weeks.calibration,]), na.rm = TRUE) 
) / unbiasing.constant.d2 
### Change this option to switch between calculation options for standard deviation 
process.stdev <- process.MRSD  ##process.stdev <- process.sd 

### Initialize upper.cusum and lower.cusum as vectors of zeroes of length n-1 
### Loop through the weeks and calculate the upper and lower cusum values. 
### Zi.minus1 is recalculated at the end of each iteration to use in the 
### following iteration. These calculations are using the normalized 
### observations (i.e. subtract the expected mean and divide the total by 
### the expected standard deviation). 

upper.cusum <- numeric(weeks.total-1) 
lower.cusum <- numeric(weeks.total-1) 
for (i in c(1:(weeks.total-1))) { 
  upper.cusum[i] = max(0, lower.cusum[i-1] + Zi.minus1 - k) 
  lower.cusum[i] = min(0, lower.cusum[i-1] + Zi.minus1 + k) 
  Zi.minus1 <- (mean(OEE.subgroups[i,],na.rm = TRUE) - process.mean)/process.stdev 
} 

### Create a new dataframe with the weeks, upper cusum values, and lower cusum values    
df.plot <-  data.frame( 
  Weeks = rownames(OEE.subgroups[2:weeks.total,]), 
  UC = upper.cusum, 
  LC = lower.cusum 
) %>% 
  mutate(belowsigma=ifelse(LC < -4,T,F),N=1:n())
  
#Plot Parameters
weeks_vec = df.plot$Weeks
min_y = floor(min(df.plot$LC))

### Plot the data 
ggplot(df.plot, aes(x = Weeks,group =1)) + 
 geom_line(aes(y = UC) ) + 
 geom_line(aes(y=LC, color=belowsigma) ) + 
 geom_abline(intercept = -4, slope=0, linetype = "dashed")+
 geom_abline(intercept = 4, slope=0, linetype = "dashed")+
 ylab("Cumulative Sum") + ylim(min_y,6)+
 scale_color_manual(values=c("black","red")) +
 scale_x_discrete(limits=weeks_vec, breaks=weeks_vec[seq(1,length(weeks_vec),by=5)]) +
 theme(legend.position = "none",
axis.text.x = element_text(size=6,color="black",angle = 90, hjust=1, vjust=0.5))

