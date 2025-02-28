#Clear Workspace----
remove(list=ls(all=TRUE))#Note: This script is the second step for processing our instrumental O isotope data. See "O Isotope Processor" for the first step.
start <- Sys.time()

#Packages Loading----
library(openxlsx)
library(ggplot2)

#Import Data----
sample <- read.xlsx(xlsxFile="sample file name and path here",sheet=1)
standard_pre <- read.xlsx(xlsxFile="standard-before-sample file name and path here",sheet=1)
standard_post <- read.xlsx(xlsxFile="standard-after-sample file name and path here",sheet=1)

#Other Inputs----
standard_delta18_actual <- -6.05
drift_trimming <- #c(5,6,7,8,9)

#Fixed----
smow_17 <- 0.0003799
smow_18 <- 0.0020052

#Standard Processing----
standard_delta17_actual <- 0.52*standard_delta18_actual
standard_17_actual <- ((standard_delta17_actual/1000)+1)*smow_17
standard_18_actual <- ((standard_delta18_actual/1000)+1)*smow_18

#Ordinal Assignment----
ordinal <- seq(from=1,to=sum(c(nrow(standard_pre),nrow(sample),nrow(standard_post))),by=1)
standard_pre_ordinal <- ordinal[1:nrow(standard_pre)]
sample_ordinal <- ordinal[(1+nrow(standard_pre)):(nrow(standard_pre)+nrow(sample))]
standard_post_ordinal <- ordinal[(1+nrow(standard_pre)+nrow(sample)):(nrow(standard_pre)+nrow(sample)+nrow(standard_post))]

#Drift Correction----
standard_ordinal <- c(standard_pre_ordinal,standard_post_ordinal)
standard_17 <- c(standard_pre$mean_17,standard_post$mean_17)
standard_17_error <- c(standard_pre$std_17,standard_post$std_17)
standard_18 <- c(standard_pre$mean_18,standard_post$mean_18)
standard_18_error <- c(standard_pre$std_18,standard_post$std_18)
standard_DF0 <- data.frame(standard_ordinal,standard_17,standard_18)
standard_17_plot <- ggplot(data=standard_DF0)+theme_classic()+xlab("Ordinal")+ylab("Delta 17O")
standard_17_plot <- standard_17_plot+geom_point(mapping=aes(x=standard_ordinal,y=standard_17))
print(standard_17_plot)
standard_18_plot <- ggplot(data=standard_DF0)+theme_classic()+xlab("Ordinal")+ylab("Delta 18O")
standard_18_plot <- standard_18_plot+geom_point(mapping=aes(x=standard_ordinal,y=standard_18))
print(standard_18_plot)
standard_DF1 <- data.frame(standard_ordinal[],standard_17[],standard_18[])
drift_17_model <- lm(data=standard_DF1,formula="standard_17~standard_ordinal")
drift_17_slope <- as.numeric(drift_17_model$coefficients[2])
drift_18_model <- lm(data=standard_DF1,formula="standard_18~standard_ordinal")
drift_18_slope <- as.numeric(drift_18_model$coefficients[2])
standard_17_drift_correction <- standard_ordinal*drift_17_slope
standard_17_corrected <- standard_17-standard_17_drift_correction
standard_delta17 <- ((standard_17_corrected/smow_17)-1)*1000
standard_18_drift_correction <- standard_ordinal*drift_18_slope
standard_18_corrected <- standard_18-standard_18_drift_correction
standard_delta18 <- ((standard_18_corrected/smow_18)-1)*1000
standard_delta18_imf_correction <- standard_delta18-standard_delta18_actual
standard_delta17_imf_correction <- standard_delta17-0.52*standard_delta18
standard_delta17_imf_mean <- mean(standard_delta17_imf_correction)
standard_delta17_imf_error <- sd(standard_delta17_imf_correction)
standard_delta18_imf_mean <- mean(standard_delta18_imf_correction)
standard_delta18_imf_error <- sd(standard_delta18_imf_correction)

#Sample Correction----
sample_17_drift_correction <- sample_ordinal*drift_17_slope
sample_18_drift_correction <- sample_ordinal*drift_18_slope
sample_17_drift_corrected <- sample$mean_17-sample_17_drift_correction
sample_18_drift_corrected <- sample$mean_18-sample_18_drift_correction
sample_delta17 <- ((sample_17_drift_corrected/smow_17)-1)*1000
sample_delta17_error <- ((sample$std_17/smow_17))*1000
sample_delta18 <- ((sample_18_drift_corrected/smow_18)-1)*1000
sample_delta18_error <- ((sample$std_18/smow_18))*1000
sample_delta17_IMF_corrected <- sample_delta17-standard_delta17_imf_mean-0.52*standard_delta18_imf_mean
sample_delta17_error_IMF_corrected <- ((sample_delta17_error^2)+(standard_delta17_imf_error^2))^0.5
sample_delta18_IMF_corrected <- sample_delta18-standard_delta18_imf_mean
sample_delta18_error_IMF_corrected <- ((sample_delta18_error^2)+(standard_delta18_imf_error^2))^0.5
sample_delta17_error_IMF_corrected <- sample_delta17_error_IMF_corrected
sample_delta18_IMF_corrected <- sample_delta18_IMF_corrected
sample_delta18_error_IMF_corrected <- sample_delta18_error_IMF_corrected
sample_ordinal <-  sample_ordinal
sample_delta17_IMF_corrected <- sample_delta17_IMF_corrected

#Export----
output <- data.frame(sample_ordinal,sample_delta17_IMF_corrected,sample_delta17_error_IMF_corrected,sample_delta18_IMF_corrected,sample_delta18_error_IMF_corrected)
colnames(output) <- c("Ordinal","d17O","d17O SD","d18O","d18O SD")
write.xlsx(x=output,file="/home/secana/Desktop/Correct O Isotopes.xlsx",rowNames=FALSE)

#Graph Data----
ymin <- sample_delta17_IMF_corrected-sample_delta17_error_IMF_corrected
ymax <- sample_delta17_IMF_corrected+sample_delta17_error_IMF_corrected
xmin <- sample_delta18_IMF_corrected-sample_delta18_error_IMF_corrected
xmax <- sample_delta18_IMF_corrected+sample_delta18_error_IMF_corrected
graph_DF <- data.frame(sample_delta17_IMF_corrected,sample_delta18_IMF_corrected,ymin,ymax,xmin,xmax)
graph <- ggplot(data=graph_DF)+theme_classic()+xlab("d18O SMOW")+ylab("d17O SMOW")+coord_fixed()
graph <- graph+geom_errorbar(mapping=aes(x=sample_delta18_IMF_corrected,ymin=ymin,ymax=ymax),color="gray")
graph <- graph+geom_errorbarh(mapping=aes(y=sample_delta17_IMF_corrected,xmin=xmin,xmax=xmax),color="gray")
graph <- graph+geom_point(mapping=aes(x=sample_delta18_IMF_corrected,y=sample_delta17_IMF_corrected))
print(graph)

#Time Stamp----
end <- Sys.time()
runtime <- (end-start)
print (runtime)
remove(start,end,runtime)