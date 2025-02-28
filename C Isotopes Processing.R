#Clear Workspace----
remove(list = ls(all = TRUE))
start <- Sys.time()

#Packages Loading----
library(openxlsx)
library(ggplot2)

#Import Inputs----
folder <- "file path here"
standard_before <- "standard-before-sample file name here"
standard_before_sheets <- 5
standard_after <- "standard-after-sample file name here"
standard_after_sheets <- 6
sample <- "sample file name here"
sample_sheets <- 17
  
#Import----
standard_before <- paste(folder,"/",standard_before,".xlsx",sep="")
standard_after <- paste(folder,"/",standard_after,".xlsx",sep="")
sample <- paste(folder,"/",sample,".xlsx",sep="")
standard_before_constants <- read.xlsx(xlsxFile=standard_before,sheet=1)
remove(folder)

#C13/13 Functions----
C12_13_Average_Calculator <- function(input){#c12_13 = carbon-13/carbon-12; c12 = carbon 12; c13 = carbon 13
  data <- read.xlsx(xlsxFile=object,sheet=input)
  constants <- read.xlsx(xlsxFile=object,sheet=1)#This "constant" is the deadtime
  c12_dt <- data$C12/(1-(data$C12*constants$Deadtime))#dt = deadtime
  c13_dt <- data$C13/(1-(data$C13*constants$Deadtime))
  c13_12 <- c13_dt/c12_dt
  output <- sum(c13_12)/as.numeric(length(c13_12))
}
C12_13_Error_Calculator <- function(input){
  data <- read.xlsx(xlsxFile=object,sheet=input)
  constants <- read.xlsx(xlsxFile=object,sheet=1)
  c12_dt <- data$C12/(1-(data$C12*constants$Deadtime))
  c13_dt <- data$C13/(1-(data$C13*constants$Deadtime))
  c13_12 <- c13_dt/c12_dt
  c13_12_mean <- sum(c13_12)/as.numeric(length(c13_12))
  output <- (((sum((c13_12-c13_12_mean)^2))/(as.numeric(length(c13_12))-1))^0.5)/(as.numeric(length(c13_12))^0.5)
  return(output)
}

#C13/12 Calculations----
object <- standard_before
sheets <- seq(by=1,from=2,to=standard_before_sheets)
standard_before_ratio <- sapply(FUN=C12_13_Average_Calculator,X=sheets)
standard_before_error <- sapply(FUN=C12_13_Error_Calculator,X=sheets)
object <- standard_after
sheets <- seq(by=1,from=2,to=standard_after_sheets)
standard_after_ratio <- sapply(FUN=C12_13_Average_Calculator,X=sheets)
standard_after_error <- sapply(FUN=C12_13_Error_Calculator,X=sheets)
object <- sample
sheets <- seq(by=1,from=2,to=sample_sheets)
sample_ratio <- sapply(FUN=C12_13_Average_Calculator,X=sheets)
sample_error <- sapply(FUN=C12_13_Error_Calculator,X=sheets)
remove(object,sample,sample_sheets,sheets,standard_after,standard_after_sheets,standard_before,standard_before_sheets,C12_13_Average_Calculator)
remove(C12_13_Error_Calculator)

#Ordinal Assignment----
ordinal <- seq(by=1,from=1,to=sum(as.numeric(length(standard_before_ratio)),as.numeric(length(sample_ratio)),as.numeric(length(standard_after_ratio))))
standard_before_ordinal <- ordinal[1:as.numeric(length(standard_before_ratio))]
sample_ordinal <- ordinal[(1+as.numeric(length(standard_before_ratio))):(as.numeric(length(standard_before_ratio))+as.numeric(length(sample_ratio)))]
standard_after_ordinal <- ordinal[(1+as.numeric(length(standard_before_ratio))+as.numeric(length(sample_ratio))):as.numeric(length(ordinal))]
remove(ordinal)

#Drift Correction----
standard_error <- c(standard_before_error,standard_after_error)
standard_ordinal <- c(standard_before_ordinal,standard_after_ordinal)
standard_ratio <- c(standard_before_ratio,standard_after_ratio)
standard <- data.frame(standard_error,standard_ordinal,standard_ratio)
drift_model <- lm(data=standard,formula="standard_ratio~standard_ordinal")
drift_slope <- as.numeric(drift_model$coefficients[2])
standard_drift_corrected <- standard_ratio-(standard_ordinal*drift_slope)
remove(drift_model,standard,standard_after_error,standard_after_ordinal,standard_after_ratio,standard_before_error,standard_before_ordinal)
remove(standard_before_ratio,standard_error,standard_ordinal,standard_ratio)

#Instrumental Mass Fractionation Correction----
standard_drift_corrected_d13 <- ((standard_drift_corrected/standard_before_constants$PDB_Air)-1)*1000
standard_imf <- standard_drift_corrected_d13-standard_before_constants$Stand_d13
imf_mean <-  sum(standard_imf)/as.numeric(length(standard_imf))
imf_error <- (((sum((standard_imf-imf_mean)^2))/(as.numeric(length(standard_imf))-1))^0.5)/(as.numeric(length(standard_imf))^0.5)
remove(standard_drift_corrected,standard_drift_corrected_d13,standard_imf)

#Sample Value Calculation----
sample_drift_corrected <- sample_ratio-(sample_ordinal*drift_slope)
sample_d13 <- (((sample_drift_corrected/standard_before_constants$PDB_Air)-1)*1000)-imf_mean
sample_d13_error <- (sample_error/standard_before_constants$PDB_Air)*1000
sample_data <- data.frame(sample_d13,sample_d13_error)
remove(standard_before_constants,drift_slope,imf_error,imf_mean,sample_drift_corrected,sample_error,sample_ordinal,sample_ratio,sample_d13,sample_d13_error)

#Export----
colnames(sample_data) <- c("d13C","Standard_Error")
write.xlsx(x=sample_data,file="/home/secana/Desktop/C Isotopes.xlsx",rowNames=FALSE)

#Plot----
x <- seq(by=1,from=1,to=nrow(sample_data))
graph_DF <- data.frame(sample_data$d13C,sample_data$Standard_Error,x)
colnames(graph_DF) <- c("d13C","Standard_Error","x")
graph <- ggplot(data=graph_DF)+theme_classic()+ylab("d13C PDB")+xlab("")
graph <- graph+geom_errorbar(mapping=aes(x=x,ymin=(d13C-Standard_Error),ymax=(d13C+Standard_Error)),color="gray",width=0)
graph <- graph+geom_point(mapping=aes(x=x,y=d13C))
print(graph)
remove(x)

#Time Stamp----
end <- Sys.time()
runtime <- (end-start)
print (runtime)
remove(start,end,runtime)