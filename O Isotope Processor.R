#Clear Workspace----
remove(list=ls(all=TRUE))#Note: This script is the first step for processing our instrumental O isotope data. See "O Isotope Corrector" for the second and final step.
start <- Sys.time()

#Packages Loading----
library(openxlsx)

#Import Settings----
file <- "file name here"
sheet_pairs <- 3

#Import Prep----
file <- paste("file path here",file,".xlsx",sep="")
sheet_pairs <- seq(from=1,to=sheet_pairs,by=1)

#Functions----
Mean17 <- function(input){
  stand_pre_misc <- read.xlsx(xlsxFile=file,sheet=((input*2)-1))
  stand_pre_data <- read.xlsx(xlsxFile=file,sheet=(input*2))
  background <- stand_pre_misc$`16OO.bkgd.c/s`
  interfer <- stand_pre_misc$`interference.peak-to-tail.ratio`
  deadtime_17 <- stand_pre_misc$Deadtime.17O
  o16 <- stand_pre_data$O16
  o17 <- stand_pre_data$O17
  item_17_1 <- stand_pre_data$Item_17_1
  o16_back_correct <- o16-background
  o17_deadtime_correct <- o17/(1-(o17*deadtime_17))
  calc_17_1 <- interfer*item_17_1
  o16h1_correct <- o17_deadtime_correct-calc_17_1
  ratio_17_16 <- o16h1_correct/o16_back_correct
  output <- sum(ratio_17_16)/(as.numeric(length(ratio_17_16)))
  return(output)
}
STD17 <- function(input){
  stand_pre_misc <- read.xlsx(xlsxFile=file,sheet=((input*2)-1))
  stand_pre_data <- read.xlsx(xlsxFile=file,sheet=(input*2))
  background <- stand_pre_misc$`16OO.bkgd.c/s`
  interfer <- stand_pre_misc$`interference.peak-to-tail.ratio`
  deadtime_17 <- stand_pre_misc$Deadtime.17O
  o16 <- stand_pre_data$O16
  o17 <- stand_pre_data$O17
  item_17_1 <- stand_pre_data$Item_17_1
  o16_back_correct <- o16-background
  o17_deadtime_correct <- o17/(1-(o17*deadtime_17))
  calc_17_1 <- interfer*item_17_1
  o16h1_correct <- o17_deadtime_correct-calc_17_1
  ratio_17_16 <- o16h1_correct/o16_back_correct
  mean <- sum(ratio_17_16)/(as.numeric(length(ratio_17_16)))
  dev <- (ratio_17_16-mean)^2
  dev <- sum(dev)/(as.numeric(length(ratio_17_16))-1)
  dev <- dev^0.5
  dev <- dev/((as.numeric(length(ratio_17_16)))^0.5)
  output <- dev
  return(output)
}
Mean18 <- function(input){
  stand_pre_misc <- read.xlsx(xlsxFile=file,sheet=((input*2)-1))
  stand_pre_data <- read.xlsx(xlsxFile=file,sheet=(input*2))
  background <- stand_pre_misc$`16OO.bkgd.c/s`
  interfer <- stand_pre_misc$`interference.peak-to-tail.ratio`
  deadtime_18 <- stand_pre_misc$Deadtime.18O
  o16 <- stand_pre_data$O16
  o18 <- stand_pre_data$O18
  o16_back_correct <- o16-background
  o18_deadtime_correct <- o18/(1-(o18*deadtime_18))
  ratio_18_16 <- o18_deadtime_correct/o16_back_correct
  output <- sum(ratio_18_16)/(as.numeric(length(ratio_18_16)))
  return(output)
}
STD18 <- function(input){
  stand_pre_misc <- read.xlsx(xlsxFile=file,sheet=((input*2)-1))
  stand_pre_data <- read.xlsx(xlsxFile=file,sheet=(input*2))
  background <- stand_pre_misc$`16OO.bkgd.c/s`
  interfer <- stand_pre_misc$`interference.peak-to-tail.ratio`
  deadtime_18 <- stand_pre_misc$Deadtime.18O
  o16 <- stand_pre_data$O16
  o18 <- stand_pre_data$O18
  o16_back_correct <- o16-background
  o18_deadtime_correct <- o18/(1-(o18*deadtime_18))
  ratio_18_16 <- o18_deadtime_correct/o16_back_correct
  mean <- sum(ratio_18_16)/(as.numeric(length(ratio_18_16)))
  dev <- (ratio_18_16-mean)^2
  dev <- sum(dev)/(as.numeric(length(ratio_18_16))-1)
  dev <- dev^0.5
  dev <- dev/((as.numeric(length(ratio_18_16)))^0.5)
  output <- dev
  return(output)
}

#Process Ratio Data----
mean_17 <- sapply(FUN=Mean17,X=sheet_pairs)
std_17 <- sapply(FUN=STD17,X=sheet_pairs)
mean_18 <- sapply(FUN=Mean18,X=sheet_pairs)
std_18 <- sapply(FUN=STD18,X=sheet_pairs)

#Export Data----
site <- sheet_pairs
data <- data.frame(site,mean_17,std_17,mean_18,std_18)
write.xlsx(x=data,file="/home/secana/Desktop/O Isotopes.xlsx",row.names=FALSE)

#Time Stamp----
end <- Sys.time()
runtime <- (end-start)
print (runtime)
remove(start,end,runtime)