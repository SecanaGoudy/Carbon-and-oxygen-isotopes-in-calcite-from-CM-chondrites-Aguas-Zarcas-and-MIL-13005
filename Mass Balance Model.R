#Clear Workspace----
remove(list=ls(all=TRUE)) #Clear workspace
start <- Sys.time() #Record start time of the script

#Packages Loading----
library(ggplot2) #Load package needed for plot generation

#Givens----
shrink <- 5.192674#10.014/1.485 #Set resolution of the model
water_rock <- seq(from=0.2,to=0.7,by=0.01/shrink) #Set range of possible ice/silicate ratios
silicate <- seq(from=-7.0,to=-2.0,by=0.1/shrink) #Set range of anhydrous silicate D17O compositions
old_silicate <- silicate #Begin setting up every possible pairing of ice/silicate ratios and anhydrous silicate O compositions
silicate <- rep(x=silicate,each=as.numeric(length(water_rock)))
water_rock <- rep(x=water_rock,times=as.numeric(length(old_silicate))) #Finish setting up every possible pairing of ice/silicate ratios and anhydrous silicate O compositions
az <- c(-3.63,-2.91,-2.07,-0.38,-0.73,-2.98,-2.20,-1.46,-2.57,-1.10,-4.22,-1.18,-1.00,-2.23,-0.87,-1.39,-2.96,0.11,-4.62,-5.32,-2.35,-2.56,-1.42,-1.58,-0.64,-1.97,-1.10,-2.70,-2.21,-1.75) #Input Aguas Zarcas calcite D17O isotope data
mil <- c(1.75,2.28,1.84,1.93,-0.26,-0.01,0.79,-1.28,-1.48,-1.96,-3.79,-2.92,-2.33,-2.22,-3.27,-2.86,-2.69) #Input MIL 13005 calcite D17O isotope data

#Grid Set Up----
xnet <- 0.01/shrink #Begin defining the grid of squares in which functions will be counted
ynet <- 0.5/shrink
gridx <- seq(from=0.195,to=(0.705-xnet),by=xnet)
gridy <- seq(from=-11,to=(36-ynet),by=ynet)
gridyID <- rep(x=seq(from=1,to=as.numeric(length(gridy)),by=1),times=as.numeric(length(gridx)))
gridxID <- rep(x=seq(from=1,to=as.numeric(length(gridx)),by=1),each=as.numeric(length(gridy)))
string <- seq(from=1,to=as.numeric(length(gridxID)),by=1) #Finish defining the grid of squares in which functions will be counted

#AZ Calculation and Graph----
input <- 1 #Start setting up model calculation for calculating the function for the first Aguas Zarcas calcite
az_ice_D17 <- (az[input]-silicate+(water_rock*silicate))/water_rock #Model function calculation for first Aguas Zarcas calcite
DetectorAZ <- function(feed){ #Create function to detect whether a model function of an Aguas Zarcas calcite grain overlaps with a counting square or not
  funY <- az_ice_D17[water_rock>=gridx[gridxID[feed]]]
  funX <- water_rock[water_rock>=gridx[gridxID[feed]]]
  funY <- funY[funX<(gridx[gridxID[feed]]+xnet)]
  funY <- funY[funY>=gridy[gridyID[feed]]]
  funY <- funY[funY<gridy[gridyID[feed]]+ynet]
  excrement <- min(as.numeric(length(funY)),1)
  return(excrement)
}
countAZ <- sapply(FUN=DetectorAZ,X=string) #Mark counting squares for which the model overlaps with the counting square
input <- input+1 #Move on to the next Aguas Zarcas calcite grain
az_ice_D17 <- (az[input]-silicate+(water_rock*silicate))/water_rock #Model function calculation for the second calcite
countAZ <- countAZ+sapply(FUN=DetectorAZ,X=string) #Mark counting square overlaps for the second calcite, and add those counts to the existing tallies
input <- input+1 #Move on to the third calcite...
az_ice_D17 <- (az[input]-silicate+(water_rock*silicate))/water_rock
countAZ <- countAZ+sapply(FUN=DetectorAZ,X=string)
input <- input+1
az_ice_D17 <- (az[input]-silicate+(water_rock*silicate))/water_rock
countAZ <- countAZ+sapply(FUN=DetectorAZ,X=string)
input <- input+1
az_ice_D17 <- (az[input]-silicate+(water_rock*silicate))/water_rock
countAZ <- countAZ+sapply(FUN=DetectorAZ,X=string)
input <- input+1
az_ice_D17 <- (az[input]-silicate+(water_rock*silicate))/water_rock
countAZ <- countAZ+sapply(FUN=DetectorAZ,X=string)
input <- input+1
az_ice_D17 <- (az[input]-silicate+(water_rock*silicate))/water_rock
countAZ <- countAZ+sapply(FUN=DetectorAZ,X=string)
input <- input+1
az_ice_D17 <- (az[input]-silicate+(water_rock*silicate))/water_rock
countAZ <- countAZ+sapply(FUN=DetectorAZ,X=string)
input <- input+1
az_ice_D17 <- (az[input]-silicate+(water_rock*silicate))/water_rock
countAZ <- countAZ+sapply(FUN=DetectorAZ,X=string)
input <- input+1
az_ice_D17 <- (az[input]-silicate+(water_rock*silicate))/water_rock
countAZ <- countAZ+sapply(FUN=DetectorAZ,X=string)
input <- input+1
az_ice_D17 <- (az[input]-silicate+(water_rock*silicate))/water_rock
countAZ <- countAZ+sapply(FUN=DetectorAZ,X=string)
input <- input+1
az_ice_D17 <- (az[input]-silicate+(water_rock*silicate))/water_rock
countAZ <- countAZ+sapply(FUN=DetectorAZ,X=string)
input <- input+1
az_ice_D17 <- (az[input]-silicate+(water_rock*silicate))/water_rock
countAZ <- countAZ+sapply(FUN=DetectorAZ,X=string)
input <- input+1
az_ice_D17 <- (az[input]-silicate+(water_rock*silicate))/water_rock
countAZ <- countAZ+sapply(FUN=DetectorAZ,X=string)
input <- input+1
az_ice_D17 <- (az[input]-silicate+(water_rock*silicate))/water_rock
countAZ <- countAZ+sapply(FUN=DetectorAZ,X=string)
input <- input+1
az_ice_D17 <- (az[input]-silicate+(water_rock*silicate))/water_rock
countAZ <- countAZ+sapply(FUN=DetectorAZ,X=string)
input <- input+1
az_ice_D17 <- (az[input]-silicate+(water_rock*silicate))/water_rock
countAZ <- countAZ+sapply(FUN=DetectorAZ,X=string)
input <- input+1
az_ice_D17 <- (az[input]-silicate+(water_rock*silicate))/water_rock
countAZ <- countAZ+sapply(FUN=DetectorAZ,X=string)
input <- input+1
az_ice_D17 <- (az[input]-silicate+(water_rock*silicate))/water_rock
countAZ <- countAZ+sapply(FUN=DetectorAZ,X=string)
input <- input+1
az_ice_D17 <- (az[input]-silicate+(water_rock*silicate))/water_rock
countAZ <- countAZ+sapply(FUN=DetectorAZ,X=string)
input <- input+1
az_ice_D17 <- (az[input]-silicate+(water_rock*silicate))/water_rock
countAZ <- countAZ+sapply(FUN=DetectorAZ,X=string)
input <- input+1
az_ice_D17 <- (az[input]-silicate+(water_rock*silicate))/water_rock
countAZ <- countAZ+sapply(FUN=DetectorAZ,X=string)
input <- input+1
az_ice_D17 <- (az[input]-silicate+(water_rock*silicate))/water_rock
countAZ <- countAZ+sapply(FUN=DetectorAZ,X=string)
input <- input+1
az_ice_D17 <- (az[input]-silicate+(water_rock*silicate))/water_rock
countAZ <- countAZ+sapply(FUN=DetectorAZ,X=string)
input <- input+1
az_ice_D17 <- (az[input]-silicate+(water_rock*silicate))/water_rock
countAZ <- countAZ+sapply(FUN=DetectorAZ,X=string)
input <- input+1
az_ice_D17 <- (az[input]-silicate+(water_rock*silicate))/water_rock
countAZ <- countAZ+sapply(FUN=DetectorAZ,X=string)
input <- input+1
az_ice_D17 <- (az[input]-silicate+(water_rock*silicate))/water_rock
countAZ <- countAZ+sapply(FUN=DetectorAZ,X=string)
input <- input+1
az_ice_D17 <- (az[input]-silicate+(water_rock*silicate))/water_rock
countAZ <- countAZ+sapply(FUN=DetectorAZ,X=string)
input <- input+1
az_ice_D17 <- (az[input]-silicate+(water_rock*silicate))/water_rock
countAZ <- countAZ+sapply(FUN=DetectorAZ,X=string)
input <- input+1 #Move onto the final Aguas Zarcas calcite
az_ice_D17 <- (az[input]-silicate+(water_rock*silicate))/water_rock #Calculate model function for the final Aguas Zarcas calcite
countAZ <- countAZ+sapply(FUN=DetectorAZ,X=string) #Count square overlaps for the final Aguas Zarcas calcite

#MIL Calculation and Graph----
input <- 1 #Begin doing model calculations and square counting for MIL 13005 calcites in the same way as was done for Aguas Zarcas calcites
mil_ice_D17 <- (mil[input]-silicate+(water_rock*silicate))/water_rock
DetectorMIL <- function(feed){
  funY <- mil_ice_D17[water_rock>=gridx[gridxID[feed]]]
  funX <- water_rock[water_rock>=gridx[gridxID[feed]]]
  funY <- funY[funX<(gridx[gridxID[feed]]+xnet)]
  funY <- funY[funY>=gridy[gridyID[feed]]]
  funY <- funY[funY<gridy[gridyID[feed]]+ynet]
  excrement <- min(as.numeric(length(funY)),1)
  return(excrement)
}
countMIL <- sapply(FUN=DetectorMIL,X=string)
input <- input+1
mil_ice_D17 <- (mil[input]-silicate+(water_rock*silicate))/water_rock
countMIL <- countMIL+sapply(FUN=DetectorMIL,X=string)
input <- input+1
mil_ice_D17 <- (mil[input]-silicate+(water_rock*silicate))/water_rock
countMIL <- countMIL+sapply(FUN=DetectorMIL,X=string)
input <- input+1
mil_ice_D17 <- (mil[input]-silicate+(water_rock*silicate))/water_rock
countMIL <- countMIL+sapply(FUN=DetectorMIL,X=string)
input <- input+1
mil_ice_D17 <- (mil[input]-silicate+(water_rock*silicate))/water_rock
countMIL <- countMIL+sapply(FUN=DetectorMIL,X=string)
input <- input+1
mil_ice_D17 <- (mil[input]-silicate+(water_rock*silicate))/water_rock
countMIL <- countMIL+sapply(FUN=DetectorMIL,X=string)
input <- input+1
mil_ice_D17 <- (mil[input]-silicate+(water_rock*silicate))/water_rock
countMIL <- countMIL+sapply(FUN=DetectorMIL,X=string)
input <- input+1
mil_ice_D17 <- (mil[input]-silicate+(water_rock*silicate))/water_rock
countMIL <- countMIL+sapply(FUN=DetectorMIL,X=string)
input <- input+1
mil_ice_D17 <- (mil[input]-silicate+(water_rock*silicate))/water_rock
countMIL <- countMIL+sapply(FUN=DetectorMIL,X=string)
input <- input+1
mil_ice_D17 <- (mil[input]-silicate+(water_rock*silicate))/water_rock
countMIL <- countMIL+sapply(FUN=DetectorMIL,X=string)
input <- input+1
mil_ice_D17 <- (mil[input]-silicate+(water_rock*silicate))/water_rock
countMIL <- countMIL+sapply(FUN=DetectorMIL,X=string)
input <- input+1
mil_ice_D17 <- (mil[input]-silicate+(water_rock*silicate))/water_rock
countMIL <- countMIL+sapply(FUN=DetectorMIL,X=string)
input <- input+1
mil_ice_D17 <- (mil[input]-silicate+(water_rock*silicate))/water_rock
countMIL <- countMIL+sapply(FUN=DetectorMIL,X=string)
input <- input+1
mil_ice_D17 <- (mil[input]-silicate+(water_rock*silicate))/water_rock
countMIL <- countMIL+sapply(FUN=DetectorMIL,X=string)
input <- input+1
mil_ice_D17 <- (mil[input]-silicate+(water_rock*silicate))/water_rock
countMIL <- countMIL+sapply(FUN=DetectorMIL,X=string)
input <- input+1
mil_ice_D17 <- (mil[input]-silicate+(water_rock*silicate))/water_rock
countMIL <- countMIL+sapply(FUN=DetectorMIL,X=string)
input <- input+1
mil_ice_D17 <- (mil[input]-silicate+(water_rock*silicate))/water_rock
countMIL <- countMIL+sapply(FUN=DetectorMIL,X=string) #Finish doing model calculations and square counting for MIL 13005 calcites in the same way as was done for Aguas Zarcas calcites

#Graph----
gridx <- gridx[gridxID[string]]+0.01 #Begin creating plot for Aguas Zarcas function counts
gridy <- gridy[gridyID[string]]+1
dfAZ <- data.frame(gridx,gridy,countAZ)
plotAZ <- ggplot()+theme_classic()+xlab("Ice/Silicate Ratio")+ylab(bquote(Δ^17~"O SMOW of Ice"))+ggtitle("Aguas Zarcas")+theme(plot.title=element_text(hjust=0.5))
plotAZ <- plotAZ+geom_tile(data=dfAZ,mapping=aes(x=gridx,y=gridy,fill=countAZ))+labs(fill="Count")+scale_fill_gradient(low="white",high="black") #Finish creating plot for Aguas Zarcas function counts
print(plotAZ) #Display plot for Aguas Zarcas function counts
dfMIL <- data.frame(gridx,gridy,countMIL) #Begin creating plot for MIL 13005 function counts
plotMIL <- ggplot()+theme_classic()+xlab("Ice/Silicate Ratio")+ylab(bquote(Δ^17~"O SMOW of Ice"))+ggtitle("MIL 13005")+theme(plot.title=element_text(hjust=0.5))
plotMIL <- plotMIL+geom_tile(data=dfMIL,mapping=aes(x=gridx,y=gridy,fill=countMIL))+labs(fill="Count")+scale_fill_gradient(low="white",high="black") #Finish creating plot for MIL 13005 function counts
print(plotMIL) #Display plot for MIL 13005 function counts

#Time Stamp----
end <- Sys.time() #Record approximate end time of script's running
runtime <- (end-start) #Calculate run-time of the script
print (runtime) #Display run-time of the script
remove(start,end,runtime) #Clean away script run-time related vectors
