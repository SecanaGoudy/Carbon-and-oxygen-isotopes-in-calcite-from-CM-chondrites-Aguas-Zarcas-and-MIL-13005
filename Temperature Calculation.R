#Clear Workspace----
remove(list=ls(all=TRUE)) #Clear workspace

#Libraries Loading----
library(openxlsx) #Load package necessary for importing/exporting from/to .xlsx files
library(ggplot2) #Load package necessary for creating plots

#MIL 13005 Loading----
df <- read.xlsx(xlsxFile="/home/secana/Documents/School Stuff/Dissertation/Project 2/Paired Calcites and Magnetites.xlsx",sheet=2) #Load calite and magnetite d18O data from excel file
df <- na.omit(df)
df$Calcite_d18O <- ((df$O_Mean_Calc/1000)+1)*0.0020052 #Begin converting data from delta permille notation to unitless fraction format
df$Calcite_Error <- ((df$Agg_Calc_Error/1000))*0.0020052
df$Magnetite_d18O <- ((df$O_Mean_Mag/1000)+1)*0.0020052
df$Magnetite_Error <- ((df$Agg_Mag_Error/1000))*0.0020052 #Finishconverting data from delta permille notation to unitless fraction format

#Function----
temp <- seq(from=(-75+273.15),to=(300+273.15),by=0.01) #Define temperature range of temperature model
alpha <- exp((((1.13*(10^6))/(temp^2))+((6.70*(10^3))/(temp))+(-2.81))/(10^3)) #Calculate alpha parameter of the temperature model, matched to temperature values

#Sample Data----
ratio <- df$Calcite_d18O/df$Magnetite_d18O #Calculate alpha parameter of calcite-magnetite pairs
error <- ratio*((((df$Calcite_Error/df$Calcite_d18O)^2)+((df$Magnetite_Error/df$Magnetite_d18O)^2))^0.5) #Calculate 1 sigma error in the alpha parameters of calcite-magnetite pairs
low <- ratio-error #Calcute 1 sigma lower error bar
high <- ratio+error #Calcute 1 sigma upper error bar

#Closest ID----
TempCalc <- function(input){ #Define function to find the temperature value of the temperature model whose corresponding alpha value is closest to a specific alpha parameter of a calcite-magnetite pair
pointa <- low[input]
pointb <- ratio[input]
pointc <- high[input]
dista <- (pointa-alpha)^2
distb <- (pointb-alpha)^2
distc <- (pointc-alpha)^2
tempa <- round(x=(temp[dista==min(dista)]-273.15),digits=2) 
tempb <- round(x=(temp[distb==min(distb)]-273.15),digits=2)
tempc <- round(x=(temp[distc==min(distc)]-273.15),digits=2) 
output <- tempc#Edit here to output desired temperature; "tempa" is mean + 1 sigma, "tempb" is mean, "tempc" is mean - 1 sigma.
return(output)} #Finish defining of function
string <- seq(by=1,from=1,to=as.numeric(length(ratio))) #Set up for application of the function
temps <- sapply(X=string,FUN=TempCalc) #Application of the function to calculate temperatures from calcite-magnetite alpha parameters
print(temps) #Display the calculated temperatures

#Graphs----
tempsdf <- data.frame(temps) #Begin code for a histogram plot of calculated temperatures
plot <- ggplot()+theme_classic()+xlab("Celsius")+ylab("Counts")
plot <- plot+geom_histogram(data=tempsdf,mapping=aes(x=temps),binwidth=10,color="black",fill="gray") #End code for a histogram plot of calculated temperatures
print(plot) #Display histogram plot

