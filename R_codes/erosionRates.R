# Erosion rates from gauge data for SNSM Geomorphology Paper
# Authors: Nicolas Perez

# Code for processing water discharge data and suspended sediment loads from the SNSM

# Variables used
# table - dataframe created by reading the datafile containing date and pairs of values of water discharge vand sediment load for that date
# table2 - dataframe created by reading the datafile containing monthly water discharge values, the regression is used to estimate the sediment load values for each of the water discharge values in this dataframe o
# 


# all variables start at a value of zero
rm(list=ls())

# Libraries used in this script

library("WriteXLS")
library("gtools")
library("plyr")
library('stringi')
library('pracma')

# Select directory where data files are stored
directory = "/Users/penico93/Dropbox/Caudales_SNSM/Data"

# Set directory
setwd(directory)

# Read data of sediment yield and water discharge
table <- read.csv("cantaclaro.csv", header = TRUE)

# Nombre de la estacion
station_name = "cantaclaro"

# Area in m2 of watershed
AREA= 170369058.863

# Extract month number from column "FECHA" in dataframe
for(i in 1:length(table$FECHA1)){
  
  table$mes[i] = stri_sub(table$FECHA1[i],5,6)
  
}

# Plot Log(discharge) vs Log(sediment load) to observe relationship
plot(log(table$CAUDAL_LIQUIDO.m3.s.),log(table$GASTO_SOLIDO.Kg.s.))

# Plot Time series of Time vs Log(sediment load) to observe variations in Sediment load through time
plot(table$FECHA1,(table$GASTO_SOLIDO.Kg.s.))

# Read monthly water discharge values into new datafram "table2". Data is organized in the following way:
# Years are stored as rows and monthly data for each year in each column
table2 <- read.csv("caudalmedio_cantaclaro.csv", header = TRUE)

# Create new dataframe with 3 initial columns, each row will contain a date and values of discharge and regressed sediment yield.
new_table <- data.frame(rep(0, 12*length(table2$ENERO)), rep(0, 12*length(table2$ENERO)), rep(0, 12*length(table2$ENERO)))

# Assign column names to the new dataframes
colnames(new_table) <- c("date","discharge","new_sedyield")

# Start counting as 0 
contador <- 0

# Loop to fill "new_table" dataframe with the corresponding discharge value for each date from "table2" dataframe
for(j in 1:12){
  
  if(contador <=600){
    
    for(i in 1:length(table2[,1])){
    
      contador <- contador + 1
   
      
      new_table$date[contador] <- paste(c(table2[i,1],"-",j,"-01"),collapse = '')
      
      
      new_table$date[contador] <- as.character(as.Date(new_table$date[contador], format = "%Y-%m-%d"))

      
      new_table$discharge[contador] <- table2[i,j+1] 
    }
    
    
  }
  
}


# Create a linear model for Log(discharge) vs Log(sediment load) 
linearmodel = lm(log(table$GASTO_SOLIDO.Kg.s.)~log(table$CAUDAL_LIQUIDO.m3.s.))

#Find the R-square coefficient
summary(linearmodel)$r.squared

#Find the m and b coefficients of the regression model: y = mx+b 
coeff.mymodel = coefficients(linearmodel)

#Use model to predict for missing values in dataframe new_table
new_table$new_sedyield <- coeff.mymodel[1] + coeff.mymodel[2]*log(new_table$discharge)

#We apply to exponential function because the value was in the log function
new_table$new_sedyield <- exp(new_table$new_sedyield)



#Multiply the sediment load in [kg/s] by 24*60*60[s/day] to conver to [kg/day]
#table$GASTO_SOLIDO.Kg.day. <- (table$GASTO_SOLIDO.Kg.s.)*(24*60*60)
new_table$new_sedyield.Kg.day <- new_table$new_sedyield*(24*60*60)

# calculate the number of days from date i to date i+1
new_table$numDays <- 0

new_table$numDays[length(new_table[,1])] <- 1

#Sort new_table dataframe by date

new_table <- new_table[order(as.Date(new_table$date, format = "%Y-%m-%d")),]

for(i in 1:(length(new_table[,1])-1)){
  
  uno = as.Date(toString(new_table$date[i]), format = "%Y-%m-%d")
  
  dt2 = as.Date(toString(new_table$date[i+1]), format = "%Y-%m-%d")

  new_table$numDays[i] <- difftime(dt2, uno, units = "days")
  
}

#Multiply the sediment load in [kg/day] by the number of days until next data available (from date i to date i+1).
# This wil result in a total amount of kgs.
new_table$new_sedyield.Kg.total <- new_table$new_sedyield.Kg.day*new_table$numDays

#save data in a table
for(i in 1:length(table[,1])){
  
  table$fecha[i] <- toString(table$FECHA1[i])
  table$date[i] <- as.character( as.Date(table$fecha[i], format = "%Y%m%d")   )

  
}

write.csv(table,'Cantaclaro_Data_Ratig_Curve.csv', row.names = F)

write.csv(new_table,'Cantaclaro_Data_After_Regression.csv', row.names = F)

valorint = sum(new_table$new_sedyield.Kg.total)

#max(x1) represental el valor en segundos de tiempo tota
#Se pasa a anos dividiendo por 60*60*24*365
uno = as.Date(toString(new_table$date[1]), format = "%Y-%m-%d")

dt2 = as.Date( toString(new_table$date[length(new_table[,1])]), format = "%Y-%m-%d" )


num.days <- difftime(dt2, uno, units = "days")[[1]]

t_anos = num.days/(365)


#Valor de kilogramos por ano

#7320 toneladas

valorint/t_anos


#AREA en m3 ??= 170369058.863
density = 2500

#Denudation rate [m/My]

denudation_rate<- (valorint/t_anos)*(1/density)*(1/AREA)*(10^6)

(valorint*10/t_anos)*(1/density)*(10/AREA)*(10^6)

#check method
eroded_volume <- 27574496*90

eroded_volume/(denudation_rate*AREA)









