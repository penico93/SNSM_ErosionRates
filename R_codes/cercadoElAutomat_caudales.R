# Codigo para procesar datos de caudales y carga de sedimento en suspension de la SNSM

AREA = 349230344.288

library("WriteXLS")
library("gtools")
library("plyr")
library("qpcR")


#funciones unicas para este codigo
library(stringi)
library(xlsx)

library(pracma)

directory = "C:/Users/penico93/Dropbox/Caudales_SNSM"

#C:\Users\penico93\Dropbox\Paper Provenance Ware Fm\ArcGIS-maps\porcentajes_geol

setwd(directory)
table <- read.xlsx("cercadoelautomat.xlsx",1, header = TRUE)

#Second table for later use

table2 <- read.xlsx("caudalmedio_cercadoelautomat.xlsx",1, header = TRUE)

#Nombre de la estacion

station_name = "Cercado_el_Automat"

# Crear una nueva columna llamada mes en la que se guarde el numero de mes en el que se tomo el dato

for(i in 1:length(table$FECHA1)){
  
  table$mes[i] = stri_sub(table$FECHA1[i],5,6)
  
  
}

# Se crean 3 categorias

#Categoria 1: de Enero a Abril 1-4
#Categoria 2: de Mayo a Septiembre 5-9
#Categoria 3: de Octubre a Diciembre 10-12

# Se crean 3 contadores para saber cuantos datos hay en cada una de las categorias

cat1 = 0
cat2 = 0
cat3 = 0
for(i in 1:length(table$mes)){
  
  if(table$mes[i]=="01" |table$mes[i]=="02"| table$mes[i]=="03" |table$mes[i]=="04"){
    cat1 = cat1 + 1
  }
  if( table$mes[i]=="05" |table$mes[i]=="06"| table$mes[i]=="07" |table$mes[i]=="08"|table$mes[i]=="09" ){
    cat2= cat2 + 1
  }
  else{
    cat3 = cat3 +1
  }
  
}


# Se crean 3 DF's del tamano de cat1, cat2 y cat3 para incluir en cada uno de ellos los valores de caudal y de transporte de sedimentos de cada una de las categorias
#gasto solido
#Los df's van a tener 2 columns, en la primera se guarda el valor de caudal y en la segunda el valor de sed en susp
vcat1  <- data.frame(matrix(ncol = 2, nrow = cat1))
vcat2  <- data.frame(matrix(ncol = 2, nrow = cat2))
vcat3  <- data.frame(matrix(ncol = 2, nrow = cat3))
cont1= 0
cont2 =0
cont3 =0
for(i in 1:length(table$mes)){
  
  if(table$mes[i]=="01" |table$mes[i]=="02"| table$mes[i]=="03" |table$mes[i]=="04"){
    
    vcat1[cont1,1] = table$CAUDAL_LIQUIDO.m3.s.[i]
      
    vcat1[cont1,2] = table$GASTO_SOLIDO.Kg.s.[i]
      
      
    cont1 = cont1 +1
  }
  
  if( table$mes[i]=="05" |table$mes[i]=="06"| table$mes[i]=="07" |table$mes[i]=="08"|table$mes[i]=="09" ){
    vcat2[cont2,1] = table$CAUDAL_LIQUIDO.m3.s.[i]
    vcat2[cont2,2] = table$GASTO_SOLIDO.Kg.s.[i]
    cont2= cont2 + 1
    
    
  }
  
  else{
    vcat3[cont3,1] = table$CAUDAL_LIQUIDO.m3.s.[i]
    vcat3[cont3,2] = table$GASTO_SOLIDO.Kg.s.[i]
    cont3= cont3 + 1
    
    
  }
    
}



# Se plotean los valores de cada una de las categorias y se obtiene una curva de ajuste exponencial

plot(log(vcat3))


plot(log(vcat2))


plot(log(vcat1))


#comienza el codigo 

plot(log(table$CAUDAL_LIQUIDO.m3.s.),log(table$GASTO_SOLIDO.Kg.s.))

plot(table$FECHA1,(table$GASTO_SOLIDO.Kg.s.))


linearmodel = lm(log(table$GASTO_SOLIDO.Kg.s.)~log(table$CAUDAL_LIQUIDO.m3.s.))

#linearmodel2 = lm(log(table$GASTO_SOLIDO.Kg.s.)~log(table$CAUDAL_LIQUIDO.m3.s.),table)


#coeficiente de determinacion R cuadrado
summary(linearmodel)$r.squared


#Coeficientes  y = mx+b del modelo
coefficients = coefficients(linearmodel)

modelo = coefficients[1] + coefficients[2]*log(table$CAUDAL_LIQUIDO.m3.s.)




#Grafica de Rating Curve
graphname = paste(c(station_name,'_Rating-Curve2',".pdf"), collapse = '')

pdf(graphname)

graphtext = paste(c(station_name,'\nRegression\n','R2 = ',toString(summary(linearmodel)$r.squared)), collapse = '')

plot(log(table$GASTO_SOLIDO.Kg.s.),log(table$CAUDAL_LIQUIDO.m3.s.), main="Rating Curve:\n Log(Water Discharge) vs. Log(Sediment Load)",
     xlab="Log(Sediment Load)", ylab="Log(Water Discharge)") 
par(cex=.8)
abline(lm(log(table$CAUDAL_LIQUIDO.m3.s.)~ log(table$GASTO_SOLIDO.Kg.s.)))
text(x =0,y =1.5,graphtext)


dev.off()



#Aqui convierto la fecha del archivo con datos de gasto solido y caudal medio para segundos

for(i in 1:length(table$FECHA1)){
  uno = as.Date(toString(table$FECHA1[1]), format = "%Y%m%d")
  
  dt2 = as.Date(toString(table$FECHA1[i]), format = "%Y%m%d")
  table$Segundos[i] = difftime(dt2, uno, units = "secs")
  
}
#Se calcula la diferencia de tiempo en segundos entre cada fecha y la fecha inicial








#Plot de Gasto solido vs. Tiempo y Caudal vs. tiempo para los datos iniciales a partir de los cuales se crean la grafica o curva de rating
graphname = paste(c(station_name,'suspended-load-time',".pdf"), collapse = '')

pdf(graphname)


plot(table$Segundos,table$GASTO_SOLIDO.Kg.s., main="Suspended Load (Kg/s) vs. Time (s)", 
     xlab="Time (s)", ylab="Suspended Load (Kg/s)") 


dev.off()

#////////////////////////////

#Ahora se grafica Water discharge vs. time
graphname = paste(c(station_name,'Water-discharge-time',".pdf"), collapse = '')

pdf(graphname)


plot(table$Segundos,table$CAUDAL_LIQUIDO.m3.s., main="Water Discharge (m3/s) vs. Time (s)", 
     xlab="Time (s)", ylab="Suspended Load (m3/s)") 


dev.off()








Ene = table2[,c('ENERO','AÃ.o')]
#se borran los datos NA o secos
Ene[Ene$ENERO != 'seco',]
Ene[Ene$ENERO != '*',]

Ene$Caud[1] = 1
Feb$Caud[1] = 1
Mar$Caud[1] = 1
Abr$Caud[1] = 1
May$Caud[1] = 1
Jun$Caud[1] = 1
Jul$Caud[1] = 1
Aug$Caud[1] = 1
Sep$Caud[1] = 1
Oct$Caud[1] = 1
Nov$Caud[1] = 1
Dic$Caud[1] = 1

#Se utiliza la regresion lineal y se generan datos de carga - RECORDAR
#que los datos estan con la funcion logaritmo y se debe aplciar o reversar esta
#Funcion para obtener los datos reales....

Ene$GastoSolido[1]=1
Ene$Fecha[1] = 1


for(i in 1:length(Ene$ENERO)){
  
  a = as.numeric(Ene$ENERO[i])
  Ene$Caud[i] = as.numeric(Ene$ENERO[i])
  
  
  Ene$GastoSolido[i] = coefficients[1] + coefficients[2]*log(a)
  #Para eliminar el logaritmo
  Ene$GastoSolido[i] = exp(Ene$GastoSolido[i])
  
  b = Ene$AÃ.o[i]
  
  
  # Se elige la fecha 1 de cada mes para asignar el valor calculado
  
  
  paste = paste(c(b,"0101"), collapse = '')
  Ene$Fecha[i] = paste
  
}



Feb = table2[,c('FEBRE','AÃ.o')]
#se borran los datos NA o secos
Feb[Feb$FEBRE != 'seco',]
Feb[Feb$FEBRE != '*',]

#Se utiliza la regresion lineal y se generan datos de carga - RECORDAR
#que los datos estan con la funcion logaritmo y se debe aplciar o reversar esta
#Funcion para obtener los datos reales....

Feb$GastoSolido[1]=1
Feb$Fecha[1] = 1
for(i in 1:length(Feb$FEBRE)){
  
  a = as.numeric(Feb$FEBRE[i])
  Feb$Caud[i] = as.numeric(Feb$FEBRE[i])
  
  
  Feb$GastoSolido[i] = coefficients[1] + coefficients[2]*log(a)
  #Para eliminar el logaritmo
  Feb$GastoSolido[i] = exp(Feb$GastoSolido[i])
  
  b = Feb$AÃ.o[i]
  
  
  paste = paste(c(b,"0201"), collapse = '')
  
  
  Feb$Fecha[i] = paste
  
}


Mar = table2[,c('MARZO','AÃ.o')]
#se borran los datos NA o secos
Mar[Mar$MARZO != 'seco',]
Mar[Mar$MARZO != '*',]

#Se utiliza la regresion lineal y se generan datos de carga - RECORDAR
#que los datos estan con la funcion logaritmo y se debe aplciar o reversar esta
#Funcion para obtener los datos reales....

Mar$GastoSolido[1]=1
Mar$Fecha[1] = 1
for(i in 1:length(Mar$MARZO)){
  
  a = as.numeric(Mar$MARZO[i])
  Mar$Caud[i] = as.numeric(Mar$MARZO[i])
  
  
  Mar$GastoSolido[i] = coefficients[1] + coefficients[2]*log(a)
  #Para eliminar el logaritmo
  Mar$GastoSolido[i] = exp(Mar$GastoSolido[i])
  
  b = Mar$AÃ.o[i]
  
  
  paste = paste(c(b,"0301"), collapse = '')
  
  
  Mar$Fecha[i] = paste
  
}


Abr = table2[,c('ABRIL','AÃ.o')]
#se borran los datos NA o secos
Abr[Abr$ABRIL != 'seco',]
Abr[Abr$ABRIL != '*',]

#Se utiliza la regresion lineal y se generan datos de carga - RECORDAR
#que los datos estan con la funcion logaritmo y se debe aplciar o reversar esta
#Funcion para obtener los datos reales....

Abr$GastoSolido[1]=1
Abr$Fecha[1] = 1
for(i in 1:length(Abr$ABRIL)){
  
  a = as.numeric(Abr$ABRIL[i])
  
  Abr$Caud[i] = as.numeric(Abr$ABRIL[i])
  
  Abr$GastoSolido[i] = coefficients[1] + coefficients[2]*log(a)
  #Para eliminar el logaritmo
  Abr$GastoSolido[i] = exp(Abr$GastoSolido[i])
  
  b = Abr$AÃ.o[i]
  
  
  paste = paste(c(b,"0401"), collapse = '')
  
  
  Abr$Fecha[i] = paste
  
}


May = table2[,c('MAYO','AÃ.o')]
#se borran los datos NA o secos
May[May$MAYO != 'seco',]
May[May$MAYO != '*',]

#Se utiliza la regresion lineal y se generan datos de carga - RECORDAR
#que los datos estan con la funcion logaritmo y se debe aplciar o reversar esta
#Funcion para obtener los datos reales....

May$GastoSolido[1]=1
May$Fecha[1] = 1
for(i in 1:length(May$MAYO)){
  
  a = as.numeric(May$MAYO[i])
  
  
  May$Caud[i] =  as.numeric(May$MAYO[i])
  
  May$GastoSolido[i] = coefficients[1] + coefficients[2]*log(a)
  #Para eliminar el logaritmo
  May$GastoSolido[i] = exp(May$GastoSolido[i])
  
  b = May$AÃ.o[i]
  
  
  paste = paste(c(b,"0501"), collapse = '')
  
  
  May$Fecha[i] = paste
  
}


Jun = table2[,c('JUNIO','AÃ.o')]
#se borran los datos NA o secos
Jun[Jun$JUNIO != 'seco',]
Jun[Jun$JUNIO != '*',]

#Se utiliza la regresion lineal y se generan datos de carga - RECORDAR
#que los datos estan con la funcion logaritmo y se debe aplciar o reversar esta
#Funcion para obtener los datos reales....

Jun$GastoSolido[1]=1
Jun$Fecha[1] = 1
for(i in 1:length(Jun$JUNIO)){
  
  a = as.numeric(Jun$JUNIO[i])
  Jun$Caud[i] =  as.numeric(Jun$JUNIO[i])
  
  
  
  Jun$GastoSolido[i] = coefficients[1] + coefficients[2]*log(a)
  #Para eliminar el logaritmo
  Jun$GastoSolido[i] = exp(Jun$GastoSolido[i])
  
  b = Jun$AÃ.o[i]
  
  
  paste = paste(c(b,"0601"), collapse = '')
  
  
  Jun$Fecha[i] = paste
  
}


Jul = table2[,c('JULIO','AÃ.o')]
#se borran los datos NA o secos
Jul[Jul$JULIO != 'seco',]
Jul[Jul$JULIO != '*',]

#Se utiliza la regresion lineal y se generan datos de carga - RECORDAR
#que los datos estan con la funcion logaritmo y se debe aplciar o reversar esta
#Funcion para obtener los datos reales....

Jul$GastoSolido[1]=1
Jul$Fecha[1] = 1
for(i in 1:length(Jul$JULIO)){
  
  a = as.numeric(Jul$JULIO[i])
  
  Jul$Caud[i] = as.numeric(Jul$JULIO[i])
  
  Jul$GastoSolido[i] = coefficients[1] + coefficients[2]*log(a)
  #Para eliminar el logaritmo
  Jul$GastoSolido[i] = exp(Jul$GastoSolido[i])
  
  b = Jul$AÃ.o[i]
  
  
  paste = paste(c(b,"0701"), collapse = '')
  
  
  Jul$Fecha[i] = paste
  
}

Ago = table2[,c('AGOST','AÃ.o')]
#se borran los datos NA o secos
Ago[Ago$AGOST != 'seco',]

Ago[Ago$AGOST != '*',]

#Se utiliza la regresion lineal y se generan datos de carga - RECORDAR
#que los datos estan con la funcion logaritmo y se debe aplciar o reversar esta
#Funcion para obtener los datos reales....

Ago$GastoSolido[1]=1
Ago$Fecha[1] = 1
for(i in 1:length(Ago$AGOST)){
  
  a = as.numeric(Ago$AGOST[i])
  Ago$Caud[i] =  as.numeric(Ago$AGOST[i])
  
  
  Ago$GastoSolido[i] = coefficients[1] + coefficients[2]*log(a)
  #Para eliminar el logaritmo
  Ago$GastoSolido[i] = exp(Ago$GastoSolido[i])
  
  b = Ago$AÃ.o[i]
  
  
  paste = paste(c(b,"0801"), collapse = '')
  
  
  Ago$Fecha[i] = paste
  
}


Sep = table2[,c('SEPTI','AÃ.o')]
#se borran los datos NA o secos
Sep[Sep$SEPTI != 'seco',]

Sep[Sep$SEPTI != '*',]


#Se utiliza la regresion lineal y se generan datos de carga - RECORDAR
#que los datos estan con la funcion logaritmo y se debe aplciar o reversar esta
#Funcion para obtener los datos reales....

Sep$GastoSolido[1]=1
Sep$Fecha[1] = 1
for(i in 1:length(Sep$SEPTI)){
  
  a = as.numeric(Sep$SEPTI[i])
  Sep$Caud[i] = as.numeric(Sep$SEPTI[i])
  Sep$GastoSolido[i] = coefficients[1] + coefficients[2]*log(a)
  #Para eliminar el logaritmo
  Sep$GastoSolido[i] = exp(Sep$GastoSolido[i])
  
  b = Sep$AÃ.o[i]
  
  
  paste = paste(c(b,"0901"), collapse = '')
  
  
  Sep$Fecha[i] = paste
  
}


Oct = table2[,c('OCTUB','AÃ.o')]
#se borran los datos NA o secos
Oct[Oct$OCTUB != 'seco',]

Oct[Oct$OCTUB != '*',]

#Se utiliza la regresion lineal y se generan datos de carga - RECORDAR
#que los datos estan con la funcion logaritmo y se debe aplciar o reversar esta
#Funcion para obtener los datos reales....

Oct$GastoSolido[1]=1
Oct$Fecha[1] = 1
for(i in 1:length(Oct$OCTUB)){
  
  a = as.numeric(Oct$OCTUB[i])
  Oct$Caud[i] =  as.numeric(Oct$OCTUB[i])
  Oct$GastoSolido[i] = coefficients[1] + coefficients[2]*log(a)
  #Para eliminar el logaritmo
  Oct$GastoSolido[i] = exp(Oct$GastoSolido[i])
  
  b = Oct$AÃ.o[i]
  
  
  paste = paste(c(b,"1001"), collapse = '')
  
  
  Oct$Fecha[i] = paste
  
}


Nov = table2[,c('NOVIE','AÃ.o')]
#se borran los datos NA o secos
Nov[Nov$NOVIE != 'seco',]

Nov[Nov$NOVIE != '*',]


#Se utiliza la regresion lineal y se generan datos de carga - RECORDAR
#que los datos estan con la funcion logaritmo y se debe aplciar o reversar esta
#Funcion para obtener los datos reales....

Nov$GastoSolido[1]=1
Nov$Fecha[1] = 1
for(i in 1:length(Nov$NOVIE)){
  
  a = as.numeric(Nov$NOVIE[i])
  Nov$Caud[i] =  as.numeric(Nov$NOVIE[i])
  Nov$GastoSolido[i] = coefficients[1] + coefficients[2]*log(a)
  #Para eliminar el logaritmo
  Nov$GastoSolido[i] = exp(Nov$GastoSolido[i])
  
  b = Nov$AÃ.o[i]
  
  
  paste = paste(c(b,"1101"), collapse = '')
  
  
  Nov$Fecha[i] = paste
  
}


Dic = table2[,c('DICIE','AÃ.o')]
#se borran los datos NA o secos
Dic[Dic$DICIE != 'seco',]

Dic[Dic$DICIE != '*',]

#Se utiliza la regresion lineal y se generan datos de carga - RECORDAR
#que los datos estan con la funcion logaritmo y se debe aplciar o reversar esta
#Funcion para obtener los datos reales....

Dic$GastoSolido[1]=1
Dic$Fecha[1] = 1
for(i in 1:length(Dic$DICIE)){
  
  a = as.numeric(Dic$DICIE[i])
  Dic$Caud[i] =  as.numeric(Dic$DICIE[i])
  Dic$GastoSolido[i] = coefficients[1] + coefficients[2]*log(a)
  #Para eliminar el logaritmo
  Dic$GastoSolido[i] = exp(Dic$GastoSolido[i])
  
  b = Dic$AÃ.o[i]
  
  
  paste = paste(c(b,"1201"), collapse = '')
  
  
  Dic$Fecha[i] = paste
  
}

# Cambio el nombre de la primera columna de todos los df por caudal

names(Ene)[1] = "caudal"
names(Feb)[1] = "caudal"
names(Mar)[1] = "caudal"
names(Abr)[1] = "caudal"
names(May)[1] = "caudal"
names(Jun)[1] = "caudal"
names(Jul)[1] = "caudal"
names(Ago)[1] = "caudal"
names(Sep)[1] = "caudal"
names(Oct)[1] = "caudal"
names(Nov)[1] = "caudal"
names(Dic)[1] = "caudal"




#Here I bind the dataframes by ROW

union = rbind(Ene,Feb,Mar)
union2 = rbind(Abr, May, Jun)
union3 = rbind(Jul, Ago, Sep)
union4 = rbind(Oct, Nov, Dic)
finalunion = rbind(union,union2,union3,union4)


#eliminar la primera columna

finalunion$caudal = NULL
finalunion$AÃ.o = NULL
finalunion = finalunion[c(3,1,2)]


r2 = table[,c('FECHA1','CAUDAL_LIQUIDO.m3.s.','GASTO_SOLIDO.Kg.s.')]
names(r2)[1] = names(finalunion)[1]
names(r2)[2] = names(finalunion)[2]
names(r2)[3] = names(finalunion)[3]



#Juntar datos originales con datos calculados por regresion en un solo DF
finaldata = rbind(finalunion,r2)



#Plot de los 659 datos

plot(finaldata$Fecha,finaldata$GastoSolido)





newdata <- finaldata[order(finaldata$Fecha),]
newdata[newdata$GastoSolido > 0,]



#Take fecha as date format

dt2 = as.Date(newdata$Fecha, format = "%Y%m%d")

#Se calcula la diferencia de tiempo en segundos entre cada fecha y la fecha inicial

dt2 = difftime(dt2, dt2[1], units = "secs")


# dt en segundos sobre /(10^6)

dt = as.numeric(dt2/(10^6))

#dt1 = as.character(table$date[2])
#dt1 = as.Date(dt1, format = "%Y%m%d")

#table$seconds[i] = difftime(dt1, dt2, units = "secs")

newdata$seconds = as.numeric(dt2)




x1 = newdata$seconds
y1 = newdata$GastoSolido


func = splinefun(x =x1,y =y1, method="fmm", ties= mean )

plot(x1,y1)

integrate(func,min(x1),max(x1), subdivisions = 800)





func2 = approxfun(x =x1,y =y1, method="linear")





integrate(func,min(x1),max(x1), subdivisions = 1500)

#Valor de la integral, correspondiente al valor en kg del gasto solido a traves de los anos
valorint = integral(func2,min(x1),max(x1))

#max(x1) represental el valor en segundos de tiempo tota
#Se pasa a anos dividiendo por 60*60*24*365

t_anos = max(x1)/(60*60*365*24)


#Valor de kilogramos por ano

#7320 toneladas
valorint/t_anos


#AREA en m3 = 170369058.863
#Densidad = 1900


denudation_rate= ((valorint/t_anos)/(1900*AREA))*1000




#Grafica de Rating Curve
graphname = paste(c(station_name,'waterdischarge_data_used',".pdf"), collapse = '')

pdf(graphname)


plot(newdata$seconds,newdata$Caud, main="Water discharge monthly data", sub=station_name,ylab="Water Discharge (m3/s)", xlab="Time (s)") 



dev.off()



#Plot del metodo de integracion



