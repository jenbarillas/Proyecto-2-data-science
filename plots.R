#PROYECTO

#lectura de datos
Datos <- read.csv("C:/Users/JennyMBB/Desktop/Data Science/clean/CLEAN.csv", stringsAsFactors = F)

#Subset de los datos, solo los de GUATEMALA
DatosGuate <- Datos[Datos$adm0_name %in% "Guatemala",]

##limpieza de datos GUATEMALA

DatosGuate<-DatosGuate[-which(DatosGuate$cm_name == "Fuel (petrol-gasoline)"),]

##haciendo grupos de alimentos
##VERDURAS

DatosGuate[DatosGuate$cm_name %in% c("Potatoes","Onions"),"cm_name"]<-"Verduras"

##FRUTAS

DatosGuate[DatosGuate$cm_name %in% c("Bananas","Tomatoes", "Plantains"),"cm_name"]<-"Frutas"

##LACTEOS

DatosGuate[DatosGuate$cm_name %in% c("Cheese","Milk (powder)", "Milk"),"cm_name"]<-"Lacteos"

##CARNES

DatosGuate[DatosGuate$cm_name %in% c("Meat (beef, chops with bones)","Eggs", "Meat (chicken)"),"cm_name"]<-"Carnes"

##CEREALES

DatosGuate[DatosGuate$cm_name %in% c("Rice (ordinary, second quality)", "Tortilla (maize)", "Pasta", "Bread"),"cm_name"]<-"Cereales"

##GRASA Y DULCE

DatosGuate[DatosGuate$cm_name %in% c("Sugar", "Oil (vegetable)"),"cm_name"]<-"Grasa y azucar"

##LEGUMBRES

DatosGuate[DatosGuate$cm_name %in% c("Beans"),"cm_name"]<-"Legumbres"


#Limpieza de datos SUR AMERICA
#Subset de los datos, solo los de BOLIVIA 
DatosBolivia <- Datos[Datos$adm0_name %in% "Bolivia",]

#Limpieza de datos BOLIVIA

##GRASA Y DULCE

DatosBolivia[DatosBolivia$cm_name %in% c("Sugar"),"cm_name"]<-"Grasa y azucar"

##CARNES

DatosBolivia[DatosBolivia$cm_name %in% c("Meat (beef, chops with bones)", "Meat (chicken, whole)", "Eggs"),"cm_name"]<-"Carnes"

##CEREALES

DatosBolivia[DatosBolivia$cm_name %in% c("Bread", "Noodles (short)", "Rice"),"cm_name"]<-"Cereales"

##VERDURAS

DatosBolivia[DatosBolivia$cm_name %in% c("Potatoes (Irish, imilla)", "Potatoes (Dutch)", "Potatoes (black)"),"cm_name"]<-"Verduras"

#Subset de los datos, solo los de COLOMBIA
DatosCol <- Datos[Datos$adm0_name %in% "Colombia",]

#Limpieza de datos COLOMBIA

##FRUTAS

DatosCol[DatosCol$cm_name %in% c("Apples", "Guava", "Mangoes", "Papaya", "Pumpkin", "Tamarillos/tree tomatoes", "Bananas", "Blackberry", "Oranges (big size)", "Tomatoes", "Plantains"),"cm_name"]<-"Frutas"

##VERDURAS

DatosCol[DatosCol$cm_name %in% c("Spinach", "Potatoes", "", "Onions", "Cucumbers", "Carrots", "Cassava", "Cauliflower", "Broccoli", "Cabbage", "Cucumbers (greenhouse)", "Potatoes (unica)", "Garlic"),"cm_name"]<-"Verduras"

##CEREALES

DatosCol[DatosCol$cm_name %in% c("Wheat flour", "Pasta", "", "Maize", "Maize flour"),"cm_name"]<-"Cereales"


##LEGUMBRES

DatosCol[DatosCol$cm_name %in% c("Beans", "Chickpeas (imported)", "Lentils", "Peas (green, dry)"),"cm_name"]<-"Legumbres"


##LACTEOS

DatosCol[DatosCol$cm_name %in% c("Cheese", "Milk", "Milk (pasteurized)"),"cm_name"]<-"Lacteos"


##CARNES
DatosCol[DatosCol$cm_name %in% c("Meat (chicken)", "Meat (beef)", "Meat (pork)", "Meat (beef, minced)", "Eggs", "Fish (tilapia)"),"cm_name"]<-"Carnes"


##GRASA Y AZUCAR

DatosCol[DatosCol$cm_name %in% c("Oil (sunflower)", "Oil (vegetable)", "Cocoa", "Sugar"),"cm_name"]<-"Grasa y azucar"

#Gas
DatosCol<-DatosCol[-which(DatosCol$cm_name == "Fuel (petrol-gasoline)"),]

#Subset de datos solo los de PERU 
DatosPeru <- Datos[Datos$adm0_name %in% "Peru",]

#Limpieza de datos PERU

##VERDURAS

DatosPeru[DatosPeru$cm_name %in% c("Potatoes"),"cm_name"]<-"Verduras"

##CEREALES

DatosPeru[DatosPeru$cm_name %in% c("Wheat flour (locally processed)", "Maize", "Rice"),"cm_name"]<-"Cereales"

##GRASA Y AZUCAR

DatosPeru[DatosPeru$cm_name %in% c("Sugar", "Oil (vegetable)"),"cm_name"]<-"Grasa y azucar"


#---------------------------------------------------------
#            PREDICCIONES
#---------------------------------------------------------

prediccion<-function(arreglo, month=FALSE){
  if (!month) {
    dat<-arreglo[c('mp_year', 'mp_price')]
    arreglo<-aggregate( mp_price ~ mp_year, dat, mean )
    cor.test(x = arreglo$mp_year, y = arreglo$mp_price, method = "pearson")
    modelo_lineal <- lm(mp_price ~ mp_year, arreglo)
    confint(modelo_lineal)
    future_years <- data.frame(mp_year=c(2016,2017,2018, 2019, 2020, 2021))  
    complemento<-data.frame(future_years,predict(modelo_lineal, newdata = future_years))
    arreglo<-rbind(arreglo, setNames(complemento, names(arreglo)))
  }
  return(arreglo)
}


# head(datos)
prediccionesGuate<-data.frame()
datosGuate<-subset(Datos,adm0_name == 'Guatemala')

prediccionesPeru<-data.frame()
datosPeru<-subset(Datos,adm0_name == 'Peru')

prediccionesBolivia<-data.frame()
datosBolivia<-subset(Datos,adm0_name == 'Bolivia')

prediccionesCol<-data.frame()
datosCol<-subset(Datos,adm0_name == 'Colombia')

#library(Hmisc)
#numdata <- datosGuatemala[c(1,3,5,7,9,11,13,15)]
#numdata <- datosGuate[c(4,8,10,12,14,15,16)]
#rcorr(numdata, type = "spearman")


# head(datosGuatemala)
VerdurasG<-subset(DatosGuate,cm_name == "Verduras")
VerdurasG<-prediccion(VerdurasG)
VerdurasG["cm_name"] = "Verduras"
prediccionesGuate<-rbind(prediccionesGuate, setNames(VerdurasG, names(VerdurasG)))

VerdurasP<-subset(DatosPeru,cm_name == 'Verduras')
VerdurasP<-prediccion(VerdurasP)
VerdurasP['cm_name'] = 'Verduras'
prediccionesPeru<-rbind(prediccionesGuate, setNames(VerdurasG, names(VerdurasG)))

VerdurasB<-subset(DatosBolivia,cm_name == 'Verduras')
VerdurasB<-prediccion(VerdurasB)
VerdurasB['cm_name'] = 'Verduras'
prediccionesBolivia<-rbind(prediccionesBolivia, setNames(VerdurasG, names(VerdurasG)))

VerdurasC<-subset(DatosCol,cm_name == 'Verduras')
VerdurasC<-prediccion(VerdurasC)
VerdurasC['cm_name'] = 'Verduras'
prediccionesCol<-rbind(prediccionesCol, setNames(VerdurasG, names(VerdurasG)))

plot(VerdurasG$mp_year,VerdurasG$mp_price,type="l",col="blue",lwd=3,main="Verduras",xlab="Años",ylab="Precio en dollar",ylim=c(-1,2.5),col.axis="Black")
lines(VerdurasB$mp_year,VerdurasB$mp_price,col="green",lwd=3)
lines(VerdurasP$mp_year,VerdurasP$mp_price,col="Red",lwd=3)
lines(VerdurasC$mp_year,VerdurasC$mp_price,col="Yellow",lwd=3)
legend("bottomleft",col=c("blue","green","Red","Yellow"),legend =c("Guatemala","Bolivia","Peru","Colombia"), lwd=3, bty = "n")

LegumbresG<-subset(DatosGuate,cm_name == "Legumbres")
LegumbresG<-prediccion(LegumbresG)
LegumbresG["cm_name"] = "Legumbres"
prediccionesGuate<-rbind(prediccionesGuate, setNames(LegumbresG, names(LegumbresG)))

LegumbresP<-subset(DatosPeru,cm_name == 'Legumbres')
LegumbresP<-prediccion(LegumbresP)
LegumbresP['cm_name'] = 'Legumbres'
prediccionesPeru<-rbind(prediccionesGuate, setNames(LegumbresG, names(LegumbresG)))

LegumbresB<-subset(DatosBolivia,cm_name == 'Legumbres')
LegumbresB<-prediccion(LegumbresB)
LegumbresB['cm_name'] = 'Legumbres'
prediccionesBolivia<-rbind(prediccionesBolivia, setNames(LegumbresG, names(LegumbresG)))

LegumbresC<-subset(DatosCol,cm_name == 'Legumbres')
LegumbresC<-prediccion(LegumbresC)
LegumbresC['cm_name'] = 'Legumbres'
prediccionesCol<-rbind(prediccionesCol, setNames(LegumbresG, names(LegumbresG)))

plot(LegumbresG$mp_year,LegumbresG$mp_price,type="l",col="blue",lwd=3,main="Legumbres",col.axis="Black")
lines(LegumbresB$mp_year,LegumbresB$mp_price,col="green",lwd=3)
lines(LegumbresP$mp_year,LegumbresP$mp_price,col="Red",lwd=3)
lines(LegumbresC$mp_year,LegumbresC$mp_price,col="Yellow",lwd=3)
legend("bottomleft",col=c("blue","green","Red","Yellow"),legend =c("Guatemala","Bolivia","Peru","Colombia"), lwd=3, bty = "n")


CerealesG<-subset(DatosGuate,cm_name == "Cereales")
CerealesG<-prediccion(CerealesG)
CerealesG["cm_name"] = "Cereales"
prediccionesGuate<-rbind(prediccionesGuate, setNames(CerealesG, names(CerealesG)))

CerealesP<-subset(DatosPeru,cm_name == 'Cereales')
CerealesP<-prediccion(CerealesP)
CerealesP['cm_name'] = 'Cereales'
prediccionesPeru<-rbind(prediccionesGuate, setNames(CerealesG, names(CerealesG)))

CerealesB<-subset(DatosBolivia,cm_name == 'Cereales')
CerealesB<-prediccion(CerealesB)
CerealesB['cm_name'] = 'Cereales'
prediccionesBolivia<-rbind(prediccionesBolivia, setNames(CerealesG, names(CerealesG)))

CerealesC<-subset(DatosCol,cm_name == 'Cereales')
CerealesC<-prediccion(CerealesC)
CerealesC['cm_name'] = 'Cereales'
prediccionesCol<-rbind(prediccionesCol, setNames(CerealesG, names(CerealesG)))

plot(CerealesG$mp_year,CerealesG$mp_price,type="l",col="blue",lwd=3,main="Cereales",xlab="Años",ylab="Precio en dollar",ylim=c(-1,2.5),col.axis="Black")
lines(CerealesB$mp_year,CerealesB$mp_price,col="green",lwd=3)
lines(CerealesP$mp_year,CerealesP$mp_price,col="Red",lwd=3)
lines(CerealesC$mp_year,CerealesC$mp_price,col="Yellow",lwd=3)
legend("bottomleft",col=c("blue","green","Red","Yellow"),legend =c("Guatemala","Bolivia","Peru","Colombia"), lwd=3, bty = "n")

FrutasG<-subset(DatosGuate,cm_name == "Frutas")
FrutasG<-prediccion(FrutasG)
FrutasG["cm_name"] = "Frutas"
prediccionesGuate<-rbind(prediccionesGuate, setNames(FrutasG, names(FrutasG)))

FrutasP<-subset(DatosPeru,cm_name == 'Frutas')
FrutasP<-prediccion(FrutasP)
FrutasP['cm_name'] = 'Frutas'
prediccionesPeru<-rbind(prediccionesGuate, setNames(FrutasG, names(FrutasG)))

FrutasB<-subset(DatosBolivia,cm_name == 'Frutas')
FrutasB<-prediccion(FrutasB)
FrutasB['cm_name'] = 'Frutas'
prediccionesBolivia<-rbind(prediccionesBolivia, setNames(FrutasG, names(FrutasG)))

FrutasC<-subset(DatosCol,cm_name == 'Frutas')
FrutasC<-prediccion(FrutasC)
FrutasC['cm_name'] = 'Frutas'
prediccionesCol<-rbind(prediccionesCol, setNames(FrutasG, names(FrutasG)))

plot(FrutasG$mp_year,FrutasG$mp_price,type="l",col="blue",lwd=3,main="Frutas",xlab="Años",ylab="Precio en dollar",ylim=c(-1,2.5),col.axis="Black")
lines(FrutasB$mp_year,FrutasB$mp_price,col="green",lwd=3)
lines(FrutasP$mp_year,FrutasP$mp_price,col="Red",lwd=3)
lines(FrutasC$mp_year,FrutasC$mp_price,col="Yellow",lwd=3)
legend("bottomleft",col=c("blue","green","Red","Yellow"),legend =c("Guatemala","Bolivia","Peru","Colombia"), lwd=3, bty = "n")

LacteosG<-subset(DatosGuate,cm_name == "Lacteos")
LacteosG<-prediccion(LacteosG)
LacteosG["cm_name"] = "Lacteos"
prediccionesGuate<-rbind(prediccionesGuate, setNames(LacteosG, names(LacteosG)))

LacteosP<-subset(DatosPeru,cm_name == 'Lacteos')
LacteosP<-prediccion(LacteosP)
LacteosP['cm_name'] = 'Lacteos'
prediccionesPeru<-rbind(prediccionesGuate, setNames(LacteosG, names(LacteosG)))

LacteosB<-subset(DatosBolivia,cm_name == 'Lacteos')
LacteosB<-prediccion(LacteosB)
LacteosB['cm_name'] = 'Lacteos'
prediccionesBolivia<-rbind(prediccionesBolivia, setNames(LacteosG, names(LacteosG)))

LacteosC<-subset(DatosCol,cm_name == 'Lacteos')
LacteosC<-prediccion(LacteosC)
LacteosC['cm_name'] = 'Lacteos'
prediccionesCol<-rbind(prediccionesCol, setNames(LacteosG, names(LacteosG)))

plot(LacteosG$mp_year,LacteosG$mp_price,type="l",col="blue",lwd=3,main="Lacteos",xlab="Años",ylab="Precio en dollar",ylim=c(0,4),col.axis="Black")
lines(LacteosB$mp_year,LacteosB$mp_price,col="green",lwd=3)
lines(LacteosP$mp_year,LacteosP$mp_price,col="Red",lwd=3)
lines(LacteosC$mp_year,LacteosC$mp_price,col="Yellow",lwd=3)
legend("bottomleft",col=c("blue","green","Red","Yellow"),legend =c("Guatemala","Bolivia","Peru","Colombia"), lwd=3, bty = "n")

CarnesG<-subset(DatosGuate,cm_name == "Carnes")
CarnesG<-prediccion(CarnesG)
CarnesG["cm_name"] = "Carnes"
prediccionesGuate<-rbind(prediccionesGuate, setNames(CarnesG, names(CarnesG)))

CarnesP<-subset(DatosPeru,cm_name == 'Carnes')
CarnesP<-prediccion(CarnesP)
CarnesP['cm_name'] = 'Carnes'
prediccionesPeru<-rbind(prediccionesGuate, setNames(CarnesG, names(CarnesG)))

CarnesB<-subset(DatosBolivia,cm_name == 'Carnes')
CarnesB<-prediccion(CarnesB)
CarnesB['cm_name'] = 'Carnes'
prediccionesBolivia<-rbind(prediccionesBolivia, setNames(CarnesG, names(CarnesG)))

CarnesC<-subset(DatosCol,cm_name == 'Carnes')
CarnesC<-prediccion(CarnesC)
CarnesC['cm_name'] = 'Carnes'
prediccionesCol<-rbind(prediccionesCol, setNames(CarnesG, names(CarnesG)))

plot(CarnesG$mp_year,CarnesG$mp_price,type="l",col="blue",lwd=3,main="Carnes",xlab="Años",ylab="Precio en dollar",ylim=c(0,4),col.axis="Black")
lines(CarnesP$mp_year,CarnesP$mp_price,col="Red",lwd=3)
lines(CarnesC$mp_year,CarnesC$mp_price,col="Yellow",lwd=3)
legend("bottomleft",col=c("blue","Red","Yellow"),legend =c("Guatemala","Peru","Colombia"), lwd=3, bty = "n")