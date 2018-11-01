#PROYECTO

#lectura de datos
Datos <- read.csv("./foodPricesClean.csv", stringsAsFactors = F)

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


