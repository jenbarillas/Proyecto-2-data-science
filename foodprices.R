library(class)
library(caret)

#carga de datos
library(readxl)
datos <- read.csv("C:/Users/JennyMBB/Desktop/Data Science/Proyecto-2/FoodPrices-.csv")
copia <- datos

#---------Limpieza conversion de moneda--------
#Se pasa todos los precios a dollar estadounidense

#AFN - 0.014 dolares
rows <- datos[which(datos$cur_name == "AFN"),]
rows$mp_price <- rows$mp_price*0.014
datos[which(datos$cur_name == "AFN"),] <- rows

#AMD - 0.0021 $
rows <- datos[which(datos$cur_name == "AMD"),]
rows$mp_price <- rows$mp_price*0.0021
datos[which(datos$cur_name == "AMD"),] <- rows

#AZN - 0.59
rows <- datos[which(datos$cur_name == "AZN"),]
rows$mp_price <- rows$mp_price*0.59
datos[which(datos$cur_name == "AZN"),] <- rows

#BDT - 0.012
rows <- datos[which(datos$cur_name == "BDT"),]
rows$mp_price <- rows$mp_price*0.012
datos[which(datos$cur_name == "BDT"),] <- rows

#BIF - 0.00056
rows <- datos[which(datos$cur_name == "BIF"),]
rows$mp_price <- rows$mp_price*0.00056
datos[which(datos$cur_name == "BIF"),] <- rows

#BOB - 0.14
rows <- datos[which(datos$cur_name == "BOB"),]
rows$mp_price <- rows$mp_price*0.14
datos[which(datos$cur_name == "BOB"),] <- rows

#BTN - 0.014
rows <- datos[which(datos$cur_name == "BTN"),]
rows$mp_price <- rows$mp_price*0.014
datos[which(datos$cur_name == "BTN"),] <- rows

#CDF - 0.00061
rows <- datos[which(datos$cur_name == "CDF"),]
rows$mp_price <- rows$mp_price*0.00061
datos[which(datos$cur_name == "CDF"),] <- rows

#COP - 0.00033
rows <- datos[which(datos$cur_name == "COP"),]
rows$mp_price <- rows$mp_price*0.00033
datos[which(datos$cur_name == "COP"),] <- rows

#CVE - 0.011
rows <- datos[which(datos$cur_name == "CVE"),]
rows$mp_price <- rows$mp_price*0.011
datos[which(datos$cur_name == "CVE"),] <- rows

#DJF - 0.0056
rows <- datos[which(datos$cur_name == "DJF"),]
rows$mp_price <- rows$mp_price*0.0056
datos[which(datos$cur_name == "DJF"),] <- rows

#DZD - 0.0085
rows <- datos[which(datos$cur_name == "DZD"),]
rows$mp_price <- rows$mp_price*0.0085
datos[which(datos$cur_name == "DZD"),] <- rows

#EGP - 0.056
rows <- datos[which(datos$cur_name == "EGP"),]
rows$mp_price <- rows$mp_price*0.056
datos[which(datos$cur_name == "EGP"),] <- rows

#ETP - 3.30
rows <- datos[which(datos$cur_name == "ETP"),]
rows$mp_price <- rows$mp_price*3.3
datos[which(datos$cur_name == "ETP"),] <- rows

#GEL - 0.40
rows <- datos[which(datos$cur_name == "GEL"),]
rows$mp_price <- rows$mp_price*0.4
datos[which(datos$cur_name == "GEL"),] <- rows

#GHS - 0.21
rows <- datos[which(datos$cur_name == "GHS"),]
rows$mp_price <- rows$mp_price*0.21
datos[which(datos$cur_name == "GHS"),] <- rows

#GMD - 0.021
rows <- datos[which(datos$cur_name == "GMD"),]
rows$mp_price <- rows$mp_price*0.021
datos[which(datos$cur_name == "GMD"),] <- rows

#GNF - 0.00011
rows <- datos[which(datos$cur_name == "GNF"),]
rows$mp_price <- rows$mp_price*0.00011
datos[which(datos$cur_name == "GNF"),] <- rows

#GTQ - 0.13
rows <- datos[which(datos$cur_name == "GTQ"),]
rows$mp_price <- rows$mp_price*0.13
datos[which(datos$cur_name == "GTQ"),] <- rows

#HTG - 0.014
rows <- datos[which(datos$cur_name == "HTG"),]
rows$mp_price <- rows$mp_price*0.014
datos[which(datos$cur_name == "HTG"),] <- rows

#IDR - 0.000068
rows <- datos[which(datos$cur_name == "IDR"),]
rows$mp_price <- rows$mp_price*0.000068
datos[which(datos$cur_name == "IDR"),] <- rows

#INR - 0.014
rows <- datos[which(datos$cur_name == "INR"),]
rows$mp_price <- rows$mp_price*0.014
datos[which(datos$cur_name == "INR"),] <- rows

#IQD - 0.00083
rows <- datos[which(datos$cur_name == "IQD"),]
rows$mp_price <- rows$mp_price*0.00083
datos[which(datos$cur_name == "IQD"),] <- rows

#IRR - 0.000024
rows <- datos[which(datos$cur_name == "IRR"),]
rows$mp_price <- rows$mp_price*0.000024
datos[which(datos$cur_name == "IRR"),] <- rows

#JOD - 1.41
rows <- datos[which(datos$cur_name == "JOD"),]
rows$mp_price <- rows$mp_price*1.41
datos[which(datos$cur_name == "JOD"),] <- rows

#KES - 0.0099
rows <- datos[which(datos$cur_name == "KES"),]
rows$mp_price <- rows$mp_price*0.0099
datos[which(datos$cur_name == "KES"),] <- rows

#KGS - 0.014
rows <- datos[which(datos$cur_name == "KGS"),]
rows$mp_price <- rows$mp_price*0.014
datos[which(datos$cur_name == "KGS"),] <- rows

#KHR - 0.00024
rows <- datos[which(datos$cur_name == "KHR"),]
rows$mp_price <- rows$mp_price*0.00024
datos[which(datos$cur_name == "KHR"),] <- rows

#LAK - 0.00012
rows <- datos[which(datos$cur_name == "LAK"),]
rows$mp_price <- rows$mp_price*0.00012
datos[which(datos$cur_name == "LAK"),] <- rows

#LBP - 0.00066
rows <- datos[which(datos$cur_name == "LBP"),]
rows$mp_price <- rows$mp_price*0.00066
datos[which(datos$cur_name == "LBP"),] <- rows

#LKR - 0.0062
rows <- datos[which(datos$cur_name == "LKR"),]
rows$mp_price <- rows$mp_price*0.0062
datos[which(datos$cur_name == "LKR"),] <- rows

#LRD - 0.0065
rows <- datos[which(datos$cur_name == "LRD"),]
rows$mp_price <- rows$mp_price*0.0065
datos[which(datos$cur_name == "LRD"),] <- rows

#LSL - 0.068
rows <- datos[which(datos$cur_name == "LSL"),]
rows$mp_price <- rows$mp_price*0.068
datos[which(datos$cur_name == "LSL"),] <- rows

#MGA - 0.00030
rows <- datos[which(datos$cur_name == "MGA"),]
rows$mp_price <- rows$mp_price*0.0003
datos[which(datos$cur_name == "MGA"),] <- rows

#MMK - 0.00066
rows <- datos[which(datos$cur_name == "MMK"),]
rows$mp_price <- rows$mp_price*0.00066
datos[which(datos$cur_name == "MMK"),] <- rows

#MRO - 0.0028
rows <- datos[which(datos$cur_name == "MRO"),]
rows$mp_price <- rows$mp_price*0.0028
datos[which(datos$cur_name == "MRO"),] <- rows

#MWK - 0.0014
rows <- datos[which(datos$cur_name == "MWK"),]
rows$mp_price <- rows$mp_price*0.0014
datos[which(datos$cur_name == "MWK"),] <- rows

#MZN - 0.017
rows <- datos[which(datos$cur_name == "MZN"),]
rows$mp_price <- rows$mp_price*0.017
datos[which(datos$cur_name == "MZN"),] <- rows

#NIS - 0.28
rows <- datos[which(datos$cur_name == "NIS"),]
rows$mp_price <- rows$mp_price*0.28
datos[which(datos$cur_name == "NIS"),] <- rows

#NPR - 0.0088
rows <- datos[which(datos$cur_name == "NPR"),]
rows$mp_price <- rows$mp_price*0.0088
datos[which(datos$cur_name == "NPR"),] <- rows

#REN - 0.029
rows <- datos[which(datos$cur_name == "REN"),]
rows$mp_price <- rows$mp_price*0.029
datos[which(datos$cur_name == "REN"),] <- rows

#PHP - 0.019
rows <- datos[which(datos$cur_name == "PHP"),]
rows$mp_price <- rows$mp_price*0.019
datos[which(datos$cur_name == "PHP"),] <- rows

#PKR - 0.0081
rows <- datos[which(datos$cur_name == "PKR"),]
rows$mp_price <- rows$mp_price*0.0081
datos[which(datos$cur_name == "PKR"),] <- rows

#RWF - 0.0011
rows <- datos[which(datos$cur_name == "RWF"),]
rows$mp_price <- rows$mp_price*0.0011
datos[which(datos$cur_name == "RWF"),] <- rows

#SDG - 0.055
rows <- datos[which(datos$cur_name == "SDG"),]
rows$mp_price <- rows$mp_price*0.055
datos[which(datos$cur_name == "SDG"),] <- rows

#Somaliland Shilling - 0.000219
rows <- datos[which(datos$cur_name == "Somaliland Shilling"),]
rows$mp_price <- rows$mp_price*0.000219
datos[which(datos$cur_name == "Somaliland Shilling"),] <- rows

#SOS - 0.0017
rows <- datos[which(datos$cur_name == "SOS"),]
rows$mp_price <- rows$mp_price*0.0017
datos[which(datos$cur_name == "SOS"),] <- rows

#SSP - 0.0076
rows <- datos[which(datos$cur_name == "SSP"),]
rows$mp_price <- rows$mp_price*0.0076
datos[which(datos$cur_name == "SSP"),] <- rows

#SYP - 0.0019
rows <- datos[which(datos$cur_name == "SYP"),]
rows$mp_price <- rows$mp_price*0.0019
datos[which(datos$cur_name == "SYP"),] <- rows

#SZL - 0.068
rows <- datos[which(datos$cur_name == "SZL"),]
rows$mp_price <- rows$mp_price*0.068
datos[which(datos$cur_name == "SZL"),] <- rows

#TJS - 0.11
rows <- datos[which(datos$cur_name == "TJS"),]
rows$mp_price <- rows$mp_price*0.11
datos[which(datos$cur_name == "TJS"),] <- rows

#TRY - 0.15
rows <- datos[which(datos$cur_name == "TRY"),]
rows$mp_price <- rows$mp_price*0.15
datos[which(datos$cur_name == "TRY"),] <- rows

#TZS - 0.00044
rows <- datos[which(datos$cur_name == "TZS"),]
rows$mp_price <- rows$mp_price*0.00044
datos[which(datos$cur_name == "TZS"),] <- rows

#UAH - 0.035
rows <- datos[which(datos$cur_name == "UAH"),]
rows$mp_price <- rows$mp_price*0.035
datos[which(datos$cur_name == "UAH"),] <- rows

#UGX - 0.00026
rows <- datos[which(datos$cur_name == "UGX"),]
rows$mp_price <- rows$mp_price*0.00026
datos[which(datos$cur_name == "UGX"),] <- rows

#XAF - 0.0018
rows <- datos[which(datos$cur_name == "SDG" & datos$cur_name == "XOF"),]
rows$mp_price <- rows$mp_price*0.055
datos[which(datos$cur_name == "SDG" & datos$cur_name == "XOF"),] <- rows

#YER - 0.004
rows <- datos[which(datos$cur_name == "YER"),]
rows$mp_price <- rows$mp_price*0.004
datos[which(datos$cur_name == "YER"),] <- rows

#ZMW - 0.096
rows <- datos[which(datos$cur_name == "ZMW"),]
rows$mp_price <- rows$mp_price*0.096
datos[which(datos$cur_name == "ZMW"),] <- rows

#---------Eliminacion de la variable de moneda-------

datos$cur_id <- NULL
datos$cur_name <- NULL

#---------Eliminacion de filas con precio 0 -----------

datos <- datos[-which(datos$mp_price == 0),]
datos <- datos[-which(is.na(datos$cm_id)),]

#-------- toda la masa de la comida convertida en lb o L todo-------

br <- datos[which(datos$cm_name == "Beans (red"),]

#Separacion de datos entre la comida y la comida dentro de la canasta basica#

datos <- datos[-which(datos$cm_name == "Livestock (Goat)"),]
datos <- datos[-which(datos$cm_name == "Livestock (Sheep)"),]
datos <- datos[-which(datos$cm_name == "Livestock (pig)"),]
datos <- datos[-which(datos$cm_name == "Livestock (sheep"),]
datos <- datos[-which(datos$cm_name == "Livestock (hen)"),]
datos <- datos[-which(datos$cm_name == "Livestock (goat"),]
datos <- datos[-which(datos$cm_name == "Livestock (cattle)"),]
datos <- datos[-which(datos$cm_name == "Electricity"),]

#unir elementos parecidos
p <- datos[which(datos$cm_name == "Apples (red)"),]
p$cm_name <- "Apples"
datos <- datos[-which(datos$cm_name == "Apples (red)"),]
datos <- rbind(datos,p)

p <- datos[which(datos$cm_name == "Bananas (medium size)"),]
p$cm_name <- "Bananas"
datos <- datos[-which(datos$cm_name == "Bananas (medium size)"),]
datos <- rbind(datos,p)

#---nivelar cantidades----

price <- datos[-which(datos$mp_price > 20),]
plot(price[,"mp_price"])
boxplot(price[,"mp_price"])
datos <- datos[-which(datos$mp_price > 5),]
plot(price5[,"mp_price"])
boxplot(price5[,"mp_price"])
#--------VARIABLES NUMERICAS------

#Estadisticas descriptivas de las variabes numericas
#Comida
summary(datos$cm_id)
#Precio
summary(datos$mp_price)
#pais
summary(datos$adm0_id)
#año
summary(datos$mp_year)


#para ver la dispersion de los datos
#comida
boxplot(datos[,"cm_id"])
#precio
mp <- datos[,"mp_price"]
plot(mp)
plot(datos[,"cm_id"])
#plot(datos[,"mp_price"])
#pais
boxplot(datos[,"adm0_id"])
#año
boxplot(datos[,"mp_year"])


#separamos los datos numericos para la demostracion de variables independientes
library(Hmisc)
numdata <- datos[c(1,3,5,7,9,11,13,15)]
rcorr(numdata, type = "spearman")

library(polycor)
hetcor(numdata)
#--------VARIABLES CATEGORICAS------

#Tablas de frecuencias
#Nombre de comida
table(datos$cm_name)
#nombre del pais
table(datos$adm0_name)
#cantidad de masa
table(datos$um_name)
#fuente de datos
table(datos$mp_commoditysource)

#------------cluster---------------
library(cluster) #Para calcular la silueta
library(e1071)#para cmeans
library(mclust) #mixtures of gaussians
library(fpc) #para hacer el plotcluster
library(dplyr)
library(VIM)
library(lubridate)

#datos <- read.csv("C:/Users/JennyMBB/Desktop/Data Science/Proyecto-2/FoodPrices.csv")

#se dividieron los datos por year (5 por subset) ya que habian demasiados registros
#por cada grupo de 5 se realizo la agrupacion de clusters.
datos1<-subset(datos,mp_year >= 1992 & mp_year <= 1996)
#distancia 
d1 <- dist(datos1, method = "euclidean") 
fit1 <- hclust(d1, method="ward.D") 
plot(fit1)#dendograma
groups <- cutree(fit1, k=5)
rect.hclust(fit1, k=5, border="red") #dendograma dividido por clusters

#datos de 1997 a 2001---------------------------------------------------------
datos2<-subset(datos,mp_year >= 1997 & mp_year <= 2001)
d2 <- dist(datos2, method = "euclidean")
fit2 <- hclust(d2, method="ward.D") 
plot(fit2)
groups2 <- cutree(fit2, k=5)
rect.hclust(fit2, k=5, border="red")

#datos de 2002 a 2006---------------------------------------------------------
datos3<-subset(datos,mp_year >= 2002 & mp_year <= 2006)
d3 <- dist(datos1, method = "euclidean")
fit3 <- hclust(d3, method="ward.D") 
plot(fit3)
groups3 <- cutree(fit3, k=5)
rect.hclust(fit3, k=5, border="red")

#datos de 2007 a 2011---------------------------------------------------------
datos4<-subset(datos,mp_year >= 2007 & mp_year <= 2011)
d4 <- dist(datos1, method = "euclidean")
fit4 <- hclust(d4, method="ward.D") 
plot(fit4)
groups4 <- cutree(fit4, k=5)
rect.hclust(fit4, k=5, border="red")

#datos de 2012 a 2017---------------------------------------------------------
datos5<-subset(datos,mp_year >= 2012 & mp_year <= 2017)
d5 <- dist(datos1, method = "euclidean")
fit5 <- hclust(d5, method="ward.D")
plot(fit5)
groups5 <- cutree(fit5, k=5)
rect.hclust(fit5, k=5, border="red")

#--------------------km--------------
#se obtienen los datos numericos de la matriz
nums <- unlist(lapply(datos, is.numeric)) 
#se usan esos datos para sacar el km y crear los grupos
numDatos1<-datos[ ,nums]
km1<-kmeans(numDatos1,4)
datos$grupo<-km1$cluster

#obtenemos el grupo de datos por cluster
g1<- datos[datos$grupo==1,]
prop.table(table(g1$cm_name))*100
plot(g1$cm_name)
g2<- datos[datos$grupo==2,]
prop.table(table(g2$cm_name))*100
plot(g2$cm_name)
g3<- datos[datos$grupo==3,]
prop.table(table(g3$cm_name))*100
plot(g3$cm_name)
g4<- datos[datos$grupo==4,]
prop.table(table(g4$cm_name))*100
plot(g4$cm_name)

#ploteamos los datos, al tener muchos rgistros se demora bastante en plotear
plotcluster(numDatos1,km1$cluster) 

#-----------PCA-------------
log.food <- log(numdata)
food.name <- datos[,7]

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
food.pca <- prcomp(log.food,
                 center = TRUE,
                 scale. = TRUE)
