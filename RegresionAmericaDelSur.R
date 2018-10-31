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

datos<-read.csv("C:/Users/JennyMBB/Desktop/Data Science/clean/foodPricesClean.csv")

#---------------------------------------------------------
#            PREDICCIONES
#---------------------------------------------------------



# head(datos)
prediccionesSA<-data.frame()
p <- subset(datos,adm0_name == 'Peru')
datosSA<-subset(datos,adm0_name == 'Bolivia')
datosSA <- rbind(datosSA,p)
p <- subset(datos,adm0_name == 'Colombia')
datosSA <- rbind(datosSA,p)

# head(datosSA)
bananas<-subset(datosSA,cm_name == 'Bananas')
bananas<-prediccion(bananas)
bananas['cm_name'] = 'Bananas'
prediccionesSA<-rbind(prediccionesSA, setNames(bananas, names(bananas)))


beans<-subset(datosSA,cm_name == 'Beans')
beans<-prediccion(beans)
beans['cm_name'] = 'Beans'
prediccionesSA<-rbind(prediccionesSA, setNames(beans, names(prediccionesSA)))


panes<-subset(datosSA, cm_name == 'Bread')
panes<-prediccion(panes)
panes['cm_name'] = 'Bread'
prediccionesSA<-rbind(prediccionesSA, setNames(panes, names(prediccionesSA)))


cheese<-subset(datosSA,cm_name == 'Cheese')
cheese<-prediccion(cheese)
cheese['cm_name'] = 'Cheese'
prediccionesSA<-rbind(prediccionesSA, setNames(cheese, names(prediccionesSA)))


coffee<-subset(datosSA,cm_name == 'Coffee')
coffee<-prediccion(coffee)
coffee['cm_name'] = 'Coffee'
prediccionesSA<-rbind(prediccionesSA, setNames(coffee, names(prediccionesSA)))


eggs<-subset(datosSA,cm_name == 'Eggs')
eggs<-prediccion(eggs)
eggs['cm_name'] = 'Eggs'
prediccionesSA<-rbind(prediccionesSA, setNames(eggs, names(prediccionesSA)))

fuel<-subset(datosSA,cm_name == 'Fuel (petrol-gasoline)')
fuel<-prediccion(fuel)
fuel['cm_name'] = 'Fuel'
prediccionesSA<-rbind(prediccionesSA, setNames(fuel, names(prediccionesSA)))

meat<-subset(datosSA,cm_name == 'Meat (beef, chops with bones)')
meat<-prediccion(meat)
meat['cm_name'] = 'Meat'
prediccionesSA<-rbind(prediccionesSA, setNames(meat, names(prediccionesSA)))

chicken<-subset(datosSA,cm_name == 'Meat (chicken)')
chicken<-prediccion(chicken)
chicken['cm_name'] = 'Chicken'
prediccionesSA<-rbind(prediccionesSA, setNames(chicken, names(prediccionesSA)))

milk<-subset(datosSA,cm_name == 'Milk')
milk<-prediccion(milk)
milk['cm_name'] = 'Milk'
prediccionesSA<-rbind(prediccionesSA, setNames(milk, names(prediccionesSA)))

oil<-subset(datosSA,cm_name == 'Oil (vegetable)')
oil<-prediccion(oil)
oil['cm_name'] = 'Oil'
prediccionesSA<-rbind(prediccionesSA, setNames(oil, names(prediccionesSA)))

pasta<-subset(datosSA,cm_name == 'Pasta')
pasta<-prediccion(pasta)
pasta['cm_name'] = 'Pasta'
prediccionesSA<-rbind(prediccionesSA, setNames(pasta, names(prediccionesSA)))

plantains<-subset(datosSA,cm_name == 'Plantains')
plantains<-prediccion(plantains)
plantains['cm_name'] = 'Plantains'
prediccionesSA<-rbind(prediccionesSA, setNames(plantains, names(prediccionesSA)))

potatoes<-subset(datosSA,cm_name == 'Potatoes')
potatoes<-prediccion(potatoes)
potatoes['cm_name'] = 'Potatoes'
prediccionesSA<-rbind(prediccionesSA, setNames(potatoes, names(prediccionesSA)))

rice<-subset(datosSA,cm_name == 'Rice (ordinary, second quality)')
rice<-prediccion(rice)
rice['cm_name'] = 'Rice'
prediccionesSA<-rbind(prediccionesSA, setNames(rice, names(prediccionesSA)))

salt<-subset(datosSA,cm_name == 'Salt')
salt<-prediccion(salt)
salt['cm_name'] = 'Salt'
prediccionesSA<-rbind(prediccionesSA, setNames(salt, names(prediccionesSA)))

sugar<-subset(datosSA,cm_name == 'Sugar')
sugar<-prediccion(sugar)
sugar['cm_name'] = 'Sugar'
prediccionesSA<-rbind(prediccionesSA, setNames(sugar, names(prediccionesSA)))

tomatoes<-subset(datosSA,cm_name == 'Tomatoes')
tomatoes<-prediccion(tomatoes)
tomatoes['cm_name'] = 'Tomatoes'
prediccionesSA<-rbind(prediccionesSA, setNames(tomatoes, names(prediccionesSA)))

tortillas<-subset(datosSA,cm_name == 'Tortilla (maize)')
tortillas<-prediccion(tortillas)
tortillas['cm_name'] = 'Tortillas'
prediccionesSA<-rbind(prediccionesSA, setNames(tortillas, names(prediccionesSA)))
write.csv(datosGuatemala, file = "Suramerica")
