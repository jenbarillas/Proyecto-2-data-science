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

datos<-read.csv('./foodPricesClean.csv')
# head(datos)
prediccionesGuatemala<-data.frame()
datosGuatemala<-subset(datos,adm0_name == 'Guatemala')
# head(datosGuatemala)
bananas<-subset(datosGuatemala,cm_name == 'Bananas')
bananas<-prediccion(bananas)
bananas['cm_name'] = 'Bananas'
prediccionesGuatemala<-rbind(prediccionesGuatemala, setNames(bananas, names(bananas)))


beans<-subset(datosGuatemala,cm_name == 'Beans')
beans<-prediccion(beans)
beans['cm_name'] = 'Beans'
prediccionesGuatemala<-rbind(prediccionesGuatemala, setNames(beans, names(prediccionesGuatemala)))


panes<-subset(datosGuatemala, cm_name == 'Bread')
panes<-prediccion(panes)
panes['cm_name'] = 'Bread'
prediccionesGuatemala<-rbind(prediccionesGuatemala, setNames(panes, names(prediccionesGuatemala)))


cheese<-subset(datosGuatemala,cm_name == 'Cheese')
cheese<-prediccion(cheese)
cheese['cm_name'] = 'Cheese'
prediccionesGuatemala<-rbind(prediccionesGuatemala, setNames(cheese, names(prediccionesGuatemala)))


coffee<-subset(datosGuatemala,cm_name == 'Coffee')
coffee<-prediccion(coffee)
coffee['cm_name'] = 'Coffee'
prediccionesGuatemala<-rbind(prediccionesGuatemala, setNames(coffee, names(prediccionesGuatemala)))


eggs<-subset(datosGuatemala,cm_name == 'Eggs')
eggs<-prediccion(eggs)
eggs['cm_name'] = 'Eggs'
prediccionesGuatemala<-rbind(prediccionesGuatemala, setNames(eggs, names(prediccionesGuatemala)))

fuel<-subset(datosGuatemala,cm_name == 'Fuel (petrol-gasoline)')
fuel<-prediccion(fuel)
fuel['cm_name'] = 'Fuel'
prediccionesGuatemala<-rbind(prediccionesGuatemala, setNames(fuel, names(prediccionesGuatemala)))

meat<-subset(datosGuatemala,cm_name == 'Meat (beef, chops with bones)')
meat<-prediccion(meat)
meat['cm_name'] = 'Meat'
prediccionesGuatemala<-rbind(prediccionesGuatemala, setNames(meat, names(prediccionesGuatemala)))

chicken<-subset(datosGuatemala,cm_name == 'Meat (chicken)')
chicken<-prediccion(chicken)
chicken['cm_name'] = 'Chicken'
prediccionesGuatemala<-rbind(prediccionesGuatemala, setNames(chicken, names(prediccionesGuatemala)))

milk<-subset(datosGuatemala,cm_name == 'Milk')
milk<-prediccion(milk)
milk['cm_name'] = 'Milk'
prediccionesGuatemala<-rbind(prediccionesGuatemala, setNames(milk, names(prediccionesGuatemala)))

oil<-subset(datosGuatemala,cm_name == 'Oil (vegetable)')
oil<-prediccion(oil)
oil['cm_name'] = 'Oil'
prediccionesGuatemala<-rbind(prediccionesGuatemala, setNames(oil, names(prediccionesGuatemala)))

pasta<-subset(datosGuatemala,cm_name == 'Pasta')
pasta<-prediccion(pasta)
pasta['cm_name'] = 'Pasta'
prediccionesGuatemala<-rbind(prediccionesGuatemala, setNames(pasta, names(prediccionesGuatemala)))

plantains<-subset(datosGuatemala,cm_name == 'Plantains')
plantains<-prediccion(plantains)
plantains['cm_name'] = 'Plantains'
prediccionesGuatemala<-rbind(prediccionesGuatemala, setNames(plantains, names(prediccionesGuatemala)))

potatoes<-subset(datosGuatemala,cm_name == 'Potatoes')
potatoes<-prediccion(potatoes)
potatoes['cm_name'] = 'Potatoes'
prediccionesGuatemala<-rbind(prediccionesGuatemala, setNames(potatoes, names(prediccionesGuatemala)))

rice<-subset(datosGuatemala,cm_name == 'Rice (ordinary, second quality)')
rice<-prediccion(rice)
rice['cm_name'] = 'Rice'
prediccionesGuatemala<-rbind(prediccionesGuatemala, setNames(rice, names(prediccionesGuatemala)))

salt<-subset(datosGuatemala,cm_name == 'Salt')
salt<-prediccion(salt)
salt['cm_name'] = 'Salt'
prediccionesGuatemala<-rbind(prediccionesGuatemala, setNames(salt, names(prediccionesGuatemala)))

sugar<-subset(datosGuatemala,cm_name == 'Sugar')
sugar<-prediccion(sugar)
sugar['cm_name'] = 'Sugar'
prediccionesGuatemala<-rbind(prediccionesGuatemala, setNames(sugar, names(prediccionesGuatemala)))

tomatoes<-subset(datosGuatemala,cm_name == 'Tomatoes')
tomatoes<-prediccion(tomatoes)
tomatoes['cm_name'] = 'Tomatoes'
prediccionesGuatemala<-rbind(prediccionesGuatemala, setNames(tomatoes, names(prediccionesGuatemala)))

tortillas<-subset(datosGuatemala,cm_name == 'Tortilla (maize)')
tortillas<-prediccion(tortillas)
tortillas['cm_name'] = 'Tortillas'
prediccionesGuatemala<-rbind(prediccionesGuatemala, setNames(tortillas, names(prediccionesGuatemala)))


