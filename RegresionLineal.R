prediccion<-function(arreglo, year=TRUE, month=FALSE){
  if (year) {
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
datosGuatemala<-subset(datos,adm0_name == 'Guatemala')
# head(datosGuatemala)
bananas<-subset(datosGuatemala,cm_name == 'Bananas')

bananas<-prediccion(bananas)
