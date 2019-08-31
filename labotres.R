#Universidad del Valle de Guatemala
# Catedrática: Lynette García 
# Pablo Viana - 16091
# Segio Marchena - 16387
# Laboratorio numero 3

#Paquetes a utilizar 
install.packages("forecast")
install.packages("fUnitRoots")
install.packages("ggfortify")
install.packages("xts")

library(forecast)
library(tseries)
library(fUnitRoots)
library(ggfortify)
library(xts)
library(dplyr)

# ANALISIS EXPLORATORIO 
datos<-read.csv("datosImp.csv")

#meses
meses<-c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago","Sep", "Oct", "Nov", "Dic")
#meses2
meses2<-c("Ene", "Feb", "Mar", "Abr", "May", "Jun")

#Unificación de Gasolina DieselLS y Diesel normal
datos[datos$Anio>=2018,"Diesel"] <- datos[datos$Anio>=2018,"DieselLS"]
datos$DieselLS<-NULL
datos$DieselULS<-NULL

#División de datos por año
anio2001<-datos[datos$Anio == 2001,]
anio2002<-datos[datos$Anio == 2002,]
anio2003<-datos[datos$Anio == 2003,]
anio2004<-datos[datos$Anio == 2004,]
anio2005<-datos[datos$Anio == 2005,]
anio2006<-datos[datos$Anio == 2006,]
anio2007<-datos[datos$Anio == 2007,]
anio2008<-datos[datos$Anio == 2008,]
anio2009<-datos[datos$Anio == 2009,]
anio2010<-datos[datos$Anio == 2010,]
anio2011<-datos[datos$Anio == 2011,]
anio2012<-datos[datos$Anio == 2012,]
anio2013<-datos[datos$Anio == 2013,]
anio2014<-datos[datos$Anio == 2014,]
anio2015<-datos[datos$Anio == 2015,]
anio2016<-datos[datos$Anio == 2016,]
anio2017<-datos[datos$Anio == 2017,]
anio2018<-datos[datos$Anio == 2018,]
anio2019<-datos[datos$Anio == 2019,]


# PICOS DE IMPORTACIONES POR ANIO DE GASOLINA SUPER
barplot(anio2001$GasSuperior, names.arg = meses, main = "Importacion de Super en 2001")
barplot(anio2002$GasSuperior, names.arg = meses, main = "Importacion de Super en 2002")
barplot(anio2003$GasSuperior, names.arg = meses, main = "Importacion de Super en 2003")
barplot(anio2004$GasSuperior, names.arg = meses, main = "Importacion de Super en 2004")
barplot(anio2005$GasSuperior, names.arg = meses, main = "Importacion de Super en 2005")
barplot(anio2006$GasSuperior, names.arg = meses, main = "Importacion de Super en 2006")
barplot(anio2007$GasSuperior, names.arg = meses, main = "Importacion de Super en 2007")
barplot(anio2008$GasSuperior, names.arg = meses, main = "Importacion de Super en 2008")
barplot(anio2009$GasSuperior, names.arg = meses, main = "Importacion de Super en 2009")
barplot(anio2010$GasSuperior, names.arg = meses, main = "Importacion de Super en 2010")
barplot(anio2011$GasSuperior, names.arg = meses, main = "Importacion de Super en 2011")
barplot(anio2012$GasSuperior, names.arg = meses, main = "Importacion de Super en 2012")
barplot(anio2013$GasSuperior, names.arg = meses, main = "Importacion de Super en 2013")
barplot(anio2014$GasSuperior, names.arg = meses, main = "Importacion de Super en 2014")
barplot(anio2015$GasSuperior, names.arg = meses, main = "Importacion de Super en 2015")
barplot(anio2016$GasSuperior, names.arg = meses, main = "Importacion de Super en 2016")
barplot(anio2017$GasSuperior, names.arg = meses, main = "Importacion de Super en 2017")
barplot(anio2018$GasSuperior, names.arg = meses, main = "Importacion de Super en 2018")
barplot(anio2019$GasSuperior, names.arg = meses2,main = "Importacion de Super en 2019")

totalSuper<-c(mean(anio2001$GasSuperior), mean(anio2002$GasSuperior),mean(anio2003$GasSuperior),
              mean(anio2004$GasSuperior), mean(anio2005$GasSuperior),mean(anio2006$GasSuperior),
              mean(anio2007$GasSuperior), mean(anio2008$GasSuperior),mean(anio2009$GasSuperior), mean(anio2010$GasSuperior),
              mean(anio2011$GasSuperior), mean(anio2012$GasSuperior),mean(anio2013$GasSuperior),
              mean(anio2014$GasSuperior), mean(anio2015$GasSuperior),mean(anio2016$GasSuperior), 
              mean(anio2017$GasSuperior), mean(anio2018$GasSuperior),mean(anio2019$GasSuperior))

totalRegular<-c(mean(anio2001$GasRegular), mean(anio2002$GasRegular),mean(anio2003$GasRegular),
                mean(anio2004$GasRegular), mean(anio2005$GasRegular),mean(anio2006$GasRegular),
                mean(anio2007$GasRegular), mean(anio2008$GasRegular),mean(anio2009$GasRegular), mean(anio2010$GasRegular),
                mean(anio2011$GasRegular), mean(anio2012$GasRegular),mean(anio2013$GasRegular),
                mean(anio2014$GasRegular), mean(anio2015$GasRegular),mean(anio2016$GasRegular), 
                mean(anio2017$GasRegular), mean(anio2018$GasRegular),mean(anio2019$GasRegular))

totalDiesel<-c(mean(anio2001$Diesel), mean(anio2002$Diesel),mean(anio2003$Diesel),
               mean(anio2004$Diesel), mean(anio2005$Diesel),mean(anio2006$Diesel),
               mean(anio2007$Diesel), mean(anio2008$Diesel),mean(anio2009$Diesel), mean(anio2010$Diesel),
               mean(anio2011$Diesel), mean(anio2012$Diesel),mean(anio2013$Diesel),
               mean(anio2014$Diesel), mean(anio2015$Diesel),mean(anio2016$Diesel), 
               mean(anio2017$Diesel), mean(anio2018$Diesel),mean(anio2019$Diesel))

totalTotal<-c(mean(anio2001$Total),  mean(anio2002$Total),mean(anio2003$Total),
              mean(anio2004$Total), mean(anio2005$Total),mean(anio2006$Total),
              mean(anio2007$Total), mean(anio2008$Total),mean(anio2009$Total),
              mean(anio2010$Total), mean(anio2011$Total),mean(anio2012$Total),
              mean(anio2013$Total), mean(anio2014$Total),mean(anio2015$Total),
              mean(anio2016$Total), mean(anio2017$Total),mean(anio2018$Total),
              mean(anio2019$Total))

#Anos
anios<-c(2001, 2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019)
barplot(totalSuper, names.arg = anios, main = "Importacion Promedio de GasSuperior")
barplot(totalRegular, names.arg = anios, main = "Importacion Promedio de GasRegular")
barplot(totalDiesel, names.arg = anios, main = "Importacion Promedio de GasRegular")
barplot(totalTotal, names.arg = anios, main = "Importacion Promedio Total")


# normalidad
qqnorm(datos$Total)
qqplot(datos$Anio, datos$Total)

hist(totalSuper, breaks = 10, main = "Histograma de Gasolina Super")
hist(totalRegular, breaks = 10, main = "Histograma de Gasolina Regular")
hist(totalTotal, breaks = 10, main = "Histograma del Total de Importaciones")

# ------------------------ PREGUNTA 2 -----------------------------

#Serie univariante de Diesel

#Hacemos una tabla unicamente con los valores de año, mes y diesel importado
importacion.diesel <- datos[,c(1,2,9)]

#Pegamos en una misma columna el año, el mes y agregamos un 1. Esto para obtener un formato de fecha estándar Y-M-D
importacion.diesel$Anio <- paste(importacion.diesel$Anio, importacion.diesel$Mes, 1, sep="-")
importacion.diesel$Mes <- NULL

#Cambiamos la clase de la variable año a Date
importacion.diesel <- mutate(importacion.diesel, Anio = as.Date(Anio, format= "%Y-%m-%d"))
#Creamos una serie de tiempo con las fechas y volumenes de importacion de diesel
serietiempo.diesel <- xts(importacion.diesel$Diesel, order.by = importacion.diesel$Anio, frequency = 12)
serietiempo.diesel <- ts(serietiempo.diesel, start=c(2001,1), end=c(2019,6), frequency = 12)

#Inicio de la serie
start(serietiempo.diesel)
#Final de la serie
end(serietiempo.diesel)
#Frecuencia de la serie
frequency(serietiempo.diesel)
#Grafico de la importacion de 2001 a 2019
plot(serietiempo.diesel, type="l", main="Serie de tiempo de importación Diesel", sub="El volumen  está  dado  en  barriles  de  42  galones", xlab="años", ylab="Volumen importado")
abline(reg=lm(serietiempo.diesel~time(serietiempo.diesel)), col=c("red"))
#Gráfico sobre el comportamiento de la media en subsets de los años
plot(aggregate(serietiempo.diesel,FUN=mean))
#descomponemos la serie de tiempo en tendencia, estacionaridad e innovación
decompose_diesel<-decompose(serietiempo.diesel)
plot(decompose_diesel)
plot(decompose_diesel$seasonal)

 
#Aplicaremos una transformación logarítmica
logdiesel <- log(serietiempo.diesel)
plot(logdiesel, type="l", main="Serie de tiempo de importación Diesel con transformación logarítmica", sub="El volumen  está  dado  en  barriles  de  42  galones", xlab="años", ylab="Volumen importado")
abline(reg=lm(logdiesel~time(logdiesel)), col=c("red"))

#Gráfico de autocorrelación
acf(logdiesel, type="correlation", lag.max = 100, main = "Gráfico de correlación serie de tiempo importacion Diesel" )

#Utilización de prueba Dickey-Fuller para raíces unitarias
adfTest(logdiesel)
adfTest(diff(logdiesel))

# funciones de autocorrelación y autocorrelación parcial
acf(diff(logdiesel),12) #2 valores que pasan la linea punteada
pacf(diff(logdiesel)) #6 valores pasan la linea punteada
#Funcion autoarima para encontrar valores propuestos de p, d y q
auto.arima(serietiempo.diesel) #5, 1, 1

# Al utilizar la funcion de auto-arima los valores del output no cambian
# en más de 1 número con los obtenidos a traves de las funciones de autocorrelacion
# y autocorrelacion parcial, por lo que la funcion autoarima pareciera tener coherencia
# y sentido con el modelo
# 
fit <- arima(logdiesel, c(6, 1, 2),seasonal = list(order = c(0, 1, 1), period = 12))
fit.2 <- arima(logdiesel, c(6, 1, 2),seasonal = list(order = c(0, 1, 0), period = 12))
#Utilizando valores p, d y q obtenidos en autoarima
fit.3 <- arima(logdiesel, c(5, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))

#predicciones con cada uno de los modelos propuestos
pred <- predict(fit, n.ahead = 10*12)
pred.1 <- predict(fit.2, n.ahead = 10*12)
pred.2 <- predict(fit.3, n.ahead = 10*12)

ts.plot(serietiempo.diesel,2.718^pred$pred, log = "y", lty = c(1,3))
