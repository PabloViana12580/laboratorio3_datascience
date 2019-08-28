#Universidad del Valle de Guatemala
# Catedrática: Lynette García 
# Pablo Viana - 16091
# Segio Marchena - 16
# Laboratorio numero 3

install.packages("forecast")
install.packages("fUnitRoots")
install.packages("ggfortify")

library(forecast)
library(tseries)
library(fUnitRoots)
library(ggfortify)


# Datos de importacion de gasolina
importacion.gasolina <- read.csv("datosImp.csv")

importacion.gasolina[importacion.gasolina$Anio>=2018,"Diesel"] <- importacion.gasolina[importacion.gasolina$Anio>=2018,"DieselLS"]
importacion.gasolina$DieselLS <- NULL

imp_diesel <- importacion.gasolina[1:2]
imp_diesel<- importacion.gasolina$Diesel

#serie univariante de diesel
impdiesel <- as.ts(imp_diesel, start = c(2001, 1), end = c(2019, 6), frequency = 12)
start(impdiesel)
end(impdiesel)
plot(impdiesel)

plot(aggregate(impdiesel,FUN=mean))
dec.import<-decompose(imp_diesel)
plot(imp_diesel)
plot(dec.import$seasonal)

#Aplicaremos una transformación logarítmica
logdiesel <- log(imp_diesel)
plot(decompose(logdiesel))

#Ver el gráfico de la serie
plot(logAirPassengers)

#Para saber si hay raíces unitarias
adfTest(logdiesel)
adfTest(diff(logdiesel))
#Gráfico de autocorrelación
acf(logdiesel)
# funciones de autocorrelación y autocorrelación parcial
acf(diff(logdiesel),12)
pacf(diff(logdiesel))

# Hacer el modelo

auto.arima(impdiesel)

fit <- arima(log(imp_diesel), c(0, 3, 2),seasonal = list(order = c(0, 1, 1), period = 12))
pred <- predict(fit, n.ahead = 10*12)