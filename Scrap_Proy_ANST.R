library(FactoMineR)
library(factoextra)
library(ggplot2)
library(readr)
library(dplyr)
library(ggthemes)
library(stringr)
library(tidyr)
library(tidyverse)
library(viridis)
library(rmutil)
library(STAR)
library(statmod)
library(invgamma)
library(gtools)
library(mvtnorm)
library(matrixStats)
library(ggpubr)
library(functional)
library(tseries)
library(zoo)
library(forecast)

# Gestion de Datos
#Leemos archivo
datos.R<- 
  read_csv('/Volumes/External/Analisis_STs/Series_Remesas_Proyecto_Team3.csv')
head(datos.R)

datos.R$Fecha<- as.Date(datos.R$Fecha, format = '%d / %m / %y')

#Obtenemos los Datos trimestrales
 datos.R$FechaTrimestral <- as.yearqtr(datos.R$Fecha, format= '%Yq%q')
 datos.R<- arrange(datos.R, FechaTrimestral)
 datos.R_Trim<- datos.R %>% group_by(FechaTrimestral) %>% summarise_all(mean)

###########################################
TS<- ts(datos.R_Trim[,3], start= c(1995,1), frequency = 12)

autoplot(TS)
TS_Decom<- decompose(TS)

TS_Estacional <- TS - TS_Decom$seasonal
autoplot(TS_Estacional)
autoplot(TS_Decom)
D2_TS<- diff(TS, lag = 1, differences = 3)

autoplot(D2_TS)
ggseasonplot(D2_TS)

ggsubseriesplot(D2_TS)

library(astsa)
library(MLmetrics)
ARIMA_max<- auto.arima(TS)
sarima_forecast <- sarima.for(TS, n.ahead=length(as.vector(datos.R[,3])),
                             p=0,d=1,q=1,P=1,D=1,Q=0,S=12)


