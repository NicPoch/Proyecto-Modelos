#----------------------paquetes-------------------------
library(readxl)
library(MASS)
library(fitdistrplus)
library(lubridate)
library(dplyr)
#-----------------Datos------------------------------
Casos_Reportados_Kennedy <- read_excel("Casos_Reportados.xlsx",sheet = "Casos Reportados Kennedy")
Casos_Reportados_Bosa <- read_excel("Casos_Reportados.xlsx",sheet = "Casos Reportados Bosa")
Casos_Reportados_Suba <- read_excel("Casos_Reportados.xlsx",sheet = "Casos Reportados Suba")
#--------------Kennedy-------------------------
difK<-as.numeric(difftime(Casos_Reportados_Kennedy$Fecha ,lag(Casos_Reportados_Kennedy$Fecha) , units = c("hours")))
difK<-difK[2:length(difK)]
plotdist(difK)
descdist(difK,boot = 1000)
fdistK<-fitdist(difK,"exp")
plot(fdistK)
gfstatK<-gofstat(fdistK)
View(gfstatK)
#-------------Bosa---------------------------------------------------------
difB<-as.numeric(difftime(Casos_Reportados_Bosa$Fecha ,lag(Casos_Reportados_Bosa$Fecha) , units = c("hours")))
difB<-difB[2:length(difB)]
plotdist(difB)
descdist(difB,boot = 1000)
fdistB<-fitdist(difB,"exp")
plot(fdistB)
gfstatB<-gofstat(fdistB)
View(gfstatB)
#-------------Suba---------------------------------------------------------
difS<-as.numeric(difftime(Casos_Reportados_Suba$Fecha ,lag(Casos_Reportados_Suba$Fecha) , units = c("hours")))
difS<-difS[2:length(difS)]
plotdist(difS)
descdist(difS,boot = 1000)
fdistS<-fitdist(difS,"exp")
plot(fdistS)
gfstatS<-gofstat(fdistS)
View(gfstatS)