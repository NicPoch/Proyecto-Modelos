#----------------------paquetes-------------------------
library(readxl)
library(MASS)
library(fitdistrplus)
library(lubridate)
#-----------------Datos------------------------------
Casos_Reportados_Kennedy <- read_excel("Casos_Reportados.xlsx",sheet = "Casos Reportados Kennedy")
Casos_Reportados_Bosa <- read_excel("Casos_Reportados.xlsx",sheet = "Casos Reportados Bosa")
Casos_Reportados_Suba <- read_excel("Casos_Reportados.xlsx",sheet = "Casos Reportados Suba")
Validacion_Utilidades <- read_excel("Validacion_Utilidades.xlsx",col_types = c("date", "numeric"))
difk<-as.numeric(difftime(Casos_Reportados_Kennedy$Fecha,lag(Casos_Reportados_Kennedy$Fecha),units = c("hours")))
difk2<-hour(Casos_Reportados_Kennedy$Fecha)
#--------------Kennedy-------------------------
#Evaluar con p-value
cantidad<-rep(1,length(Casos_Reportados_Kennedy))

Casos_Reportados_Kennedy<-cbind(Casos_Reportados_Kennedy,cantidad)
Casos_Reportados_Kennedy$Hora<-hour(Casos_Reportados_Kennedy$Fecha)
Casos_Reportados_Kennedy$Dia<-day(Casos_Reportados_Kennedy$Fecha)
Casos_Reportados_Kennedy$Mes<-month(Casos_Reportados_Kennedy$Fecha)
Casos_Reportados_Kennedy$Ano<-year(Casos_Reportados_Kennedy$Fecha)

Casos_Reportados_Kennedy_Hora<-aggregate(x=Casos_Reportados_Kennedy$cantidad,by=list(Casos_Reportados_Kennedy$Hora,Casos_Reportados_Kennedy$Dia,Casos_Reportados_Kennedy$Mes,Casos_Reportados_Kennedy$Ano),FUN=sum)

colnames(Casos_Reportados_Kennedy_Hora)<-list("Hora","Dia","Mes","Ano","Casos")

plotdist(Casos_Reportados_Kennedy_Hora$Casos, histo=TRUE, demp=TRUE)
fdst<-fitdist(Casos_Reportados_Kennedy_Hora$Casos,distr = "exp")
tempEst<-gofstat(fdst)
print(tempEst)
#--------------Bosa-------------------------------------------------------------
cantidad<-rep(1,length(Casos_Reportados_Bosa))
Casos_Reportados_Bosa<-cbind(Casos_Reportados_Bosa,cantidad)
Casos_Reportados_Bosa$Hora<-hour(Casos_Reportados_Bosa$Fecha)
Casos_Reportados_Bosa$Dia<-day(Casos_Reportados_Bosa$Fecha)
Casos_Reportados_Bosa$Mes<-month(Casos_Reportados_Bosa$Fecha)
Casos_Reportados_Bosa$Ano<-year(Casos_Reportados_Bosa$Fecha)
Casos_Reportados_Bosa_Hora<-aggregate(x=Casos_Reportados_Bosa$cantidad,by=list(Casos_Reportados_Bosa$Hora,Casos_Reportados_Bosa$Dia,Casos_Reportados_Bosa$Mes,Casos_Reportados_Bosa$Ano),FUN=sum)
colnames(Casos_Reportados_Bosa_Hora)<-list("Hora","Dia","Mes","Ano","Casos")
plotdist(Casos_Reportados_Bosa_Hora$Casos, histo=TRUE, demp=TRUE)
fdst<-fitdist(Casos_Reportados_Bosa_Hora$Casos,distr = "exp")
tempEst<-gofstat(fdst)
summary(fdst)
#--------------Suba-------------------------------------------------------------
cantidad<-rep(1,length(Casos_Reportados_Suba))
Casos_Reportados_Suba<-cbind(Casos_Reportados_Suba,cantidad)
Casos_Reportados_Suba$Hora<-hour(Casos_Reportados_Suba$Fecha)
Casos_Reportados_Suba$Dia<-day(Casos_Reportados_Suba$Fecha)
Casos_Reportados_Suba$Mes<-month(Casos_Reportados_Suba$Fecha)
Casos_Reportados_Suba$Ano<-year(Casos_Reportados_Suba$Fecha)
Casos_Reportados_Suba_Hora<-aggregate(x=Casos_Reportados_Suba$cantidad,by=list(Casos_Reportados_Suba$Hora,Casos_Reportados_Suba$Dia,Casos_Reportados_Suba$Mes,Casos_Reportados_Suba$Ano),FUN=sum)
colnames(Casos_Reportados_Suba_Hora)<-list("Hora","Dia","Mes","Ano","Casos")
plotdist(Casos_Reportados_Suba_Hora$Casos, histo=TRUE, demp=TRUE)
fdst<-fitdist(Casos_Reportados_Suba_Hora$Casos,distr = "exp")
tempEst<-gofstat(fdst)
summary(fdst)
#-------------Encuesta de Seguridad--------------------------------------------
