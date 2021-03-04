#----------------------paquetes-------------------------
library(readxl)
library(MASS)
library(fitdistrplus)
library(lubridate)
#-----------------Datos------------------------------
Encuesta_Seguridad <- read_excel("Encuesta_Seguridad.xlsx")
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
#Por Métodos de montecarlo
infoEncuesta<-matrix(0,ncol = 5,nrow = 1)
infoEncuesta<-data.frame(infoEncuesta)
colnames(infoEncuesta)<-list("Estado N Percepción","Estado N Policías","Estado N+1 Percepción","Estado N+1 Policías","Freq")
dLength<-length(Encuesta_Seguridad$Semana)

for(i in 1:dLength)
{
  temp<-data.frame(list(Encuesta_Seguridad[i,"Percepción"],round((Encuesta_Seguridad[i,"Número de policías"]-100)/500),Encuesta_Seguridad[i+1,"Percepción"],(Encuesta_Seguridad[i+1,"Número de policías"]-100)/500),1)
  colnames(temp)<-list("Estado N Percepción","Estado N Policías","Estado N+1 Percepción","Estado N+1 Policías","Freq")
  infoEncuesta<-rbind(infoEncuesta,temp)
}
infoEncuesta<-infoEncuesta[2:dLength,]
infoEncuesta$`Estado N Policías`<-round(infoEncuesta$`Estado N Policías`,digits = 2)
infoEncuesta$`Estado N+1 Policías`<-round(infoEncuesta$`Estado N Policías`,digits = 2)

infoEncuestaAgg<-aggregate(x=infoEncuesta$Freq,by=list(infoEncuesta$`Estado N Percepción`,infoEncuesta$`Estado N Policías`,infoEncuesta$`Estado N+1 Percepción`,infoEncuesta$`Estado N+1 Policías`),FUN=sum)
colnames(infoEncuestaAgg)<-list("Estado N Percepción","Estado N Policías","Estado N+1 Percepción","Estado N+1 Policías","Freq")

probabilidadesNN1<-matrix(0,ncol=5,nrow = 1)
probabilidadesNN1<-data.frame(probabilidadesNN1)
colnames(probabilidadesNN1)<-list("Percepcion N","Policias N","Percepcion N+1","Policias N+1","Prob")
for(pn in unique(infoEncuestaAgg$`Estado N Percepción`))
{
  tempPerc<-infoEncuestaAgg[infoEncuestaAgg$`Estado N Percepción`==pn,]
  for(cn in unique(infoEncuestaAgg$`Estado N Policías`))
  {
    tempPercPol<-tempPerc[tempPerc$`Estado N Policías`==cn,]
    totalEv<-sum(tempPercPol$Freq)
    for(pn1 in unique(infoEncuestaAgg$`Estado N+1 Percepción`))
    {
      tempPercPolPerc1<-tempPerc[tempPercPol$`Estado N+1 Percepción`==pn1,]
      print(tempPercPolPerc1)
      for(cn1 in unique(infoEncuestaAgg$`Estado N+1 Policías`))
      {
        tempPercPolPerc1Pol1<-tempPercPolPerc1[tempPercPolPerc1$`Estado N Policías`,]
        ansP<-0
        if(length(tempPercPolPerc1Pol1$Freq)!=0)
        {
          ansP<-sum(tempPercPol$Freq)/totalEv
        }
        ans<-data.frame(list(pn,cn,pn1,cn1,ansP))
        colnames(ans)<-list("Percepcion N","Policias N","Percepcion N+1","Policias N+1","Prob")
        probabilidadesNN1<-rbind(probabilidadesNN1,ans)
      }
    }
  }
}
View(probabilidadesNN1)

Percepciones<-unique(infoEncuesta$`Estado N Percepción`)
CantidadPolicias<-0:5
CantidadPoliciasXPercepciones<-expand.grid(CantidadPolicias,Percepciones)
S_percepcion<-c()
for(i in 1:18)
{
  S_percepcion<-c(S_percepcion,c(paste(CantidadPoliciasXPercepciones$Var1, CantidadPoliciasXPercepciones$Var2, sep = ",")))
}






estadoNBueno<-infoEncuesta[infoEncuesta$`Estado N`=="Buena",]
estadoNRegular<-infoEncuesta[infoEncuesta$`Estado N`=="Regular",]
estadoNMalo<-infoEncuesta[infoEncuesta$`Estado N`=="Mala",]

plotdist(estadoNBueno$Delta,histo = T,demp = T,breaks = 20)
plotdist(estadoNBueno$Delta[estadoNBueno$`Estado N+1`=="Buena"],histo = T,demp = T)
plotdist(estadoNBueno$Delta[estadoNBueno$`Estado N+1`=="Regular"],histo = T,demp = T)
plotdist(estadoNBueno$Delta[estadoNBueno$`Estado N+1`=="Mala"],histo = T,demp = T)

plotdist(Encuesta_Seguridad$`Número de policías`[Encuesta_Seguridad$Percepción=="Buena"],histo = T,demp = T,breaks = 20)
plot(fitdist(Encuesta_Seguridad$`Número de policías`[Encuesta_Seguridad$Percepción=="Buena"],"norm"))
summary(fitdist(Encuesta_Seguridad$`Número de policías`[Encuesta_Seguridad$Percepción=="Buena"],"norm"))

descdist(estadoNBueno$Delta,boot=1000)
descdist(estadoNBueno$Delta[estadoNBueno$`Estado N+1`=="Buena"],boot=1000)
descdist(estadoNBueno$Delta[estadoNBueno$`Estado N+1`=="Regular"],boot=1000)
descdist(estadoNBueno$Delta[estadoNBueno$`Estado N+1`=="Mala"],boot=1000)

plot(fitdist((estadoNRegular$Delta),"pois"),breaks=10)
plot(fitdist(estadoNRegular$Delta[estadoNRegular$`Estado N+1`=="Buena"],"norm"),breaks=10)
plot(fitdist(estadoNRegular$Delta[estadoNRegular$`Estado N+1`=="Regular"],"norm"),breaks=10)
plot(fitdist(estadoNRegular$Delta[estadoNRegular$`Estado N+1`=="Buena"],"norm"),breaks=10)

plot(fitdist(estadoNMalo$Delta,"norm"),breaks=10)
plot(fitdist(estadoNMalo$Delta[estadoNMalo$`Estado N+1`=="Buena"],"norm"),breaks=10)
plot(fitdist(estadoNMalo$Delta[estadoNMalo$`Estado N+1`=="Regular"],"norm"),breaks=10)
plot(fitdist(estadoNMalo$Delta[estadoNMalo$`Estado N+1`=="Mala"],"norm"),breaks=10)