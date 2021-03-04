library(readxl)
Encuesta_Seguridad <- read_excel("Encuesta_Seguridad.xlsx")
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
infoEncuesta$`Estado N Policías`<-round(infoEncuesta$`Estado N Policías`,digits = 0)
infoEncuesta$`Estado N+1 Policías`<-round(infoEncuesta$`Estado N+1 Policías`,digits = 0)

infoEncuestaAgg<-aggregate(x=infoEncuesta$Freq,by=list(infoEncuesta$`Estado N Percepción`,infoEncuesta$`Estado N Policías`,infoEncuesta$`Estado N+1 Percepción`,infoEncuesta$`Estado N+1 Policías`),FUN=sum)
colnames(infoEncuestaAgg)<-list("Estado N Percepción","Estado N Policías","Estado N+1 Percepción","Estado N+1 Policías","Freq")

probabilidadesNN1<-matrix(0,ncol=5,nrow = 1)
probabilidadesNN1<-data.frame(probabilidadesNN1)
colnames(probabilidadesNN1)<-list("Percepcion N","Policias N","Percepcion N+1","Policias N+1","Prob")
#Conseguir el subconjunto N
for(pn in unique(infoEncuestaAgg$`Estado N Percepción`))
{
  tempPerc<-infoEncuestaAgg[infoEncuestaAgg$`Estado N Percepción`==pn,]
  #Conseguir el subconjunto de Cantidad de policias
  #Se define el estado N
  for(cn in unique(infoEncuestaAgg$`Estado N Policías`))
  {
    tempPercPol<-tempPerc[tempPerc$`Estado N Policías`==cn,]
    totalEv<-sum(tempPercPol$Freq)
    print(tempPercPol)
    #Conaseguir la percepción en N+1
    for(pn1 in unique(infoEncuestaAgg$`Estado N+1 Percepción`))
    {
      tempPercPolPerc1<-tempPercPol[tempPercPol$`Estado N+1 Percepción`==pn1,]
      for(cn1 in unique(infoEncuestaAgg$`Estado N+1 Policías`))
      {
        tempPercPolPerc1Pol1<-tempPercPolPerc1[tempPercPolPerc1$`Estado N+1 Policías`==cn1,]
        ansP<-0
        if(length(tempPercPolPerc1Pol1$Freq)!=0)
        {
          ansP<-sum(tempPercPolPerc1Pol1$Freq[1])/totalEv
          print(c(ansP,totalEv))
        }
        ans<-data.frame(list(pn,cn,pn1,cn1,ansP))
        colnames(ans)<-list("Percepcion N","Policias N","Percepcion N+1","Policias N+1","Prob")
        probabilidadesNN1<-rbind(probabilidadesNN1,ans)
      }
    }
  }
}
View(probabilidadesNN1)