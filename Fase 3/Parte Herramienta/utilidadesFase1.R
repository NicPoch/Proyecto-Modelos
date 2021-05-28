library(markovchain)
library(expm)
library(plotly)
library(readxl)
library(plotly)
library(MASS)
library(fitdistrplus)

utilidades<-function(combustible,dotacion,salario)
{
  #-------------------------Datos------------------------------------
  Encuesta_Seguridad <- read_excel("Encuesta_Seguridad.xlsx")
  #-----------------------Preprocesar--------------------------------
  infoEncuesta<-matrix(0,ncol = 5,nrow = 1)
  infoEncuesta<-data.frame(infoEncuesta)
  colnames(infoEncuesta)<-list("Estado N Percepcion","Estado N policias","Estado N+1 Percepcion","Estado N+1 policias","Freq")
  dLength<-length(Encuesta_Seguridad$Semana)
  for(i in 1:dLength)
  {
    temp<-data.frame(list(Encuesta_Seguridad[i,"Percepcion"],as.integer((Encuesta_Seguridad[i,"Numero de policias"]-100)/500),Encuesta_Seguridad[i+1,"Percepcion"],as.integer((Encuesta_Seguridad[i+1,"Numero de policias"]-100)/500),1))
    colnames(temp)<-list("Estado N Percepcion","Estado N policias","Estado N+1 Percepcion","Estado N+1 policias","Freq")
    infoEncuesta<-rbind(infoEncuesta,temp)
  }
  infoEncuesta<-infoEncuesta[2:dLength,]
  infoEncuesta$`Estado N policias`<-round(infoEncuesta$`Estado N policias`,digits = 0)
  infoEncuesta$`Estado N+1 policias`<-round(infoEncuesta$`Estado N+1 policias`,digits = 0)
  infoEncuestaAgg<-aggregate(x=infoEncuesta$Freq,by=list(infoEncuesta$`Estado N Percepcion`,infoEncuesta$`Estado N policias`,infoEncuesta$`Estado N+1 Percepcion`,infoEncuesta$`Estado N+1 policias`),FUN=sum)
  colnames(infoEncuestaAgg)<-list("Estado N Percepcion","Estado N policias","Estado N+1 Percepcion","Estado N+1 policias","Freq")
  probabilidadesNN1<-matrix(0,ncol=5,nrow = 1)
  probabilidadesNN1<-data.frame(probabilidadesNN1)
  colnames(probabilidadesNN1)<-list("Percepcion N","Policias N","Percepcion N+1","Policias N+1","Prob")
  #----------------Calcular probabilidades P[X_n+1=j|X_n=i]--------------------------------------------
  for(pn in unique(infoEncuestaAgg$`Estado N Percepcion`))
  {
    tempPerc<-infoEncuestaAgg[infoEncuestaAgg$`Estado N Percepcion`==pn,]
    for(cn in unique(infoEncuestaAgg$`Estado N policias`))
    {
      tempPercPol<-tempPerc[tempPerc$`Estado N policias`==cn,]
      totalEv<-sum(tempPercPol$Freq)
      for(pn1 in unique(infoEncuestaAgg$`Estado N+1 Percepcion`))
      {
        tempPercPolPerc1<-tempPercPol[tempPercPol$`Estado N+1 Percepcion`==pn1,]
        for(cn1 in unique(infoEncuestaAgg$`Estado N+1 policias`))
        {
          tempPercPolPerc1Pol1<-tempPercPolPerc1[tempPercPolPerc1$`Estado N+1 policias`==cn1,]
          ansP<-0
          if(length(tempPercPolPerc1Pol1$Freq)!=0)
          {
            ansP<-sum(tempPercPolPerc1Pol1$Freq[1])/totalEv
          }
          ans<-data.frame(list(pn,cn,pn1,cn1,ansP))
          colnames(ans)<-list("Percepcion N","Policias N","Percepcion N+1","Policias N+1","Prob")
          probabilidadesNN1<-rbind(probabilidadesNN1,ans)
        }
      }
    }
  }
  elementos<-2:length(probabilidadesNN1$Prob)
  probabilidadesNN1<-probabilidadesNN1[elementos,]
  #------------------------------Cadena de markov---------------------------------------------
  estados<-unique(probabilidadesNN1[c("Percepcion N","Policias N")])
  estadosFinales<-paste(estados$`Percepcion N`,estados$`Policias N`,sep=",")
  matrizP<-matrix(0,ncol=length(estadosFinales),nrow=length(estadosFinales))
  dimnames(matrizP)<-list(estadosFinales,estadosFinales)
  numEstados<-length(estados$`Percepcion N`)
  for(i in estadosFinales)
  {
    N_percepcion=strsplit(i, split =",")[[1]][1]
    N_policias=as.numeric(strsplit(i, split =",")[[1]][2])
    for(j in estadosFinales)
    {
      N1_percepcion=strsplit(j, split =",")[[1]][1]
      N1_policias=as.numeric(strsplit(j, split =",")[[1]][2])
      tempProbs<-probabilidadesNN1[probabilidadesNN1$`Percepcion N`==N_percepcion,]
      tempProbs<-tempProbs[tempProbs$`Policias N`==N_policias,]
      tempProbs<-tempProbs[tempProbs$`Percepcion N+1`==N1_percepcion,]
      tempProbs<-tempProbs[tempProbs$`Policias N+1`==N1_policias,]
      if(length(tempProbs$Prob)==0)
      {
        matrizP[i,j]<-0
      }
      else
      {
        matrizP[i,j]<-tempProbs$Prob[1]
      }
    }
  }
  CMTD_Percepcion<-new("markovchain",states=colnames(matrizP),transitionMatrix=matrizP)
  View(matrizP)
  #---------------------------------------Steady States--------------------------------------
  poliPorEstado<-matrix(c(rep(c(2,0,1,3,4),3)),ncol = 1,nrow = 15)
  steadyStatesPerc<-steadyStates(CMTD_Percepcion)
  #Ingresos por Percepcion
  ingresos<-c(rep(50000437450,5),rep(43330151900,5),rep(30080406700,5))
  ingresos<-matrix(ingresos,nrow = length(ingresos),ncol = 1)
  #Egresos por mantenimiento
  egresos<-rep(c((salario+dotacion+combustible)*((2*500)+350),(salario+dotacion+combustible)*((0*500)+350),(salario+dotacion+combustible)*((1*500)+350),(salario+dotacion+combustible)*((3*500)+350),(salario+dotacion+combustible)*((4*500)+350)),3)
  egresos<-matrix(egresos,nrow = length(ingresos),ncol = 1)
  View((steadyStatesPerc%*%ingresos)*0.03)
  View((steadyStatesPerc%*%egresos))
  
  utilidad<-((steadyStatesPerc%*%ingresos)*0.03)-(steadyStatesPerc%*%egresos)
  return(utilidad[1,1]*4)
}