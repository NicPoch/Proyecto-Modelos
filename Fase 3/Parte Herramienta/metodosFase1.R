#-----------------------Paquetes------------------------------------
library(markovchain)
library(expm)
library(plotly)
library(readxl)
library(plotly)
library(MASS)
library(fitdistrplus)
matrixQFunc<-function(maxC,lambdaIn,lambdaOut,policias)
{
  estados<-0:maxC
  matrizQ<-matrix(0,ncol=maxC+1,nrow=maxC+1)
  dimnames(matrizQ)<-list(estados,estados)
  for(i in estados)
  {
    for(j in estados)
    {
      if(j==i+1)
      {
        matrizQ[i+1,j+1]<-lambdaIn
      }
      else if(j==i-1)
      {
        matrizQ[i+1,j+1]<-lambdaOut*min(c(i,policias))
      }
      else
      {
        matrizQ[i+1,j+1]<-0
      }
    }
  }
  for(d in estados)
  {
    matrizQ[d+1,d+1]<- -1*sum(matrizQ[d+1,])
  }
  return(matrizQ)
}
valoresEsperados<-function(matrizQKennedy,matrizQBosa,matrizQSuba)
{
  horas<-0:(14*24)
  #----------------------------------Valores esperados--------------------------------------------
  #-----------------------------Kennedy------------------------------------------------------------
  estadosK<-0:40
  estadosK<-matrix(estadosK,ncol = 41,nrow=1)
  alphaK<-rep(0,41)
  alphaK[1]<-1
  alphaK<-matrix(alphaK,ncol = 41,nrow=1)
  datosK<-matrix(0,ncol=2,nrow = length(horas)+1)
  for(hora in horas)
  {
    datosK[hora+1,1]<-hora
    pi_t<-alphaK%*%expm(matrizQKennedy*hora)
    datosK[hora+1,2]<-(estadosK%*%t(pi_t))[1,1]
  }
  datosK<-data.frame(datosK)
  #-----------------------------Bosa------------------------------------------------------------
  estadosB<-0:31
  estadosB<-matrix(estadosB,ncol = 32,nrow=1)
  alphaB<-rep(0,32)
  alphaB[1]<-1
  alphaB<-matrix(alphaB,ncol = 32,nrow=1)
  datosB<-matrix(0,ncol=2,nrow = length(horas)+1)
  for(hora in horas)
  {
    datosB[hora+1,1]<-hora
    pi_t<-alphaB%*%expm(matrizQBosa*hora)
    datosB[hora+1,2]<-(estadosB%*%t(pi_t))[1,1]
  }
  datosB<-data.frame(datosB)
  #-----------------------------Suba------------------------------------------------------------
  estadosS<-0:26
  estadosS<-matrix(estadosS,ncol = 27,nrow=1)
  alphaS<-rep(0,27)
  alphaS[1]<-1
  alphaS<-matrix(alphaS,ncol = 27,nrow=1)
  datosS<-matrix(0,ncol=2,nrow = length(horas)+1)
  for(hora in horas)
  {
    datosS[hora+1,1]<-hora
    pi_t<-alphaS%*%expm(matrizQSuba*hora)
    datosS[hora+1,2]<-(estadosS%*%t(pi_t))[1,1]
  }
  datosS<-data.frame(datosS)
  #----------------------Gráfica final------------------------------------------------------------
  EF<-datosS[,1]
  EF<-cbind(EF,datosK[,2])
  EF<-cbind(EF,datosS[,2])
  EF<-cbind(EF,datosB[,2])
  EF<-data.frame(EF)
  return(EF)
}
valorVarianzas<-function(matrizQKennedy,matrizQBosa,matrizQSuba)
{
  horas<-0:(14*24)
  #-----------------------------Kennedy------------------------------------------------------------
  estadosK<-0:40
  estadosK<-matrix(estadosK,ncol = 41,nrow=1)
  alphaK<-rep(0,41)
  alphaK[1]<-1
  alphaK<-matrix(alphaK,ncol = 41,nrow=1)
  datosK<-matrix(0,ncol=2,nrow = length(horas)+1)
  for(hora in horas)
  {
    datosK[hora+1,1]<-hora
    pi_t<-alphaK%*%expm(matrizQKennedy*hora)
    datosK[hora+1,2]<-(estadosK%*%t(pi_t))[1,1]
  }
  datosK<-data.frame(datosK)
  estadosK<-estadosK^2
  alphaK<-rep(0,41)
  alphaK[1]<-1
  alphaK<-matrix(alphaK,ncol = 41,nrow=1)
  varianzaK<-matrix(0,ncol=2,nrow = length(horas)+1)
  for(hora in horas)
  {
    varianzaK[hora+1,1]<-hora
    pi_t<-alphaK%*%expm(matrizQKennedy*hora)
    varianzaK[hora+1,2]<-(estadosK%*%t(pi_t))[1,1]-datosK[hora+1,2]^2
  }
  varianzaK<-data.frame(varianzaK)
  #-----------------------------Bosa------------------------------------------------------------
  estadosB<-0:31
  estadosB<-matrix(estadosB,ncol = 32,nrow=1)
  alphaB<-rep(0,32)
  alphaB[1]<-1
  alphaB<-matrix(alphaB,ncol = 32,nrow=1)
  datosB<-matrix(0,ncol=2,nrow = length(horas)+1)
  for(hora in horas)
  {
    datosB[hora+1,1]<-hora
    pi_t<-alphaB%*%expm(matrizQBosa*hora)
    datosB[hora+1,2]<-(estadosB%*%t(pi_t))[1,1]
  }
  datosB<-data.frame(datosB)
  estadosB<-estadosB^2
  alphaB<-rep(0,32)
  alphaB[1]<-1
  alphaB<-matrix(alphaB,ncol = 32,nrow=1)
  varianzaB<-matrix(0,ncol=2,nrow = length(horas)+1)
  for(hora in horas)
  {
    varianzaB[hora+1,1]<-hora
    pi_t<-alphaB%*%expm(matrizQBosa*hora)
    varianzaB[hora+1,2]<-(estadosB%*%t(pi_t))[1,1]-datosB[hora+1,2]^2
  }
  varianzaB<-data.frame(varianzaB)
  #-----------------------------Suba------------------------------------------------------------
  estadosS<-0:26
  estadosS<-matrix(estadosS,ncol = 27,nrow=1)
  alphaS<-rep(0,27)
  alphaS[1]<-1
  alphaS<-matrix(alphaS,ncol = 27,nrow=1)
  datosS<-matrix(0,ncol=2,nrow = length(horas)+1)
  for(hora in horas)
  {
    datosS[hora+1,1]<-hora
    pi_t<-alphaS%*%expm(matrizQSuba*hora)
    datosS[hora+1,2]<-(estadosS%*%t(pi_t))[1,1]
  }
  datosS<-data.frame(datosS)
  estadosS<-estadosS^2
  alphaS<-rep(0,27)
  alphaS[1]<-1
  alphaS<-matrix(alphaS,ncol = 27,nrow=1)
  varianzaS<-matrix(0,ncol=2,nrow = length(horas)+1)
  for(hora in horas)
  {
    varianzaS[hora+1,1]<-hora
    pi_t<-alphaS%*%expm(matrizQSuba*hora)
    varianzaS[hora+1,2]<-(estadosS%*%t(pi_t))[1,1]-datosS[hora+1,2]^2
  }
  varianzaS<-data.frame(varianzaS)
  #----------------------Gráfica final------------------------------------------------------------
  EF<-varianzaS[,1]
  EF<-cbind(EF,varianzaK[,2])
  EF<-cbind(EF,varianzaS[,2])
  EF<-cbind(EF,varianzaB[,2])
  EF<-data.frame(EF)
  return(EF)
}
utilidades<-function(combustible,dotacion,salario)
{
  #-------------------------Datos------------------------------------
  Encuesta_Seguridad <- read_excel("Encuesta_Seguridad.xlsx")
  #-----------------------Preprocesar--------------------------------
  infoEncuesta<-matrix(0,ncol = 5,nrow = 1)
  infoEncuesta<-data.frame(infoEncuesta)
  colnames(infoEncuesta)<-list("Estado N Percepcion","Estado N Policías","Estado N+1 Percepcion","Estado N+1 Policías","Freq")
  dLength<-length(Encuesta_Seguridad$Semana)
  for(i in 1:dLength)
  {
    temp<-data.frame(list(Encuesta_Seguridad[i,"Percepción"],as.integer((Encuesta_Seguridad[i,"Número de policías"]-100)/500),Encuesta_Seguridad[i+1,"Percepción"],as.integer((Encuesta_Seguridad[i+1,"Número de policías"]-100)/500),1))
    colnames(temp)<-list("Estado N Percepcion","Estado N Policías","Estado N+1 Percepcion","Estado N+1 Policías","Freq")
    infoEncuesta<-rbind(infoEncuesta,temp)
  }
  infoEncuesta<-infoEncuesta[2:dLength,]
  infoEncuesta$`Estado N Policías`<-round(infoEncuesta$`Estado N Policías`,digits = 0)
  infoEncuesta$`Estado N+1 Policías`<-round(infoEncuesta$`Estado N+1 Policías`,digits = 0)
  infoEncuestaAgg<-aggregate(x=infoEncuesta$Freq,by=list(infoEncuesta$`Estado N Percepcion`,infoEncuesta$`Estado N Policías`,infoEncuesta$`Estado N+1 Percepcion`,infoEncuesta$`Estado N+1 Policías`),FUN=sum)
  colnames(infoEncuestaAgg)<-list("Estado N Percepcion","Estado N Policías","Estado N+1 Percepcion","Estado N+1 Policías","Freq")
  probabilidadesNN1<-matrix(0,ncol=5,nrow = 1)
  probabilidadesNN1<-data.frame(probabilidadesNN1)
  colnames(probabilidadesNN1)<-list("Percepcion N","Policias N","Percepcion N+1","Policias N+1","Prob")
  #----------------Calcular probabilidades P[X_n+1=j|X_n=i]--------------------------------------------
  for(pn in unique(infoEncuestaAgg$`Estado N Percepcion`))
  {
    tempPerc<-infoEncuestaAgg[infoEncuestaAgg$`Estado N Percepcion`==pn,]
    for(cn in unique(infoEncuestaAgg$`Estado N Policías`))
    {
      tempPercPol<-tempPerc[tempPerc$`Estado N Policías`==cn,]
      totalEv<-sum(tempPercPol$Freq)
      for(pn1 in unique(infoEncuestaAgg$`Estado N+1 Percepcion`))
      {
        tempPercPolPerc1<-tempPercPol[tempPercPol$`Estado N+1 Percepcion`==pn1,]
        for(cn1 in unique(infoEncuestaAgg$`Estado N+1 Policías`))
        {
          tempPercPolPerc1Pol1<-tempPercPolPerc1[tempPercPolPerc1$`Estado N+1 Policías`==cn1,]
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
  #---------------------------------------Steady States--------------------------------------
  poliPorEstado<-matrix(c(rep(c(2,0,1,3,4),3)),ncol = 1,nrow = 15)
  steadyStatesPerc<-steadyStates(CMTD_Percepcion)
  #Ingresos por Percepcion
  ingresos<-c(rep(50000437450,5),rep(43330151900,5),rep(30080406700,5))
  ingresos<-matrix(ingresos,ncol = length(ingresos),nrow = 1)
  #Egresos por mantenimiento
  egresos<-rep(c((salario+dotacion)*((2*500)+100),(salario+dotacion)*((0*500)+100),(salario+dotacion)*((1*500)+100),(salario+dotacion)*((3*500)+100),(salario+dotacion)*((4*500)+100)),3)
  egresos<-matrix(egresos,ncol = length(egresos),nrow = 1)
  
  utilidad<-(ingresos%*%steadyStatesPerc)-(egresos%*%steadyStatesPerc)
  return(utilidad)
}