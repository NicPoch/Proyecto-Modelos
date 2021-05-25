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