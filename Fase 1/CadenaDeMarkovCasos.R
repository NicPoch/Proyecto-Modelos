#-----------------------Paquetes------------------------------------
library(markovchain)
library(expm)
library(plotly)
#----------------------Función para generar matriz Q---------------------
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
#----------------Crear matrices Q----------------------------------------------------------------
matrizQKennedy<-matrixQFunc(maxC=40,lambdaIn=0.3801787,lambdaOut=1/24,policias = 8)
matrizQBosa<-matrixQFunc(maxC=31,lambdaIn=0.4574637,lambdaOut=0.83/24,policias = 13)
matrizQSuba<-matrixQFunc(maxC=26,lambdaIn=0.4203695,lambdaOut=1.5/24,policias = 18)
#------------------Usar paquete markovchain sobre las matrices-----------------------------------
CMTC_Kennedy <- new(Class="ctmc", generator = matrizQKennedy,states=colnames(matrizQKennedy))
CMTC_Bosa <- new(Class="ctmc", generator = matrizQBosa,states=colnames(matrizQBosa))
CMTC_Suba <- new(Class="ctmc", generator = matrizQSuba,states=colnames(matrizQSuba))
#----------------Crear embebidas-----------------------------------------------------------------
Embebida_Kennedy <- generatorToTransitionMatrix(matrizQKennedy)
Embebida_Bosa <- generatorToTransitionMatrix(matrizQBosa)
Embebida_Suba <- generatorToTransitionMatrix(matrizQSuba)
#----------------Modelar comportamiento de Cai en 2 semanas--------------------------------------
# 2 semanas == 14 días == 336 horas
horas<-1:(14*24)
#-----------------------------Kennedy------------------------------------------------------------
estadosK<-0:40
estadosK<-matrix(estadosK,ncol = 41,nrow=1)
alphaK<-rep(0,41)
alphaK[1]<-1
alphaK<-matrix(alphaK,ncol = 41,nrow=1)
datosK<-matrix(0,ncol=2,nrow = length(horas))
for(hora in horas)
{
  datosK[hora+1,1]<-hora
  pi_t<-alphaK%*%expm(matrizQKennedy*hora)
  datosK[hora+1,2]<-(estadosK%*%t(pi_t))[1,1]
}
datosK<-data.frame(datosK)
figK<-plot_ly(datosK,x=datosK[,1],y=datosK[,2],type='scatter',mode='lines+markers')
figK<-figK%>%layout(xaxis=list(title="Horas"),yaxis=list(title="Casos"),title="E[Casos] por Hora Kennedy")
figK
steadyStatesK<-steadyStates(CMTC_Kennedy)
endStateKennedy<-estadosK%*%t(steadyStatesK)
print(paste("Se estima que el valor final del CAI en Kennedy sea ",endStateKennedy," casos.",sep=""))
#-----------------------------Bosa------------------------------------------------------------
estadosB<-0:31
estadosB<-matrix(estadosB,ncol = 32,nrow=1)
alphaB<-rep(0,32)
alphaB[1]<-1
alphaB<-matrix(alphaB,ncol = 32,nrow=1)
datosB<-matrix(0,ncol=2,nrow = length(horas))
for(hora in horas)
{
  datosB[hora+1,1]<-hora
  pi_t<-alphaB%*%expm(matrizQBosa*hora)
  datosB[hora+1,2]<-(estadosB%*%t(pi_t))[1,1]
}
datosB<-data.frame(datosB)
figB<-plot_ly(datosB,x=datosB[,1],y=datosB[,2],type='scatter',mode='lines+markers')
figB<-figB%>%layout(xaxis=list(title="Horas"),yaxis=list(title="Casos"),title="E[Casos] por Hora Bosa")
figB
steadyStatesB<-steadyStates(CMTC_Bosa)
endStateBosa<-estadosB%*%t(steadyStatesB)
print(paste("Se estima que el valor final del CAI en Bosa sea ",endStateBosa," casos.",sep=""))
#-----------------------------Suba------------------------------------------------------------
estadosS<-0:26
estadosS<-matrix(estadosS,ncol = 27,nrow=1)
alphaS<-rep(0,27)
alphaS[1]<-1
alphaS<-matrix(alphaS,ncol = 27,nrow=1)
datosS<-matrix(0,ncol=2,nrow = length(horas))
for(hora in horas)
{
  datosS[hora+1,1]<-hora
  pi_t<-alphaS%*%expm(matrizQSuba*hora)
  datosS[hora+1,2]<-(estadosS%*%t(pi_t))[1,1]
}
datosS<-data.frame(datosS)
figS<-plot_ly(datosS,x=datosS[,1],y=datosS[,2],type='scatter',mode='lines+markers')
figS<-figS%>%layout(xaxis=list(title="Horas"),yaxis=list(title="Casos"),title="E[Casos] por Hora Suba")
figS
steadyStatesS<-steadyStates(CMTC_Suba)
endStateSuba<-estadosS%*%t(steadyStatesS)
print(paste("Se estima que el valor final del CAI en Suba sea ",endStateSuba," casos.",sep=""))
#----------------------Gráfica final------------------------------------------------------------
EF<-datosS[,1]
EF<-cbind(EF,datosK[,2])
EF<-cbind(EF,datosS[,2])
EF<-cbind(EF,datosB[,2])
figF<-plot_ly(datosS,x=EF[,1],y=EF[,2],type='scatter',mode='lines+markers',name = "Casos Kennedy")
figF<-figF%>%add_trace(y=EF[,3],mode='lines+markers',name = "Casos Suba")
figF<-figF%>%add_trace(y=EF[,4],mode='lines+markers',name = "Casos Bosa")
figF<-figF%>%layout(title="Casos Esperados por Hora",xaxis=list(title="horas"),yaxis=list(title="Casos"))
figF