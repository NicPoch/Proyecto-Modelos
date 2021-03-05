#-----------------------Paquetes------------------------------------
library(markovchain)
library(expm)
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