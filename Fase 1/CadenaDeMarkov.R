library(markovchain)
library(expm)
matrixQFunc<-function(maxP,maxC,lambdaC,lambdaP)
{
  estadosP <- rev(0:maxP)
  estadosC <- 1:maxC
  sz<-c()
  for(i in estadosP)
  {
    sz<-c(sz,paste(c(i),c(0),sep = ","))
  }
  for(j in estadosC)
  {
    sz<-c(sz,c(paste(c(0),c(j),sep = ",")))
  }
  estados_Definitivos <- sz
  matrizQ <- matrix(0, nrow = length(estados_Definitivos), ncol = length(estados_Definitivos))
  dimnames(matrizQ) <- list(estados_Definitivos,estados_Definitivos)
  for(filas in estados_Definitivos)
  {
    i <- as.numeric(strsplit(filas, split =",")[[1]][1])
    j <- as.numeric(strsplit(filas, split =",")[[1]][2])
    
    for(columnas in estados_Definitivos)
    {
      iprima <- as.numeric(strsplit(columnas, split =",")[[1]][1])
      jprima <- as.numeric(strsplit(columnas, split =",")[[1]][2])
      
      if(iprima == i & iprima == 0 & jprima == (j+1) & j < maxC)
      {
        matrizQ[filas,columnas] = lambdaC
      }
      else if (iprima == (i-1) & jprima == j & jprima == 0 & i>0)
      {
        matrizQ[filas,columnas] = lambdaC
      }
      else if (iprima == i & iprima == 0 & jprima == (j-1) & j>0)
      {
        matrizQ[filas,columnas] = lambdaP
      }
      else if (iprima == (i+1)  & jprima == j & j==0 & i<maxP)
      {
        matrizQ[filas,columnas] = lambdaP
      }
      
    }
  }
  for(diagonal in estados_Definitivos)
  {
    matrizQ[diagonal,diagonal] = -rowSums(matrizQ)[diagonal]
  }
  return(matrizQ)
}
matrizQKennedy<-matrixQFunc(maxP=8,maxC=40,lambdaC=0.8278,lambdaP=1/24)
matrizQBosa<-matrixQFunc(maxP=13,maxC=31,lambdaC=0.8031,lambdaP=0.83/24)
matrizQSuba<-matrixQFunc(maxP=18,maxC=26,lambdaC=0.8163 ,lambdaP=1.5/24)

CMTC_Kennedy <- new(Class="ctmc", generator = matrizQKennedy,states=colnames(matrizQKennedy))
CMTC_Bosa <- new(Class="ctmc", generator = matrizQBosa,states=colnames(matrizQBosa))
CMTC_Suba <- new(Class="ctmc", generator = matrizQSuba,states=colnames(matrizQSuba))

Embebida_Kennedy <- generatorToTransitionMatrix(matrizQKennedy)
Embebida_Bosa <- generatorToTransitionMatrix(matrizQBosa)
Embebida_Suba <- generatorToTransitionMatrix(matrizQSuba)