#-----------Paquetes---------------------
library(expm)
library(markovchain)
library(readxl)
#----------Prepross-----------------------
casos <- data.frame(read_excel("Casos2019_Hist.xlsx",col_types = c("skip", "text", "text")))
estados<-unlist(unique(casos[,"Estatus"]))
casos$Ocurrencia<-1
for(i in 1:length(casos$Semana))
{
  casos[i,"Semana"]<-as.numeric(strsplit(casos[i,"Semana"],split = " ")[[1]][3])
}
casos$Semana<-as.numeric(casos$Semana)
#---------Crear Matriz P------------------
matriP<-matrix(0,ncol = length(estados),nrow = length(estados))
dimnames(matriP)<-list(estados,estados)
contador<-0
L<-(length(casos$Semana)/53)-1
for(ss_i in 0:L)
{
  ss<-casos[c((1+(53*ss_i)):(53*(ss_i+1))),]
  for( n in 1:52)
  {
    x_n<-ss[n,"Estatus"]
    x_n1<-ss[(n+1),"Estatus"]
    matriP[x_n,x_n1]<-matriP[x_n,x_n1]+1
    contador<-1+contador
    
  }
}
for(s in estados)
{
  matriP[s,]<-matriP[s,]/sum(matriP[s,])
}
View(matriP)
CMTD<-new("markovchain",states=estados,transitionMatrix=matriP)
plot(CMTD)
#-----------Estados absorbentes--------------------
absStates<-absorbingStates(CMTD)
absStates
#-----------Tiempo de absorción--------------------
times<-meanAbsorptionTime(CMTD)
times
#-----------Probabilidades de Absorción------------
absP<-absorptionProbabilities(CMTD)
absP