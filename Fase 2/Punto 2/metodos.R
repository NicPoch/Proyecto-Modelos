#-----------Paquetes---------------------
library(expm)
library(markovchain)
library(readxl)
#----------Métodos-----------------------
#Cargar Cadena de Markov
cargarCadena<-function(archivo)
{
  casos <- data.frame(read_excel(archivo,col_types = c("skip", "text", "text")))
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
  CMTD<-new("markovchain",states=estados,transitionMatrix=matriP)
  return(CMTD)
}
#Calcula el tiempo promedio en el sistema si se empieza en un estados s0
tiempoEsperadoCaso<-function(s0,CMTD)
{
  return(meanAbsorptionTime(CMTD)[s0])
}
#Se calcula la probabilidad de "Liberado no reincidente" dado que empieza en el estado s0
pResocializado<-function(s0,CMTD)
{
  return(absorptionProbabilities(CMTD)[s0,"Liberado no reincidente"])
}