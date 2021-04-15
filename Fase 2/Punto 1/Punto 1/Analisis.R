#---------------Paquetes----------------------------------------
library(expm)
library(markovchain)
#---------------Variables---------------------------------------
#Tasas
lambda_llegada<-0.4
lambda_123<-1/25
lambda_policia<-1/(40/60)
lambda_uri<-1
#servidores
S_123<-3
S_policia<-3
S_uri<-4
#capacidades
C_123<-1
C_policia<-2
C_uri<-1
#---------------------Cadena de Markov CMTC---------------------
#Estados
S_sistema<-c()
for(s_123 in 0:(C_123+S_123))
{
  for(s_policia in 0:(C_policia+S_policia))
  {
    for(s_uri in 0:(C_uri+S_uri))
    {
      S_sistema<-c(S_sistema,paste(s_123,s_policia,s_uri,sep = ","))
    }
  }
}
#matrizQ
matrizQ<-matrix(0,ncol = length(S_sistema),nrow = length(S_sistema))
dimnames(matrizQ)<-list(S_sistema,S_sistema)
for(X_n in S_sistema)
{
  X_n_123<-as.numeric(strsplit(X_n,split=",")[[1]][1])
  X_n_policia<-as.numeric(strsplit(X_n,split=",")[[1]][2])
  X_n_uri<-as.numeric(strsplit(X_n,split=",")[[1]][3])
  for(X_n1 in S_sistema)
  {
    X_n1_123<-as.numeric(strsplit(X_n1,split=",")[[1]][1])
    X_n1_policia<-as.numeric(strsplit(X_n1,split=",")[[1]][2])
    X_n1_uri<-as.numeric(strsplit(X_n1,split=",")[[1]][3])
    #Llega una nueva entidad al sistema ~ llega una llamada
    if(((X_n_123+X_n_policia+X_n_uri+1)==(X_n1_123+X_n1_policia+X_n1_uri)))
    {
      matrizQ[X_n,X_n1]<-lambda_llegada
    }
    #Estaba en 123 y se va del sistema 
    if((X_n1_123==X_n_123-1) && (X_n1_uri==X_n_uri) && (X_n1_policia==X_n_policia))
    {
      matrizQ[X_n,X_n1]<-min(X_n_123,S_123)*lambda_123*0.76
    }
    #Estaba en 123 y pasa a policía
    if((X_n1_123==X_n_123-1) && (X_n1_uri==X_n_uri) && (X_n1_policia==X_n_policia+1))
    {
      matrizQ[X_n,X_n1]<-min(X_n_123,S_123)*lambda_123*0.24
    }
    #Estaba en policía y se va del sistema
    if((X_n1_123==X_n_123) && (X_n1_uri==X_n_uri) && (X_n1_policia==X_n_policia-1))
    {
      matrizQ[X_n,X_n1]<-min(X_n_policia,S_policia)*lambda_policia*0.5
    }
    #Estaba en policía y se va a uri
    if((X_n1_123==X_n_123) && (X_n1_uri==X_n_uri+1) && (X_n1_policia==X_n_policia-1))
    {
      matrizQ[X_n,X_n1]<-min(X_n_policia,S_policia)*lambda_policia*0.35
    }
    #Estaba en uri y se va del sistema
    if((X_n1_123==X_n_123) && (X_n1_uri==X_n_uri-1) && (X_n1_policia==X_n_policia))
    {
      matrizQ[X_n,X_n1]<-min(X_n_uri,S_uri)*lambda_uri
    }
  }
}
for(s in S_sistema)
{
  matrizQ[s,s]<-sum(matrizQ[s,])*-1
}
View(matrizQ)
CMTC<-new("ctmc",states=S_sistema,generator=matrizQ)
Embebida<-generatorToTransitionMatrix(matrizQ)
#------------------Medidas de desempeño----------------------
#Primera parte
#Considerando que el sistema empieza en el estado 0,0,0
tiempoParaOcupado<-ExpectedTime(CMTC,1,180)
print(paste("El tiempo (horas) que toma el sistema para llegar a máxima capacidad es: ",tiempoParaOcupado,sep = ""))
#Segunda parte
h<-8
alhphaInicial<-rep(0,nrow(matrizQ))
alhphaInicial[145]<-1
L<--1/diag(matrizQ)
matrizMultiplicar<-Embebida
contador<-1
valorEsperadoTiempo<-t(alhphaInicial)%*%L
while(valorEsperadoTiempo<h)
{
  contador<-contador+1
  valorEsperadoTiempo<-valorEsperadoTiempo+t(alhphaInicial)%*%matrizMultiplicar%*%L
  matrizMultiplicar<-matrizMultiplicar%*%Embebida
}
matrizOcupacion<-diag(nrow(matrizQ))+Embebida
resultado<-Embebida
for(i in 1:contador)
{
  resultado=resultado%*%Embebida
  matrizOcupacion<-matrizOcupacion+resultado
}
M_t<-alhphaInicial%*%matrizOcupacion*L
ans<-0
for(s in S_sistema)
{
  if(as.numeric(strsplit(s,split=",")[[1]][2])==(C_policia+S_policia))
  {
    ans<-ans+M_t[1,s][[1]]
  }
}
print(paste("Se va a encontrar ocupado ~",ans," horas",sep = ""))