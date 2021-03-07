#------------------Paquetes----------------------------------------
library(readxl)
library(markovchain)
library(expm)
library(plotly)
library(MASS)
library(fitdistrplus)
#-------------------------Datos------------------------------------
Encuesta_Seguridad <- read_excel("Encuesta_Seguridad.xlsx")
Validacion_Utilidades <- read_excel("Validacion_Utilidades.xlsx",col_types = c("date", "numeric"))
#-----------------------Preprocesar--------------------------------
infoEncuesta<-matrix(0,ncol = 5,nrow = 1)
infoEncuesta<-data.frame(infoEncuesta)
colnames(infoEncuesta)<-list("Estado N Percepción","Estado N Policías","Estado N+1 Percepción","Estado N+1 Policías","Freq")
dLength<-length(Encuesta_Seguridad$Semana)
for(i in 1:dLength)
{
  temp<-data.frame(list(Encuesta_Seguridad[i,"Percepción"],as.integer((Encuesta_Seguridad[i,"Número de policías"]-100)/500),Encuesta_Seguridad[i+1,"Percepción"],as.integer((Encuesta_Seguridad[i+1,"Número de policías"]-100)/500),1))
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
#----------------Calcular probabilidades P[X_n+1=j|X_n=i]--------------------------------------------
for(pn in unique(infoEncuestaAgg$`Estado N Percepción`))
{
  tempPerc<-infoEncuestaAgg[infoEncuestaAgg$`Estado N Percepción`==pn,]
  for(cn in unique(infoEncuestaAgg$`Estado N Policías`))
  {
    tempPercPol<-tempPerc[tempPerc$`Estado N Policías`==cn,]
    totalEv<-sum(tempPercPol$Freq)
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
#-------------------------------------Verificar-------------------------------------
reviewN<-aggregate(probabilidadesNN1$Prob,list(probabilidadesNN1$`Percepcion N`,probabilidadesNN1$`Policias N`),FUN=sum)
reviewN1<-aggregate(probabilidadesNN1$Prob,list(probabilidadesNN1$`Percepcion N+1`,probabilidadesNN1$`Policias N+1`),FUN=sum)
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
View(matrizP)
CMTD_Percepcion<-new("markovchain",states=colnames(matrizP),transitionMatrix=matrizP)
#---------------------------------------Steady States--------------------------------------
poliPorEstado<-matrix(c(rep(c(2,0,1,3,4),3)),ncol = 1,nrow = 15)
steadyStatesPerc<-steadyStates(CMTD_Percepcion)
print((((steadyStatesPerc%*%poliPorEstado)*500)+100))
#-----------------------------Verificar con validación-------------------------------------
#prepross validación
Validacion_Utilidades<-Validacion_Utilidades[44:length(Validacion_Utilidades$Mes),]
#Estado inicial
alpha<-rep(0,length(colnames(matrizP)))
alpha<-matrix(alpha,ncol=length(alpha),nrow = 1)
colnames(alpha)<-colnames(matrizP)
estadoInit<-paste(Encuesta_Seguridad$Percepción[1],as.integer((Encuesta_Seguridad$`Número de policías`[1]-100)/500),sep=",")
alpha[1,estadoInit]<-1
#Ingresos por percepción
ingresos<-c(rep(50000437450,5),rep(43330151900,5),rep(30080406700,5))
ingresos<-matrix(ingresos,ncol = length(ingresos),nrow = 1)
colnames(ingresos)<-colnames(matrizP)
#Egresos por mantenimiento
egresos<-rep(c((416250+67230)*((2*500)+100),(416250+67230)*((0*500)+100),(416250+67230)*((1*500)+100),(416250+67230)*((3*500)+100),(416250+67230)*((4*500)+100)),3)
egresos<-matrix(egresos,ncol = length(egresos),nrow = 1)
colnames(egresos)<-colnames(matrizP)
#Creacion de la matriz
ValidMatrix<-matrix(0,ncol=4,nrow = length(Validacion_Utilidades$Mes))
colnames(ValidMatrix)<-list("Mes","Resultado Modelo","Valor Real","Diferencia")
meses<-1:length(Validacion_Utilidades$Mes)
for(mes in meses)
{
  ingresoMes<-0
  for(semana in 1:4)
  {
    pi_n<-alpha%*%(matrizP%^%(semana+((mes-1)*4)))
    ingresoSemana<-(ingresos%*%t(pi_n))*(0.03)
    egresoSemana<-(egresos%*%t(pi_n))+150450
    ingresoMes<-ingresoMes+(ingresoSemana-egresoSemana)
  }
  ValidMatrix[mes,1]<-as.Date.character(Validacion_Utilidades$Mes[mes])
  ValidMatrix[mes,2]<-ingresoMes
  ValidMatrix[mes,3]<-Validacion_Utilidades$Utilidades[mes]
  ValidMatrix[mes,4]<-ValidMatrix[mes,3]-ValidMatrix[mes,2]
}
View(ValidMatrix)
#------------------------------------------------Probabilidades de estados----------------------
alphaT<-rep(0,length(colnames(matrizP)))
alphaT<-matrix(alphaT,ncol=length(alpha),nrow = 1)
colnames(alphaT)<-colnames(matrizP)
estadoInit<-paste(Encuesta_Seguridad$Percepción[1],as.integer((Encuesta_Seguridad$`Número de policías`[1]-100)/500),sep=",")
alphaT[1,estadoInit]<-1
matrizProbabilidesEstado<-matrix(0,ncol=length(estadosFinales),nrow = length(Validacion_Utilidades$Mes)*4)
for(r in 1:length(matrizProbabilidesEstado))
{
  matrizProbabilidesEstado[r,]<-alphaT%*%(matrizP%^%(r))
}
colnames(matrizProbabilidesEstado)<-estadosFinales
View(alphaT)
View(matrizProbabilidesEstado)
View(rowSums(matrizProbabilidesEstado))