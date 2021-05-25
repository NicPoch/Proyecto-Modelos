heatmapInfo<-function()
{
  #Epocas
  E_max<-24
  #Variables
  #{A_n,n>=0} estado de la zona A en n-esimo mes
  #{B_n,n>=0} estado de la zona B en n-esimo mes
  #{C_n,n>=0} estado de la zona C en n-esimo mes
  #{D_n,n>=0} estado de la zona D en n-esimo mes
  #{E_n,n>=0} estado de la zona E en n-esimo mes
  #{F_n,n>=0} estado de la zona F en n-esimo mes
  #Estados (zona más insegura)
  S<-c("A","B","C","D","E","F")
  #Decisiones
  decisiones<-c("intervenir A","intervenir B","intervenir C","intervenir D","intervenir E","intervenir F")
  #retornos
  b<-matrix(c(1.5,3.5,3,4,4.5,3),nrow=1,ncol=length(S))
  colnames(b)<-S
  #retornos sin factor de riesgo
  temp_C<-c((b[1,"A"])**1    ,(b[1,"A"]*0.8)**1,0                ,0                ,0                ,0                ,
            (b[1,"B"]*0.8)**1,(b[1,"B"])**1    ,(b[1,"B"]*0.8)**1,(b[1,"B"]*0.8)**1,0                ,0                ,
            0                ,(b[1,"C"]*0.8)**1,(b[1,"C"])**1    ,(b[1,"C"]*0.8)**1,0                ,0                ,
            0                ,(b[1,"D"]*0.8)**1,(b[1,"D"]*0.8)**1,(b[1,"D"])**1    ,(b[1,"D"]*0.8)**1,(b[1,"D"]*0.8)**1,
            0                ,0                ,0                ,(b[1,"E"]*0.8)**1,(b[1,"E"])**1    ,(b[1,"E"]*0.8)**1,
            0                ,0                ,0                ,(b[1,"F"]*0.8)**1,(b[1,"F"]*0.8)**1,(b[1,"F"])**1    )
  C_sinRiesgo<-matrix(temp_C,ncol = length(decisiones),nrow = length(S),byrow = T)
  dimnames(C_sinRiesgo)<-list(S,decisiones)
  
  #retornos con factor de riesgo
  temp_C<-c((b[1,"A"])**2    ,(b[1,"A"]*0.8)**2,0                ,0                ,0                ,0                ,
            (b[1,"B"]*0.8)**2,(b[1,"B"])**2    ,(b[1,"B"]*0.8)**2,(b[1,"B"]*0.8)**2,0                ,0                ,
            0                ,(b[1,"C"]*0.8)**2,(b[1,"C"])**2    ,(b[1,"C"]*0.8)**2,0                ,0                ,
            0                ,(b[1,"D"]*0.8)**2,(b[1,"D"]*0.8)**2,(b[1,"D"])**2    ,(b[1,"D"]*0.8)**2,(b[1,"D"]*0.8)**2,
            0                ,0                ,0                ,(b[1,"E"]*0.8)**2,(b[1,"E"])**2    ,(b[1,"E"]*0.8)**2,
            0                ,0                ,0                ,(b[1,"F"]*0.8)**2,(b[1,"F"]*0.8)**2,(b[1,"F"])**2    )
  C_conRiesgo<-matrix(temp_C,ncol = length(decisiones),nrow = length(S),byrow = T)
  dimnames(C_conRiesgo)<-list(S,decisiones)
  #Probabilidades de transición por decision
  #Intervenir A
  temp_d<-c(0.14           ,1-0.14    ,0         ,0         ,0         ,0         ,
            0.02           ,(1-0.02)/3,(1-0.02)/3,(1-0.02)/3,0         ,0         ,
            0              ,1/3       ,1/3       ,1/3       ,0         ,0         ,
            0              ,1/5       ,1/5       ,1/5       ,1/5       ,1/5       ,
            0              ,0         ,0         ,1/3       ,1/3       ,1/3       ,
            0              ,0         ,0         ,1/3       ,1/3       ,1/3       )
  P_iA<-matrix(temp_d,nrow = length(S),ncol = length(S),byrow = T)
  #Intervenir B
  temp_d<-c((1-0.04)/3     ,0.04      ,0         ,0         ,0         ,0         ,
            (1-0.10)/3     ,0.10      ,(1-0.10)/3,(1-0.10)/3,0         ,0         ,
            0              ,0.04      ,(1-0.04)/2,(1-0.04)/2,0         ,0         ,
            0              ,0.04      ,(1-0.04)/5,(1-0.04)/5,(1-0.04)/5,(1-0.04)/5,
            0              ,0         ,0         ,1/3       ,1/3       ,1/3       ,
            0              ,0         ,0         ,1/3       ,1/3       ,1/3       )
  P_iB<-matrix(temp_d,nrow = length(S),ncol = length(S),byrow = T)
  #Intervenir C
  temp_d<-c(1/2            ,1/2       ,0         ,0         ,0         ,0         ,
            (1-0.06)/3     ,(1-0.06)/3,0.06      ,(1-0.06)/3,0         ,0         ,
            0              ,(1-0.18)/2,0.18      ,(1-0.18)/2,0         ,0         ,
            0              ,(1-0.06)/4,0.06      ,(1-0.06)/4,(1-0.06)/4,(1-0.06)/4,
            0              ,0         ,0         ,1/3       ,1/3       ,1/3       ,
            0              ,0         ,0         ,1/3       ,1/3       ,1/3       )
  P_iC<-matrix(temp_d,nrow = length(S),ncol = length(S),byrow = T)
  #Intervenir D
  temp_d<-c(1/2            ,1/2       ,0         ,0         ,0         ,0         ,
            (1-0.03)/3     ,(1-0.03)/3,(1-0.03)/3,0.03      ,0         ,0         ,
            0              ,(1-0.03)/2,(1-0.03)/2,0.03      ,0         ,0         ,
            0              ,(1-0.12)/4,(1-0.12)/4,0.12      ,(1-0.12)/4,(1-0.12)/4,
            0              ,0         ,0         ,0.03      ,(1-0.03)/2,(1-0.03)/2,
            0              ,0         ,0         ,0.03      ,(1-0.03)/2,(1-0.03)/2)
  P_iD<-matrix(temp_d,nrow = length(S),ncol = length(S),byrow = T)
  #Intervenir E
  temp_d<-c(1/2            ,1/2       ,0         ,0         ,0         ,0         ,
            1/4            ,1/4       ,1/4       ,1/4       ,0         ,0         ,
            0              ,1/3       ,1/3       ,1/3       ,0         ,0         ,
            0              ,(1-0.02)/4,(1-0.02)/4,(1-0.02)/4,0.02      ,(1-0.02)/4,
            0              ,0         ,0         ,(1-0.19)/2,0.19      ,(1-0.19)/2,
            0              ,0         ,0         ,(1-0.02)/2,0.02      ,(1-0.02)/2)
  P_iE<-matrix(temp_d,nrow = length(S),ncol = length(S),byrow = T)
  #Intervenir F
  temp_d<-c(1/2            ,1/2       ,0         ,0         ,0         ,0         ,
            1/4            ,1/4       ,1/4       ,1/4       ,0         ,0         ,
            0              ,1/3       ,1/3       ,1/3       ,0         ,0         ,
            0              ,(1-0.07)/4,(1-0.07)/4,(1-0.07)/4,(1-0.07)/4,0.07      ,
            0              ,0         ,0         ,(1-0.07)/2,(1-0.07)/2,0.07      ,
            0              ,0         ,0         ,(1-0.22)/2,(1-0.22)/2,0.22      )
  P_iF<-matrix(temp_d,nrow = length(S),ncol = length(S),byrow = T)
  #---------Reolver problema------------------------
  #Crear estructuras para solución
  f_i=matrix(0,nrow = length(S),ncol=E_max)
  dimnames(f_i)<-list(S,1:E_max)
  
  politicas_opt<-matrix(0,nrow = length(S),ncol = E_max)
  row.names(politicas_opt)<-S
  #Recursión hacia atras
  #Epoca Final
  #Estructura de recursión
  f<-matrix(0,nrow = length(S),ncol=length(decisiones))
  dimnames(f)<-list(S,decisiones)
  #Crear ecuación de bellman
  f<-C_conRiesgo
  #Resolver ecuación
  f_i[,E_max]<-apply(f,1,max)
  politicas_opt[,E_max]<-decisiones[apply(f, 1, which.max)]
  #Epocas anteriores
  for (t in seq(E_max-1,1,by=-1))
  {
    #Estructuras
    f<-matrix(0,nrow = length(S),ncol=length(decisiones))
    dimnames(f)<-list(S,decisiones)
    costo<-C_sinRiesgo
    if (t%%12==0||t%%12==6||t%%12==7) 
    {
      costo<-C_conRiesgo 
    }
    #Crear función de recursión
    f[,decisiones[1]]<-(costo[,"intervenir A"])+(P_iA%*%f_i[,t+1])
    f[,decisiones[2]]<-(costo[,"intervenir B"])+(P_iB%*%f_i[,t+1])
    f[,decisiones[3]]<-(costo[,"intervenir C"])+(P_iC%*%f_i[,t+1])
    f[,decisiones[4]]<-(costo[,"intervenir D"])+(P_iD%*%f_i[,t+1])
    f[,decisiones[5]]<-(costo[,"intervenir E"])+(P_iE%*%f_i[,t+1])
    f[,decisiones[6]]<-(costo[,"intervenir F"])+(P_iF%*%f_i[,t+1])
    #Resolver Ecuación
    f_i[,t]<-apply(f,1,max)
    politicas_opt[,t]<-decisiones[apply(f, 1, which.max)]
  }
  return(politicas_opt)
}

getF_i<-function()
{
  #Epocas
  E_max<-24
  #Variables
  #{A_n,n>=0} estado de la zona A en n-esimo mes
  #{B_n,n>=0} estado de la zona B en n-esimo mes
  #{C_n,n>=0} estado de la zona C en n-esimo mes
  #{D_n,n>=0} estado de la zona D en n-esimo mes
  #{E_n,n>=0} estado de la zona E en n-esimo mes
  #{F_n,n>=0} estado de la zona F en n-esimo mes
  #Estados (zona más insegura)
  S<-c("A","B","C","D","E","F")
  #Decisiones
  decisiones<-c("intervenir A","intervenir B","intervenir C","intervenir D","intervenir E","intervenir F")
  #retornos
  b<-matrix(c(1.5,3.5,3,4,4.5,3),nrow=1,ncol=length(S))
  colnames(b)<-S
  #retornos sin factor de riesgo
  temp_C<-c((b[1,"A"])**1    ,(b[1,"A"]*0.8)**1,0                ,0                ,0                ,0                ,
            (b[1,"B"]*0.8)**1,(b[1,"B"])**1    ,(b[1,"B"]*0.8)**1,(b[1,"B"]*0.8)**1,0                ,0                ,
            0                ,(b[1,"C"]*0.8)**1,(b[1,"C"])**1    ,(b[1,"C"]*0.8)**1,0                ,0                ,
            0                ,(b[1,"D"]*0.8)**1,(b[1,"D"]*0.8)**1,(b[1,"D"])**1    ,(b[1,"D"]*0.8)**1,(b[1,"D"]*0.8)**1,
            0                ,0                ,0                ,(b[1,"E"]*0.8)**1,(b[1,"E"])**1    ,(b[1,"E"]*0.8)**1,
            0                ,0                ,0                ,(b[1,"F"]*0.8)**1,(b[1,"F"]*0.8)**1,(b[1,"F"])**1    )
  C_sinRiesgo<-matrix(temp_C,ncol = length(decisiones),nrow = length(S),byrow = T)
  dimnames(C_sinRiesgo)<-list(S,decisiones)
  
  #retornos con factor de riesgo
  temp_C<-c((b[1,"A"])**2    ,(b[1,"A"]*0.8)**2,0                ,0                ,0                ,0                ,
            (b[1,"B"]*0.8)**2,(b[1,"B"])**2    ,(b[1,"B"]*0.8)**2,(b[1,"B"]*0.8)**2,0                ,0                ,
            0                ,(b[1,"C"]*0.8)**2,(b[1,"C"])**2    ,(b[1,"C"]*0.8)**2,0                ,0                ,
            0                ,(b[1,"D"]*0.8)**2,(b[1,"D"]*0.8)**2,(b[1,"D"])**2    ,(b[1,"D"]*0.8)**2,(b[1,"D"]*0.8)**2,
            0                ,0                ,0                ,(b[1,"E"]*0.8)**2,(b[1,"E"])**2    ,(b[1,"E"]*0.8)**2,
            0                ,0                ,0                ,(b[1,"F"]*0.8)**2,(b[1,"F"]*0.8)**2,(b[1,"F"])**2    )
  C_conRiesgo<-matrix(temp_C,ncol = length(decisiones),nrow = length(S),byrow = T)
  dimnames(C_conRiesgo)<-list(S,decisiones)
  #Probabilidades de transición por decision
  #Intervenir A
  temp_d<-c(0.14           ,1-0.14    ,0         ,0         ,0         ,0         ,
            0.02           ,(1-0.02)/3,(1-0.02)/3,(1-0.02)/3,0         ,0         ,
            0              ,1/3       ,1/3       ,1/3       ,0         ,0         ,
            0              ,1/5       ,1/5       ,1/5       ,1/5       ,1/5       ,
            0              ,0         ,0         ,1/3       ,1/3       ,1/3       ,
            0              ,0         ,0         ,1/3       ,1/3       ,1/3       )
  P_iA<-matrix(temp_d,nrow = length(S),ncol = length(S),byrow = T)
  #Intervenir B
  temp_d<-c((1-0.04)/3     ,0.04      ,0         ,0         ,0         ,0         ,
            (1-0.10)/3     ,0.10      ,(1-0.10)/3,(1-0.10)/3,0         ,0         ,
            0              ,0.04      ,(1-0.04)/2,(1-0.04)/2,0         ,0         ,
            0              ,0.04      ,(1-0.04)/5,(1-0.04)/5,(1-0.04)/5,(1-0.04)/5,
            0              ,0         ,0         ,1/3       ,1/3       ,1/3       ,
            0              ,0         ,0         ,1/3       ,1/3       ,1/3       )
  P_iB<-matrix(temp_d,nrow = length(S),ncol = length(S),byrow = T)
  #Intervenir C
  temp_d<-c(1/2            ,1/2       ,0         ,0         ,0         ,0         ,
            (1-0.06)/3     ,(1-0.06)/3,0.06      ,(1-0.06)/3,0         ,0         ,
            0              ,(1-0.18)/2,0.18      ,(1-0.18)/2,0         ,0         ,
            0              ,(1-0.06)/4,0.06      ,(1-0.06)/4,(1-0.06)/4,(1-0.06)/4,
            0              ,0         ,0         ,1/3       ,1/3       ,1/3       ,
            0              ,0         ,0         ,1/3       ,1/3       ,1/3       )
  P_iC<-matrix(temp_d,nrow = length(S),ncol = length(S),byrow = T)
  #Intervenir D
  temp_d<-c(1/2            ,1/2       ,0         ,0         ,0         ,0         ,
            (1-0.03)/3     ,(1-0.03)/3,(1-0.03)/3,0.03      ,0         ,0         ,
            0              ,(1-0.03)/2,(1-0.03)/2,0.03      ,0         ,0         ,
            0              ,(1-0.12)/4,(1-0.12)/4,0.12      ,(1-0.12)/4,(1-0.12)/4,
            0              ,0         ,0         ,0.03      ,(1-0.03)/2,(1-0.03)/2,
            0              ,0         ,0         ,0.03      ,(1-0.03)/2,(1-0.03)/2)
  P_iD<-matrix(temp_d,nrow = length(S),ncol = length(S),byrow = T)
  #Intervenir E
  temp_d<-c(1/2            ,1/2       ,0         ,0         ,0         ,0         ,
            1/4            ,1/4       ,1/4       ,1/4       ,0         ,0         ,
            0              ,1/3       ,1/3       ,1/3       ,0         ,0         ,
            0              ,(1-0.02)/4,(1-0.02)/4,(1-0.02)/4,0.02      ,(1-0.02)/4,
            0              ,0         ,0         ,(1-0.19)/2,0.19      ,(1-0.19)/2,
            0              ,0         ,0         ,(1-0.02)/2,0.02      ,(1-0.02)/2)
  P_iE<-matrix(temp_d,nrow = length(S),ncol = length(S),byrow = T)
  #Intervenir F
  temp_d<-c(1/2            ,1/2       ,0         ,0         ,0         ,0         ,
            1/4            ,1/4       ,1/4       ,1/4       ,0         ,0         ,
            0              ,1/3       ,1/3       ,1/3       ,0         ,0         ,
            0              ,(1-0.07)/4,(1-0.07)/4,(1-0.07)/4,(1-0.07)/4,0.07      ,
            0              ,0         ,0         ,(1-0.07)/2,(1-0.07)/2,0.07      ,
            0              ,0         ,0         ,(1-0.22)/2,(1-0.22)/2,0.22      )
  P_iF<-matrix(temp_d,nrow = length(S),ncol = length(S),byrow = T)
  #---------Reolver problema------------------------
  #Crear estructuras para solución
  f_i=matrix(0,nrow = length(S),ncol=E_max)
  dimnames(f_i)<-list(S,1:E_max)
  
  politicas_opt<-matrix(0,nrow = length(S),ncol = E_max)
  row.names(politicas_opt)<-S
  #Recursión hacia atras
  #Epoca Final
  #Estructura de recursión
  f<-matrix(0,nrow = length(S),ncol=length(decisiones))
  dimnames(f)<-list(S,decisiones)
  #Crear ecuación de bellman
  f<-C_conRiesgo
  #Resolver ecuación
  f_i[,E_max]<-apply(f,1,max)
  politicas_opt[,E_max]<-decisiones[apply(f, 1, which.max)]
  #Epocas anteriores
  for (t in seq(E_max-1,1,by=-1))
  {
    #Estructuras
    f<-matrix(0,nrow = length(S),ncol=length(decisiones))
    dimnames(f)<-list(S,decisiones)
    costo<-C_sinRiesgo
    if (t%%12==0||t%%12==6||t%%12==7) 
    {
      costo<-C_conRiesgo 
    }
    #Crear función de recursión
    f[,decisiones[1]]<-(costo[,"intervenir A"])+(P_iA%*%f_i[,t+1])
    f[,decisiones[2]]<-(costo[,"intervenir B"])+(P_iB%*%f_i[,t+1])
    f[,decisiones[3]]<-(costo[,"intervenir C"])+(P_iC%*%f_i[,t+1])
    f[,decisiones[4]]<-(costo[,"intervenir D"])+(P_iD%*%f_i[,t+1])
    f[,decisiones[5]]<-(costo[,"intervenir E"])+(P_iE%*%f_i[,t+1])
    f[,decisiones[6]]<-(costo[,"intervenir F"])+(P_iF%*%f_i[,t+1])
    #Resolver Ecuación
    f_i[,t]<-apply(f,1,max)
    politicas_opt[,t]<-decisiones[apply(f, 1, which.max)]
  }
  return(f_i)
}