##############################################
# Egg ########################################
Temp<-c(15,20,25,28,30)
xm<-c(9.74,7.03,4.15,3.46, 3.29) # tiempo de desarrollo
n1<-c(35,33,34,35,31)
#xsd<-c(0.23,0.11,0.07,0.09,0.06)*sqrt(n1) # desviaciones estandar
#xsd<-ceiling(10*c(0.23,0.11,0.07,0.09,0.06)*sqrt(n1))/10 # desviaciones estandar
xsd<-ceiling(c(0.23,0.11,0.07,0.09,0.06)*sqrt(n1))
x1mort<-c(0.155,0.106,0.045,0.038,0.068) # mortalidades

##############################################
# Ninfa ######################################
Temp<-c(15,20,25,28,30)
xm<-c(39.60,21.76,12.82,10.60,13.00) # tiempo de desarrollo
n1<-c(35,33,34,35,31)
#xsd<-c(0.26,0.44 ,0.17,0.18,0.29)*sqrt(n1) # desviaciones estandar
xsd<-ceiling(10*c(0.26,0.44 ,0.17,0.18,0.29)*sqrt(n1))/10 # desviaciones estandar
#xsd<-ceiling(c(0.26,0.44 ,0.17,0.18,0.29)*sqrt(n1))
x1mort<-c(0.0586,0.0464,0.0452,0.0264,0.0452) # mortalidades

#############################
# Simulacion de frecuencias #

TablaF<-data.frame(Temp=NA,day=NA,N=NA,freq=NA)
n<-100 # tamaño de muestra

for(i in 1:length(Temp))
{
  r1<-round(rnorm(n,xm[i],floor(xsd[i])))
  r1[r1<=0]<-1
  f1<-table(r1)
  F1<-f1/sum(f1)
  
  TrueMort1<-(1-x1mort[i])*n
  freq1<-round(F1*TrueMort1)
  
  TablaTemp<-data.frame(Temp=Temp[i],cbind(day=as.numeric(as.character(labels(f1)$r1)),N=rep(n,length(f1)),freq=as.numeric(as.character(freq1))))
  TablaF<-rbind(TablaF,TablaTemp)
}
TablaF<-TablaF[-1,]
#write.table(TablaF,"D:/_BK-D/Pablo/GIS/Prediccion de ambientes/Life table simulated/Egg.txt",row.names = FALSE,col.names = FALSE)
write.table(TablaF,"D:/_BK-D/Pablo/GIS/Prediccion de ambientes/Life table simulated/Nymphs.txt",row.names = FALSE,col.names = FALSE)

########################################################################################################################

###############################################
# Female ######################################
Temp<-c(15,20,25,28,30,33)
xm<-c(88.3,50.6,39.7,34.7,33.5,28.7) # longevidad
n1<-c(18,22,25,21,25,23)
xsd<-c(4.31,2.61 ,1.39,1.13,1.08,1.38)*sqrt(n1) # desviaciones estandar
#xsd<-ceiling(10*c(4.31,2.61 ,1.39,1.13,1.08,1.38)*sqrt(n1))/10 # desviaciones estandar

###########################################
# Simulacion de frecuencias en Longevidad #

TablaF<-data.frame(Temp=NA,day=NA,N=NA,freq=NA)
n<-50 # tamaño de muestra

for(i in 1:length(Temp))
{
  r1<-round(rnorm(n,xm[i],floor(xsd[i])))
  r1[r1<=0]<-1
  f1<-table(r1)
  
  TablaTemp<-data.frame(Temp=Temp[i],cbind(day=as.numeric(as.character(labels(f1)$r1)),N=rep(n,length(f1)),freq=as.numeric(as.character(f1))))
  TablaF<-rbind(TablaF,TablaTemp)
}
TablaF<-TablaF[-1,]
#write.table(TablaF,"D:/_BK-D/Pablo/GIS/Prediccion de ambientes/Life table simulated/Egg.txt",row.names = FALSE,col.names = FALSE)
write.table(TablaF,"D:/_BK-D/Pablo/GIS/Prediccion de ambientes/Life table simulated/Female.txt",row.names = FALSE,col.names = FALSE)

########################################################################################################################

###########################################
# Oviposition #############################

Temp<-c(15,20,25,28,30,33)

ovip<-c(171,494,626,748,316,67) # fecundity
n1<-c(18,22,25,21,25,23)
ovisd<-floor(c(25.1,50.5,22.3,34.7,30.9,10.3)*sqrt(n1)) # desviaciones estandar redondeada pesimista

xAm<-c(88.3,50.6,39.7,34.7,33.5,28.7) # longevity (the same values are before lines)
xAsd<-c(4.31,2.61 ,1.39,1.13,1.08,1.38)*sqrt(n1)

temp<-rnorm(1000,max(xAm),n1[max(xAm)==xAm]+0.1*n1[max(xAm)==xAm])
MaxN<-ceiling(max(temp))

n<-100 # tamaño de muestra
TablaF<-data.frame(matrix(1,1,MaxN))
colnames(TablaF)<-paste("V",1:MaxN,sep="")

for(i in 1:length(Temp))
{
  r1<-floor(rnorm(n,xAm[i],xAsd[i]))
  r1[r1<=0]<-1
  
  r2<-MaxN - r1
  
  ov1<-round(rnorm(n,ovip[i],ovisd[i]))
  ov1[ov1<=0]<-1
  mean(ov1)

  rate1<-ov1/r1
  
  TablaTemp<-rep(1,MaxN)
  
  for(j in 1:n)
  {
    #tempo1<-rhyper(r1[j], r1[j], ov1[j], ceiling(0.5+rate1[j]))
    tempo1<-rhyper(r1[j], ov1[j], r1[j], ceiling(0.5+rate1[j]))
    tempo2<-rep(NA,r2[j])
    tempo<-c(tempo1,tempo2)
    TablaTemp<-cbind(TablaTemp,tempo)
  }
  TablaTemp<-TablaTemp[,-1]
  TablaTemp<-t(TablaTemp)
  TablaF<-rbind(TablaF,TablaTemp)
}
TablaF<-TablaF[-1,]
TablaF<-data.frame(Temp=rep(Temp,rep(n,length(Temp))),TablaF)

write.table(TablaF,"D:/_BK-D/Pablo/GIS/Prediccion de ambientes/Life table simulated/Oviposition.txt",row.names = FALSE,col.names = FALSE)


