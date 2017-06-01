#########################################################

# Uniendo de Indices por zonas separadas segun la latitud

#########################################################
zone.div<-function(dir1,dir2,ilon,ilat,R,dir.out,name.out=name.out,out,method=method,modelim=modelim,modelm=modelm,estadios=estadios,xi=xi,steps=steps, filtro=filtro,hfeno=hfeno){
  
  lats=p.area(ilat,R)
  
  #############################################
  # Corriendo por cada area y almacenar su data
  #############################################
  
  for(j in R:1){
    
    TempsAll=GenMatriz(dir1,dir2,ilon,lats[j,])
    
    coords=TempsAll$coords;nfil=nrow(coords)
    
    x1=TempsAll$x1
    
    y1=TempsAll$y1
    
    
    RFinal=matrix(NA,nfil,4) ## cambiar la dimension
    
    ################################
    
    # Corriendo por punto del Area j
    
    ################################
    
    #system.time(
    #pb <- txtProgressBar(min = 0, max = nfil, style = 3) # progress bar
    
    for(i in 1:nfil){
      
      RFinal[i,]=GenActIndex(i,TempsAll=TempsAll,coords=coords,x1=x1,y1=y1,out,method=method,modelim=modelim,modelm=modelm,estadios=estadios,xi=xi,steps=steps,filtro=filtro,DL=DL,hfeno=hfeno)$indices
      #setTxtProgressBar(pb, i)
    }
    
    #)
    
    rm(TempsAll);rm(x1);rm(y1)
    
    Inds=data.frame(coords[1:nfil,],RFinal)
    
    rm(RFinal)
    
    ##################################################################
    
    # Generando los archivos con encabezado: Lon - Lat - GI - AI - ERI
    
    ##################################################################
    
    write.table(Inds,paste(dir.out,"file",j,".txt",sep=""),row.names = F)
    
    rm(Inds)
    #setTxtProgressBar(pb, j)
  }
  
  ###########################################
  
  # Corriendo por cada area y uniendo su data
  
  ###########################################
  
  Inds=read.table(paste(dir.out,"file",R,".txt",sep=""),header=T)
  
  if(R!=1)
  {
    for(j in (R-1):1){
      
      TempInds=read.table(paste(dir.out,"file",j,".txt",sep=""),header=T)
      Inds=rbind(Inds,TempInds)
    }
  }
  
  #mins<-apply(Inds[,-c(1,2)], 2,min, na.rm=T)
  #maxs<-apply(Inds[,-c(1,2)], 2,max, na.rm=T)
  
  rm(TempInds)
  gridded(Inds) = ~x+y ## Creando el objeto Grid
  
  #writeAsciiGrid(Inds["RFinal"], na.value = -9999,paste(dir.out,"ITT.asc",sep=""))
  writeAsciiGrid(Inds["X1"], na.value = -9999,paste(dir.out,"ITT.asc",sep=""))
  writeAsciiGrid(Inds["X2"], na.value = -9999,paste(dir.out,"PTav.asc",sep=""))
  writeAsciiGrid(Inds["X3"], na.value = -9999,paste(dir.out,"PTP.asc",sep=""))
  writeAsciiGrid(Inds["X4"], na.value = -9999,paste(dir.out,"LSA.asc",sep=""))
}
#############################################
########################################
# Funcion generadora de Indices Mejorado
########################################

########################################################################
GenActIndex<-function(posic,TempsAll=TempsAll,coords=coords,x1=x1,y1=y1,out,method=method,modelim=modelim,modelm=modelm,estadios=estadios,xi=xi,steps=steps,filtro=NULL,DL=NULL,hfeno)
{
  d1<-1:length(x1);d2<-1:length(y1);filtroin=TRUE
  plon=d1[x1==coords[posic,1]]
  plat=d2[y1==coords[posic,2]]
  
  Table=data.frame(mini=c(TempsAll$zTmin1[plon,plat],TempsAll$zTmin2[plon,plat],TempsAll$zTmin3[plon,plat],TempsAll$zTmin4[plon,plat],TempsAll$zTmin5[plon,plat],TempsAll$zTmin6[plon,plat],TempsAll$zTmin7[plon,plat],TempsAll$zTmin8[plon,plat],TempsAll$zTmin9[plon,plat],TempsAll$zTmin10[plon,plat],TempsAll$zTmin11[plon,plat],TempsAll$zTmin12[plon,plat]),
                   maxi=c(TempsAll$zTmax1[plon,plat],TempsAll$zTmax2[plon,plat],TempsAll$zTmax3[plon,plat],TempsAll$zTmax4[plon,plat],TempsAll$zTmax5[plon,plat],TempsAll$zTmax6[plon,plat],TempsAll$zTmax7[plon,plat],TempsAll$zTmax8[plon,plat],TempsAll$zTmax9[plon,plat],TempsAll$zTmax10[plon,plat],TempsAll$zTmax11[plon,plat],TempsAll$zTmax12[plon,plat]))
  
  Table=Table/10;Table2=cbind(id=1:nrow(Table),Table) ## filtrar en esta temperatura
  # Table=data.frame(mini=rnorm(12,10,3),maxi=rnorm(12,20,3)) 
  
  if(length(Table[is.na(Table)])==0)
  {
    if(!is.null(filtro)){tmm=apply(Table,2,mean,na.rm=TRUE);if(tmm[1] > filtro[1] && tmm[2] < filtro[2]){filtroin=TRUE}else{filtroin=FALSE}}
    if(filtroin)
    {
      steps <- 48
      inmaduros <-  estadios[-(length(estadios)-1):-(length(estadios))]
      maduros   <-  estadios[(length(estadios)-1):(length(estadios))]
      nmax=nrow(Table)
      matriz<-matrix(0,ncol=2*(length(inmaduros)+2),nrow=nrow(Table))
      
      for(K in 1:length(inmaduros))
      {
        parametrosm <- hfeno$pmortal[[K]]
        funcionm <- as.expression(hfeno$mortal[[K]][[3]])
        
        RM=apply(Table2,1,RateI,Table2,K=K,parametrosm=parametrosm,funcionm=funcionm,nmax=nmax,steps=steps) ## procesamiento de tasa de desarrollo y mortalidad por cada temperatura
        matriz[,2*K-1]=RM[1,];matriz[,2*K]=RM[2,]
        #matriz[,2*K-1]=signif(RM[1,],1);matriz[,2*K]=signif(RM[2,],1)
      }
      
      parametrosc <- out
      #for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
      #formulac <- hfeno$ftazaeh_h
      funciont <- out
      RM=apply(Table2,1,RateI,Table2,(K+1),parametrosc,funciont=funciont,nmax=nmax,steps=steps)
      matriz[,2*(K)+1]=RM[1,];matriz[,2*(K)+2]=RM[2,]
      
      #  Fecundidad
      parametrosc <- hfeno$ptazaeh_h
      for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
      formulac <- hfeno$ftazaeh_h
      funciont <- as.expression(formulac[[3]])
      RM=apply(Table2,1,RateI,Table2,(K+2),parametrosc,funciont=funciont,nmax=nmax,steps=steps,J=3)
      matriz[,2*(K+1)+1]=RM[1,];matriz[,2*(K+1)+2]=RM[2,]
      
      temp1<-matriz[,1]
      temp2<-matriz[,2]
      #temp1<-temp2<-1;
      for(j in 2:K){temp1=matriz[,2*j-1]*(1-temp1);  temp2=matriz[,2*j]*(1-temp2)} ## aqui usaba como contador ala "i" en ves de la "j"
      #TF$Is1=temp1;TF$Is2=temp2 # maxima probabilidad de supervivencia de adultos
      
      TF <- data.frame(Ind=1:(nrow(Table)),Table) # Orden y Temperatura
      TF$TRE1 <- matriz[,2*(K)+1]
      TF$TRE2 <- matriz[,2*(K)+2]
      #TF$ls <- (1-MortT1)*(1-MortT2)*(1-MortT3) # INTERACCION ENTRE LA TRANSMISION Y LA MORTALIDAD
      #TF$ls1 <- apply(matriz[,2*(1:K)-1],1,prod, na.rm = TRUE)
      #TF$ls2 <- apply(matriz[,2*(1:K)],1,prod, na.rm = TRUE)
      TF$ls1 <- 1-temp1
      TF$ls2 <- 1-temp2
      TF$ITe1 <- TF$TRE1*TF$ls1
      TF$ITe2 <- TF$TRE2*TF$ls2
      TF$PPTe1<-TF$ITe1*matriz[,2*(K)+3]
      TF$PPTe2<-TF$ITe1*matriz[,2*(K)+4]
      
      meses=c(31,28,31,30,31,30,31,31,30,31,30,31);r1=rep(1,nrow(TF))
      
      ITT <- round(mean(((meses-1)*TF[,"ITe1"]+1*TF[,"ITe2"])/meses, na.rm = TRUE),3) # Supervivencia x Transmision
      PTav <- round(mean(((meses-1)*TF[,"TRE1"] + 1*TF[,"TRE2"])/meses, na.rm = TRUE),3) # Promedio de Transmisiones
      PTP<-round(mean(((meses-1)*TF[,"PPTe1"] + 1*TF[,"PPTe2"])/meses, na.rm = TRUE),3) # Supervivencia x Transmision x Oviposicion
      LSA <- round(mean(((meses-1)*TF[,"ls1"] + 1*TF[,"ls2"])/meses, na.rm = TRUE),3) # Supervivencia máxima
      
      indices=c(ITT=ITT,PTav=PTav,PTP=PTP,LSA=LSA)
      return(list(indices=indices, TF=TF))
    }else{indices=c(ITT=0,PTav=0,PTP=0,LSA=0);return(list(indices=indices))}
  }else{
    indices=c(ITT=NA,PTav=NA,PTP=NA,LSA=NA)
    return(list(indices=indices))
  }
  
}
##########################################################
##########################################################
GenMatriz<-function(dir1,dir2,ilon,ilat){
  archivos1=list.files(dir1,pattern="flt");archivos1=paste(dir1,"/",archivos1,sep="")
  archivos2=list.files(dir2,pattern="flt");archivos2=paste(dir2,"/",archivos2,sep="")
  
  Tmin1=readGDAL(archivos1[1])
  
  # para la extracion de los datos usamos la funcion:
  
  geodat=data.frame(coordinates(Tmin1))
  
  ###########################################
  ## Extraccion de las longitudes y latitudes
  
  x <- c(geodat[,1]);x <- unique(x)
  n1=length(x) ## tamaño
  
  y <- c(geodat[,2]); y <- unique(y)
  n2=length(y)
  
  #rm(geodat)
  
  #######################################################################
  ## Extraccion de las resoluciones tanto para longitud como para latitud
  
  v1=Tmin1@grid@cellsize[1]/2
  v2=Tmin1@grid@cellsize[2]/2
  
  
  ##############################################################
  ## Definiendo los rangos en la longitud y latitud con posicion
  
  r11=ilon[1];r12=ilon[2];r21=ilat[1];r22=ilat[2]
  
  k1=rownames(geodat[geodat[,1] >= (r11-v1) & geodat[,1] <= (r12+v1),])
  k2=rownames(geodat[geodat[,2] >= (r21-v2) & geodat[,2] <= (r22+v2),])
  
  sector=intersect(k1,k2)
  sector=as.numeric(sector)
  
  coords=geodat[sector,]
  
  rm(geodat)
  
  
  #################################################
  ## Definiendo los rangos en la longitud y latitud 
  
  ind1<-1:n1
  d1=x>=(r11-v1) & x<=(r12+v1)
  x1=x[d1]
  ind1=ind1[d1]
  
  
  ind2<-1:n2
  d2=y>=(r21-v2) & y<=(r22+v2)
  y1=y[d2]
  ind2=ind2[d2]
  
  
  ####################################################################################
  ## Creacion de la matriz que contiene los valores de la variable por cada coordenada
  
  z <- c(Tmin1@data[,1]);zTmin1=matrix(z,n1,n2);rm(Tmin1)
  zTmin1=zTmin1[ind1,ind2]; rownames(zTmin1)=x1; colnames(zTmin1)=y1
  Tmin2=readGDAL(archivos1[2])
  z <- c(Tmin2@data[,1]);zTmin2=matrix(z,n1,n2);rm(Tmin2)
  zTmin2=zTmin2[ind1,ind2]; rownames(zTmin2)=x1; colnames(zTmin2)=y1
  Tmin3=readGDAL(archivos1[3])
  z <- c(Tmin3@data[,1]);zTmin3=matrix(z,n1,n2);rm(Tmin3)
  zTmin3=zTmin3[ind1,ind2]; rownames(zTmin3)=x1; colnames(zTmin3)=y1
  Tmin4=readGDAL(archivos1[4])
  z <- c(Tmin4@data[,1]);zTmin4=matrix(z,n1,n2);rm(Tmin4)
  zTmin4=zTmin4[ind1,ind2]; rownames(zTmin4)=x1; colnames(zTmin4)=y1
  Tmin5=readGDAL(archivos1[5])
  z <- c(Tmin5@data[,1]);zTmin5=matrix(z,n1,n2);rm(Tmin5)
  zTmin5=zTmin5[ind1,ind2]; rownames(zTmin5)=x1; colnames(zTmin5)=y1
  Tmin6=readGDAL(archivos1[6])
  z <- c(Tmin6@data[,1]);zTmin6=matrix(z,n1,n2);rm(Tmin6)
  zTmin6=zTmin6[ind1,ind2]; rownames(zTmin6)=x1; colnames(zTmin6)=y1
  Tmin7=readGDAL(archivos1[7])
  z <- c(Tmin7@data[,1]);zTmin7=matrix(z,n1,n2);rm(Tmin7)
  zTmin7=zTmin7[ind1,ind2]; rownames(zTmin7)=x1; colnames(zTmin7)=y1
  Tmin8=readGDAL(archivos1[8])
  z <- c(Tmin8@data[,1]);zTmin8=matrix(z,n1,n2);rm(Tmin8)
  zTmin8=zTmin8[ind1,ind2]; rownames(zTmin8)=x1; colnames(zTmin8)=y1
  Tmin9=readGDAL(archivos1[9])
  z <- c(Tmin9@data[,1]);zTmin9=matrix(z,n1,n2);rm(Tmin9)
  zTmin9=zTmin9[ind1,ind2]; rownames(zTmin9)=x1; colnames(zTmin9)=y1
  Tmin10=readGDAL(archivos1[10])
  z <- c(Tmin10@data[,1]);zTmin10=matrix(z,n1,n2);rm(Tmin10)
  zTmin10=zTmin10[ind1,ind2]; rownames(zTmin10)=x1; colnames(zTmin10)=y1
  Tmin11=readGDAL(archivos1[11])
  z <- c(Tmin11@data[,1]);zTmin11=matrix(z,n1,n2);rm(Tmin11)
  zTmin11=zTmin11[ind1,ind2]; rownames(zTmin11)=x1; colnames(zTmin11)=y1
  Tmin12=readGDAL(archivos1[12])
  z <- c(Tmin12@data[,1]);zTmin12=matrix(z,n1,n2);rm(Tmin12)
  zTmin12=zTmin12[ind1,ind2]; rownames(zTmin12)=x1; colnames(zTmin12)=y1
  rm(z)
  
  Tmax1=readGDAL(archivos2[1])
  z <- c(Tmax1@data[,1]);zTmax1=matrix(z,n1,n2);rm(Tmax1)
  zTmax1=zTmax1[ind1,ind2]; rownames(zTmax1)=x1; colnames(zTmax1)=y1
  Tmax2=readGDAL(archivos2[2])
  z <- c(Tmax2@data[,1]);zTmax2=matrix(z,n1,n2);rm(Tmax2)
  zTmax2=zTmax2[ind1,ind2]; rownames(zTmax2)=x1; colnames(zTmax2)=y1
  Tmax3=readGDAL(archivos2[3])
  z <- c(Tmax3@data[,1]);zTmax3=matrix(z,n1,n2);rm(Tmax3)
  zTmax3=zTmax3[ind1,ind2]; rownames(zTmax3)=x1; colnames(zTmax3)=y1
  Tmax4=readGDAL(archivos2[4])
  z <- c(Tmax4@data[,1]);zTmax4=matrix(z,n1,n2);rm(Tmax4)
  zTmax4=zTmax4[ind1,ind2]; rownames(zTmax4)=x1; colnames(zTmax4)=y1
  Tmax5=readGDAL(archivos2[5])
  z <- c(Tmax5@data[,1]);zTmax5=matrix(z,n1,n2);rm(Tmax5)
  zTmax5=zTmax5[ind1,ind2]; rownames(zTmax5)=x1; colnames(zTmax5)=y1
  Tmax6=readGDAL(archivos2[6])
  z <- c(Tmax6@data[,1]);zTmax6=matrix(z,n1,n2);rm(Tmax6)
  zTmax6=zTmax6[ind1,ind2]; rownames(zTmax6)=x1; colnames(zTmax6)=y1
  Tmax7=readGDAL(archivos2[7])
  z <- c(Tmax7@data[,1]);zTmax7=matrix(z,n1,n2);rm(Tmax7)
  zTmax7=zTmax7[ind1,ind2]; rownames(zTmax7)=x1; colnames(zTmax7)=y1
  Tmax8=readGDAL(archivos2[8])
  z <- c(Tmax8@data[,1]);zTmax8=matrix(z,n1,n2);rm(Tmax8)
  zTmax8=zTmax8[ind1,ind2]; rownames(zTmax8)=x1; colnames(zTmax8)=y1
  Tmax9=readGDAL(archivos2[9])
  z <- c(Tmax9@data[,1]);zTmax9=matrix(z,n1,n2);rm(Tmax9)
  zTmax9=zTmax9[ind1,ind2]; rownames(zTmax9)=x1; colnames(zTmax9)=y1
  Tmax10=readGDAL(archivos2[10])
  z <- c(Tmax10@data[,1]);zTmax10=matrix(z,n1,n2);rm(Tmax10)
  zTmax10=zTmax10[ind1,ind2]; rownames(zTmax10)=x1; colnames(zTmax10)=y1
  Tmax11=readGDAL(archivos2[11])
  z <- c(Tmax11@data[,1]);zTmax11=matrix(z,n1,n2);rm(Tmax11)
  zTmax11=zTmax11[ind1,ind2]; rownames(zTmax11)=x1; colnames(zTmax11)=y1
  Tmax12=readGDAL(archivos2[12])
  z <- c(Tmax12@data[,1]);zTmax12=matrix(z,n1,n2);rm(Tmax12)
  zTmax12=zTmax12[ind1,ind2]; rownames(zTmax12)=x1; colnames(zTmax12)=y1
  rm(z)
  
  return(list(zTmin1=zTmin1,zTmin2=zTmin2,zTmin3=zTmin3,zTmin4=zTmin4,zTmin5=zTmin5,zTmin6=zTmin6,zTmin7=zTmin7,zTmin8=zTmin8,zTmin9=zTmin9,zTmin10=zTmin10,zTmin11=zTmin11,zTmin12=zTmin12,zTmax1=zTmax1,zTmax2=zTmax2,zTmax3=zTmax3,zTmax4=zTmax4,zTmax5=zTmax5,zTmax6=zTmax6,zTmax7=zTmax7,zTmax8=zTmax8,zTmax9=zTmax9,zTmax10=zTmax10,zTmax11=zTmax11,zTmax12=zTmax12,coords=coords,x1=x1,y1=y1))
}

####################################################################
####################################################################
p.area<-function(ilat,R){
  R=R+1
  lats=seq(ilat[1],ilat[2],length.out=R)+0.0000000001
  mat.lat=matrix(NA,R-1,2)
  for(i in 2:R) 
  {
    mat.lat[i-1,]=c(lats[i-1],lats[i])
  }
  return(mat.lat)
}


####################################################################
####################################################################

model.prac<-function(dat,Temp,Rep,Survi,Mort,f1,ff,ini)
{
  freqm=aggregate(dat[,Mort],list(Temperature=dat[,Temp]),sum)
  max1=aggregate(dat[,Survi],list(Temperature=dat[,Temp]),max)
  n1=aggregate(dat[,Mort],list(Temperature=dat[,Temp],Replicate=dat[,Rep]),length)
  n2=data.frame(table(n1[,1]))
  
  tablaM=data.frame(freqm,N=n2[,2]*max1[,2],Mort=freqm[,2]/(n2[,2]*max1[,2]))
  y=tablaM[,4]
  x=tablaM[,1]
  
  tabxy=data.frame(x,y)
  
  plot(x,y,axes=FALSE,xlim=c(0,max(x)+10),xlab="Temperature °C",ylab="Mortality",pch=19,col="lightblue")
  axis(1)
  axis(2)
  
  out <- nls(f1, start = ini,trace = TRUE,data=tabxy)
  
  return(list(tablaM=tablaM,out=out))
} 

create.obj<-function(parametros,funcion,Table)
{
  x<-apply(Table,1,mean)
  sal <- parametros
  for (i in names(sal))
  {
    temp <- sal[i]
    storage.mode(temp) <- "double"
    assign(i, temp)
  }
  Mort<-eval(funcion)
  return(Mort)
}


##########################################
# Funcion generadora de Tasas y mortalidad
##########################################
RateI<-function(vec,Table2,Ki,parametrosc=NULL,parametrosm=NULL,funciont=NULL,funcionm=NULL,nmax,steps,J=NA)
{
  #vec<-c(1,12,24)
  i=0:(steps-1)
  T1=((vec[3]-vec[2])/2)*cos(pi*(i+0.5)/steps) + (vec[3]+vec[2])/2
  if(vec[1]!=nmax){T2<-((vec[3]-Table2[vec[1]+1,2])/2)*cos(pi*(i+0.5)/steps) + (vec[3]+Table2[vec[1]+1,2])/2}else{
    T2<-((vec[3]-Table2[1,2])/2)*cos(pi*(i+0.5)/steps) + (vec[3]+Table2[1,2])/2}
  x <- T1
  x2 <- T2
  
  if(!is.null(funciont))
  {
    if(!is.na(J))
    {
      for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
      i=0:(steps-1)
      Rat=eval(funciont);if(!is.na(J)){Rat[Rat<=0]=0}
      Ratetot1<-(sum(Rat))/steps    ### aqui evalua la tasa de desarrollo de todas las divisiones
      x=x2;Rat=eval(funciont);if(!is.na(J)){Rat[Rat<=0]=0}
      Ratetot2<-(sum(Rat))/steps    ### aqui evalua la tasa de desarrollo de todas las divisiones del segundo dia
      Rate=(Ratetot1+Ratetot2)/2
    }else{
      out<-funciont
      Rat=predict(out,list(x=x));Rat[Rat<0]=0
      Ratetot1<-(sum(Rat))/steps    ### aqui evalua la tasa de desarrollo de todas las divisiones
      Rat=predict(out,list(x=x2));Rat[Rat<0]=0
      Ratetot2<-(sum(Rat))/steps    ### aqui evalua la tasa de desarrollo de todas las divisiones del segundo dia
      Rate=(Ratetot1+Ratetot2)/2
    }
    return(c(Rate1=Ratetot1,Rate2=Rate))
  }
  
  if(!is.null(funcionm))
  {
    for (i in names(parametrosm)){temp <- parametrosm[i];storage.mode(temp) <- "double";assign(i, temp)}
    x=T1;M1=unlist(eval(funcionm));M1[M1>1]=1;M1[is.nan(M1)]<-1;Mortality1 <- (sum(M1))/steps    ### aqui evalua la Mortalidad de todas las divisiones
    x=T2;M2=unlist(eval(funcionm));M2[M2>1]=1;M2[is.nan(M2)]<-1;Mortality2 <- (sum(M2))/steps    ### aqui evalua la Mortalidad de todas las divisiones del segundo dia
    Mortality=(Mortality1+Mortality2)/2
    return(c(Mortality1=Mortality1,Mortality2=Mortality))
  }else{return(c(Rate1=Ratetot1,Rate2=Rate))}
}