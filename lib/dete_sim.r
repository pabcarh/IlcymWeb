#################################
# Tasas variables ###############

RateR<-function(vec,Table2,nmax,steps,ff,paramR)
{
  for (i in names(paramR)){temp <- paramR[i];storage.mode(temp) <- "double";assign(i, temp)}
  i=0:(steps-1) 
  T1=((vec[3]-vec[2])/2)*cos(pi*(i+0.5)/steps) + (vec[3]+vec[2])/2
  if(vec[1]!=nmax){T2<-((vec[3]-Table2[vec[1]+1,2])/2)*cos(pi*(i+0.5)/steps) + (vec[3]+Table2[vec[1]+1,2])/2}else{
    T2<-((vec[3]-Table2[1,2])/2)*cos(pi*(i+0.5)/steps) + (vec[3]+Table2[1,2])/2}
  
  x=T1;r1=eval(ff[[3]]);r1[r1 < 0]=0;r1[r1 > 1]=1
  R1=(sum(r1))/steps
  x=T2;r1=eval(ff[[3]]);r1[r1 < 0]=0;r1[r1 > 1]=1
  R2=(sum(r1))/steps
  Rt=(R1+R2)/2
  return(Rt)
}
###########################
graphsimulp<-function(pars,modls=c(1,1,1,1,1,1),direc,ecua,nomec,ejex,ejey,tecu,grises,tit,ax,ay){
  
  if(grises==TRUE){ccol=c("gray20","gray50")}else{ccol=c("royalblue","red")}        
  
  
  
  etip=c("Rm","Ro","GRR","GL","Lambda","Dt")
  
  nfile=etip[!is.na(modls)]
  
  NMS=paste(nfile,collapse="_")
  
  pathIm = paste(direc,NMS,".jpg",sep="")
  
  #jpeg(pathIm, width = 720, height = 800,quality = 100)##
  
  
  
  
  
  mx<-mmx<-max(pars[,1],na.rm = TRUE);mnx<-min(pars[,1],na.rm = TRUE)
  
  
  
  if(mx<=40){mx=40}else{mx=mx+10}
  
  N=length(modls[!is.na(modls)]) 
  
  if(N==1){par(mfrow=c(1,1));  if(is.na(ejex)){ejex=-1.73};  if(is.na(ejey)){ejey=-1.61}  }
  
  if(N==2){par(mfrow=c(2,1));  if(is.na(ejex)){ejex=-0.69};  if(is.na(ejey)){ejey=-1.60}  }
  
  if(N==3 || N==4){par(mfrow=c(2,2));  if(is.na(ejex)){ejex=-0.86};  if(is.na(ejey)){ejey=-0.84}  }
  
  if(N==5 || N==6){par(mfrow=c(3,2));  if(is.na(ejex)){ejex=-0.66};  if(is.na(ejey)){ejey=-1.25}  }
  
  
  
  
  
  vars=c(2,3,4,5,6,7)
  nomv=c(Temper="temperature",r = expression(intrinsic~rate~group("(",r[m],")")),Ro = "net reproduction rate (Ro)",GRR = "gross reproduction rate (GRR)",T = "mean generation time (T) (days)",lambda = expression(finite~rate~of~increase~group("(",symbol("l"),")") ),Dt = "doubling time (Dt) (days)") ## nombre de los parametros
  modelss=c("cubic","quadratic","logarithmic","exponential")  ## tipo de modelos propuestos
  
  modsel<-matrix(NA,6,4)
  colnames(modsel)<-c("R2","R2_Adj","AIC","Deviance")
  sal2<-et<-list(1);ubix=20
  
  ax<-1.6
  ay<-1.6
  
  for(k in vars) ## gr?fico por cada par?metro
  { 
    if(ecua==TRUE){if(is.na(modls[k-1])){next}} ## este codigo hace que solo ejecute los parametros que tienen un modelo escogido
    my=max(pars[,k],na.rm = TRUE);mny=min(pars[,k],na.rm = TRUE)
    if(mny<(my-mny) & my>0){mny=0}
    
    par(family="serif",font=1,cex.axis=1.8,cex.lab=1.8)
    plot(pars[,1],pars[,k],xlim=c(0,mx),ylim=c(mny-0.05*(abs(mny)),my+0.13*(abs(my))),xlab="Temperature (°C)",ylab=nomv[k],pch=19,col=ccol[1],mgp = c(1.5, 1, 0),axes=FALSE)  ## 1
    axis(1,at=seq(0,mx,by=10),line=ejex,cex.axis=ax) ## falta incrementar los tamaños de los subtitulos a 12
    axis(2,las=2,line=ejey-0.5,cex.axis=ay)
    
    x=pars[,1]
    y=pars[,k]
    
    if(ecua==TRUE)
    {
      
      ## Aqui se definen las funciones de los modelos
      
      if(modls[k-1]==1){f1=as.formula(y ~ b0 + b1*x + b2*x^2 + b3*x^3);inis=list(b0=1,b1=1,b2=2,b3=2);ff <- function(x){b0 + b1*x + b2*x^2 + b3*x^3}}
      
      if(modls[k-1]==2){f1=as.formula(y ~ b0 + b1*x + b2*x^2);inis=list(b0=1,b1=1,b2=2);ff <- function(x){b0 + b1*x + b2*x^2}}
      
      if(modls[k-1]==3){f1=as.formula(y ~ b0 + b1*x + b2*log(x));inis0=coef(lm(y~x+I(log(x))));names(inis0)=NULL;inis=list(b0=inis0[1],b1=inis0[2],b2=inis0[3]);ff <- function(x){b0 + b1*x + b2*log(x)}}
      
      if(modls[k-1]==4){f1=as.formula(y ~ b0 + b1*x + b2*exp(x));inis0=coef(lm(y~x+I(exp(x))));names(inis0)=NULL;inis=list(b0=inis0[1],b1=inis0[2],b2=inis0[3]);ff <- function(x){b0 + b1*x + b2*exp(x)}}
      
      
      
      if(tit==TRUE){title(paste("Using the",modelss[modls[k-1]],"model"))}
      
      
      
      ## Se estiman los parametros usando el algoritmo marc quart
      
      out <- nls(f1, start = inis,trace = TRUE)
      
      yl=fitted(out)
      
      sqe=sum(residuals(out)^2)
      
      sal <- coef(out);sal2[[k-1]]=sal
      
      
      
      r<-1-sqe/sum((y-mean(y))^2) # este es el R^2
      
      r_ajus<- 1 - ((length(x) - 1) / (length(x) - length(sal))) * (1-r)  # este es el R^2 adjus
      
      AC<-AIC(out) # este es el AIC 
      
      Dev=deviance(out) # este es el indicador  Deviance
      
      
      
      modsel0<-c(R2=round(r,3),R2_Adj=round(r_ajus,3),AIC=round(AC,3),Deviance=round(Dev,3))
      
      modsel[k-1,]<-modsel0
      
      
      
      ## En esta parte solo se agrega la acuacion al gr?fico
      
      if((k-1)==1)
        
      {
        
        if(modls[k-1]==1){et[[k-1]]=expression(text(ubix, my+0.09*(my),substitute(bolditalic(r[m]) == list(sal1) + list(sal2)*T + list(sal3)*T^2 + list(sal4)*T^3,list(sal1=sal[1],sal2=sal[2],sal3=sal[3],sal4=sal[4])),cex=tecu))}
        
        if(modls[k-1]==2){et[[k-1]]=expression(text(ubix, my+0.09*(my),substitute(bolditalic(r[m]) == list(sal1) + list(sal2)*T + list(sal3)*T^2,list(sal1=sal[1],sal2=sal[2],sal3=sal[3])),cex=tecu))}
        
        if(modls[k-1]==3){et[[k-1]]=expression(text(ubix, my+0.09*(my),substitute(bolditalic(r[m]) == list(sal1) + list(sal2)*T + list(sal3)*log(T),list(sal1=sal[1],sal2=sal[2],sal3=sal[3])),cex=tecu))}
        
        if(modls[k-1]==4){et[[k-1]]=expression(text(ubix, my+0.09*(my),substitute(bolditalic(r[m]) == list(sal1) + list(sal2)*T + list(sal3)*e^(T),list(sal1=sal[1],sal2=sal[2],sal3=sal[3])),cex=tecu))}
        
      }
      
      
      
      if((k-1)==2 || (k-1)==3 || (k-1)==4 || (k-1)==6)
        
      {
        
        if(modls[k-1]==1){et[[k-1]]=expression(text(ubix, my+0.09*(my),substitute(bolditalic(list(etip1)) == list(sal1) + list(sal2)*T + list(sal3)*T^2 + list(sal4)*T^3,list(etip1=etip[k-1],sal1=sal[1],sal2=sal[2],sal3=sal[3],sal4=sal[4])),cex=tecu))}
        
        if(modls[k-1]==2){et[[k-1]]=expression(text(ubix, my+0.09*(my),substitute(bolditalic(list(etip1)) == list(sal1) + list(sal2)*T + list(sal3)*T^2,list(etip1=etip[k-1],sal1=sal[1],sal2=sal[2],sal3=sal[3])),cex=tecu))}
        
        if(modls[k-1]==3){et[[k-1]]=expression(text(ubix, my+0.09*(my),substitute(bolditalic(list(etip1)) == list(sal1) + list(sal2)*T + list(sal3)*log(T),list(etip1=etip[k-1],sal1=sal[1],sal2=sal[2],sal3=sal[3])),cex=tecu))}
        
        if(modls[k-1]==4){et[[k-1]]=expression(text(ubix, my+0.09*(my),substitute(bolditalic(list(etip1)) == list(sal1) + list(sal2)*T + list(sal3)*e^(T),list(etip1=etip[k-1],sal1=sal[1],sal2=sal[2],sal3=sal[3])),cex=tecu))}
        
      } 
      
      
      
      if((k-1)==5)
        
      {
        
        if(modls[k-1]==1){et[[k-1]]=expression(text(ubix, my+0.09*(my),substitute(symbol(list(etip1)) == list(sal1) + list(sal2)*T + list(sal3)*T^2 + list(sal4)*T^3,list(etip1=etip[k-1],sal1=sal[1],sal2=sal[2],sal3=sal[3],sal4=sal[4])),cex=tecu))}
        
        if(modls[k-1]==2){et[[k-1]]=expression(text(ubix, my+0.09*(my),substitute(symbol(list(etip1)) == list(sal1) + list(sal2)*T + list(sal3)*T^2,list(etip1=etip[k-1],sal1=sal[1],sal2=sal[2],sal3=sal[3])),cex=tecu))}
        
        if(modls[k-1]==3){et[[k-1]]=expression(text(ubix, my+0.09*(my),substitute(symbol(list(etip1)) == list(sal1) + list(sal2)*T + list(sal3)*log(T),list(etip1=etip[k-1],sal1=sal[1],sal2=sal[2],sal3=sal[3])),cex=tecu))}
        
        if(modls[k-1]==4){et[[k-1]]=expression(text(ubix, my+0.09*(my),substitute(symbol(list(etip1)) == list(sal1) + list(sal2)*T + list(sal3)*e^(T),list(etip1=etip[k-1],sal1=sal[1],sal2=sal[2],sal3=sal[3])),cex=tecu))}
        
      }
      
      if(nomec==TRUE){eval(et[[k-1]])}
      
      for (i in names(sal))
        
      { 
        
        temp <- sal[i] 
        
        storage.mode(temp) <- "double"
        
        assign(i, temp)
        
      }
      
      curve(ff,add=TRUE,from=mnx,to=mmx,col=ccol[2],lwd=2)  ## 2
      
    }
    
  }
  
  #dev.off()
  
  
  
  if(ecua==TRUE) 
    
  {
    
    #jpeg(paste(direc,"Indicadores",NMS,".jpg",sep=""), width = 770, height = 500,quality = 100)##
    
    
    
    plot(1:12,1:12,type="n",axes=FALSE,xlab="",ylab="")
    
    grid(0,6,lty=1)
    
    ubix=8.5;ubix2=8.5
    
    lines(c(4.5,4.5),c(0,13),lty=1,lwd=2,col="gray90",type = "l")
    
    box(lty = "solid", col = "gray30")
    
    
    
    if(!is.na(modls[1])){
      
      sal=sal2[[1]]
      
      my=(12/1.09);k=2;if(modls[1]==1){ubix=8.5};if(modls[1]==2){ubix=7.6};if(modls[1]==3 || modls[1]==4){ubix=6.7}
      
      text(1.8,12,nomv[2], col="gray40") ## para rm
      
      eval(et[[1]])
      
      text(ubix2-3, my*(1.09)-0.94,  substitute(bolditalic(R^{2})==list(r2),list(r2=modsel[1,1])))
      
      text(ubix2-1.2, my*(1.09)-0.97,  substitute(bolditalic(R^{2}~Adj)==list(r2a),list(r2a=modsel[1,2])))
      
      text(ubix2+0.6, my*(1.09)-1,  substitute(bolditalic(AIC)==list(aic),list(aic=modsel[1,3])))
      
      text(ubix2+2.6, my*(1.09)-1,  substitute(bolditalic(Deviance)==list(dev),list(dev=modsel[1,4])))
      
    }
    
    
    
    if(!is.na(modls[2])){
      
      sal=sal2[[2]]
      
      my=(12/1.09)-1.8;k=3;if(modls[2]==1){ubix=8.5};if(modls[2]==2){ubix=7.6};if(modls[2]==3 || modls[2]==4){ubix=6.7}
      
      text(2.2,10,nomv[3], col="gray40") ## para Ro
      
      eval(et[[2]])
      
      text(ubix2-3, my*(1.09)-0.94,  substitute(bolditalic(R^{2})==list(r2),list(r2=modsel[2,1])))
      
      text(ubix2-1.2, my*(1.09)-0.97,  substitute(bolditalic(R^{2}~Adj)==list(r2a),list(r2a=modsel[2,2])))
      
      text(ubix2+0.6, my*(1.09)-1,  substitute(bolditalic(AIC)==list(aic),list(aic=modsel[2,3])))
      
      text(ubix2+2.6, my*(1.09)-1,  substitute(bolditalic(Deviance)==list(dev),list(dev=modsel[2,4])))
      
    }
    
    
    
    if(!is.na(modls[3])){
      
      sal=sal2[[3]]
      
      my=(12/1.09)-3.6;k=4;if(modls[3]==1){ubix=8.5};if(modls[3]==2){ubix=7.6};if(modls[3]==3 || modls[3]==4){ubix=6.7}
      
      text(2.5,8,nomv[4], col="gray40") ## para GRR
      
      eval(et[[3]])
      
      text(ubix2-3, my*(1.09)-0.94,  substitute(bolditalic(R^{2})==list(r2),list(r2=modsel[3,1])))
      
      text(ubix2-1.2, my*(1.09)-0.97,  substitute(bolditalic(R^{2}~Adj)==list(r2a),list(r2a=modsel[3,2])))
      
      text(ubix2+0.6, my*(1.09)-1,  substitute(bolditalic(AIC)==list(aic),list(aic=modsel[3,3])))
      
      text(ubix2+2.6, my*(1.09)-1,  substitute(bolditalic(Deviance)==list(dev),list(dev=modsel[3,4])))
      
    }
    
    
    
    if(!is.na(modls[4])){
      
      sal=sal2[[4]]
      
      my=(12/1.09)-5.4;k=5;if(modls[4]==1){ubix=8.5};if(modls[4]==2){ubix=7.6};if(modls[4]==3 || modls[4]==4){ubix=6.7}
      
      text(2.5,6,nomv[5], col="gray40") ## para GL
      
      eval(et[[4]])
      
      text(ubix2-3, my*(1.09)-0.94,  substitute(bolditalic(R^{2})==list(r2),list(r2=modsel[4,1])))
      
      text(ubix2-1.2, my*(1.09)-0.97,  substitute(bolditalic(R^{2}~Adj)==list(r2a),list(r2a=modsel[4,2])))
      
      text(ubix2+0.6, my*(1.09)-1,  substitute(bolditalic(AIC)==list(aic),list(aic=modsel[4,3])))
      
      text(ubix2+2.6, my*(1.09)-1,  substitute(bolditalic(Deviance)==list(dev),list(dev=modsel[4,4])))
      
    }
    
    
    
    if(!is.na(modls[5])){
      
      sal=sal2[[5]]
      
      my=(12/1.09)-7.2;k=6;if(modls[5]==1){ubix=8.5};if(modls[5]==2){ubix=7.6};if(modls[5]==3 || modls[5]==4){ubix=6.7}
      
      text(2.4,4,nomv[6], col="gray40") ## para lambda
      
      eval(et[[5]])
      
      text(ubix2-3, my*(1.09)-0.94,  substitute(bolditalic(R^{2})==list(r2),list(r2=modsel[5,1])))
      
      text(ubix2-1.2, my*(1.09)-0.97,  substitute(bolditalic(R^{2}~Adj)==list(r2a),list(r2a=modsel[5,2])))
      
      text(ubix2+0.6, my*(1.09)-1,  substitute(bolditalic(AIC)==list(aic),list(aic=modsel[5,3])))
      
      text(ubix2+2.6, my*(1.09)-1,  substitute(bolditalic(Deviance)==list(dev),list(dev=modsel[5,4])))
      
    }
    
    
    
    if(!is.na(modls[6])){
      
      sal=sal2[[6]]
      
      my=(12/1.09)-9;k=7;if(modls[6]==1){ubix=8.5};if(modls[6]==2){ubix=7.6};if(modls[6]==3 || modls[6]==4){ubix=6.7}
      
      text(2,2,nomv[7], col="gray40") ## para Dt
      
      eval(et[[6]])
      
      text(ubix2-3, my*(1.09)-0.94,  substitute(bolditalic(R^{2})==list(r2),list(r2=modsel[6,1])))
      
      text(ubix2-1.2, my*(1.09)-0.97,  substitute(bolditalic(R^{2}~Adj)==list(r2a),list(r2a=modsel[6,2])))
      
      text(ubix2+0.6, my*(1.09)-1,  substitute(bolditalic(AIC)==list(aic),list(aic=modsel[6,3])))
      
      text(ubix2+2.6, my*(1.09)-1,  substitute(bolditalic(Deviance)==list(dev),list(dev=modsel[6,4])))
      
    }
    
    
    
    #dev.off()
    
  }
  
  return(list(coefs=sal2,modsel=modsel,et=et, pathIm=pathIm))
  
  
}
#############################################################################################################
##########################################################################################################

simultemp<-function(N,sexratio,isFixed,temp,reps,xi,steps, poli, params, ..){
  n1=length(temp)
  hfeno=params$hfeno
  estadios<-params$estadios
  modelim<-params$modelim
  modelm<-params$modelm  
  modelim=c(modelim,modelm)
  pars=data.frame(Temper=NA,r=NA,Ro=NA,GRR=NA,T=NA,lambda=NA,Dt=NA)
  
  for(i in 1:n1)
  {
    Table=data.frame(V1=rep(temp[i],365),V2=rep(temp[i],365))
    
    if(isFixed){
      Rs=rep(sexratio,365)
    }else{
      paramR=params$paramR
      if(is.na(paramR)){
        sexratio <- params$ff
        Rs=rep(sexratio,365)
      }else{
        Table2=cbind(id=1:nrow(Table),Table)
        #paramR=params$paramR
        ff=params$ff
        Rs=apply(Table2,1,RateR,Table2,nmax=nrow(Table),steps=steps,ff,paramR)
      }
    }
    
    
    for(j in 1:reps[i])
    {
      #cuadro<-Ratios(Table,modelim,modelme,estadios, xi,steps)$MATRIZ
      
      cuadro <- matrizA(estadios, hfeno, Table, steps, modelim=modelim)
      #insectos<-simulacion(cuadro,estadios,N,M,hfeno,sexratio)
      
      
      ageclases <- AgeClases(cuadro, estadios, hfeno)
      estadiosAges <- ageclases$estadiosAges
      oviFreq <- ageclases$oviFreq
      ageClassMatriz <- ageclases$ageClassMatriz
      Day=1
      severalYear=FALSE
      Steps=364
      simu2<-simulacionUnaGeneracion(Day,estadiosAges, oviFreq, ageClassMatriz, hfeno, cuadro, estadios,N,sexratio,Rs,Steps)
      matrizOut<-simu2$matrizOut
      
      pravida<-parameters(N, estadios, matrizOut,poli )
      parametros<-pravida$parametro
      pars=rbind(pars,c(Temper=temp[i],t(parametros)))
    }
  }
  pars=pars[-1,]
  return(pars)
}

###############
# Desacumulador

descum<-function(vec){dc<-c(1);dc[1]=vec[1];for(i in 2:length(vec)){dc[i]=vec[i]-vec[i-1]};return(dc)}


#########################################################################
life.tableD<-function(cuadro,estad,n,m,hfeno,Rs,Amp=1)
{
  tab1 <- matrix(0,nrow = m,ncol = (length(estad)-1))
  tab2 <- matrix(0,nrow = m,ncol = 3) ## Siempre son 3 columnas asumiendo que solo se tiene Hembra, Macho y Oviposici?n
  
  k=1:(length(estad)-2) ## inmaduros
  k2=length(k)+1 ## maduros
  
  colnames(tab1)=c(estad[k],"nMad")
  colnames(tab2)=c(estad[-k],"Oviposicion")
  
  tab1[1,1]=n
  
  Insect_age <- (cuadro[1,k*2-1] / 2)
  Insect_age_m<-rep(0,2)
  survival_p <- 1
  Rat<-c(1);Rat[1]=0;Ov=0
  
  for(Day in 2:m)
  {
    ##########
    # Inmaduro
    ##########
    
    dev_p<-dev_p_m<-c(1)
    djx <- cuadro[Day-1,k*2-1]
    mjx <- cuadro[Day-1,k*2]
    
    #############################
    # Calcula la edad fisiologica
    Insect_age <- Insect_age + djx
    for(i in k){dev_p[i]=distrimodel(Insect_age[i],k[i])} ## probabilidad de desarrollo
    
    #############################################
    # Se evalua la cantidad de insectos que viven 
    survival_p = survival_p * ((1 - mjx/Amp) ^ (djx/Amp))  ## Probabilidad de supervivencia
    nviv=round(survival_p*tab1[Day-1,-ncol(tab1)])   ## El valor que multiplica ala supervivencia debe ser el numero de insectos vivos el dia anterior
    
    #######################################################################
    # Se evalua la cantidad de insectos que desarrollan al siguiente estado
    ndes=round(dev_p*nviv)
    ndif=nviv-ndes
    tab1[Day,]=c(ndif,0)+c(0,ndes)
    for(i in k){if(tab1[Day-1,k[i]]==0 & tab1[Day,k[i]]>0){survival_p[i] = 1;Insect_age[i] = djx[i] / 2}}
    
    
    ########
    # Maduro
    ########
    
    
    ################################################################
    # Se evalua la probabilidad de que el insecto viva en cierto dia
    djx <- c(cuadro[Day,k2*2-1],cuadro[Day,k2*2])
    if(tab1[Day-1,k2]==0 & tab1[Day,k2]>0){Insect_age_m = c(cuadro[Day,k2*2-1]/2,cuadro[Day,k2*2]/2)}else{Insect_age_m = Insect_age_m + (djx / 2)} ## Edad fisiologica
    Insect_age_m = Insect_age_m + (djx / 2)
    
    for(j in 1:2){dev_p_m[j]=distriModelAdults(Insect_age_m[j],k2+j-1,length(k))}
    
    
    ############################################
    # Se evalua la cantidad de adultos que viven 
    
    ndes2=c(round(Rs[Day]*tab1[Day,k2]),round((1-Rs[Day])*tab1[Day,k2])) ## Suma de Nro de Hembras y Machos desarrollados
    nvivt=c(tab2[Day-1,1],tab2[Day-1,2])
    nviv2=nvivt-round(c(dev_p_m[1]*nvivt[1],dev_p_m[2]*nvivt[2])) ## Sobrevivientes del dia anterior para este dia
    
    ntot=ndes2+nviv2
    tab2[Day,]=c(ntot,cuadro[Day,k2*2+1]*ntot[1])
    
    if(tab2[Day,1]>0){Ov=1}
    
    ################################################
    # Se evalua la cantidad de huevos que se generan
    
    parametrosc <- hfeno$povih_h
    for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
    formulac <- hfeno$fovih_h
    funcionO <- as.expression(formulac[[3]])
    
    x=Insect_age_m[1];Rat[Day]=eval(funcionO)*Ov
    
  }
  Ov2=rep(1,m);Ov2[tab2[,1]==0]=0 ## Para considerar solo los valores de vida de las hembras
  Rat=descum(Rat)
  lifeT=data.frame(tab1[,-k2],tab2[,1:2],Oviposicion=round(tab2[,3]*Rat*Ov2)) ## En la oviposicion las tasas relativas "Rat" se multiplican con la oviposicion total
  ncero=apply(lifeT,1,sum);ncero=ncero==0
  lifeT=lifeT[!ncero,]
  return(list(life.table=lifeT))
  
}

#########################################3
#######################################
matriz<-function(data)
{
  mes<-c(31,28,31,30,31,30,31,31,30,31,30,31)
  mat<-cbind(round(seq(data[,1][1],data[,1][2],length=mes[1]),1),round(seq(data[,2][1],data[,2][2],length=mes[1]),1))
  for(i in 2:nrow(data))
  {
    if(i != nrow(data)) mat<-rbind(mat,cbind(round(seq(data[,1][i],data[,1][i+1],length=mes[i]),1),round(seq(data[,2][i],data[,2][i+1],length=mes[i]),1)))
    else mat<-rbind(mat,cbind(round(seq(data[,1][i],data[,1][i-11],length=mes[i]),1),round(seq(data[,2][i],data[,2][i-11],length=mes[i]),1)))
  }
  return(list(temperaturas=mat))
}
################################################
#####################################################################
#####################################################################
AgeClases<- function(cuadro, estadios, hfeno){
  matrizA=cuadro
  sizeMatrizA = nrow(matrizA)
  inmad <-  estadios[-(length(estadios)-1):-(length(estadios))]
  numInmaduros = length(inmad)
  maduros   <-  estadios[(length(estadios)-1):(length(estadios))]
  numMaduros = length(maduros)
  estadiosAges<-list(1)
  ageClassMatriz = matrix(NA, nrow=sizeMatrizA, ncol=(numInmaduros+numMaduros))
  
  Day = 1
  Ageclass = 1
  age = 0
  
  #primera linea
  for(k in 1:(numInmaduros + numMaduros)){
    Ageclass = 1
    age = 0
    Ageclass0 = c(rep(NA, 253))# porq 253??
    matrizAges = matrix(NA, nrow=sizeMatrizA, ncol=(length(Ageclass0)+1))
    #matrizAges = matrix(NA, nrow=366, ncol=(length(Ageclass0)+1))
    matrizAges[Day,1] = 0
    
    ## if(k==1){
    ##     Ageclass = Ageclass + 1
    ## }
    
    Ageclass = Ageclass + 1
    #for(Ii in 2:length(Ageclass0)){
    for(Ii in 1:length(Ageclass0)){
      if(k == (numInmaduros + numMaduros)){
        Ageclass0[Ii] = matrizA[(sizeMatrizA+1)-Ii,k*2-2]
      }else{
        Ageclass0[Ii] = matrizA[(sizeMatrizA+1)-Ii,k*2-1]
      }
      
      age = age + Ageclass0[Ii]
      matrizAges[Day,Ii+1] = age
    }
    if(k <= numInmaduros){
      ageClassMatriz[Day,k] = length(matrizAges[1,][matrizAges[1,]<2])+1
    }else{
      ageClassMatriz[Day,k] = length(matrizAges[1,][matrizAges[1,]<1.8])+1
    }
    estadiosAges[[k]] = matrizAges
  }
  
  # a partir de la 2da linea
  for(k in 1:(numInmaduros + numMaduros)){
    Day=2
    Ageclass = 1
    age = 0
    matrizAges = estadiosAges[[k]]
    while(Day <= sizeMatrizA){#porq 367 en excel?
      matrizAges[Day,1] = 0
      if(k == (numInmaduros + numMaduros)){
        Rate = matrizA[Day,k*2-2]
      }else{
        Rate = matrizA[Day,k*2-1]
      }
      
      if(k <= numInmaduros){
        while(age < 2){ # cambiar a valor del slop
          ageacc = matrizAges[Day-1, Ageclass]
          age = ageacc + Rate
          matrizAges[Day, Ageclass+1] = age
          Ageclass = Ageclass + 1
          
          if(Ageclass == 254){ ## por que 254?????
            age = 3
          }
        }
        ageClassMatriz[Day,k] = Ageclass
      }else{
        while(age < 1.8){ # cambiar a valor del slop
          ageacc = matrizAges[Day-1, Ageclass]
          age = ageacc + Rate
          matrizAges[Day, Ageclass+1] = age
          Ageclass = Ageclass + 1
          
          
          if(Ageclass == 254){     ## antes : if(Ageclass == 255){
            age = 3
          }
        }
        ageClassMatriz[Day,k] = Ageclass
      }
      age = 0
      Day = Day + 1
      Ageclass = 1
    }
    estadiosAges[[k]] = matrizAges
  }
  
  ## Ovifreq ##
  matrizAges = estadiosAges[[numInmaduros+1]]
  oviFreq = matrix(NA, nrow=sizeMatrizA, ncol=length(Ageclass0))
  Day = 1
  Ageclass = 1
  age1 = 0
  age2 = 0
  
  parametrosc <- hfeno$povih_h
  parametrosc<-as.list(parametrosc)
  for (i in names(parametrosc)){
    temp <- parametrosc[[i]]
    storage.mode(temp) <- "double"
    assign(i, temp)
  }
  
  formulac <- hfeno$fovih_h
  forexc <- formulac[[length(formulac)]]
  funcionc <- as.expression(forexc)
  
  while(Day < sizeMatrizA){#porq 367 en excel?
    matrizAges[Day,1] = 0
    age1 = matrizAges[Day, Ageclass]
    x = age1
    OviFrec1 = eval(funcionc)
    
    while(age2 < 1.8 & Ageclass<254){
      age2 = matrizAges[Day, Ageclass]
      x= age2
      OviFrec2 = eval(funcionc)
      OviFrec = OviFrec2 - OviFrec1
      OviFrec1 = OviFrec2
      if(Ageclass == 1){
        OviFrec = 0
      }
      oviFreq[Day,Ageclass] = OviFrec
      Ageclass = Ageclass + 1
    }
    age2 = 0
    Day = Day + 1
    Ageclass = 1
  }
  
  
  return(list(estadiosAges=estadiosAges, oviFreq=oviFreq, ageClassMatriz=ageClassMatriz))
}

#############################################################
simulation2<-function(Day, estadiosAges, oviFreq, ageClassMatriz, hfeno, cuadro, estadios,numIni,sexratio,Rs, Steps,severalYear){
  #Step=0
  
  matrizA = cuadro
  
  sizeMatriz = nrow(matrizA)
  
  inmad <-  estadios[-(length(estadios)-1):-(length(estadios))]
  
  numInmaduros = length(inmad)
  
  maduros   <-  estadios[(length(estadios)-1):(length(estadios))]
  
  numMaduros = length(maduros)
  
  NEWIND = 0
  
  dias<-254#porq?
  
  
  
  matrizOut = matrix(NA, nrow=(nrow(matrizA)), (numInmaduros+numMaduros+1))
  
  #matrizOut[1,1] = numIni# numero inicial de individuos
  
  stagesN = c(rep(0,dias))
  
  stagesN1 = c(numIni, rep(0,(dias-1)))
  
  femalesN = c(rep(NA,dias))
  
  eggsN = stagesN1
  
  stagesDev = c(rep(0,dias))
  
  Step=0
  
  
  
  ## vectorN <- list(1)
  
  ## vectorNini <- list(1)
  
  
  
  if(!severalYear){
    
    vectorN <- list(1)
    
    vectorNini <- list(1)
    
    for(k in 1:(numInmaduros + numMaduros)){
      
      #v1 = c(100,rep(0,(ageClassMatriz[1,k]-1)))
      v1 = c(numIni,rep(0,(ageClassMatriz[1,k]-1)))####################
      
      v2 = c(0,rep(0,(ageClassMatriz[1,k]-1)))
      
      v3 = c(0,rep(0,(ageClassMatriz[1,k]-1)))#
      
      
      
      if(k == 1){
        
        vectorN[[k]] = list(v1,v2,v3)#
        
        vectorNini[[k]] = v1
        
      }else{
        
        vectorN[[k]] = list(v2,v2,v3)#
        
        vectorNini[[k]] = v2
        
      }
      
    }
    
    
    
    matrizOut[1,1] = numIni# numero inicial de individuos
    matrizOut[1,2:ncol(matrizOut)] = 0#####################
    
  }else{
    
    vectorNini <- list(1)
    
    for(k in 1:(numInmaduros + numMaduros)){
      
      vectorNini[[k]] <- vectorN[[k]][[1]]
      
      matrizOut[1,k] = round(numInd[[k]])
      
    }
    
  }
  
  
  
  #while(Day < (sizeMatriz-1)){
  
  while(Step < Steps){
    
    for(k in 1:(numInmaduros + numMaduros)){
      
      Ageclass = ageClassMatriz[Day+1,k]
      
      
      
      if(k <= numInmaduros){
        
        stagesN = c(rep(0,Ageclass))
        
      }
      
      
      
      if(k == (numInmaduros+1)){
        
        femalesN = c(rep(0,Ageclass))
        
      }
      
      
      
      matrizAges = estadiosAges[[k]]
      
      NEWIND = 0
      
      
      
      E <- c(rep(0, Ageclass))
      
      EL <- c(rep(0, Ageclass))
      
      Eage <- c(rep(0, Ageclass))
      
      pacc <- c(rep(0, Ageclass))
      
      p1 <- c(rep(0, Ageclass))
      
      p2 <- c(rep(0, Ageclass))
      
      
      
      if(k <= numInmaduros){
        
        s1 <- c(rep(NA, Ageclass))
        
      }
      
      
      
      if(k == numInmaduros+1){
        
        vectorN[[k]][[2]] = vectorN[[k]][[3]]#
        
      }
      
      if(k == numInmaduros+2){
        
        vectorN[[k]][[2]] = vectorN[[k]][[3]]#
        
      }
      
      
      
      for(Ii in 1:(Ageclass-1)){
        
        Eage[Ii] = matrizAges[Day+1, Ii+1]
        
        
        
        if(k <= numInmaduros){
          
          survivalk = (1 - matrizA[Day,k*2]) ^ matrizA[Day,k*2-1]
          
        }
        
        
        
        if(k <= numInmaduros){
          
          if(Eage[Ii] > 1){
            
            s1[Ii] = 1
            
          }else{
            
            s1[Ii] = survivalk
            
          }
          
        }
        
        
        
        if(k <= numInmaduros){
          
          pacc[Ii] = distrimodeldeim(Eage[Ii], k, hfeno)
          
        }else{
          
          pacc[Ii] = distrimodeldema(Eage[Ii], k - numInmaduros, hfeno)
          
        }
        
        
        
        if(Ii == 1){
          
          p1[Ii] = pacc[Ii] - 0
          
        }else{
          
          p1[Ii] = pacc[Ii] - pacc[Ii - 1]
          
        }
        
        
        
        if(p1[Ii] == 0){
          
          p2[Ii] = 0
          
        }else{
          
          if(Ii == 1){
            
            p2[Ii] = p1[Ii] / (1 - 0)
            
          }else{
            
            p2[Ii] = p1[Ii] / (1 - pacc[Ii - 1])
            
          }
          
        }
        
        
        
        if(Day==1){
          
          E[Ii] = vectorNini[[k]][Ii]
          
        }else{
          
          E[Ii] = vectorN[[k]][[1]][Ii]#modifique el 1 por el 2
          
        }
        
        
        
        
        
        if(k <= numInmaduros){
          
          EL[Ii] = E[Ii] * p2[Ii] * s1[Ii]
          
        }else{
          
          EL[Ii] = E[Ii] * p2[Ii]
          
        }
        
        
        
        if(k <= numInmaduros){
          
          E1 = E[Ii] * s1[Ii] - EL[Ii]
          
        }else{
          
          E1 = E[Ii] - EL[Ii]
          
        }
        
        
        
        stagesN[Ii+1] = E1
        
        
        
        if(k <= numInmaduros){
          
          NEWIND = NEWIND + EL[Ii]
          
        }
        
        
        
      } # fin for clases
      
      
      
      vectorN[[k]][[2]] = c(vectorN[[k]][[2]][1], stagesN[2:length(stagesN)])
      
      vectorN[[k]][[1]] = vectorN[[k]][[2]]
      
      
      
      if(k <= (numInmaduros-1)){
        
        vectorN[[k]][[3]][1]=NEWIND
        
        vectorN[[k+1]][[2]] = vectorN[[k]][[3]]
        
      }
      
      
      
      if(k == numInmaduros){
        
        vectorN[[k+1]][[3]][1] = NEWIND * Rs[Day]
        
        vectorN[[k+2]][[3]][1] = NEWIND * (1-Rs[Day])
        
        
        
        vectorN[[k+1]][[2]] = vectorN[[k]][[3]]#
        
      }
      
      
      
      if(k == (numInmaduros+1)){
        
        femalesN = E
        
      }
      
      
      
      matrizOut[Day+1,k] = round(sum(vectorN[[k]][[2]]))
      
      #matrizOut[1:3,]
      
      
      
    }#fin for estadios
    
    
    
    #Oviposition
    
    Ageclass = ageClassMatriz[Day,numInmaduros+1]
    
    matrizAges = estadiosAges[[numInmaduros+1]]
    
    Ovi = c(rep(0,Ageclass))
    
    
    
    E <- c(rep(0, Ageclass))
    
    EL <- c(rep(0, Ageclass))
    
    Eage <- c(rep(0, Ageclass))
    
    pacc <- c(rep(0, Ageclass))
    
    p1 <- c(rep(0, Ageclass))
    
    p2 <- c(rep(0, Ageclass))
    
    
    
    for(Ii in 1:(Ageclass-1)){
      
      E[Ii] = vectorN[[k-1]][[2]][Ii]
      
      Eage[Ii] = oviFreq[Day, Ii]
      
      E1 = matrizA[Day,k*2-1]
      
      E1 = E1 * Eage[Ii] * E[Ii]
      
      Ovi[Ii+1] = E1
      
      NEWIND = NEWIND + E1
      
    }
    
    
    
    Ovi = na.omit(Ovi)
    
    matrizOut[Day+1,(numInmaduros + numMaduros +1)] = round(sum(Ovi),2)
    
    
    
    vectorN[[1]][[2]][1] = NEWIND
    
    matrizOut[Day+1,1] = round(sum(vectorN[[1]][[2]]))
    
    vectorN[[1]][[1]] = vectorN[[1]][[2]]
    
    
    
    NEWIND=0
    
    Day=Day+1
    
    Step = Step + 1
    
  }#fin bucle steps
  
  
  
  namesMatriz = c(estadios, "New Egg");
  
  colnames(matrizOut) = namesMatriz
  
  matrizOutDead = matrizOut
  #matrizOutDead[is.infinite(matrizOutDead)]="Dead"
  matrizOutDead[is.na(matrizOutDead)]="Dead"
  matrizOutDead[matrizOutDead == "Inf"]="Dead"
  #matrizOutDead[is.nan(matrizOutDead)]="Dead"
  
  return(list(matrizOut=matrizOut, vectorN=vectorN, matrizOutDead=matrizOutDead))
  
  
  
  
}

###############################################################################
grafSimDete<-function(matrizOut, estadios, labx,laby,titulo,legx,legy,corrx1,corrx2){
  numEstadios = length(estadios)
  names1 = colnames(matrizOut)
  names1=names1[1:(length(names1)-2)]
  matrizOut[matrizOut<1]=1
  dat = matrizOut
  cap=rep(10,15)
  tamY = cap^(0:14)
  tamsY = seq(0,30,length.out=15)
  
  x1 = 1:nrow(dat)
  #x1 = corrx
  z1 = log(dat[,numEstadios-1])
  mod1 = lm(z1~x1)
  
  plot(1:nrow(dat),log(dat[,1]),type="n", xlab=labx, ylab=laby,axes=FALSE, xlim = c(corrx1,corrx2))
  #plot(corrx,log(dat[,1]),type="n", xlab=labx, ylab=laby,axes=FALSE)
  grid(NA,15,lwd=1)
  title(titulo)
  axis(1,seq(corrx1,corrx2,round(corrx2/8)))
  axis(2,tamsY,tamY, las=2, cex.axis=0.6)
  abline(mod1, lwd=2)
  
  for(i in 1:(numEstadios-1)){
    points(1:nrow(dat),log(dat[,i]),type="l",col=(i+1))
    #points(corrx,log(dat[,i]),type="l",col=(i+1))
  }
  
  cols = c(2:(i+1),1)
  legend(legx,legy,c(names1,paste("Expon.(",names1[length(names1)],")")),col =cols,lty = 1)
  
}
#################################################################
simulacionVariosAnios <- function(matrizOut, vectorN, estadios, estadiosAges){
  
  inmad <-  estadios[-(length(estadios)-1):-(length(estadios))]
  numInmaduros = length(inmad)
  maduros   <-  estadios[(length(estadios)-1):(length(estadios))]
  numMaduros = length(maduros)
  sizeMatriz = nrow(matrizOut)
  numTotal <- sum(matrizOut[sizeMatriz,1:(numInmaduros+numMaduros)])
  numIni <- 0
  numInd <- list(1)
  
  for(k in 1:(numInmaduros+numMaduros)){
    if(k <= numInmaduros){
      slope= 2
    }else{
      slope= 1.8
    }
    
    age=0
    class=1
    matrizAges = estadiosAges[[k]]
    while(age < slope){
      age =  matrizAges[sizeMatriz,class]
      Number = vectorN[[k]][[2]][class] / numTotal * 100 
      estadiosAges[[k]][1,class] = age
      vectorN[[k]][[1]][class] = Number
      class = class+1
    }
    
    numInd[[k]] <- sum(vectorN[[k]][[2]] / numTotal * 100)
  }
  return(list(vectorN=vectorN,numInd=numInd))	
}
################################################################
simulacionUnaGeneracion <- function(Day, estadiosAges, oviFreq, ageClassMatriz, hfeno, cuadro, estadios,numIni,sexratio,Rs,Steps){
  Step=0
  
  matrizA = cuadro
  
  sizeMatriz = nrow(matrizA)
  
  inmad <-  estadios[-(length(estadios)-1):-(length(estadios))]
  
  numInmaduros = length(inmad)
  
  maduros   <-  estadios[(length(estadios)-1):(length(estadios))]
  
  numMaduros = length(maduros)
  
  NEWIND = 0
  
  dias<-254#porq?
  
  
  
  #matrizOut = matrix(NA, nrow=(nrow(matrizA)-1), (numInmaduros+numMaduros+1))
  
  matrizOut = matrix(NA, nrow=nrow(matrizA), (numInmaduros + numMaduros + 1))
  
  stagesN = c(rep(0,dias))
  
  stagesN1 = c(numIni, rep(0,(dias-1)))
  
  femalesN = c(rep(NA,dias))
  
  eggsN = stagesN1
  
  stagesDev = c(rep(0,dias))
  
  
  
  ## vectorN <- list(1)
  
  ## vectorNini <- list(1)
  
  
  
  #if(!severalYear){
  
  vectorN <- list(1)
  
  vectorNini <- list(1)
  
  for(k in 1:(numInmaduros + numMaduros)){
    
    v1 = c(100,rep(0,(ageClassMatriz[1,k]-1)))
    
    v2 = c(0,rep(0,(ageClassMatriz[1,k]-1)))
    
    v3 = c(0,rep(0,(ageClassMatriz[1,k]-1)))#
    
    
    
    if(k == 1){
      
      vectorN[[k]] = list(v1,v2,v3)#
      
      vectorNini[[k]] = v1
      
    }else{
      
      vectorN[[k]] = list(v2,v2,v3)#
      
      vectorNini[[k]] = v2
      
    }
    
  }
  
  
  
  matrizOut[1,] = c(numIni,rep(0,(numInmaduros + numMaduros)))# numero inicial de individuos
  
  #}
  
  ## else{
  
  ##     vectorNini <- list(1)
  
  ##     for(k in 1:(numInmaduros + numMaduros)){
  
  ##         vectorNini[[k]] <- vectorN[[k]][[1]]
  
  ##         matrizOut[1,k] = round(numInd[[k]])
  
  ##     }
  
  ## }
  
  
  
  #while(Day < (sizeMatriz-1)){
  
  while(Step < 363){
    
    for(k in 1:(numInmaduros + numMaduros)){
      
      #Ageclass = ageClassMatriz[Day,k]
      
      Ageclass = ageClassMatriz[Day+1,k]
      
      
      
      if(k <= numInmaduros){
        
        stagesN = c(rep(0,Ageclass))
        
      }
      
      
      
      if(k == (numInmaduros+1)){
        
        femalesN = c(rep(0,Ageclass))
        
      }
      
      
      
      matrizAges = estadiosAges[[k]]
      
      NEWIND = 0
      
      
      
      E <- c(rep(0, Ageclass))
      
      EL <- c(rep(0, Ageclass))
      
      Eage <- c(rep(0, Ageclass))
      
      pacc <- c(rep(0, Ageclass))
      
      p1 <- c(rep(0, Ageclass))
      
      p2 <- c(rep(0, Ageclass))
      
      
      
      if(k <= numInmaduros){
        
        s1 <- c(rep(NA, Ageclass))
        
      }
      
      
      
      if(k == numInmaduros+1){
        
        vectorN[[k]][[2]] = vectorN[[k]][[3]]#
        
      }
      
      if(k == numInmaduros+2){
        
        vectorN[[k]][[2]] = vectorN[[k]][[3]]#
        
      }
      
      
      
      for(Ii in 1:(Ageclass-1)){
        
        Eage[Ii] = matrizAges[Day+1, Ii+1]
        
        
        
        if(k <= numInmaduros){
          
          survivalk = (1 - matrizA[Day,k*2]) ^ matrizA[Day,k*2-1]
          
        }
        
        
        
        if(k <= numInmaduros){
          
          if(Eage[Ii] > 1){
            
            s1[Ii] = 1
            
          }else{
            
            s1[Ii] = survivalk
            
          }
          
        }
        
        
        
        if(k <= numInmaduros){
          
          pacc[Ii] = distrimodeldeim(Eage[Ii], k, hfeno)
          
        }else{
          
          pacc[Ii] = distrimodeldema(Eage[Ii], k - numInmaduros, hfeno)
          
        }
        
        
        
        if(Ii == 1){
          
          p1[Ii] = pacc[Ii] - 0
          
        }else{
          
          p1[Ii] = pacc[Ii] - pacc[Ii - 1]
          
        }
        
        
        
        if(p1[Ii] == 0){
          
          p2[Ii] = 0
          
        }else{
          
          if(Ii == 1){
            
            p2[Ii] = p1[Ii] / (1 - 0)
            
          }else{
            
            p2[Ii] = p1[Ii] / (1 - pacc[Ii - 1])
            
          }
          
        }
        
        
        
        if(Day==1){
          
          E[Ii] = vectorNini[[k]][Ii]
          
        }else{
          
          E[Ii] = vectorN[[k]][[1]][Ii]#modifique el 1 por el 2
          
        }
        
        
        
        
        
        if(k <= numInmaduros){
          
          EL[Ii] = E[Ii] * p2[Ii] * s1[Ii]
          
        }else{
          
          EL[Ii] = E[Ii] * p2[Ii]
          
        }
        
        
        
        if(k <= numInmaduros){
          
          E1 = E[Ii] * s1[Ii] - EL[Ii]
          
        }else{
          
          E1 = E[Ii] - EL[Ii]
          
        }
        
        
        
        stagesN[Ii+1] = E1
        
        
        
        if(k <= numInmaduros){
          
          NEWIND = NEWIND + EL[Ii]
          
        }
        
        
        
      } # fin for clases
      
      
      
      vectorN[[k]][[2]] = c(vectorN[[k]][[2]][1], stagesN[2:length(stagesN)])
      
      vectorN[[k]][[1]] = vectorN[[k]][[2]]
      
      
      
      if(k <= (numInmaduros-1)){
        
        vectorN[[k]][[3]][1]=NEWIND
        
        vectorN[[k+1]][[2]] = vectorN[[k]][[3]]
        
      }
      
      
      
      if(k == numInmaduros){
        
        vectorN[[k+1]][[3]][1] = NEWIND * Rs[Day]
        
        vectorN[[k+2]][[3]][1] = NEWIND * (1-Rs[Day])
        
        
        
        vectorN[[k+1]][[2]] = vectorN[[k]][[3]]#
        
      }
      
      
      
      if(k == (numInmaduros+1)){
        
        femalesN = E
        
      }
      
      
      
      matrizOut[Day+1,k] = round(sum(vectorN[[k]][[2]]))
      
      #matrizOut[1:3,]
      
      
      
    }#fin for estadios
    
    
    
    
    
    #Oviposition
    
    Ageclass = ageClassMatriz[Day,numInmaduros+1]
    
    matrizAges = estadiosAges[[numInmaduros+1]]
    
    Ovi = c(rep(0,Ageclass))
    
    
    
    E <- c(rep(0, Ageclass))
    
    EL <- c(rep(0, Ageclass))
    
    Eage <- c(rep(0, Ageclass))
    
    pacc <- c(rep(0, Ageclass))
    
    p1 <- c(rep(0, Ageclass))
    
    p2 <- c(rep(0, Ageclass))
    
    
    
    for(Ii in 1:(Ageclass-2)){
      
      E[Ii] = vectorN[[k-1]][[2]][Ii]
      
      Eage[Ii] = oviFreq[Day, Ii]
      
      E1 = matrizA[Day,k*2-1]
      
      E1 = E1 * Eage[Ii] * E[Ii]
      
      Ovi[Ii+1] = E1
      
      NEWIND = NEWIND + E1
      
    }
    
    
    
    Ovi = na.omit(Ovi)
    
    matrizOut[Day+1,(numInmaduros + numMaduros +1)] = round(sum(Ovi),2)
    
    
    
    #vectorN[[1]][[2]][1] = NEWIND
    
    matrizOut[Day+1,1] = round(sum(vectorN[[1]][[2]]))
    
    vectorN[[1]][[1]] = vectorN[[1]][[2]]
    
    
    
    #                      if(round(matrizOut[Day,(numInmaduros + numMaduros)]) > 0 && round(matrizOut[Day+1,(numInmaduros + numMaduros)]) == 0){
    
    #                                  break;
    
    #                      }
    
    if(matrizOut[Day,1]==0 && matrizOut[Day,2]==0 && matrizOut[Day,3]==0 && matrizOut[Day,4]==0 && matrizOut[Day,5]==0){
      
      #matrizOut[Day:nrow(matrizOut),]="Dead"
      break;
      
    }
    
    
    
    NEWIND=0
    
    Day=Day+1
    
    Step = Step + 1
    
  }#fin bucle steps
  
  
  
  
  
  namesMatriz = c(estadios, "New Egg");
  
  colnames(matrizOut) = namesMatriz
  
  matrizOutDead = matrizOut
  matrizOutDead[is.na(matrizOutDead)]="Dead"
  
  matrizOut[is.na(matrizOut)]=0
  
  return(list(matrizOut=matrizOut, vectorN=vectorN, matrizOutDead=matrizOutDead))
}
###############################################################################
deter1<-function(cuadro,estadios,hfeno){
  dia<-366 #tama?o fila
  day<-0:363
  inmad <-  estadios[-(length(estadios)-1):-(length(estadios))]
  maduros   <-  estadios[(length(estadios)-1):(length(estadios))]
  ratest<-Rate<-rest<-list(1)
  for(i in 1:length(inmad))      rest[i]<-list(cuadro[,2*i]/cuadro[,(2*i-1)])
  for(i in 1:(length(inmad)+1))  Rate[i]<-list(cuadro[,(2*i-1)])
  Rate[[i+1]]<-cuadro[,(2*i)]
  ovi<-cuadro[,(2*i)+1]
  riter<-dij<-dacc<-pij<-plij<-list(1)
  for(k in 1:length(inmad))
  {
    pd<-d<-enditer<-pl<-p<-c(0)
    i<-1
    while(i<=100)#length(day))
    {
      d[i]<-Rate[[k]][i]*day[i]
      pd[i]<-distrimodeldeim(d[i],k,hfeno)
      if(i==1)p[i]<-pd[i]
      if(i!=1)p[i]<-pd[i]-pd[i-1]
      ifelse(pd[i]==1,enditer[i]<-0,enditer[i]<-1)
      if(enditer[i]==1) i<-i+1 else break
    }
    for(h in 2:i)ifelse(pd[h]==1,pl[h]<-1,pl[h]<-p[h]/(1-pd[h-1]))
    dij[[k]]<-d
    dacc[[k]]<-pd
    pij[[k]]<-p
    plij[[k]]<-pl
    riter[[k]]<-enditer
  }
  for(kk in 1:length(maduros))
  {
    pd<-d<-enditer<-pl<-p<-c(0)
    i<-1
    while(i<=length(day))
    {
      d[i]<-Rate[[k]][i]*day[i]
      pd[i]<-distrimodeldema(d[i],kk)
      if(i==1)p[i]<-pd[i]
      if(i!=1)p[i]<-pd[i]-pd[i-1]
      ifelse(pd[i]==1,enditer[i]<-0,enditer[i]<-1)
      if(enditer[i]==1) i<-i+1 else break
    }
    for(h in 2:i)ifelse(pd[h]==1,pl[h]<-1,pl[h]<-p[h]/(1-pd[h-1]))
    dij[[kk+length(inmad)]]<-d
    dacc[[kk+length(inmad)]]<-pd
    pij[[kk+length(inmad)]]<-p
    plij[[kk+length(inmad)]]<-pl
    riter[[kk+length(inmad)]]<-enditer
  }
  return(list(dij,dacc,pij,plij,riter,rest,Rate,ovi))
}
###############################################################################
###############################################################################
detrm2<-function(dij,dacc,pij,plij,riter,rest,rate,ovi,tamano,generacion,dia,Day,hfeno)
{
  N<-tamano
  inmad <-  estadios[-(length(estadios)-1):-(length(estadios))]
  maduros   <-  estadios[(length(estadios)-1):(length(estadios))]
  inmaduros<-list(1)
  forid<-diasend<-diaseld<-list(1)
  for(h in 1:length(estadios))
  {
    forid[h]<-list(rep(0,length(dij[[h]])))
    diasend[h]<-list(rep(0,length(dij[[h]])))
    diaseld[h]<-list(rep(0,length(dij[[h]])))
  }
  for(GG in 1:generacion)
  {
    if(GG==1) forid[[1]][1]<-N else  forid<-nuevos
    tiernosmat<-matrix(0,dia,length(inmad))
    viejos<-matrix(0,dia,3)
    g<-1
    while(g<=dia)
    {
      for(h in 1:length(inmad))
      {
        if(h!=1) diasend[[h]][1]<-M
        estados<-estinma(forid[[h]],diasend[[h]],diaseld[[h]],dia,dij[[h]],rest[[h]],rate[[h]],riter[[h]],plij[[h]])
        M<-sum(estados$diasel)
        inmaduros[[h]]<-estados$diasen
      }
      kkk<-h+1
      M<-M/2
      while(kkk<=(length(estadios)+1))
      {
        if(kkk==h+1)
        {
          diasend[[kkk]][1]<-M
          female1<-matri(dia,forid[[kkk]],diasend[[kkk]],dij[[kkk]],plij[[kkk]])
          inmaduros[[kkk]]<- female1$fema
        }
        if(kkk==h+2)
        {
          diasend[[kkk]][1]<-M
          male1<-matri(dia,forid[[kkk]],diasend[[kkk]],dij[[kkk]],plij[[kkk]])
          inmaduros[[kkk]]<- male1$fema
        }
        if(kkk==h+3)
        {
          paraovi<-as.list(hfeno$povih_h)
          for (i in names(paraovi))
          {
            temp <- paraovi[[i]]
            storage.mode(temp) <- "double"
            assign(i, temp)
          }
          funcovi <- as.expression(hfeno$fovih_h[[length(hfeno$fovih_h)]])
          x<-dij[[4]]
          pd<-eval(funcovi)
          p<-c(0) #dail ovi frec
          for(i in 1:length(pd))
          {
            if(i==1)p[i]<-pd[i]
            if(i!=1)p[i]<-pd[i]-pd[i-1]
          }
          feer<-female1$fema
          movi<-matovi(dia,diasend[[4]],dij[[4]],ovi,p,feer)
        }
        kkk<-kkk+1
      }
      N<-sum(movi$ovip[-1])
      inmaduros[[1]][1]<-N
      tiernos<-sum(inmaduros[[1]])
      for(i in 2:length(inmad)) tiernos<-cbind(tiernos,cbind(sum(inmaduros[[i]])))
      tiernos<-as.numeric(as.character(tiernos))
      tiernosmat[g,]<-tiernos
      viejos[g,]<-c(sum(female1$fema),sum(male1$fema),sum(movi$ovip[-1]))
      forid<-inmaduros
      forid[[1]][1]<-N
      if(g==1) newovs<-sum(movi$ovip[-1])
      g<-g+1
    }
    tabla<-cbind(tiernosmat,viejos)
    colnames(tabla)<-c(estadios,"New_Egg")
    tot<-as.numeric(tabla[(dia-1),-(length(estadios)-1)])
    totales<-sum(tot)
    fretot<-tot/totales
    nuevos<-list(1)
    for(i in 1:(length(estadios))) nuevos[i]<-list(100*fretot[i]*inmaduros[[i]]/tot[i])
  }
  print(round(as.data.frame(tabla)))
  return(list(tabla=round(as.data.frame(tabla))))
}
#############################################################
#############################################################
#############################################################
grafdete<-function(simudeter,dia,corrx,corry,tam,lgx,lgy)
{
  tablesimu<-matrix(0,nrow(simudeter),ncol(simudeter))
  for(i in 1:nrow(simudeter)) for(j in 1:ncol(simudeter)) tablesimu[i,j]<-ifelse(simudeter[i,j]>1,simudeter[i,j],1)
  par(cex=tam)
  plot(1:dia,log(tablesimu[,1]),type="l",col=1,frame=F,ylim=corry,xlim=corrx)
  for(i in 2:(length(estadios)-1)) lines(1:dia,log(tablesimu[,i]),col=i)
  legend(lgx,lgy,names(simudeter)[-c(length(estadios),(length(estadios)+1))],col =1:i,lty = 1)
  return(list(table=tablesimu))
}
###########################################
###########################################
moddete<-function(dia,tabdete)
{
  day<-1:dia
  t<-log(tabdete[,1])
  mod<-lm(t~day)
  resu<-summary(mod)
  r<-resu$coef[2]
  print(r)
}
###############################################################################
###############################################################################

distrimodeldeim<-function(vec,sll,hfeno) ## this is GML function
{
  # probit
  if(hfeno$distri_dv[[sll]]=="probit")
  {
    pdd<-pnorm(log(vec)*hfeno$slope_dv[[sll]])
  }
  # logit
  if(hfeno$distri_dv[[sll]]=="logit")
  {
    pdd<-1/(1+exp(-(log(vec)*hfeno$slope_dv[[sll]])))
  }
  # cloglog
  if(hfeno$distri_dv[[sll]]=="cloglog")
  {
    pdd<-1-exp(-exp(log(vec)*hfeno$slope_dv[[sll]]))
  }
  return(pdd)
}
###############################################################################
###############################################################################
distrimodeldema<-function(vec,sll,hfeno)
{
  if(sll==1)
  {
    #hembras
    # probit
    if(hfeno$distri_snh=="probit")
    {
      pdd<-pnorm(log(vec)*hfeno$slope_snh)
    }
    # logit
    if(hfeno$distri_snh=="logit")
    {
      pdd<-1/(1+exp(-(log(vec)*hfeno$slope_snh)))
    }
    # cloglog
    if(hfeno$distri_snh=="cloglog")
    {
      pdd<-1-exp(-exp(log(vec)*hfeno$slope_snh))
    }
  }
  if(sll==2)
  {
    #machos
    # probit
    if(hfeno$distri_snm=="probit")
    {
      pdd<-pnorm(log(vec)*hfeno$slope_snm)
    }
    # logit
    if(hfeno$distri_snm=="logit")
    {
      pdd<-1/(1+exp(-(log(vec)*hfeno$slope_snm)))
    }
    # cloglog
    if(hfeno$distri_snm=="cloglog")
    {
      pdd<-1-exp(-exp(log(vec)*hfeno$slope_snm))
    }
  }
  return(pdd)
}
###############################################################################
##########################################   DETE   #####################################
estinma<-function(fori,diasen,diasel,dia,dij,rest,rate,riter,plij)
{
  for(j in 2:length(dij))
  {
    if(dij[j]>1) Si<-1
    if(dij[j]<1) Si<-(1-rest[j-1])^rate[j-1]
    diasen[j]<-fori[j-1]*Si-ifelse(riter[j-1]==0,0,fori[j-1]*plij[j]*Si)
    ifelse(riter[j-1]==0,diasel[j-1]<-0,diasel[j-1]<-fori[j-1]*plij[j]*Si)
  }
  salidas<-list(diasen=diasen,diasel=diasel)
  return(salidas)
}
#############################################################################################################
###########################################################################################################
matri<-function(dia,fori,diasen,dij,plij)
{
  for(j in 2:length(dij))diasen[j]<-fori[j-1]-fori[j-1]*plij[j]
  return(list(fema=diasen))
}
#############################################################################################################
###########################################################################################################
matovi<-function(dia,diasen,dij,ovi,p,feer){
  for(i in 2:length(dij)) diasen[i]<-feer[i]*ovi[i]*p[i]
  return(list(ovip=diasen))
}
#######################################################3
parameters <- function (N, estad, ltb,poli=1){
  estadios <- estad
  lifetable <- ltb
  s.x <- m.x <- l.x <- rep(0, nrow(lifetable))
  for (i in 1:nrow(lifetable)) l.x[i] <- sum(lifetable[i, 1:(length(estadios))])/N  ## porcentaje de vivos por dias
  # for (i in 1:nrow(lifetable)) if(l.x[i]==0) m.x[i] <- 0 else m.x[i] <- lifetable[,(length(estad)+1)][i]/N/l.x[i]
  for (i in 1:nrow(lifetable)) if(l.x[i]==0) m.x[i] <- 0 else m.x[i] <- poli*lifetable[,(length(estad)+1)][i]/N/l.x[i]
  ## el objeto m.x no es afectado por el efecto poliembrionico pero asumimos la mortalidad del host es la unica que afecta ala mortalidad del parasitoide
  s.x1 <- c(l.x[1], l.x[2])
  s.x2 <- rep(0, nrow(lifetable) - 2)
  for (i in 1:(nrow(lifetable) - 2)) s.x2[i] <- l.x[i + 2]/l.x[i + 1]
  s.x <- c(s.x1, s.x2) ## es el porcentaje de vivos segun la cantidad del dia anterior
  Ro <- sum(l.x * m.x)  ## este parametro depende del procentaje de vivos de los inmaduros
  Day <- 0:(nrow(lifetable)-1)
  Tg <- sum(Day * l.x * m.x)/Ro
  GRR <- sum(m.x)
  r <- log(Ro)/Tg
  euler <- sum(l.x * m.x * exp(-r * (Day + 1)))
  r1 <- r - 1e-06
  for (k in 1:100) {
    euler1 <- sum(l.x * m.x * exp(-r1 * (Day + 1)))
    r2 <- ((1 - euler) * (r1 - r)/(euler1 - euler)) + r
    r1 <- r2
  }
  rf <- r1
  Dt <- log(2)/rf
  lamda <- exp(rf)
  T <- log(Ro)/rf
  parameters <- t(data.frame(r = rf, Ro = Ro, GRR = GRR, T = T, lambda = lamda, Dt = Dt))
  colnames(parameters) <- "Parameters"
  print(parameters)
  if(poli>1)
  {
    matrizOut=data.frame(ltb[,1:(length(estadios)-2)],poli*ltb[,(ncol(ltb)-2):ncol(ltb)])
    nombres=colnames(ltb);nombres2=c(paste(nombres[1:(length(estadios)-2)],"Host",sep="_"),paste(nombres[(ncol(ltb)-2):ncol(ltb)],"Paras",sep="_"))
    colnames(matrizOut)=nombres2
  }else{matrizOut=ltb}
  return(list(ltb=matrizOut,m.x=m.x,s.x=s.x,l.x=as.matrix(l.x),parametro=parameters))
}

#######################################################3
parameters2 <- function (N, estad, ltb){
  estadios <- estad
  lifetable <- ltb
  s.x <- m.x <- l.x <- rep(0, nrow(lifetable))
  for (i in 1:nrow(lifetable)) l.x[i] <- sum(lifetable[i, 1:(length(estadios))])/N
  for (i in 1:nrow(lifetable)) if(l.x[i]==0) m.x[i] <- 0 else m.x[i] <- lifetable[,(length(estad)+1)][i]/N/l.x[i]
  s.x1 <- c(l.x[1], l.x[2])
  s.x2 <- rep(0, nrow(lifetable) - 2)
  for (i in 1:(nrow(lifetable) - 2)) s.x2[i] <- l.x[i + 2]/l.x[i + 1]
  s.x <- c(s.x1, s.x2)
  Ro <- sum(l.x * m.x)
  Day <- 0:(nrow(lifetable)-1)
  Tg <- sum(Day * l.x * m.x)/Ro
  GRR <- sum(m.x)
  r <- log(Ro)/Tg
  euler <- sum(l.x * m.x * exp(-r * (Day + 1)))
  r1 <- r - 1e-06
  for (k in 1:100) {
    euler1 <- sum(l.x * m.x * exp(-r1 * (Day + 1)))
    r2 <- ((1 - euler) * (r1 - r)/(euler1 - euler)) + r
    r1 <- r2
  }
  rf <- r1
  Dt <- log(2)/rf
  lamda <- exp(rf)
  T <- log(Ro)/rf
  parameters <- t(data.frame(r = rf, Ro = Ro, GRR = GRR, T = T, lambda = lamda, Dt = Dt))
  colnames(parameters) <- "Parameters"
  print(parameters)
  #return(list(m.x=m.x,s.x=s.x,l.x=as.matrix(l.x),parametro=parameters))
  return(list(r = rf, Ro = Ro, GRR = GRR, T = T, lambda = lamda, Dt = Dt))
}


##########################################
# Funcion generadora de Tasas y mortalidad
##########################################
RateI<-function(vec,Table2,Ki,parametrosc,parametrosm=NULL,funciont,funcionm=NULL,nmax,steps,J=NA,modelim)
{
  for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
  i=0:(steps-1)
  T1=((vec[3]-vec[2])/2)*cos(pi*(i+0.5)/steps) + (vec[3]+vec[2])/2
  if(vec[1]!=nmax){T2<-((vec[3]-Table2[vec[1]+1,2])/2)*cos(pi*(i+0.5)/steps) + (vec[3]+Table2[vec[1]+1,2])/2}else{
    T2<-((vec[3]-Table2[1,2])/2)*cos(pi*(i+0.5)/steps) + (vec[3]+Table2[1,2])/2}
  if((modelim[Ki]==1 || modelim[Ki]==2 || modelim[Ki]==3 ||
      modelim[Ki]==4 || modelim[Ki]==5 || modelim[Ki]==6 ||
      modelim[Ki]==7 || modelim[Ki]==8 || modelim[Ki]==9 ||
      modelim[Ki]==10 || modelim[Ki]==11 || modelim[Ki]==12 || modelim[Ki]==13 || modelim[Ki]==14) & is.na(J)){x = T1 + 273.15;x2 = T2 + 273.15}else{x = T1;x2 = T2}
  
  
  Rat=eval(funciont);if(!is.na(J)){Rat[Rat<=0]=0}
  Ratetot1<-(sum(Rat))/steps    ### aqui evalua la tasa de desarrollo de todas las divisiones
  x=x2;Rat=eval(funciont);if(!is.na(J)){Rat[Rat<=0]=0}
  Ratetot2<-(sum(Rat))/steps    ### aqui evalua la tasa de desarrollo de todas las divisiones del segundo dia
  Rate=(Ratetot1+Ratetot2)/2
  
  if(!is.null(funcionm))
  {
    for (i in names(parametrosm)){temp <- parametrosm[i];storage.mode(temp) <- "double";assign(i, temp)}
    x=T1;M1=eval(funcionm);M1[M1>1]=1; Mortality1 <- (sum(M1))/steps    ### aqui evalua la Mortalidad de todas las divisiones
    x=T2;M2=eval(funcionm);M2[M2>1]=1; Mortality2 <- (sum(M2))/steps    ### aqui evalua la Mortalidad de todas las divisiones del segundo dia
    Mortality=(Mortality1+Mortality2)/2
    return(c(Rate1=Ratetot1,Rate2=Rate,Mortality1=Mortality1,Mortality2=Mortality))
  }else{return(c(Rate1=Ratetot1,Rate2=Rate))}
}
##############################################
matrizA <- function(estadios, hfeno, Table, steps, modelim){
  inmaduros <-  estadios[-(length(estadios)-1):-(length(estadios))]
  maduros   <-  estadios[(length(estadios)-1):(length(estadios))]
  nmax=nrow(Table)
  matriz<-matrix(0,ncol=2*(length(inmaduros)*2+length(maduros)+1),nrow=nrow(Table))
  Table2=cbind(id=1:nrow(Table),Table)
  
  for(K in 1:length(inmaduros))
  {
    #  Desarrollo  # extraccion de funciones y parametros
    
    parametrosc <- hfeno$pdv_dv[[K]]
    funciont <- as.expression(hfeno$fdv_dv[[K]][[3]])
    
    #   Mortalidad # extraccion de funciones y parametros
    
    parametrosm <- hfeno$pmortal[[K]]
    funcionm <- as.expression(hfeno$mortal[[K]][[3]])
    
    RM=apply(Table2,1,RateI,Table2,K,parametrosc,parametrosm,funciont,funcionm,nmax,steps,modelim=modelim) ## procesamiento de tasa de desarrollo y mortalidad por cada temperatura
    matriz[,4*K-3]=RM[1,];matriz[,4*K-2]=RM[2,]
    matriz[,4*K-1]=RM[3,];matriz[,4*K]=RM[4,]
  }
  #  Hembras
  
  parametrosc <- hfeno$pfh_h
  for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
  formulac <- hfeno$fh_h
  funciont <- as.expression(formulac[[3]])
  RM=apply(Table2,1,RateI,Table2,(K+1),parametrosc,funciont=funciont,nmax=nmax,steps=steps,J=NA,modelim=modelim)
  matriz[,4*(K+1)-3]=RM[1,];matriz[,4*(K+1)-2]=RM[2,]
  
  #  Machos
  
  parametrosc <- hfeno$pfm_m;
  for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
  formulac <- hfeno$fm_m
  funciont <- as.expression(formulac[[3]])
  RM=apply(Table2,1,RateI,Table2,(K+2),parametrosc,funciont=funciont,nmax=nmax,steps=steps,J=NA,modelim=modelim)
  matriz[,4*(K+1)-1]=RM[1,];matriz[,4*(K+1)]=RM[2,]
  matriz[matriz>1]<-1;matriz[matriz<0]<-0
  
  #  Fecundidad
  
  parametrosc <- hfeno$ptazaeh_h
  for (i in names(parametrosc)){temp <- parametrosc[i];storage.mode(temp) <- "double";assign(i, temp)}
  formulac <- hfeno$ftazaeh_h
  funciont <- as.expression(formulac[[3]])
  RM=apply(Table2,1,RateI,Table2,(K+3),parametrosc,funciont=funciont,nmax=nmax,steps=steps,J=3,modelim=modelim)
  matriz[,4*(K+1)+1]=RM[1,];matriz[,4*(K+1)+2]=RM[2,]
  
  vectorPares = (1:(ncol(matriz)/2))*2
  matrizA = matriz[,vectorPares]
  return(matrizA)
}

###############################################
###################################################
Ratios<-function(Table,modelim,modelm,estadios,xi, steps,hfeno){
  inmaduros <-  estadios[-(length(estadios)-1):-(length(estadios))]
  maduros   <-  estadios[(length(estadios)-1):(length(estadios))]
  matriz<-matrix(0,ncol=(length(inmaduros)*2+length(maduros)+1),nrow=nrow(Table))
  K<-1
  while(K<=length(inmaduros)){
    #  Desarrollo  # extraccion de funciones y parametros
    parametrosc <- hfeno$pdv_dv[[K]]
    parametrosc<-as.list(parametrosc)
    for (i in names(parametrosc)){
      temp <- parametrosc[[i]]
      storage.mode(temp) <- "double"
      assign(i, temp)
    }
    formulac <- hfeno$fdv_dv[[K]]
    forexc <- formulac[[length(formulac)]]
    funcionc <- as.expression(forexc)
    
    #   Mortalidad # extraccion de funciones y parametros
    parametrosm <- hfeno$pmortal[[K]]
    parametrosm<-as.list(parametrosm)
    for (i in names(parametrosm)){
      temp <- parametrosm[[i]]
      storage.mode(temp) <- "double"
      assign(i, temp)
    }
    formulam <- hfeno$mortal[[K]]
    forexm <- formulam[[length(formulam)]]
    funcionm <- as.expression(forexm)
    M<-R<-M2<-R2<-rep(0,length(Table[,1]))
    
    Ratesumme<-Rateacum<-Mortalitysumme<-Mortacum<-rep(0,length(Table[,1]))
    Ratetot1<-Mortality1<-Ratetot2<-Mortality2<-0
    
    for(i in 1:length(Table[,1])){#recorre cada valor de la tabla d temperaturas
      
      M<-(Table[,1][i]+Table[,2][i])/2
      R<-(Table[,2][i]-Table[,1][i])/2
      ## ifelse(i!=length(Table[,1]),M2<-(Table[,2][i]+Table[,1][i+1])/2,M2<-(Table[,2][i]+Table[,1][1])/2)
      ## ifelse(i!=length(Table[,1]),R2<-(Table[,2][i]-Table[,1][i+1])/2,R2<-(Table[,2][i]-Table[,1][1])/2)
      ifelse(i!=length(Table[,1]),M2<-(Table[,2][i]+Table[,1][i+1])/2,M2<-(Table[,2][i]+0)/2)
      ifelse(i!=length(Table[,1]),R2<-(Table[,2][i]-Table[,1][i+1])/2,R2<-(Table[,2][i]-0)/2)
      
      Ratetot1<-Mortality1<-Ratetot2<-Mortality2<-0
      #M <- round(M,1)
      #R <- round(R,1)
      #M2 <- round(M2,1)
      #R2 <- round(R2,1)
      
      
      Ratetot1<-Mortality1<-Ratetot2<-Mortality2<-0
      for(j in 0:(steps-1)){#por defecto 48 pasos
        T<-pi/steps*(j+.5)
        ## eval1 ##
        DEGC<-R*cos(T)+M
        if(modelim[K]==1 || modelim[K]==2 || modelim[K]==3 ||
           modelim[K]==4 || modelim[K]==5 || modelim[K]==6 ||
           modelim[K]==7 || modelim[K]==8 || modelim[K]==9 ||
           modelim[K]==10){
          DEGK<-DEGC+273.15
          x<-DEGK
        }else{ x<-DEGC }
        
        Rate<-eval(funcionc)
        Ratetot1<-Ratetot1+Rate
        x<-DEGC
        Mort<-eval(funcionm)
        if(Mort > 1){
          Mort = 1
        }else{
          Mort = Mort
        }
        Mortality1 <- Mortality1+ Mort
        
        ## eval2 ##
        T<-pi/steps*(j+.5)#borrar
        DEGC<-R2*cos(T)+M2
        if(modelim[K]==1 || modelim[K]==2 || modelim[K]==3 ||
           modelim[K]==4 || modelim[K]==5 || modelim[K]==6 ||
           modelim[K]==7  || modelim[K]==8 || modelim[K]==9 ||
           modelim[K]==10){
          DEGK<-DEGC+273.15
          x<-DEGK
        }else{x<-DEGC}
        
        Rate<-eval(funcionc)
        Ratetot2<-Ratetot2+Rate
        x<-DEGC
        Mort<-eval(funcionm)
        if(Mort > 1){
          Mort = 1
        }else{
          Mort = Mort
        }
        Mortality2 <- Mortality2+ Mort
        Mortality2
        
      }
      
      Ratetot1 <- Ratetot1/steps
      Ratetot2 <- Ratetot2/steps
      Ratetot<-(Ratetot1+Ratetot2)/2
      matriz[i,2*K-1] <- Ratetot
      
      Mortality1 <- Mortality1/steps
      Mortality2 <- Mortality2/steps
      Mortality<- (Mortality1+Mortality2)/2
      matriz[i,2*K] <- Mortality
      
    }
    K<-K+1
  }
  for(J in 1:(length(maduros)+1)){
    if(J==1)parametrosc <- hfeno$pfh_h
    if(J==2)parametrosc <- hfeno$pfm_m
    if(J==3)parametrosc <- hfeno$ptazaeh_h
    parametrosc<-as.list(parametrosc)
    for (i in names(parametrosc)){
      temp <- parametrosc[[i]]
      storage.mode(temp) <- "double"
      assign(i, temp)
    }
    if(J==1) formulac <- hfeno$fh_h
    if(J==2) formulac <- hfeno$fm_m
    if(J==3) formulac <- hfeno$ftazaeh_h
    forexc <- formulac[[length(formulac)]]
    funcionc <- as.expression(forexc)
    M<-R<-M2<-R2<-rep(0,length(Table[,1]))
    
    Ratesumme<-Rateacum<-rep(0,length(Table[,1]))
    ratetot1<-ratetot2<-ratetot<-0
    fectot1<-fectot2<-fectot<-fectotal<-0
    
    for(i in 1:length(Table[,1])){
      Ratetot1<-Ratetot2<-Ratetot<-0
      if(J==3){
        fectot1<-fectot2<-fectot<-0
      }
      M<-(Table[,1][i]+Table[,2][i])/2
      R<-(Table[,2][i]-Table[,1][i])/2
      ## ifelse(i!=length(Table[,1]),M2<-(Table[,2][i]+Table[,1][i+1])/2,M2<-(Table[,2][i]+Table[,1][1])/2)
      ## ifelse(i!=length(Table[,1]),R2<-(Table[,2][i]-Table[,1][i+1])/2,R2<-(Table[,2][i]-Table[,1][1])/2)
      ifelse(i!=length(Table[,1]),M2<-(Table[,2][i]+Table[,1][i+1])/2,M2<-(Table[,2][i]+0)/2)
      ifelse(i!=length(Table[,1]),R2<-(Table[,2][i]-Table[,1][i+1])/2,R2<-(Table[,2][i]-0)/2)
      
      for(j in 0:(steps-1)){
        T<-pi/steps*(j+.5)
        #### eval1 ###
        DEGC<-R*cos(T)+M
        x<-DEGC
        if(J==1 || J==2){
          if(modelm[J]==1 || modelm[J]==2 || modelm[J]==3 ||
             modelm[J]==4 || modelm[J]==5 || modelm[J]==6 ||
             modelm[J]==7 || modelm[J]==8 || modelm[J]==9 ||
             modelm[J]==10){
            DEGK<-DEGC+273.15
            x<-DEGK
          }else{x<-DEGC}
        }
        
        Rate<-eval(funcionc)
        Ratetot1<-Ratetot1+Rate
        if(J==3){
          x<-DEGC
          fec1<-eval(funcionc)
          if(fec1>0){ fec1=fec1}else{ fec1=0}
          fectot1<-fectot1+fec1
        }
        
        #### eval2 ###
        DEGC<-R2*cos(T)+M2
        if(J==1 || J==2){
          if(modelm[J]==1 || modelm[J]==2 || modelm[J]==3 ||
             modelm[J]==4 || modelm[J]==5 || modelm[J]==6 ||
             modelm[J]==7 || modelm[J]==8 || modelm[J]==9 ||
             modelm[J]==10){
            DEGK<-DEGC+273.15
            x<-DEGK
          }else{x<-DEGC}
        }
        #if(J==3) x<-DEGC
        Rate<-eval(funcionc)
        Ratetot2<-Ratetot2+Rate
        if(J==3){
          x<-DEGC
          fec2<-eval(funcionc)
          if(fec2>0){
            fec2=fec2
          }else{
            fec2=0
          }
          fectot2<-fectot2+fec2
        }
      }
      Ratetot1 <- Ratetot1/steps
      Ratetot2 <- Ratetot2/steps
      Ratetot<-(Ratetot1+Ratetot2)/2
      if(J==1){
        matriz[i, length(inmaduros)*2+1] <- Ratetot
      }
      if(J==2){
        matriz[i, length(inmaduros)*2+2] <- Ratetot
      }
      if(J==3){
        fectot1 = fectot1 / steps
        fectot2 = fectot2 / steps
        fectot = (fectot1 + fectot2) / 2
        if(fectot < 0){
          fectot = 0
        }
        matriz[i, length(inmaduros)*2+3] <- fectot
      }
    }
  }
  
  T<-1/matriz[,1]+1/matriz[,3]+1/matriz[,5]+xi/matriz[,7]
  Ro<-matriz[,9]*(1-matriz[,2])*(1-matriz[,4])*(1-matriz[,6])/2
  rm<-log(Ro)/T
  J<-exp(rm)
  Dt<-log(2)/rm
  E<-(1-matriz[,2])^matriz[,1]
  L<-(1-matriz[,4])^matriz[,3]
  P<-(1-matriz[,6])^matriz[,5]
  ERI<-(1-ifelse(length(E[E==0])==0,0,length(E[E==0])/12))*
    (1-ifelse(length(L[L==0])==0,0,length(L[L==0])/12))*
    (1-ifelse(length(P[P==0])==0,0,length(P[P==0])/12))
  
  return(list(MATRIZ=data.frame(matriz),ERI=ERI,J=J))
}