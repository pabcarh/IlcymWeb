graffvida<-function(Tablelife,cuadro,estadios,N,M,hfeno,labx,laby,corrx,corry,lgx,lgy, Rs){
  table_life<-estimado<-list(1)
  NUM <- 8
  TABLEmean<-matrix(0, nrow=nrow(cuadro), ncol=length(estadios)+1)
  TABLEmin<-matrix(0, nrow=nrow(cuadro), ncol=length(estadios)+1)
  TABLEmax<-matrix(0, nrow=nrow(cuadro), ncol=length(estadios)+1)
  for(k in 1:NUM)
  {
    #estimado [[k]]<- simulacion(cuadro,estadios,N,hfeno,sexratio)$matriz
    estimado [[k]]<- simulacion(cuadro,estadios,N,M,hfeno,Rs)$mat
    table_life[[k]] <- life.table2(estimado[[k]], estadios)$life.table
  }
  
  for(i in 1:ncol(table_life[[1]]))
  {
    for(j in 1:(nrow(table_life[[1]])-1))
    {
      TABLEmin[j,i] <- min(c(table_life[[1]][j,i],table_life[[2]][j,i],table_life[[3]][j,i],table_life[[4]][j,i]))
      TABLEmax[j,i] <- max(c(table_life[[1]][j,i],table_life[[2]][j,i],table_life[[3]][j,i],table_life[[4]][j,i]))
      TABLEmean[j,i]<- mean(c(table_life[[1]][j,i],table_life[[2]][j,i],table_life[[3]][j,i],table_life[[4]][j,i]))
    }
  }
  # GRAFF
  
  #########################################################
  # Ploteo de los puntos esperados visto como lineas unidas
  #########################################################
  
  plot(1:nrow(TABLEmean),TABLEmean[,1],ylim=corry,xlim=corrx,type="l", col=1,lwd=1,frame=F,ylab=laby,xlab=labx)
  
  for(i in 1:length(estadios))
  {
    temp1=TABLEmean[,i];temp1[temp1<0.5]=NA;lines(1:nrow(TABLEmean),temp1,lwd=1,col=i)
    temp2=TABLEmin[,i];temp2[temp2<0.5]=NA;lines(1:nrow(TABLEmin),temp2,lwd=1,col=i,lty=4)
    temp3=TABLEmax[,i];temp3[temp3<0.5]=NA;lines(1:nrow(TABLEmax),temp3,lwd=1,col=i,lty=4)
  }
  
  #################################
  # Ploteo de los puntos observados
  #################################
  
  #Tablelife<-life.table2(vidalife, estadios)$life.table
  newTablelife <- matrix(NA,nrow(Tablelife), ncol(Tablelife))
  matrizPosic <- matrix(NA,nrow(Tablelife), length(estadios))
  posic1 <- 1
  for(j in 1:length(estadios))
  {
    posic1 <- 1
    temp1 = Tablelife[1,j]
    for(t in 2:length(Tablelife[,j])){
      if(Tablelife[(t-1),j] == Tablelife[t,j]){
        temp1 <- c(temp1,NA)
        posic1 <- c(posic1, t)
      }else{
        temp1 <- c(temp1,Tablelife[t,j])
      }
      
    }
    posic1 <- posic1[2:length(posic1)]
    matrizPosic[,j] = c(posic1,rep(0, (nrow(Tablelife)-length(posic1))))
    temp <- temp1;
    temp[is.na(temp)] = 0
    newTablelife[,j] <- temp
    temp[temp<0.5]=NA#
    #temp=Tablelife[,j];temp[temp<0.5]=NA
    points(1:length(temp),temp,col=j, pch=19)
  }
  legend(lgx,lgy,c(estadios),col=1:length(estadios),lty=rep(1,length(estadios)))
  
  newTablelife[,length(estadios)+1] = Tablelife[,length(estadios)+1]
  #######################
  # Indicadores de Ajuste
  #######################
  
  Indic.sim <- function(y,yl){sqe <- sum((y-yl)^2);DE <- sqrt(sqe);return(DE)}
  itab<-matrix(NA,length(estadios),1);colnames(itab)=c("Euclidian.Dist");rownames(itab)=estadios
  cat("\n", "\n","\n","Fitting indicator for each state","\n")
  for(i in 1:length(estadios))
  {
    y=c(Tablelife[-matrizPosic[,i],i],rep(0,100))
    yl=c(TABLEmean[-matrizPosic[,i],i],rep(0,100))
    #y=c(newTablelife[,i],rep(0,100))
    #yl=c(TABLEmean[,i],rep(0,100))
    id1=(1:length(y))[y!=0];id2=(1:length(yl))[yl!=0];id=union(id1,id2)
    y=y[id]+ 1e-12;yl=yl[id]+ 1e-12
    y=c(y,rep(1e-12,4));yl=c(yl,rep(1e-12,4)) ## aumento el valor de 4 como referencia, aunque puede ser un poco mas o un poco menos
    itab[i,]=Indic.sim(y,yl)
  }
  print(itab)
  
}
#########################################




simulacion<-function(cuadro,estadios,n,m,hfeno,Rs){
  mat<-matrix("dead",ncol=n,nrow=(365)) ## m es reemplazado por 365
  for(z in 1:n){
    k <- 1
    Insect_age <- 0
    survival_p <- 1
    dead <- 0
    Day <- 1
    slope<- 0
    djx <- 0
    mjx <- 0
    kj <- NA
    rand_d <- round(runif(1),3) #' random number between 0 and 1 with 3 digits
    rand_m <- round(runif(1),3)
    rand_ovi <- round(runif(1),3)
    rand_sex = 0
    
    if(rand_ovi == 0){
      rand_ovi <- 0.0001
    }
    
    sizeInmaduros = length(estadios[-(length(estadios)-1):-(length(estadios))])
    numFemale = sizeInmaduros + 1
    numMale = sizeInmaduros + 2
    
    #loop for immature life stages
    while(dead != 1 || k <= sizeInmaduros){
      if(k > sizeInmaduros){
        break
      }
      if(dead ==1){
        break
      }
      
      #read the parameter for variation and for each specific stage
      slope = hfeno$slope_dv[[k]]
      
      #read value from Matrix "A"
      djx <- cuadro[Day,k*2-1]
      mjx <- cuadro[Day,k*2]
      
      #calculate physiological age
      if(Insect_age == 0){
        Insect_age = Insect_age + (djx / 2)
      }else{
        Insect_age <- Insect_age + djx
      }
      
      dev_p <- distrimodel(Insect_age,k,hfeno)
      
      ################################################
      # Se evalua si el insecto muere en cierto estado
      ################################################
      
      #evaluate survival
      survival_p = survival_p * ((1 - mjx) ^ djx)  ## esta probabilidad es muy alta
      survival_rand = (1 - rand_m)
      
      if(survival_p < survival_rand || Day==365){
        dead = 1
        mat[Day,z] <- "dead"
        survival_p = 1
        k = 1
      }
      
      ##################################################
      # Se evalua si el insecto pasa al siguiente estado
      ##################################################
      
      #evaluate development to next stage
      if(dev_p > rand_d){   ### si dev_p es demasiado pequeño nunca va pasar al siguiente estado fenológico
        #read djx from Matrix "A" of the next stage
        djx <- cuadro[Day,(k+1)*2-1]
        Insect_age = djx / 2
        
        rand_d <- round(runif(1),3) #' random number for the next stage
        rand_m <- round(runif(1),3)
        
        survival_p = 1
        k = k + 1
      }
      
      #inmature dev stages
      if(k <= sizeInmaduros){
        kj = estadios[k]
      }
      
      if(k > sizeInmaduros){
        Insect_age = 0
      }
      if(dead == 1){
        kj = "dead"
      }
      mat[Day,z] <- kj
      Day = Day + 1
      dead
      
      
    }
    # print(paste(z,"_",Day))
    # end loop for inmatures
    
    #print(paste(Day,Rs[Day],sep="_")) ## Taza calculada en base ala temperatura que tiene según el dia cuando el insecto termina de ser inmaduro
    if(k == (sizeInmaduros+1) && Insect_age == 0 && Day!=nrow(cuadro) + 1){
      rand_sex <- round(runif(1),3)
      
      if(rand_sex < Rs[Day]){ ## Aquí se aplica
        k = numFemale
      }else{
        k = numMale
      }
    }
    
    #loop for adult males
    while(dead != 1 && k != numFemale && Day!=nrow(cuadro) + 1){
      if(dead ==1){
        break
      }
      
      #read the parameter for variation and for each specific stage
      slope = hfeno$slope_snm
      
      #read value from Matrix "A"
      djx <- cuadro[Day,k*2-2]
      
      #calculate physiological age
      if(Insect_age == 0){
        Insect_age = Insect_age + (djx / 2)
      }else{
        Insect_age <- Insect_age + djx
      }
      
      dev_p <- distriModelAdults(Insect_age,k, sizeInmaduros,hfeno)
      #dev_p = log(Insect_age)
      #dev_p <- distrimodel(dev_p,k)
      
      #evaluate survival of males
      if(dev_p > rand_d){
        dead <- 1
        Insect_age = 0
        
        rand_d <- round(runif(1),3) #' random number for the next stage
        rand_m <- round(runif(1),3)
        
        survival_p = 1
      }
      
      kj = estadios[k]
      
      if(dead == 1){
        kj = "dead"
      }
      
      mat[Day,z] <- kj
      Day = Day + 1
    }# end loop for males
    #print(Day)
    #loop for adult females
    
    # print(paste(z,"_",Day))
    
    
    while(dead != 1 && k != numMale && Day!= nrow(cuadro) + 1){
      if(dead ==1){
        break
      }
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
      
      slope = hfeno$slope_snh
      djx <- cuadro[Day,k*2-1]
      
      #calculate physiological age
      if(Insect_age == 0){
        djx <- djx + cuadro[Day-1,k*2-1]
      }
      
      Insect_age = Insect_age + djx
      dev_p <- distriModelAdults(Insect_age,k, sizeInmaduros,hfeno)
      #dev_p = log(Insect_age)
      #dev_p <- distrimodel(dev_p,k)
      
      #evaluate survival of females
      if(dev_p > rand_d){
        dead = 1
        Insect_age = 0
        rand_d <- round(runif(1),3) #' random number for the next stage
        rand_m <- round(runif(1),3)
        
        survival_p = 1
      }
      
      #calculate oviposition
      #read parameters
      if(dead < 1){
        alfa = hfeno$povih_h[[1]]
        beta = hfeno$povih_h[[2]]
        cv = 0.3 # Averiguar acerca de este valor
        
        #read ovitot from Matrix "A"
        Ovitot = cuadro[Day-1,k*2+1]
        
        #calculate age-dependent oviposition frequency
        x  <-  Insect_age - djx
        OviFrec1  <- eval(funcionc)
        x  <-  Insect_age
        OviFrec2  <- eval(funcionc)
        OviFrec = OviFrec2 - OviFrec1
        
        #calculate oviposition
        Ovitot = qnorm(rand_ovi, Ovitot, Ovitot * cv);Ovitot[Ovitot<0]=0
        Oviposition = Ovitot * OviFrec
        Oviposition = round(Oviposition, 0)
        kj<- Oviposition
      }
      
      if(dead == 1){
        kj = "dead"
      }
      #print("error5");print(paste(Day,z))
      
      mat[Day,z] <- kj
      Day = Day + 1
    }#end loop females and oviposition
    
    Day = 1
  }# end loop individuos
  return(list(mat=data.frame(mat)))
}
#################################################################################
########################################
VIDA<-function(vidalife,Tablelife,p,cuadro,estadios,N,M,hfeno,Rs,Rs2=NULL){
  table_life<-estimado<-res<-list(1)
  NUM <- 3;ni=length(estadios)-2
  tablita<-matrix(NA,6,NUM)
  RES<-matrix(NA,ni+1,NUM)
  RESM<-matrix(NA,ni,NUM)
  
  for(k in 1:NUM){
    estimado [[k]]<-simulacion(cuadro,estadios,N,M,hfeno,Rs)$mat
    table_life[[k]] <- life.table2(estimado[[k]], estadios)$life.table
    tablita[,k]<-PARAlife(N, estadios,table_life[[k]])$parametro
    RES[,k]<-(statistlife(estimado[[k]], estadios)$Survival_Time)[,1]
    RESM[,k]<-as.numeric(as.character((statistlife0(estimado[[k]],estadios)$Mortality)[,1]))
  }
  
  tablita=t(na.omit(t(tablita)))
  MEANlife<-apply(tablita,1,median)
  SElife<-apply(tablita,1,se)
  
  MEANres<-apply(RES[1:ni,],1,mean)
  SEres<-apply(RES[1:ni,],1,se)
  
  MEANresm<-apply(RESM,1,median) ## este vector contiene el porcentaje promedio de mortalidad de cada estado inmaduro
  SEresm<-apply(RESM,1,se) ## contiene los errores estandar de los porcentajes de mortalidad de cada estado
  
  OBS1<-statistlife(vidalife, estadios)$Survival_Time
  OBS2<-statistlife(vidalife,estadios)$Mortality
  
  colnames(p)<-"Observed";t95=abs(qt(0.05/2,2))
  
  P<-c(1)
  for(s in 1:6){P[s]<-t.test(tablita[s,],mu=p[s,],alt="two.side",conf.level = 0.99)$p.value} ## p-value de la prueba de hipotesis de los parametros
  CUADRO1<-data.frame(Simulated=paste(round(MEANlife,3),"(","±",round(t95*SElife,3),")"),round(p,3),P=round(P,4))
  cat("Life-table parameters\n")
  print(CUADRO1)
  
  P<-c(1)
  for(s in 1:ni){P[s]<-t.test(RES[s,],mu=OBS1[s,],alt="two.side")$p.value}
  CUADRO2<-data.frame(Simulated=paste(round(MEANres,3),"(","±",round(t95*SEres,3),")"),Observed=OBS1[1:ni,],P=round(P,4))
  cat("\nDevelopment time (days)\n");rownames(CUADRO2)=estadios[1:ni]
  print(CUADRO2)
  
  P<-c(1)
  for(s in 1:ni){P[s]<- t.test(RESM[s,],mu=as.numeric(as.character(OBS2[s,])),alt="two.side")$p.value}
  CUADRO3<-data.frame(Simulated=paste(round(MEANresm,3),"(","±",round(t95*SEresm,3),")"),Observed=OBS2,P=round(P,4))
  cat("\nMortality (%)\n")
  print(CUADRO3)
}


#########################################
########################################
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
#################################
#################################
simulacion_ant<-function(cuadro,estadios,n,m,hfeno,Rs){
  mat<-matrix("dead",ncol=n,nrow=(m))
  for(z in 1:n){
    k <- 1
    Insect_age <- 0
    survival_p <- 1
    dead <- 0
    Day <- 1
    slope<- 0
    djx <- 0
    mjx <- 0
    kj <- NA
    rand_d <- round(runif(1),3) #' random number between 0 and 1 with 3 digits
    rand_m <- round(runif(1),3)
    rand_ovi <- round(runif(1),3)
    rand_sex = 0
    
    if(rand_ovi == 0){
      rand_ovi <- 0.0001
    }
    
    sizeInmaduros = length(estadios[-(length(estadios)-1):-(length(estadios))])
    numFemale = sizeInmaduros + 1
    numMale = sizeInmaduros + 2
    
    #loop for immature life stages
    while(dead != 1 || k <= sizeInmaduros){
      if(k > sizeInmaduros){
        break
      }
      if(dead ==1){
        break
      }
      
      #read the parameter for variation and for each specific stage
      slope = hfeno$slope_dv[[k]]
      
      #read value from Matrix "A"
      djx <- cuadro[Day,k*2-1]
      mjx <- cuadro[Day,k*2]
      
      #calculate physiological age
      if(Insect_age == 0){
        Insect_age = Insect_age + (djx / 2)
      }else{
        Insect_age <- Insect_age + djx
      }
      
      dev_p <- distrimodel(Insect_age,k,hfeno)
      
      ################################################
      # Se evalua si el insecto muere en cierto estado
      ################################################
      
      #evaluate survival
      survival_p = survival_p * ((1 - mjx) ^ djx)  ## esta probabilidad es muy alta
      survival_rand = (1 - rand_m)
      
      if(survival_p < survival_rand || Day==365){
        dead = 1
        mat[Day,z] <- "dead"
        survival_p = 1
        k = 1
      }
      
      ##################################################
      # Se evalua si el insecto pasa al siguiente estado
      ##################################################
      
      #evaluate development to next stage
      if(dev_p > rand_d){   ### si dev_p es demasiado pequeño nunca va pasar al siguiente estado fenológico
        #read djx from Matrix "A" of the next stage
        djx <- cuadro[Day,(k+1)*2-1]
        Insect_age = djx / 2
        
        rand_d <- round(runif(1),3) #' random number for the next stage
        rand_m <- round(runif(1),3)
        
        survival_p = 1
        k = k + 1
      }
      
      #inmature dev stages
      if(k <= sizeInmaduros){
        kj = estadios[k]
      }
      
      if(k > sizeInmaduros){
        Insect_age = 0
      }
      if(dead == 1){
        kj = "dead"
      }
      mat[Day,z] <- kj
      Day = Day + 1
      dead
      
    }
    # end loop for inmatures
    
    #print(paste(Day,Rs[Day],sep="_")) ## Taza calculada en base ala temperatura que tiene según el dia cuando el insecto termina de ser inmaduro
    if(k == (sizeInmaduros+1) && Insect_age == 0){
      rand_sex <- round(runif(1),3)
      
      if(rand_sex < Rs[Day]){ ## Aquí se aplica
        k = numFemale
      }else{
        k = numMale
      }
    }
    
    #loop for adult males
    while(dead != 1 && k != numFemale){
      if(dead ==1){
        break
      }
      
      #read the parameter for variation and for each specific stage
      slope = hfeno$slope_snm
      
      #read value from Matrix "A"
      djx <- cuadro[Day,k*2-2]
      
      #calculate physiological age
      if(Insect_age == 0){
        Insect_age = Insect_age + (djx / 2)
      }else{
        Insect_age <- Insect_age + djx
      }
      
      dev_p <- distriModelAdults(Insect_age,k, sizeInmaduros,hfeno)
      #dev_p = log(Insect_age)
      #dev_p <- distrimodel(dev_p,k)
      
      #evaluate survival of males
      if(dev_p > rand_d){
        dead <- 1
        Insect_age = 0
        
        rand_d <- round(runif(1),3) #' random number for the next stage
        rand_m <- round(runif(1),3)
        
        survival_p = 1
      }
      
      kj = estadios[k]
      
      if(dead == 1){
        kj = "dead"
      }
      
      mat[Day,z] <- kj
      Day = Day + 1
    }# end loop for males
    
    #loop for adult females
    while(dead != 1 && k != numMale){
      if(dead ==1){
        break
      }
      
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
      
      slope = hfeno$slope_snh
      djx <- cuadro[Day,k*2-1]
      
      #calculate physiological age
      if(Insect_age == 0){
        djx <- djx + cuadro[Day-1,k*2-1]
      }
      
      Insect_age = Insect_age + djx
      dev_p <- distriModelAdults(Insect_age,k, sizeInmaduros,hfeno)
      #dev_p = log(Insect_age)
      #dev_p <- distrimodel(dev_p,k)
      
      #evaluate survival of females
      if(dev_p > rand_d){
        dead = 1
        Insect_age = 0
        rand_d <- round(runif(1),3) #' random number for the next stage
        rand_m <- round(runif(1),3)
        
        survival_p = 1
      }
      
      #calculate oviposition
      #read parameters
      if(dead < 1){
        alfa = hfeno$povih_h[[1]]
        beta = hfeno$povih_h[[2]]
        cv = 0.3 # Averiguar acerca de este valor
        
        #read ovitot from Matrix "A"
        Ovitot = cuadro[Day-1,k*2+1]
        
        #calculate age-dependent oviposition frequency
        x  <-  Insect_age - djx
        OviFrec1  <- eval(funcionc)
        x  <-  Insect_age
        OviFrec2  <- eval(funcionc)
        OviFrec = OviFrec2 - OviFrec1
        
        #calculate oviposition
        Ovitot = qnorm(rand_ovi, Ovitot, Ovitot * cv)
        Oviposition = Ovitot * OviFrec
        Oviposition = round(Oviposition, 0)
        kj<- Oviposition
      }
      
      if(dead == 1){
        kj = "dead"
      }
      
      mat[Day,z] <- kj
      Day = Day + 1
      #mat[,1]
    }#end loop females and oviposition
    
    Day = 1
  }# end loop individuos
  return(list(matriz=data.frame(mat)))
}
##################################################
##################################################

contar1<-function(vec,est){n=length(vec[vec==est]);return(n)}


##################################################
##################################################
life.table2<-function (data, estad,ovi=TRUE){
  if(ovi)
  {
    maduros   <-  estad[(length(estad)-1):(length(estad))]
    inmaduros <-  estad[-(length(estad)-1):-(length(estad))]
    estadios <- estad
    datat <- as.matrix(data)
    num <- matrix(as.numeric(datat), nrow(datat), ncol(datat))
    numc <- num
    for (i in 1:nrow(numc)) for (j in 1:ncol(numc)) if (is.na(numc[i, j])) numc[i, j] <- 0
    newegg <- matrix(apply(numc, 1, sum), nrow(numc))
    day <- 0:(nrow(num) - 1)
    oviposition <- apply(numc, 2, sum)
    estados1 <- matrix(rep(0, length(day)), nrow = length(day),
                       ncol = length(estadios))
    for (i in 1:day) for (j in 1:length(estadios)) estados1[, j] <- Estado(data, estad, estadios[j])
    life.table <- data.frame(estados1, newegg)
    nombres <- rep(0, length(estadios))
    for (i in 1:length(estadios)) nombres[i] <- estadios[i]
    colnames(life.table) <- c(nombres, "new.Egg")
    #trues <- rep(TRUE, nrow(life.table))
    #for (i in 1:nrow(life.table)) trues[i] <- sum(life.table[i, ]) != 0
    #life.table <- life.table[trues, ]
    #print(life.table)
    return(list(life.table=life.table))
  }else
  {
    datos=data;estad=estadios
    n=ncol(datos);m=nrow(datos)
    #n1=ncol(ovipo);m1=nrow(ovipo)
    tab1 <- matrix(0,nrow = m,ncol = (length(estad)+1))
    colnames(tab1)=c(estad,"new.Egg")
    #dat=matrix(as.numeric(as.matrix(ovipo)),m1,n1)
    for(i in 1:(length(estad)+1))
    {
      #if(i==(length(estad)+1)){tab1[,i]=apply(dat,1,sum,na.rm=T);next} ## oviposicion
      if(i==(length(estad)+1)){tab1[,i]=rep(0,m);next} ## oviposicion
      tab1[,i]=apply(datos,1,contar1,estad[i])
    }
    return(list(life.table=data.frame(tab1)))
  } 
}
##########################################################################
###################################################################
#############################################################################################################
PARAlife<-function (N, estad, ltb,Rs2=NULL,ovipo=NULL,nfem2=NULL){
  if(is.null(ovipo))
  {
    estadios <- estad
    lifetable <- ltb
    s.x <- m.x <- l.x <- rep(0, nrow(lifetable))
    for (i in 1:nrow(lifetable)) l.x[i] <- sum(lifetable[i, 1:(length(estadios))])/N ## es un porcentaje de los conteos
    for (i in 1:nrow(lifetable)) if(l.x[i]==0) m.x[i] <- 0 else m.x[i] <- lifetable[,(length(estad)+1)][i]/N/l.x[i] ## este objeto d
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
    #print(parameters)
    return(list(parametro=parameters))
  }else
  {
    n1=length(estadios)
    n2=min((1:nrow(ltb))[ltb[,n1-1]!=0]) ## AQUI PONGO -1 (vector de hembras) CONSIDERANDO QUE HAY 2 ADULTOS, ESTO PUEDE CAMBIAR PARA LOS NUEVOS DATOS
    W1=n2+nrow(ovipo)-1
    x=n2:W1+1.5
    n1=ncol(ovipo);m1=nrow(ovipo);c1=ncol(ovipo)
    dat=matrix(as.numeric(as.matrix(ovipo)),m1,n1)
    neggs = apply(dat,1,sum,na.rm=T) ## num of eggs
    neggs2 = round(ltb[1,1]*c1/nfem2)
    me_f = apply(dat,1,mean,na.rm=T) ## mean eggs on female
    sr= Rs2 ## sexual ratio -- constante por el momento
    mef_f = sr*me_f
    ies = rep(nfem2/ltb[1,1],length(x)) ##inmature estages survival
    nhem<-function(vec){n3=length(vec[!is.na(vec)]);return(n3)}
    nfem = apply(dat,1,nhem) ## num of females by day
    asurv = nfem/nfem[1]  ## adult survival
    lx= ies*asurv
    mx1 = mef_f*asurv ;mx2 = neggs*sr/lx/neggs2
    lx.mx1 = lx*mx1 ;lx.mx2=lx*mx2
    x.lx.mx1 = x*lx.mx1 ;x.lx.mx2 = x*lx.mx2
    S.mx1=sum(mx1)   ;S.mx2=sum(mx2)   ;S.lx.mx1=sum(lx.mx1)   ;S.lx.mx2=sum(lx.mx2)   ;S.x.lx.mx1=sum(x.lx.mx1)   ;S.x.lx.mx2= sum(x.lx.mx2)
    T01=S.x.lx.mx1/S.lx.mx1   ;T02=S.x.lx.mx2/S.lx.mx2
    
    r1=(log(S.lx.mx1))/T01   ;r2=(log(S.lx.mx2))/T02
    euler = (exp((-1)*r1*x))*lx.mx1  ## se tiene que jugar con el "r1" para que la suma de este vector sea 1
    Seuler = sum(euler)
    #r1 puede ser modificable hasta que Seuler sea 1
    T1=(log(S.lx.mx1))/r1   ;T2=(log(S.lx.mx2))/r2
    Dt1=log(2)/r1   ;Dt2=log(2)/r2
    lambda1=exp(r1)   ;lambda2=exp(r2)
    Ro1=S.lx.mx1   ;Ro2=S.lx.mx2
    GRR1=S.mx1   ;GRR2=S.mx2
    #data.frame(neggs,me_f,sr,mef_f,ies,nfem,asurv,lx,mx1,mx2,lx.mx1,lx.mx2,x.lx.mx1,x.lx.mx2)
    parameters=data.frame(c(r=r1,Ro=Ro1,GRR=GRR1,T=T1,lambda=lambda1,Dt=Dt1));colnames(parameters)="Parameters"
    #parameters=data.frame(c(r=r2,Ro=Ro2,GRR=GRR2,T=T2,lambda=lambda2,Dt=Dt2));colnames(parameters)="Parameters" ## este es el otro calculo segun la plantilla de excel
    return(list(parametro=parameters))
  }
}

##########################################################
#############################################################################################################
#VIDA<-function(vidalife,Tablelife,p,cuadro,estadios,N,M,hfeno,sexratio){
VIDA_ant<-function(vidalife,Tablelife,p,cuadro,estadios,N,M,hfeno,Rs){
  table_life<-estimado<-list(1)
  NUM <- 4
  for(k in 1:NUM){
    #estimado [[k]]<- simulacion(cuadro,estadios,N,M,hfeno,sexratio)$matriz
    estimado [[k]]<-simulacion(cuadro,estadios,N,M,hfeno,Rs)$mat
    table_life[[k]] <- life.table2(estimado[[k]], estadios)$life.table
  }
  p1<-PARAlife (N, estadios,table_life[[1]])$parametro
  p2<-PARAlife (N,estadios,table_life[[2]])$parametro
  p3<-PARAlife (N,estadios,table_life[[3]])$parametro
  p4<-PARAlife (N, estadios,table_life[[4]])$parametro
  tablita<-cbind(p1,p2,p3,p4)
  MEANlife<-apply(tablita,1,mean)
  SElife<-apply(tablita,1,se)
  
  rtest<-tablita[1,]
  Rotest<-tablita[2,]
  GRRtest<-tablita[3,]
  Ttest<-tablita[4,]
  Ltest<-tablita[5,]
  Dttest<-tablita[6,]
  res1 <- statistlife(estimado[[1]], estadios)$Survival_Time
  res2 <- statistlife(estimado[[2]], estadios)$Survival_Time
  res3 <- statistlife(estimado[[3]], estadios)$Survival_Time
  res4 <- statistlife(estimado[[4]], estadios)$Survival_Time
  resm1 <- statistlife(estimado[[1]],estadios)$Mortality
  resm2 <- statistlife(estimado[[2]], estadios)$Mortality
  resm3 <- statistlife(estimado[[3]], estadios)$Mortality
  resm4 <- statistlife(estimado[[4]],estadios)$Mortality
  RES<-cbind(res1[-4,],res2[-4,],res3[-4,],res4[-4,])
  RESM<-cbind(as.numeric(levels(resm1$Percent)),as.numeric(levels(resm2$Percent)),
              as.numeric(levels(resm3$Percent)),as.numeric(levels(resm4$Percent)))
  MEANres<-apply(RES,1,mean)
  SEres<-apply(RES,1,se)
  MEANresm<-apply(RESM,1,mean)
  SEresm<-apply(RES,1,se)
  OBS1<-statistlife(vidalife, estadios)$Survival_Time
  OBS2<-statistlife(vidalife,estadios)$Mortality
  colnames(p)<-"Observed"
  CUADRO1<-data.frame(Simulated=paste(round(MEANlife,3),"(","±",round(SElife,3),")"),
                      round(p,3),P=round(c(t.test(rtest,mu=p[1,],alt="two.side")$p.value,
                                           t.test(Rotest,mu=p[2,],alt="two.side")$p.value,
                                           t.test(GRRtest,mu=p[3,],alt="two.side")$p.value,
                                           t.test(Ttest,mu=p[4,],alt="two.side")$p.value,
                                           t.test(Ltest,mu=p[5,],alt="two.side")$p.value,
                                           t.test(Dttest,mu=p[6,],alt="two.side")$p.value),4))
  cat("Life-table parameters\n")
  print(CUADRO1)
  CUADRO2<-data.frame(Simulated=paste(round(MEANres,3),"(","±",round(SEres,3),")"),Observed=OBS1[-4,],
                      P=round(c(t.test(RES[1,],mu=OBS1[1,],alt="two.side")$p.value,
                                t.test(RES[2,],mu=OBS1[2,],alt="two.side")$p.value,
                                t.test(RES[3,],mu=OBS1[3,],alt="two.side")$p.value),4))
  cat("\nDevelopment time (days)\n")
  print(CUADRO2)
  CUADRO3<-data.frame(Simulated=paste(round(MEANresm,3),"(","±",round(SEresm,3),")"),Observed=OBS2,#[-4,],
                      P=round(c(t.test(RESM[1,],mu=as.numeric(as.character(OBS2[1,])),alt="two.side")$p.value,
                                t.test(RESM[2,],mu=as.numeric(as.character(OBS2[2,])),alt="two.side")$p.value,
                                t.test(RESM[3,],mu=as.numeric(as.character(OBS2[3,])),alt="two.side")$p.value),4))
  cat("\nMortality (%)\n")
  print(CUADRO3)
}
#############################################################################################################
#############################################################################################################
statistlife<-function (data, estad){
  estadios <- estad
  esadult <- estadios[(length(estadios) - 1):length(estadios)]
  datat <- as.matrix(data)
  estadoa <- as.list(as.data.frame((datat)))
  estadob <- as.list(rep(0, ncol(data)))
  num <- matrix(as.numeric(datat), nrow(datat), ncol(datat))
  numc <- num
  for (i in 1:nrow(numc)) for (j in 1:ncol(numc)) if (is.na(numc[i,
                                                                 j]))
    numc[i, j] <- 0
  newegg <- matrix(apply(numc, 1, sum), nrow(numc))
  est1 <- subset(estadios, estadios != esadult[2])
  nopupa1 <- nopupa2 <- rep(0, (length(est1) - 2))
  estados3 <- huevo <- matrix(rep(0, length(nopupa1) * ncol(data)),
                              nrow = ncol(data), ncol = length(nopupa1))
  for (is in 1:(length(est1) - 1)) {
    est2 <- est1[is + 1]
    ifelse(est2 == esadult[1], caso <- "caso2", caso <- "caso1")
    if (caso == "caso1") {
      for (i in 1:ncol(data)) {
        estadoa[[i]] <- as.matrix(estadoa[[i]])
        estadob[[i]] <- length(subset(estadoa[[i]], estadoa[[i]] ==
                                        est2))
      }
      estados3[, is] <- as.numeric(as.character(as.matrix(estadob)))
      for (i in 1:length(estados3[, 1])) for (j in 1:length(nopupa1)) if (estados3[i,
                                                                                   j] != 0)
        estados3[i, j] <- 1
      nopupa1[is] <- sum(estados3[, is])
      huevo[, is] <- Estado(t(data), estad, est1[is])
      for (i in 1:length(estados3[, 1])) for (j in 1:length(nopupa1)) ifelse(estados3[i,
                                                                                      j] == 1, huevo[i, j] <- huevo[i, j], huevo[i,
                                                                                                                                 j] <- 0)
      for (i in 1:length(estados3[, 1])) for (j in 1:length(nopupa1)) ifelse(huevo[i,
                                                                                   j] == 0, huevo[i, j] <- NA, huevo[i, j] <- huevo[i,
                                                                                                                                    j])
      nopupa2[is] <- mean(huevo[, is], na.rm = T)
    }
  }
  if (caso == "caso2") {
    for (i in 1:ncol(data)) {
      estadoa[[i]] <- as.matrix(estadoa[[i]])
      estadob[[i]] <- length(subset(estadoa[[i]], estadoa[[i]] ==
                                      esadult[2]))
    }
    estados2 <- as.numeric(as.character(as.matrix(estadob)))
    for (i in 1:length(estados2)) if (estados2[i] != 0)
      estados2[i] <- 1
    male <- sum(estados2)
    num <- matrix(as.numeric(datat), nrow(datat), ncol(datat))
    a <- as.list(as.data.frame((num)))
    b <- as.list(rep(0, ncol(data)))
    for (i in 1:ncol(data)) b[[i]] <- sum(as.matrix(table(a[[i]])))
    female1 <- as.numeric(as.character(as.matrix(b)))
    for (i in 1:length(female1)) if (female1[i] != 0)
      female1[i] <- 1
    female <- sum(female1)
    for (i in 1:ncol(data)) {
      estadoa[[i]] <- as.matrix(estadoa[[i]])
      estadob[[i]] <- length(subset(estadoa[[i]], estadoa[[i]] ==
                                      (estad[is + 3])))
    }
    estados5 <- as.numeric(as.character(as.matrix(estadob)))
    for (i in 1:length(estados5)) if (estados5[i] != 0)
      estados5[i] <- 1
    dead <- sum(estados5)
    pupa1 <- male + female
    capullo <- Estado(t(data), estad, est1[is])
    capullo2 <- rep(0, length(female1))
    for (i in 1:length(female1)) ifelse(estados2[i] == 0 &
                                          female1[i] == 0, capullo2[i] <- 0, capullo2[i] <- 1)
    for (i in 1:length(female1)) ifelse(capullo2[i] == 1,
                                        capullo[i] <- capullo[i], capullo[i] <- 0)
    for (i in 1:length(female1)) ifelse(capullo[i] == 0,
                                        capullo[i] <- NA, capullo[i] <- capullo[i])
    pupa <- mean(capullo, na.rm = T)
  }
  f <- c(ncol(data), female/(female + male), male, female,
         ncol(data) - male - female, male + female + dead, sum(newegg)/female)
  observations <- data.frame(f)
  rownames(observations) <- c("Number of insect :", "Sex ratio :",
                              "Males :", "Females :", "Immature death :", "", "Eggs/Females :")
  colnames(observations) <- c("Observations")
  todo <- rep(0, length(female1))
  for (i in 1:length(female1)) todo[i] <- sum(capullo[i], huevo[i,
                                                                ])
  todos <- mean(todo, na.rm = T)
  time <- c(round(nopupa2, 3), round(pupa, 3), round(todos,
                                                     3))
  insect <- c(nopupa1, pupa1, "")
  d1 <- rep(0, (length(nopupa2) - 1))
  for (i in 1:(length(nopupa2) - 1)) d1[i] <- nopupa1[i + 1] *
    100/nopupa1[i]
  d2 <- pupa1 * 100/nopupa1[-1:-(length(nopupa2) - 1)]
  percente <- c(round((nopupa1[1]/(ncol(data))) * 100, 2),
                round(d1, 2), round(d2, 2))/100
  percent <- c(paste(percente * 100, "%"), "")
  acc <- rep(1, length(nopupa1) + 1)
  acc[1] <- percente[1]
  for (i in 2:(length(nopupa1) + 1)) acc[i] <- acc[i - 1] *
    percente[i]
  accume <- acc
  accum <- c(paste(round(100 * accume, 3), "%"), "")
  survival <- data.frame(cbind(time))
  rownames(survival) <- c(est1[-(length(est1))], "Total")
  colnames(survival) <- c("Time")
  inmor <- rep(0, (length(nopupa1) - 1))
  for (i in 1:(length(nopupa1) - 1)) inmor[i] <- nopupa1[i] -
    nopupa1[i + 1]
  insectmor <- c(ncol(data) - nopupa1[1], inmor, nopupa1[-1:-(length(nopupa2) -
                                                                1)] - pupa1)
  pmor <- rep(0, (length(nopupa1) - 1))
  for (i in 1:(length(nopupa1) - 1)) pmor[i] <- (nopupa1[i] -
                                                   nopupa1[i + 1]) * 100/nopupa1[i]
  ##
  contar1<-function(vec,est){n=length(vec[vec==est]);return(n)}
  contar2<-function(vec){n=length(vec[!is.na(vec)]);return(n)}
  
  myd<-function(vec,estad,est)
  {
    pos=(1:length(estad))[estad==est]
    vec=data.frame(vec,1)
    v1=rownames(vec[vec[,1]==est,])
    if(est!=estad[length(estadios) - 2]){ 
      if(length(v1) != 0){
        n=as.numeric(v1[length(v1)]) 
        if(vec[n+1,1]==estad[pos+1]){mort=0;dsr=1}else{mort=1;dsr=0}
        salida=c(Mort=mort,Desar=dsr,di=n+1)
      }else(salida=c(Mort=NA,Desar=NA,di=NA))
      return(salida)  
    }else{
      if(length(v1) != 0){
        n=as.numeric(v1[length(v1)]) 
        if(vec[n+1,1]==estad[pos+2] || is.numeric(estad[pos+1])){mort=0;dsr=1}else{mort=1;dsr=0}
        salida=c(Mort=mort,Desar=dsr,di=n+1)
      }else(salida=c(Mort=NA,Desar=NA,di=NA))
      return(salida)  
    }    
  }
  
  datos=data
  estadinm=estadios[1:(length(estadios) - 2)]
  
  mor=c(1:length(estadinm))
  
  for(i in 1:length(estadinm))
  {
    o1=apply(datos,2,myd,estadios,estadinm[i]);o1=data.frame(t(o1))
    o2=na.omit(o1)
    mort=aggregate(Mort ~ di,data=o2,sum);v1=sum(mort[,2])
    desa=aggregate(Desar ~ di,data=o2,sum);v2=sum(desa[,2])
    mor[i]=v1/sum(v1,v2)
  }
  
  ##
  permore <- c(round(((ncol(data) - nopupa1[1])/(ncol(data))) *
                       100, 2), round(pmor, 2), round((nopupa1[-1:-(length(nopupa2) -
                                                                      1)] - pupa1) * 100/nopupa1[-1:-(length(nopupa2) - 1)],
                                                      2))
  permor <- paste(round(mor, 3))
  #permor <- paste(round(permore/100, 3))
  acc <- c(paste(round((1 - accume) * 100, 3), "%"))
  mortality <- data.frame(cbind(permor))
  rownames(mortality) <- est1[-(length(est1))]
  colnames(mortality) <- c("Percent")
  #print(observations)
  cat("\n")
  #print(survival)
  cat("\n")
  #print(mortality)
  salida <- list(Survival_Time = survival, Mortality = mortality)
  return(salida)
}
#############################################################################################################
#############################################################################################################
Estado <- function (data, estad,est){
  maduros   <-  estad[(length(estad)-1):(length(estad))]
  if(est==maduros[1]){
    datat <- as.matrix(data)
    num <- matrix(as.numeric(datat), nrow(datat), ncol(datat))
    a <- as.list(as.data.frame(t(num)))
    b <- as.list(rep(0, nrow(data)))
    for (i in 1:nrow(data)) b[[i]] <- sum(as.matrix(table(a[[i]])))
    female <- as.numeric(as.character(as.matrix(b)))
    return(female)
    
  }
  if(est!=maduros[1]){
    datat <- as.matrix(data)
    estadoa <- as.list(as.data.frame(t(datat)))
    estadob <- as.list(rep(0, nrow(data)))
    for (i in 1:nrow(data)) {
      estadoa[[i]] <- as.matrix(estadoa[[i]])
      estadob[[i]] <- length(subset(estadoa[[i]], estadoa[[i]] == est))
    }
    estados <- as.numeric(as.character(as.matrix(estadob)))
    return(estados)
  }
}
#########################################################
simulacion_ant<-function(cuadro,estadios,n,m,hfeno,sexratio){
  #LifeTable<-function(cuadro, estadios, n, m, hfeno, sexratio){
  mat<-matrix("dead",ncol=n,nrow=(m))
  for(z in 1:n){
    k <- 1 # insect life stages (E=1, L=2, P=3, M=4, F=5)
    Insect_age <- 0
    survival_p <- 1
    dead <- 0
    Day <- 1
    slope<- 0
    djx <- 0
    mjx <- 0
    kj <- NA
    rand_d <- round(runif(1),3) #' random number between 0 and 1 with 3 digits
    rand_m <- round(runif(1),3)
    rand_ovi <- round(runif(1),3)
    rand_sex = 0
    
    if(rand_ovi == 0){
      rand_ovi <- 0.0001
    }
    
    sizeInmaduros = length(estadios[-(length(estadios)-1):-(length(estadios))])
    numFemale = sizeInmaduros + 1
    numMale = sizeInmaduros + 2
    
    #loop for immature life stages
    while(dead != 1 || k <= sizeInmaduros){
      if(k > sizeInmaduros){
        break
      }
      if(dead ==1){
        break
      }
      
      #read the parameter for variation and for each specific stage
      slope = hfeno$slope_dv[[k]]
      
      #read value from Matrix "A"
      djx <- cuadro[Day,k*2-1]
      mjx <- cuadro[Day,k*2]
      
      #calculate physiological age
      if(Insect_age == 0){
        Insect_age = Insect_age + (djx / 2)
      }else{
        Insect_age <- Insect_age + djx
      }
      
      dev_p <- distrimodel(Insect_age,k,hfeno)
      
      #evaluate survival
      survival_p = survival_p * ((1 - mjx) ^ djx)
      survival_rand = (1 - rand_m)
      
      if(survival_p < survival_rand){
        dead = 1
        #guarda en matriz
        mat[Day,z] <- "dead"
        survival_p = 1
        k = 1
      }
      
      #evaluate development to next stage
      if(dev_p > rand_d){
        #read djx from Matrix "A" of the next stage
        djx <- cuadro[Day,(k+1)*2-1]
        Insect_age = djx / 2
        
        rand_d <- round(runif(1),3) #' random number for the next stage
        rand_m <- round(runif(1),3)
        
        survival_p = 1
        k = k + 1
      }
      
      #inmature dev stages
      if(k <= sizeInmaduros){
        kj = estadios[k]
      }
      
      if(k > sizeInmaduros){
        Insect_age = 0
      }
      if(dead == 1){
        kj = "dead"
      }
      
      mat[Day,z] <- kj
      
      Day = Day + 1
      #k
      #dead
      
    }
    # end loop for inmatures
    
    if(k == (sizeInmaduros+1) && Insect_age == 0){
      rand_sex <- round(runif(1),3)
      #}
      if(rand_sex < sexratio){
        k = numFemale
      }else{
        k = numMale
      }
    }
    
    #loop for adult males
    while(dead != 1 && k != numFemale){
      if(dead ==1){
        break
      }
      
      #read the parameter for variation and for each specific stage
      slope = hfeno$slope_snm
      
      #read value from Matrix "A"
      djx <- cuadro[Day,k*2-2]
      
      #calculate physiological age
      if(Insect_age == 0){
        Insect_age = Insect_age + (djx / 2)
      }else{
        Insect_age <- Insect_age + djx
      }
      
      dev_p <- distriModelAdults(Insect_age,k,sizeInmaduros,hfeno)
      #dev_p = log(Insect_age)
      #dev_p <- distrimodel(dev_p,k)
      
      #evaluate survival of males
      if(dev_p > rand_d){
        dead <- 1
        Insect_age = 0
        
        rand_d <- round(runif(1),3) #' random number for the next stage
        rand_m <- round(runif(1),3)
        
        survival_p = 1
      }
      
      kj = estadios[k]
      
      if(dead == 1){
        kj = "dead"
      }
      
      mat[Day,z] <- kj
      Day = Day + 1
    }# end loop for males
    
    #loop for adult females
    while(dead != 1 && k != numMale){
      if(dead ==1){
        break
      }
      
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
      
      slope = hfeno$slope_snh
      djx <- cuadro[Day,k*2-1]
      
      #calculate physiological age
      if(Insect_age == 0){
        djx <- djx + cuadro[Day-1,k*2-1]
      }
      
      Insect_age = Insect_age + djx
      dev_p <- distriModelAdults(Insect_age,k,hfeno)
      #dev_p = log(Insect_age)
      #dev_p <- distrimodel(dev_p,k)
      
      #evaluate survival of females
      if(dev_p > rand_d){
        dead = 1
        Insect_age = 0
        rand_d <- round(runif(1),3) #' random number for the next stage
        rand_m <- round(runif(1),3)
        
        survival_p = 1
      }
      
      #calculate oviposition
      #read parameters
      if(dead < 1){
        alfa = hfeno$povih_h[[1]]
        beta = hfeno$povih_h[[2]]
        cv = 0.3 # Averiguar acerca de este valor
        
        #read ovitot from Matrix "A"
        Ovitot = cuadro[Day-1,k*2+1]
        
        #calculate age-dependent oviposition frequency
        x  <-  Insect_age - djx
        OviFrec1  <- eval(funcionc)
        x  <-  Insect_age
        OviFrec2  <- eval(funcionc)
        OviFrec = OviFrec2 - OviFrec1
        
        #calculate oviposition
        Ovitot = qnorm(rand_ovi, Ovitot, Ovitot * cv)
        Oviposition = Ovitot * OviFrec
        Oviposition = round(Oviposition, 0)
        kj<- Oviposition
      }
      
      if(dead == 1){
        kj = "dead"
      }
      
      mat[Day,z] <- kj
      Day = Day + 1
      #mat[,1]
    }#end loop females and oviposition
    
    Day = 1
  }# end loop individuos
  return(list(matriz=mat))
}
#############################################################################################################
##########################################################################################################
distrimodel<-function(vec,sll,hfeno)  # sl es 1, 2 ,3,4 ...)
{
  # probit
  if(hfeno$distri_dv[[sll]]=="probit")
  {
    WW<-pnorm(log(vec)*hfeno$slope_dv[[sll]])
  }
  # logit
  if(hfeno$distri_dv[[sll]]=="logit")
  {
    WW<-1/(1+exp(-(log(vec)*hfeno$slope_dv[[sll]])))
  }
  # cloglog
  if(hfeno$distri_dv[[sll]]=="cloglog")
  {
    WW<-1-exp(-exp(log(vec)*hfeno$slope_dv[[sll]]))
  }
  return(WW)
}
##########################################
distriModelAdults<-function(vec,k, sizeInmaduros,hfeno){  # sl es 4 o 5
  if(k == (sizeInmaduros +1)){
    # probit
    if(hfeno$distri_snh=="probit"){
      WW<-pnorm(log(vec)*hfeno$slope_snh)
    }
    # logit
    if(hfeno$distri_snh=="logit"){
      WW<-1/(1+exp(-(log(vec)*hfeno$slope_snh)))
    }
    # cloglog
    if(hfeno$distri_snh=="cloglog"){
      WW<-1-exp(-exp(log(vec)*hfeno$slope_snh))
    }
  }
  
  if(k == (sizeInmaduros +2)){
    # probit
    if(hfeno$distri_snm=="probit"){
      WW<-pnorm(log(vec)*hfeno$slope_snm)
    }
    # logit
    if(hfeno$distri_snm=="logit"){
      WW<-1/(1+exp(-(log(vec)*hfeno$slope_snm)))
    }
    # cloglog
    if(hfeno$distri_snm=="cloglog"){
      WW<-1-exp(-exp(log(vec)*hfeno$slope_snm))
    }
  }
  return(WW)
}
#############################################################################################################
#graffvida<-function(vidalife,cuadro,estadios,N,M,hfeno,sexratio,labx,laby,corrx,corry,lgx,lgy){
graffvida_ant<-function(vidalife,cuadro,estadios,N,M,hfeno,labx,laby,corrx,corry,lgx,lgy, Rs){
  table_life<-estimado<-list(1)
  NUM <- 4
  TABLEmean<-matrix(0, nrow=nrow(cuadro), ncol=6)
  TABLEmin<-matrix(0, nrow=nrow(cuadro), ncol=6)
  TABLEmax<-matrix(0, nrow=nrow(cuadro), ncol=6)
  for(k in 1:NUM)
  {
    #estimado [[k]]<- simulacion(cuadro,estadios,N,M,hfeno,sexratio)$matriz
    estimado [[k]]<- simulacion(cuadro,estadios,N,M,hfeno,Rs)$matriz
    table_life[[k]] <- life.table2(estimado[[k]], estadios)$life.table
  }
  for(i in 1:ncol(table_life[[1]]))
  {
    for(j in 1:(nrow(table_life[[1]])-1))
    {
      TABLEmin[j,i] <- min(c(table_life[[1]][j,i],table_life[[2]][j,i],table_life[[3]][j,i],table_life[[4]][j,i]))
      TABLEmax[j,i] <- max(c(table_life[[1]][j,i],table_life[[2]][j,i],table_life[[3]][j,i],table_life[[4]][j,i]))
      TABLEmean[j,i]<- mean(c(table_life[[1]][j,i],table_life[[2]][j,i],table_life[[3]][j,i],table_life[[4]][j,i]))
    }
  }
  # GRAFF
  plot(1:nrow(TABLEmean),TABLEmean[,1],ylim=corry,xlim=corrx,type="l", col=1,lwd=1,frame=F,ylab=laby,xlab=labx)
  for(i in 1:length(estadios)){
    lines(1:nrow(TABLEmean),TABLEmean[,i],lwd=1,col=i)
    lines(1:nrow(TABLEmin),TABLEmin[,i],lwd=1,col=i,lty=4)
    lines(1:nrow(TABLEmax),TABLEmax[,i],lwd=1,col=i,lty=4)
  }
  
  ## lines(1:nrow(TABLEmean),TABLEmean[,2],lwd=1,col=2)
  ## lines(1:nrow(TABLEmean),TABLEmean[,3],lwd=1,col=3)
  ## lines(1:nrow(TABLEmean),TABLEmean[,4],lwd=1,col=4)
  ## lines(1:nrow(TABLEmean),TABLEmean[,5],lwd=1,col=5)
  ## lines(1:nrow(TABLEmin),TABLEmin[,1],lwd=1,col=1,lty=4)
  ## lines(1:nrow(TABLEmin),TABLEmin[,2],lwd=1,col=2,lty=4)
  ## lines(1:nrow(TABLEmin),TABLEmin[,3],lwd=1,col=3,lty=4)
  ## lines(1:nrow(TABLEmin),TABLEmin[,4],lwd=1,col=4,lty=4)
  ## lines(1:nrow(TABLEmin),TABLEmin[,5],lwd=1,col=5,lty=4)
  ## lines(1:nrow(TABLEmax),TABLEmax[,1],lwd=1,col=1,lty=4)
  ## lines(1:nrow(TABLEmax),TABLEmax[,2],lwd=1,col=2,lty=4)
  ## lines(1:nrow(TABLEmax),TABLEmax[,3],lwd=1,col=3,lty=4)
  ## lines(1:nrow(TABLEmax),TABLEmax[,4],lwd=1,col=4,lty=4)
  ## lines(1:nrow(TABLEmax),TABLEmax[,5],lwd=1,col=5,lty=4)
  
  Tablelife<-life.table2(vidalife, estadios)$life.table
  
  for(j in 1:length(estadios)){
    a<-NA
    for(i in 1:(length(Tablelife[,j])-1)){
      ## if(Tablelife[,j][i+1]>=
      ## Tablelife[,j][i])
      ## a[i]<-Tablelife[,j][i] else break
      if(Tablelife[,j][i+1] >= Tablelife[,j][i]){
        if(Tablelife[,j][i] > 0)
          a[i]<-Tablelife[,j][i] 
      }else break
      
    }
    points(1:length(a),a,col=j, pch=19)
  }
  
  ltyrep = rep(1,length(estadios))
  colrep = seq(1:length(estadios))
  legend(lgx,lgy,c(estadios),col=colrep,lty=ltyrep)
  #legend(lgx,lgy,c(estadios),col=c(1,2,3,4,5),lty=c(1,1,1,1,1))
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


matrizA <- function(estadios, hfeno, Table, steps,modelim){
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
    
    RM=apply(Table2,1,RateI,Table2,K,parametrosc,parametrosm,funciont,funcionm,nmax,steps,J=NA,modelim=modelim) ## procesamiento de tasa de desarrollo y mortalidad por cada temperatura
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
##########################################
simulacion_ant2<-function(cuadro,estadios,n,m,hfeno,sexratio){
  #LifeTable<-function(cuadro, estadios, n, m, hfeno, sexratio){
  mat<-matrix("dead",ncol=n,nrow=(m))
  for(z in 1:n){
    k <- 1
    Insect_age <- 0
    survival_p <- 1
    dead <- 0
    Day <- 1
    slope<- 0
    djx <- 0
    mjx <- 0
    kj <- NA
    rand_d <- round(runif(1),3) #' random number between 0 and 1 with 3 digits
    rand_m <- round(runif(1),3)
    rand_ovi <- round(runif(1),3)
    rand_sex = 0
    
    if(rand_ovi == 0){
      rand_ovi <- 0.0001
    }
    
    sizeInmaduros = length(estadios[-(length(estadios)-1):-(length(estadios))])
    numFemale = sizeInmaduros + 1
    numMale = sizeInmaduros + 2
    
    #loop for immature life stages
    while(dead != 1 || k <= sizeInmaduros){
      if(k > sizeInmaduros){
        break
      }
      if(dead ==1){
        break
      }
      
      #read the parameter for variation and for each specific stage
      slope = hfeno$slope_dv[[k]]
      
      #read value from Matrix "A"
      djx <- cuadro[Day,k*2-1]
      mjx <- cuadro[Day,k*2]
      
      #calculate physiological age
      if(Insect_age == 0){
        Insect_age = Insect_age + (djx / 2)
      }else{
        Insect_age <- Insect_age + djx
      }
      
      dev_p <- distrimodel(Insect_age,k,hfeno)
      
      #evaluate survival
      survival_p = survival_p * ((1 - mjx) ^ djx)
      survival_rand = (1 - rand_m)
      
      if(survival_p < survival_rand){
        dead = 1
        mat[Day,z] <- "dead"
        survival_p = 1
        k = 1
      }
      
      #evaluate development to next stage
      if(dev_p > rand_d){
        #read djx from Matrix "A" of the next stage
        djx <- cuadro[Day,(k+1)*2-1]
        Insect_age = djx / 2
        
        rand_d <- round(runif(1),3) #' random number for the next stage
        rand_m <- round(runif(1),3)
        
        survival_p = 1
        k = k + 1
      }
      
      #inmature dev stages
      if(k <= sizeInmaduros){
        kj = estadios[k]
      }
      
      if(k > sizeInmaduros){
        Insect_age = 0
      }
      if(dead == 1){
        kj = "dead"
      }
      
      mat[Day,z] <- kj
      
      Day = Day + 1
      k
      dead
      
    }
    # end loop for inmatures
    
    if(k == (sizeInmaduros+1) && Insect_age == 0){
      rand_sex <- round(runif(1),3)
      #}
      if(rand_sex < sexratio){
        k = numFemale
      }else{
        k = numMale
      }
    }
    
    #loop for adult males
    while(dead != 1 && k != numFemale){
      if(dead ==1){
        break
      }
      
      #read the parameter for variation and for each specific stage
      slope = hfeno$slope_snm
      
      #read value from Matrix "A"
      djx <- cuadro[Day,k*2-2]
      
      #calculate physiological age
      if(Insect_age == 0){
        Insect_age = Insect_age + (djx / 2)
      }else{
        Insect_age <- Insect_age + djx
      }
      
      dev_p <- distriModelAdults(Insect_age,k, sizeInmaduros,hfeno)
      #dev_p = log(Insect_age)
      #dev_p <- distrimodel(dev_p,k)
      
      #evaluate survival of males
      if(dev_p > rand_d){
        dead <- 1
        Insect_age = 0
        
        rand_d <- round(runif(1),3) #' random number for the next stage
        rand_m <- round(runif(1),3)
        
        survival_p = 1
      }
      
      kj = estadios[k]
      
      if(dead == 1){
        kj = "dead"
      }
      
      mat[Day,z] <- kj
      Day = Day + 1
    }# end loop for males
    
    #loop for adult females
    while(dead != 1 && k != numMale){
      if(dead ==1){
        break
      }
      
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
      
      slope = hfeno$slope_snh
      djx <- cuadro[Day,k*2-1]
      
      #calculate physiological age
      if(Insect_age == 0){
        djx <- djx + cuadro[Day-1,k*2-1]
      }
      
      Insect_age = Insect_age + djx
      dev_p <- distriModelAdults(Insect_age,k, sizeInmaduros,hfeno)
      #dev_p = log(Insect_age)
      #dev_p <- distrimodel(dev_p,k)
      
      #evaluate survival of females
      if(dev_p > rand_d){
        dead = 1
        Insect_age = 0
        rand_d <- round(runif(1),3) #' random number for the next stage
        rand_m <- round(runif(1),3)
        
        survival_p = 1
      }
      
      #calculate oviposition
      #read parameters
      if(dead < 1){
        alfa = hfeno$povih_h[[1]]
        beta = hfeno$povih_h[[2]]
        cv = 0.3 # Averiguar acerca de este valor
        
        #read ovitot from Matrix "A"
        Ovitot = cuadro[Day-1,k*2+1]
        
        #calculate age-dependent oviposition frequency
        x  <-  Insect_age - djx
        OviFrec1  <- eval(funcionc)
        x  <-  Insect_age
        OviFrec2  <- eval(funcionc)
        OviFrec = OviFrec2 - OviFrec1
        
        #calculate oviposition
        Ovitot = qnorm(rand_ovi, Ovitot, Ovitot * cv)
        Oviposition = Ovitot * OviFrec
        Oviposition = round(Oviposition, 0)
        kj<- Oviposition
      }
      
      if(dead == 1){
        kj = "dead"
      }
      
      mat[Day,z] <- kj
      Day = Day + 1
      #mat[,1]
    }#end loop females and oviposition
    
    Day = 1
  }# end loop individuos
  return(list(matriz=data.frame(mat)))
}










##########################################
Ratios<-function(Table,modelim,modelm,estadios,xi, steps){
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
      ifelse(i!=length(Table[,1]),M2<-(Table[,2][i]+Table[,1][i+1])/2,M2<-(Table[,2][i]+Table[,1][1])/2)
      ifelse(i!=length(Table[,1]),R2<-(Table[,2][i]-Table[,1][i+1])/2,R2<-(Table[,2][i]-Table[,1][1])/2)
      
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
      ifelse(i!=length(Table[,1]),M2<-(Table[,2][i]+Table[,1][i+1])/2,M2<-(Table[,2][i]+Table[,1][1])/2)
      ifelse(i!=length(Table[,1]),R2<-(Table[,2][i]-Table[,1][i+1])/2,R2<-(Table[,2][i]-Table[,1][1])/2)
      
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

statistlife0<-function (data, estad){
  estadios <- estad
  esadult <- estadios[(length(estadios) - 1):length(estadios)]
  datat <- as.matrix(data)
  estadoa <- as.list(as.data.frame((datat)))
  estadob <- as.list(rep(0, ncol(data)))
  num <- matrix(as.numeric(datat), nrow(datat), ncol(datat))
  numc <- num
  for (i in 1:nrow(numc)) for (j in 1:ncol(numc)) if (is.na(numc[i,
                                                                 j]))
    numc[i, j] <- 0
  newegg <- matrix(apply(numc, 1, sum), nrow(numc))
  est1 <- subset(estadios, estadios != esadult[2])
  nopupa1 <- nopupa2 <- rep(0, (length(est1) - 2))
  estados3 <- huevo <- matrix(rep(0, length(nopupa1) * ncol(data)),
                              nrow = ncol(data), ncol = length(nopupa1))
  for (is in 1:(length(est1) - 1)) {
    est2 <- est1[is + 1]
    ifelse(est2 == esadult[1], caso <- "caso2", caso <- "caso1")
    if (caso == "caso1") {
      for (i in 1:ncol(data)) {
        estadoa[[i]] <- as.matrix(estadoa[[i]])
        estadob[[i]] <- length(subset(estadoa[[i]], estadoa[[i]] ==
                                        est2))
      }
      estados3[, is] <- as.numeric(as.character(as.matrix(estadob)))
      for (i in 1:length(estados3[, 1])) for (j in 1:length(nopupa1)) if (estados3[i,
                                                                                   j] != 0)
        estados3[i, j] <- 1
      nopupa1[is] <- sum(estados3[, is])
      huevo[, is] <- Estado(t(data), estad, est1[is])
      for (i in 1:length(estados3[, 1])) for (j in 1:length(nopupa1)) ifelse(estados3[i,
                                                                                      j] == 1, huevo[i, j] <- huevo[i, j], huevo[i,
                                                                                                                                 j] <- 0)
      for (i in 1:length(estados3[, 1])) for (j in 1:length(nopupa1)) ifelse(huevo[i,
                                                                                   j] == 0, huevo[i, j] <- NA, huevo[i, j] <- huevo[i,
                                                                                                                                    j])
      nopupa2[is] <- median(huevo[, is], na.rm = T)
    }
  }
  if (caso == "caso2") {
    for (i in 1:ncol(data)) {
      estadoa[[i]] <- as.matrix(estadoa[[i]])
      estadob[[i]] <- length(subset(estadoa[[i]], estadoa[[i]] ==
                                      esadult[2]))
    }
    estados2 <- as.numeric(as.character(as.matrix(estadob)))
    for (i in 1:length(estados2)) if (estados2[i] != 0)
      estados2[i] <- 1
    male <- sum(estados2)
    num <- matrix(as.numeric(datat), nrow(datat), ncol(datat))
    a <- as.list(as.data.frame((num)))
    b <- as.list(rep(0, ncol(data)))
    for (i in 1:ncol(data)) b[[i]] <- sum(as.matrix(table(a[[i]])))
    female1 <- as.numeric(as.character(as.matrix(b)))
    for (i in 1:length(female1)) if (female1[i] != 0)
      female1[i] <- 1
    female <- sum(female1)
    for (i in 1:ncol(data)) {
      estadoa[[i]] <- as.matrix(estadoa[[i]])
      estadob[[i]] <- length(subset(estadoa[[i]], estadoa[[i]] ==
                                      (estad[is + 3])))
    }
    estados5 <- as.numeric(as.character(as.matrix(estadob)))
    for (i in 1:length(estados5)) if (estados5[i] != 0)
      estados5[i] <- 1
    dead <- sum(estados5)
    pupa1 <- male + female
    capullo <- Estado(t(data), estad, est1[is])
    capullo2 <- rep(0, length(female1))
    for (i in 1:length(female1)) ifelse(estados2[i] == 0 &
                                          female1[i] == 0, capullo2[i] <- 0, capullo2[i] <- 1)
    for (i in 1:length(female1)) ifelse(capullo2[i] == 1,
                                        capullo[i] <- capullo[i], capullo[i] <- 0)
    for (i in 1:length(female1)) ifelse(capullo[i] == 0,
                                        capullo[i] <- NA, capullo[i] <- capullo[i])
    pupa <- median(capullo, na.rm = T)
  }
  f <- c(ncol(data), female/(female + male), male, female,
         ncol(data) - male - female, male + female + dead, sum(newegg)/female)
  observations <- data.frame(f)
  rownames(observations) <- c("Number of insect :", "Sex ratio :",
                              "Males :", "Females :", "Immature death :", "", "Eggs/Females :")
  colnames(observations) <- c("Observations")
  todo <- rep(0, length(female1))
  for (i in 1:length(female1)) todo[i] <- sum(capullo[i], huevo[i,
                                                                ])
  todos <- median(todo, na.rm = T)
  time <- c(round(nopupa2, 4), round(pupa, 4), round(todos,
                                                     4))
  insect <- c(nopupa1, pupa1, "")
  d1 <- rep(0, (length(nopupa2) - 1))
  for (i in 1:(length(nopupa2) - 1)) d1[i] <- nopupa1[i + 1] *
    100/nopupa1[i]
  d2 <- pupa1 * 100/nopupa1[-1:-(length(nopupa2) - 1)]
  percente <- c(round((nopupa1[1]/(ncol(data))) * 100, 4),
                round(d1, 3), round(d2, 3))/100
  percent <- c(paste(percente * 100, "%"), "")
  acc <- rep(1, length(nopupa1) + 1)
  acc[1] <- percente[1]
  for (i in 2:(length(nopupa1) + 1)) acc[i] <- acc[i - 1] *
    percente[i]
  accume <- acc
  accum <- c(paste(round(100 * accume, 4), "%"), "")
  survival <- data.frame(cbind(time))
  rownames(survival) <- c(est1[-(length(est1))], "Total")
  colnames(survival) <- c("Time")
  inmor <- rep(0, (length(nopupa1) - 1))
  for (i in 1:(length(nopupa1) - 1)) inmor[i] <- nopupa1[i] -
    nopupa1[i + 1]
  insectmor <- c(ncol(data) - nopupa1[1], inmor, nopupa1[-1:-(length(nopupa2) -
                                                                1)] - pupa1)
  pmor <- rep(0, (length(nopupa1) - 1))
  for (i in 1:(length(nopupa1) - 1)) pmor[i] <- (nopupa1[i] -
                                                   nopupa1[i + 1]) * 100/nopupa1[i]
  ##
  contar1<-function(vec,est){n=length(vec[vec==est]);return(n)}
  contar2<-function(vec){n=length(vec[!is.na(vec)]);return(n)}
  
  myd<-function(vec,estad,est)
  {
    pos=(1:length(estad))[estad==est]
    vec=data.frame(vec,1)
    v1=rownames(vec[vec[,1]==est,])
    if(est!=estad[length(estadios) - 2]){ 
      if(length(v1) != 0){
        n=as.numeric(v1[length(v1)]) 
        if(vec[n+1,1]==estad[pos+1]){mort=0;dsr=1}else{mort=1;dsr=0}
        salida=c(Mort=mort,Desar=dsr,di=n+1)
      }else(salida=c(Mort=NA,Desar=NA,di=NA))
      return(salida)  
    }else{
      if(length(v1) != 0){
        n=as.numeric(v1[length(v1)]) 
        if(vec[n+1,1]==estad[pos+2] || is.numeric(estad[pos+1])){mort=0;dsr=1}else{mort=1;dsr=0}
        salida=c(Mort=mort,Desar=dsr,di=n+1)
      }else(salida=c(Mort=NA,Desar=NA,di=NA))
      return(salida)  
    }    
  }
  
  datos=data
  estadinm=estadios[1:(length(estadios) - 2)]
  
  mor=c(1:length(estadinm))
  
  for(i in 1:length(estadinm))
  {
    o1=apply(datos,2,myd,estadios,estadinm[i]);o1=data.frame(t(o1))
    o2=na.omit(o1)
    mort=aggregate(Mort ~ di,data=o2,sum);v1=sum(mort[,2])
    desa=aggregate(Desar ~ di,data=o2,sum);v2=sum(desa[,2])
    mor[i]=v1/sum(v1,v2)
  }
  
  ##
  permore <- c(round(((ncol(data) - nopupa1[1])/(ncol(data))) *
                       100, 4), round(pmor, 4), round((nopupa1[-1:-(length(nopupa2) -
                                                                      1)] - pupa1) * 100/nopupa1[-1:-(length(nopupa2) - 1)],
                                                      4))
  permor <- paste(round(mor, 4))
  #permor <- paste(round(permore/100, 3))
  acc <- c(paste(round((1 - accume) * 100, 4), "%"))
  mortality <- data.frame(cbind(permor))
  rownames(mortality) <- est1[-(length(est1))]
  colnames(mortality) <- c("Percent")
  #print(observations)
  cat("\n")
  #print(survival)
  cat("\n")
  #print(mortality)
  salida <- list(Survival_Time = survival, Mortality = mortality)
  return(salida)
}

se <- function(x) sd(x)/sqrt(length(x))
