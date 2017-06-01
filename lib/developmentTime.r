 temp<-function(datos,estadios,est)
 {
     tip<-1:length(estadios)
     for (is in 1:(length(estadios))) if (estadios[is] == est) dat<-datos[[is]]
     not <- as.numeric(levels(factor(dat[, 1])))
     return(list(temperature=not))
 }

###############################

findval=function(dat,cate)
{n1=ncol(dat)
	result=rep(0,n1)
	for(i in 1:n1)
	{
		p1=unique(dat[,i])==cate
		if(length(p1[p1==TRUE])>=1){result[i]=1}else{result[i]=0}
	}
	ntot=sum(result)
	if(ntot>=1){exist=TRUE}else{exist=FALSE}
	return(list(ntot=ntot,exist=exist))
}

###############################

verif.val<-function(datos,cate)
{
	verif=rep(FALSE,length(datos))
	for(j in 1:length(datos))
	{
		verif[j]=findval(datos[[j]],cate)$exist
	}
	return(verif)
}

###############################

findval.num=function(dat)
{n1=ncol(dat)
	for(i in 1:n1)
	{
		p1=unique(dat[,i])
		p1=as.character(p1)
		p1=as.numeric(p1)
		exist=FALSE
		for(j in 1:length(p1))
		{
			pp=p1[j]+1
			if(!is.na(pp)){exist=TRUE;break}
		}
		if(exist==TRUE){break}
	}
	return(list(exist=exist))
}

###############################

verif.val.num<-function(datos,cate,estadios)
{
	if(cate==estadios[length(estadios)-1])
	{
		verif=rep(FALSE,length(datos))
		for(j in 1:length(datos))
		{
			verif[j]=findval.num(datos[[j]])$exist
			#if(exist==TRUE){break} ## corregir
		}
		return(verif)
	}
}

###############################

conteo <- function (data,est) 
{
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

###############################
countdata<-function(datos,estadios,datset,intervalo){     
	n11=length(estadios)
	ovi<-tcont<-list(1)
	k<-1
	while(k<=length(datos)){
		if(datset==1){
			estados1 <- matrix(rep(0, ncol(datos[[k]])), nrow = ncol(datos[[k]]), ncol = length(estadios))
			for (j in 1:length(estadios)) estados1[, j] <- conteo(t(datos[[k]]), estadios[j])  ## hace el conteo del estado por cada temperatura
			for(j in 1:ncol(estados1))
				for(i in 1:nrow(estados1)) if(estados1[,j][i]==0) estados1[,j][i]<-NA ## reemplaza los valores 0 por NA, en toda la tabla
			tcont[[k]]<-intervalo*estados1 ## cuando hay multi
			for(j in 2:length(estadios)) ## por estado
				for(i in 1:nrow(estados1)) ## por insecto
				{
					if(j==(n11-1)){if(is.na(tcont[[k]][,j][i]) & is.na(tcont[[k]][,j+1][i]) ){tcont[[k]][,(j-1)][i]<-NA}  }else{
						if(is.na(tcont[[k]][,j][i]) & j!= length(estadios)){ tcont[[k]][,(j-1)][i]<-NA}  }
				}
			k<-k+1
		}
		if(datset==2){
			estados1 <- matrix(rep(0, ncol(datos[[k]])), nrow = ncol(datos[[k]]), ncol = length(estadios))
			for (j in 1:length(estadios)) estados1[, j] <- conteo(t(datos[[k]]), estadios[j])
			estados1[estados1==0]<-NA   ## nuevo
			
			tcont[[k]]<-estados1
			lista1 <- as.matrix(datos[[k]])
			lista2 <- as.list(as.data.frame(matrix(as.numeric(lista1),nrow(lista1),ncol(lista1))))
			lis3<-matrix(rep(NA,ncol(datos[[k]])*nrow(datos[[k]])),nrow=nrow(datos[[k]]),ncol=ncol(datos[[k]]))
			lis3 <- as.list(as.data.frame(matrix(as.numeric(lis3),nrow(lista1),ncol(lista1))))
			lis4<-lis3
			for(j in 1:ncol(datos[[k]])) lis3[[j]]<-subset(lista2[[j]],is.na(lista2[[j]])==F)
			e<-list(0)
			for(j in 1:ncol(datos[[k]])) e[j]<-length(lis3[[j]])
			e<-max(as.numeric(as.character(e)))
			lis4<-matrix(rep(NA,ncol(datos[[k]])*e),nrow=e,ncol=ncol(datos[[k]]))
			lis4<-as.list(as.data.frame(matrix(lis4,e,ncol(lista1))))
			for(j in 1:ncol(datos[[k]])) for(h in 1:length(lis4[[j]])) lis4[[j]][h]<-lis3[[j]][h]
			lis5<-matrix(rep(0,ncol(datos[[k]])*e),nrow=e,ncol=ncol(datos[[k]]))
			for(j in 1:ncol(datos[[k]])) lis5[,j]<-lis4[[j]]
			ovi[[k]]<-t(lis5)
			lis6<-matrix(rep(0,ncol(datos[[k]])),nrow=ncol(datos[[k]]))
			for(j in 1:ncol(datos[[k]])) lis6[j,]<-length(subset(ovi[[k]][j,],is.na(ovi[[k]][j,])==F))
			for(j in 1:ncol(datos[[k]])) if(lis6[j,]==0) lis6[j,]<-NA
			tcont[[k]][,(length(estadios)-1)]<-lis6
			for(j in 2:(length(estadios)-2))
				for(i in 1:nrow(estados1)) if(is.na(tcont[[k]][,j][i])) tcont[[k]][,(j-1)][i]<-NA
			for(i in 1:nrow(estados1))
				if(is.na(tcont[[k]][,(length(estadios)-2)+1][i]) & is.na(tcont[[k]][,(length(estadios)-2)+2][i]))
					tcont[[k]][,(length(estadios)-2)][i]<-NA
			tcont[[k]]<-cbind(intervalo*tcont[[k]],ovi[[k]])
			k<-k+1
		}
	}
	return(tcont)
}



###############################
devath <- function(opc, datos, estadios, est, tp,intervalo,modelo,poli=1)
{
	if (opc == 1) {      ########********************datos en forma tabla
		tip<-1:length(estadios);exis.val<-c(1)
		for (is in 1:(length(estadios))){  if (estadios[is] == est){ data<-datos[[is]]; data[,4]=data[,4]*poli ; data[,3]=data[,3]*poli }}
		nag <- length(levels(factor(data[, 1])))
		not <- levels(factor(data[, 1]))
		tran <- puntos <- poracum <- por <- tol <- agente <- sampli<-list(1)
		for (i in 1:nag) {
			agente[i] <- list(subset(data, data[, 1] == not[i]))
			tol[i] <- list(agente[[i]][, 4])
			por[i] <- list(rep(0, nrow(agente[[i]])))
			for (j in 1:length(tol[[i]])) por[[i]][j] <- (tol[[i]][j])
			poracum[i] <- list(rep(0, length(tol[[i]])))
			for (j in 1:length(tol[[i]])) {
				if (j == 1) poracum[[i]][j] <- por[[i]][j]
				if (j != 1) poracum[[i]][j] <- por[[i]][j] + poracum[[i]][j - 1]
			}
			tran[i] <- list(round(poracum[[i]], 4))
			sampli[[i]]<-tran[[i]][length(tran[[i]])]
			if( nrow(agente[[i]])==1 & agente[[i]][1,3]==agente[[i]][1,4]){exis.val[i]=0}else{exis.val[i]=1}
		}
		cap=rep(TRUE,length(tp))  ## modificacion
		matriz <- cbind(agente[[1]][, 1], agente[[1]][, 2],
				log(agente[[1]][, 2]), agente[[1]][, 3],tran[[1]],sampli[[1]])
		for (i in 2:nag) matriz <- rbind(matriz, cbind(agente[[i]][, 1],
							agente[[i]][, 2], log(agente[[i]][, 2]),
							agente[[i]][, 3],tran[[i]],sampli[[i]]))
		matri <- as.data.frame(matriz)
		medobs<-sd<-se<-c(0)
		rept<-list(1)
		for (i in 1:nag)
		{
			rept[[i]]<-rep(agente[[i]][,2],agente[[i]][,4])
			medobs[i]<-median(rept[[i]])
			sd[i]<- sd(rept[[i]])
			se[i]<-sd[i]/sqrt(length(rept[[i]]))
		}
		matriztotal<-as.matrix(cbind(agente[[1]][,1:2],round(log(agente[[1]][,2]),3),agente[[1]][,3:4],sampli[[1]],agente[[1]][,3]-sampli[[1]],round(agente[[1]][,4]/sampli[[1]],3),round(tran[[1]]*100/sampli[[1]],3)))
		for (i in 2:nag) matriztotal<-rbind(matriztotal,as.matrix(cbind(agente[[i]][,1:2],round(log(agente[[i]][,2]),3),agente[[i]][,3:4],sampli[[i]],agente[[i]][,3]-sampli[[i]],round(agente[[i]][,4]/sampli[[i]],3),round(tran[[i]]*100/sampli[[i]],3))))
		ntabla<-unique(matriztotal[,7])
	}
	if(opc==2){          ########********************datos en forma de conteo
		datas<-dia<-cont<-puntos<-poracum<-por<-fre<-tran <- n <- temp <- list(1)
		muestra<-posi<-exis.val<-c(1);posi[1]=NA
		tip<-1:length(estadios)
                num=tip[estadios==est]   ## nuevo
		#r2=1
		#for (r in 1:length(datos)) {
		#	dprueba<-list(datos[[r]][,num])
		#	if(length(dprueba[[1]][is.na(dprueba[[1]])])==length(dprueba[[1]])){ posi[r2]=r;r2=r2+1 }
		#}
		#if(length(posi[is.na(posi)])!=length(posi)){ datos=datos[-posi];tp=tp[-posi]}
		#for (i in 1:length(datos)) {
		for (i in 1:length(datos)) {  # por cada temperatura

			datas[i]<-list(datos[[i]][,num])
			vect1=datas[[i]]; if(length(vect1)==length(vect1[is.na(vect1)])){exis.val[i]<-FALSE}else{exis.val[i]<-TRUE}
			#if(length(datas[[i]][is.na(datas[[i]])])==length(datas[[i]]))
			#{
			# datas[i]<-dia[i]<-cont[i]<-puntos[i]<-poracum[i]<-por[i]<-fre[i]<-tran[i] <- n[i] <- temp[i] <- NA
			# next
			#}

			#fre[i] <- list(rep(0, max(datas[[i]],na.rm=T)+1))    #### Aqui se crea la dimension
			#fre[i] <- list(seq(0,max(datas[[i]],na.rm=T)+intervalo,intervalo))
			fre[i] <- list(seq(0,max(c(datas[[i]],0),na.rm=T)+intervalo,intervalo))  ### se crea el vector hasta el maximo valir de los dias

			for (j in 1:length(fre[[i]])) fre[[i]][j] <- length(subset(datas[[i]],datas[[i]] == intervalo*(j-1))) ## me indica cuantos insectos vivieron hasta tales dias
			#por[i] <- list(rep(0, max(datas[[i]],na.rm=T)+1))
			#por[i] <- list(seq(0,max(datas[[i]],na.rm=T)+intervalo,intervalo))
			por[i] <- list(seq(0,max(c(datas[[i]],0),na.rm=T)+intervalo,intervalo))

			#for (j in 1:length(fre[[i]])) por[[i]][j] <- (fre[[i]][j]/(sum(fre[[i]])+0.000000000001))
			sfre=sum(fre[[i]]);if(sfre!=0){por[[i]] <- fre[[i]]/sfre}else{por[[i]] <- fre[[i]]}

			#poracum[i] <- list(rep(0, max(datas[[i]],na.rm=T) - 1))
			#poracum[i] <- list(seq(0,max(datas[[i]],na.rm=T)-intervalo,intervalo))
			#for (j in 1:length(fre[[i]]) - 1) {
                        #       if(j==0){k1=1}else{k1=j}   ##### Cambio cuando haya j=0 ya que hay problemas de dimensiones
			#	if (j == 1) poracum[[i]][j] <- por[[i]][j]
			#	if (j != 1) poracum[[i]][j] <- por[[i]][j] + poracum[[i]][k1 - 1]
			#}
			poracum[[i]] <- cumsum(por[[i]][-length(por[[i]])])

			puntos[i] <- list(as.character(poracum[[i]]))
			tran[i] <- list(round(as.numeric(puntos[[i]]), 2))
			#cont[i] <- list(rep(0,  max(datas[[i]],na.rm=T)+1))
			#cont[i] <- list(seq(0,max(datas[[i]],na.rm=T)+intervalo,intervalo))

			cont[i] <- list(seq(0,max(c(datas[[i]],0),na.rm=T)+intervalo,intervalo))


			for (j in 1:length(poracum[[i]])) {
			#	cont[[i]][j] <- j
				cont[[i]][j] <- intervalo*(j-1)
				dia[[i]] <- subset(cont[[i]], cont[[i]] != 0)
			}
			n[i] <- list(rep(length(datas[[i]]), length(tran[[i]])))
			temp[i] <- list(rep(tp[i], length(tran[[i]])))
			#sampli[[i]]<-tran[[i]][length(tran[[i]])]
		}
		cap=rep(TRUE,length(tp))  ## modificacion
		matriz <- cbind(NA, NA, NA, NA, NA,NA)  ## modificacion
		for (i in 1:length(datos))
		{
		 if(length(dia[[i]][is.na(dia[[i]])])==length(dia[[i]]))
		 {cap[i]=FALSE
		  next
		 }
		 matriz <- rbind(matriz, cbind(temp[[i]],dia[[i]], log(dia[[i]]), n[[i]], round(c(tran[[i]][-1],1.0)*n[[i]]),n[[i]]))
		}
		matriz1 <- as.data.frame(matriz[-1,])
                if(dim(matriz1)[1]==0){stop("Hay valores vacios en los conjuntos de datos de temperaturas")}

		nag <- length(levels(factor(matriz1[, 1])))
		not <- levels(factor(matriz1[, 1]))
		matriz2 <- agente <- list(1)
		for (i in 1:nag) {
			if(sum(fre[[i]])!=0)
			{
			 ind3=1:length(fre[[i]][-1]);ind3=ind3[fre[[i]][-1]!=0]
			 agente[i] <- list(subset(matriz1, matriz1[, 1] == not[i]))

			 #matriz2[i] <- list(agente[[i]][!duplicated(agente[[i]][, 5]), ])
			 matriz2[i] <- list(agente[[i]][ind3, ]) ### esta operacion define que valores estan incluidos en cada temperatura

			 #matriz2[[i]][,2][1]<-matriz2[[i]][,2][2]-1
			 matriz2[[i]]<-rbind(agente[[i]][min(ind3)-1,],matriz2[[i]])  ## aumentando la fila anterior
			}else{ind3=1; agente[i] <- list(subset(matriz1, matriz1[, 1] == not[i])); matriz2[[i]]<-agente[[i]]}
		}
		matri <- matriz2[[1]]
		for (i in 2:nag) matri <- rbind(matri, matriz2[[i]])
		medobs<-sd<-se<-c(0)
		rept<-estdis<-list(1)
		for (i in 1:nag){
			estdis[[i]]<-subset(matri, matri[, 1] == not[i])
			#for(j in nrow(estdis[[i]]):1) if(j==1)estdis[[i]][,6][j]<-0 else estdis[[i]][,6][j]<-estdis[[i]][,5][j]-estdis[[i]][,5][j-1]
			for(j in nrow(estdis[[i]]):1){ if(j==1){if(estdis[[i]][,2][j]!=1){estdis[[i]][,6][j]<-0}else{estdis[[i]][,6][j]<-estdis[[i]][,5][j]} }else{ estdis[[i]][,6][j]<-estdis[[i]][,5][j]-estdis[[i]][,5][j-1]}}
			rept[[i]]<-rep(estdis[[i]][,2],estdis[[i]][,6])
			if(length(rept[[i]])==0){medobs[i]<-0;sd[i]=NA;se[i]=NA}else{
			medobs[i]<-median(rept[[i]]);sd[i]<- sd(rept[[i]])
			se[i]<-sd[i]/sqrt(length(rept[[i]]))}
		}
		estdism<-estdis[[1]]
		for(i in 2:nag) estdism<-rbind(estdism,cbind(estdis[[i]]))
		matriztotal<-data.frame(estdism[,1],estdism[,2],estdism[,3],estdism[,4],estdism[,6],estdism[,6]/estdism[,4],estdism[,5])
	}
	Temperature <- as.factor(matri[,1])
	muestra<-matri[,6]
	Slope <- matri[,3]
	Develomep <- matri[,5]
	aicp <- suppressWarnings(warning(AIC(modelop<-glm(cbind(Develomep,muestra-Develomep) ~ Temperature - 1 + Slope, family = binomial(probit)))))
	aicl <- suppressWarnings(warning(AIC(modelol<-glm(cbind(Develomep,muestra-Develomep) ~ Temperature - 1 + Slope, family = binomial(logit)))))
	aicc <- suppressWarnings(warning(AIC(modeloc<-glm(cbind(Develomep,muestra-Develomep) ~ Temperature - 1 + Slope, family = binomial(cloglog)))))
	rp<-1-sum(((matri[,5]/matri[,6])-round(fitted.values(modelop),4))^2)/sum(((matri[,5]/matri[,6])-mean(matri[,5]/matri[,6]))^2)
	r_ajusp<- 1 - ((length(matri[,2]) - 1) / (length(matri[,2]) - length(tp)+1)) * (1-rp)
	rl<-1-sum(((matri[,5]/matri[,6])-round(fitted.values(modelol),4))^2)/sum(((matri[,5]/matri[,6])-mean(matri[,5]/matri[,6]))^2)
	r_ajusl<- 1 - ((length(matri[,2]) - 1) / (length(matri[,2]) - length(tp)+1)) * (1-rl)
	rc<-1-sum(((matri[,5]/matri[,6])-round(fitted.values(modeloc),4))^2)/sum(((matri[,5]/matri[,6])-mean(matri[,5]/matri[,6]))^2)
	r_ajusc<- 1 - ((length(matri[,2]) - 1) / (length(matri[,2]) - length(tp)+1)) * (1-rc)
	mscp<-log(sum((round(fitted.values(modelop),4)-mean(round(fitted.values(modelop),4)))^2)/sum(((matri[,5]/matri[,6])-mean((matri[,5]/matri[,6])))^2))-2*(length(tp)+1)/length(matri[,2])
	mscl<-log(sum((round(fitted.values(modelol),4)-mean(round(fitted.values(modelol),4)))^2)/sum(((matri[,5]/matri[,6])-mean((matri[,5]/matri[,6])))^2))-2*(length(tp)+1)/length(matri[,2])
	mscc<-log(sum((round(fitted.values(modeloc),4)-mean(round(fitted.values(modeloc),4)))^2)/sum(((matri[,5]/matri[,6])-mean((matri[,5]/matri[,6])))^2))-2*(length(tp)+1)/length(matri[,2])
	model<-c("probit","logit","cloglog")
	aic<-as.numeric(c(aicp,aicl,aicc))
	maic<-min(aic)
	#for(m in 1:3) {
	#	if(aic[m]==maic)     {
	#		mod<-model[m]
	#		aicv<-aic[m]
	#	}
	#}
	mod=modelo
	aicv=aic[model==modelo]
	if (mod == "probit") {
		familia <- family(glm(cbind(Develomep,muestra-Develomep) ~ Temperature - 1 + Slope, family = binomial(probit)))
		coeficientes <- coef(summary(glm(cbind(Develomep,muestra-Develomep) ~ Temperature - 1 + Slope, family = binomial(probit))))
		parametros <- coef(glm(cbind(Develomep,muestra-Develomep) ~ Temperature - 1 + Slope, family = binomial(probit)))
		modelo<-glm(cbind(Develomep,muestra-Develomep) ~ Temperature - 1 + Slope, family = binomial(probit))
		matri[,7]<-100*round(fitted.values(modelop),4)
		medobsp<-sdp<-sep<-c(0)
		reptp<-estdisp<-list(1)
		for (i in 1:nag){
			estdisp[[i]]<-subset(matri, matri[, 1] == not[i])
			for(j in nrow(estdisp[[i]]):1) if(j==1)estdisp[[i]][,5][j]<-0 else estdisp[[i]][,5][j]<-estdisp[[i]][,7][j]-estdisp[[i]][,7][j-1]
			reptp[[i]]<-rep(estdisp[[i]][,2],estdisp[[i]][,5])

			if(length(reptp[[i]])==0){medobs[i]<-0;sdp[i]=NA;sep[i]=NA}else{
			medobsp[i]<-median(reptp[[i]])
			sdp[i]<- sd(reptp[[i]])
			sep[i]<-sdp[i]/sqrt(length(reptp[[i]]))}
		}

	}
	if (mod=="logit") {
		familia <- family(glm(cbind(Develomep,muestra-Develomep) ~ Temperature - 1 + Slope, family = binomial(logit)))
		coeficientes <- coef(summary(glm(cbind(Develomep,muestra-Develomep) ~ Temperature - 1 + Slope, family = binomial(logit))))
		parametros <- coef(glm(cbind(Develomep,muestra-Develomep) ~ Temperature - 1 + Slope, family = binomial(logit)))
		modelo<-glm(cbind(Develomep,muestra-Develomep) ~ Temperature -1 + Slope, family = binomial(probit))
		matri[,7]<-100*round(fitted.values(modelol),4)
		medobsp<-sdp<-sep<-c(0)
		reptp<-estdisp<-list(1)
		for (i in 1:nag){
			estdisp[[i]]<-subset(matri, matri[, 1] == not[i])
			for(j in nrow(estdisp[[i]]):1) if(j==1)estdisp[[i]][,5][j]<-0 else estdisp[[i]][,5][j]<-estdisp[[i]][,7][j]-estdisp[[i]][,7][j-1]
			reptp[[i]]<-rep(estdisp[[i]][,2],estdisp[[i]][,5])

			if(length(reptp[[i]])==0){medobs[i]<-0;sdp[i]=NA;sep[i]=NA}else{
			medobsp[i]<-median(reptp[[i]])
			sdp[i]<- sd(reptp[[i]])
			sep[i]<-sdp[i]/sqrt(length(reptp[[i]]))}
		}

	}
	if (mod=="cloglog") {
		familia <- family(glm(cbind(Develomep,muestra-Develomep) ~ Temperature - 1 + Slope, family = binomial(cloglog)))
		coeficientes <- coef(summary(glm(cbind(Develomep,muestra-Develomep) ~ Temperature - 1 + Slope, family = binomial(cloglog))))  ###### el cambio es aqui
		parametros <- coef(glm(cbind(Develomep,muestra-Develomep) ~ Temperature - 1 + Slope, family = binomial(cloglog)))
		modelo<-glm(cbind(Develomep,muestra-Develomep) ~ Temperature - 1 + Slope, family = binomial(probit))
		matri[,7]<-100*round(fitted.values(modeloc),4)
		medobsp<-sdp<-sep<-c(0)
		reptp<-estdisp<-list(1)
		for (i in 1:nag){
			estdisp[[i]]<-subset(matri, matri[, 1] == not[i])
			for(j in nrow(estdisp[[i]]):1) if(j==1)estdisp[[i]][,5][j]<-0 else estdisp[[i]][,5][j]<-estdisp[[i]][,7][j]-estdisp[[i]][,7][j-1]
			reptp[[i]]<-rep(estdisp[[i]][,2],estdisp[[i]][,5])

			if(length(reptp[[i]])==0){medobs[i]<-0;sdp[i]=NA;sep[i]=NA}else{
			medobsp[i]<-median(reptp[[i]])
			sdp[i]<- sd(reptp[[i]])
			sep[i]<-sdp[i]/sqrt(length(reptp[[i]]))}
		}
	}
	parametros <- as.vector(parametros)
	slope <- parametros[nag + 1]
	intercepto <- parametros[1:nag]
	if(mod=="probit") cua1<-as.data.frame(t(as.matrix(c("probit",paste(round(slope,3),"(","±",round(as.numeric(coeficientes[,2][nag+1]),3),")"),round(as.numeric(coeficientes[,3][nag+1]),3),round(as.numeric(coeficientes[,4][nag+1]),3),
										round(deviance(modelop),3),round(aic[1],3),round(mscp,3),round(rp,3),round(r_ajusp,3)))))
	if(mod=="logit") cua1<-as.data.frame(t(as.matrix(c("logit",paste(round(slope,3),"(","±",round(as.numeric(coeficientes[,2][nag+1]),3),")"),round(as.numeric(coeficientes[,3][nag+1]),3),round(as.numeric(coeficientes[,4][nag+1]),3),
										round(deviance(modelol),3),round(aic[2],3),round(mscl,3),round(rl,3),round(r_ajusl,3)))))
	if(mod=="cloglog") cua1<-as.data.frame(t(as.matrix(c("cloglog",paste(round(slope,3),"(","±",round(as.numeric(coeficientes[,2][nag+1]),3),")"),round(as.numeric(coeficientes[,3][nag+1]),3),round(as.numeric(coeficientes[,4][nag+1]),3),
										round(deviance(modeloc),3),round(aic[3],3),round(mscc,3),round(rc,3),round(r_ajusc,3)))))
	if(mod=="logit" || mod=="probit") p50 <- (-intercepto)/slope else p50 <- (-(-log(-log(0.5)))-intercepto)/slope   ##  ERROR
	cat("\nESTIMATION OF PARAMETERS")
	print(familia)
	pval1=rep(0.0001,nrow(coeficientes))
	coeficientes[coeficientes[,4]<0.0001,4]=pval1[coeficientes[,4]<0.0001]
	coeficientes=cbind(round(coeficientes[,1:3],3),"Pr(>|z|)"=coeficientes[,4])
	print(coeficientes)

#  n<-length(Develomep); k<-length(parametros);  if(n/k<40) {aic<-aic+2*k*(k+1)/(n-k-1); nombre="AICc"} else nombre="AIC"  #Nuevo DM
	crite<-data.frame(Deviance=c(deviance(modelop),deviance(modelol),deviance(modeloc)), AIC=round(aic,3),MSC=round(c(mscp,mscl,mscc),3),R_Squared=round(c(rp,rl,rc),3),Adj_R_squared=round(c(r_ajusp,r_ajusl,r_ajusc),3))
#  colnames(crite)[2]<-nombre # Nuevo DM
	rownames(crite)<-model
	cat("\n")
	cat("\nSELECTION CRITERIA\n")
	print(crite)
	cat("\n")
	li<-ls<-c(1)
	for(i in 1:length(tp[cap])){
		li[i] <- p50[i] - qt(0.975, length(matri[, 3]) - length(intercepto) -
						1) * (1/slope)/sqrt(length(intercepto) + 1)
		ls[i] <- p50[i] + qt(0.975, length(matri[, 3]) - length(intercepto) -
						1) * (1/slope)/sqrt(length(intercepto) + 1)}
	tabmedi1<-data.frame(Temperature=tp[cap],Log_median=round(p50,3),Log_lower=round(li,3),Log_upper=round(ls,3),Days=round(exp(p50),3),Lower=round(exp(li),3),Upper=round(exp(ls),3),SD=round(sdp,3),SE=round(sep,3))  ####### aqui se caeee!!!!!!!!!!!! ptm
	tabmedi2<-data.frame(Temperature=tp[cap],Days_obs=medobs,SD_obs=round(sd,3),SE_obs=round(se,3))
	if(opc==2)colnames(matriztotal)<-c("T","Day's","Ln(Day's)","Sample","Dev_Sen","fr_Dev","Fr_Dev (%)")
	if(opc==1)colnames(matriztotal)<-c("T","Day's","Ln(Day's)","Sample","Dev_Sen","Sample_Dev","Sample_mort","fr_Dev","Fr_Dev (%)")
	cat("\nESTIMATED\n")
	print(tabmedi1)
	cat("\nOBSERVED\n")
	print(tabmedi2)
	cat("\n")
	cat("\nTABLE OF FREQUENCY\n")
	print(matriztotal)
	salidas <- list(Slope = slope, Temperaturas = as.numeric(not),
			Interceptos = intercepto, matriz = matri, parametros = parametros,model=mod,tabest=tabmedi1,tabobs=tabmedi2,mattotal=matriztotal,cua=cua1,exis.val=exis.val, Std.Error=coeficientes[,2]) # Agregue "Std.Error=coeficientes[,2] DM
	return(salidas)
}

###############################
plot.devath <- function (matri, parametros,test,corrx,lgx,lgy,tam,labx = NULL, laby = NULL,titulo=NULL,grises=FALSE,intervalo=intervalo,exis.val)
{
  
  slope <- parametros[-1:-(length(parametros) - 1)]
  intercepto <- parametros[-(length(parametros))]
  nag <- length(levels(factor(matri[, 1])))
  not <- levels(factor(matri[, 1]))
  p50 <- li <- ls <- rep(0, nag)
  #for (i in 1:nag) agente <- subset(matri, matri[, 1] == not[i])
  agente <- subset(matri, matri[, 1] == not[nag])
  
  #corrx[2]<-5 # 5
  lgx<- 0 #4.4
  lgy<-100
  laby<-"accumulated frequency"
  labx<-"log-development time (log-days)"
  MARGEN=c(4.2, 5, 4, 0.5)
  #par(cex=tam)
  par(mar=MARGEN,family="serif",font=1,cex.axis=1.8,cex.lab=1.8)
  plot(agente[, 3], agente[, 5]*100/agente[,6], axes=F,xaxt = "n",xlim =corrx , ylim = c(0, 100),
       frame = F, ylab = laby, xlab = labx,main=titulo,col="transparent")
  #plot(agente[, 3], agente[, 5]*100/agente[,6], axes=F,xaxt = "n",xlim =corrx , ylim = c(0, 100),
  #     frame = F, ylab = laby, xlab = labx,main=titulo, cex.lab=0.6)
  
  #axis(1, xaxp=c(corrx,5))
  axis(1, seq(corrx[1],corrx[2],0.5),cex.axis=1.6)
  axis(2,seq(0,100,20),paste(seq(0,100,20),"%",sep=""),las=2,tck=-0.025, line=-1.1,cex.axis=1.6)
  
  library("RColorBrewer")
  #co<-c(colorRampPalette(brewer.pal(9,"Greys"))(11)[-1],colorRampPalette(brewer.pal(9,"YlOrBr"))(11)[-1],brewer.pal(8,"Reds")[-1],
  #colorRampPalette(brewer.pal(9,"Greens"))(10)[-1],brewer.pal(4,"Blues")[-1],brewer.pal(4,"Purples")[-1])
  #pcho<-c(1:25,1:17)
  co<-c(colorRampPalette(brewer.pal(9,"YlOrBr"))(11)[-c(1:3)],brewer.pal(8,"Reds")[-c(1:2)],
        colorRampPalette(brewer.pal(9,"Greens"))(10)[-c(1:4)],brewer.pal(4,"Blues")[-1],brewer.pal(4,"Purples")[-1],colorRampPalette(brewer.pal(9,"Greys"))(11)[-c(1:4)])
  co<-co[round(seq(1,33,length=nag))]
  pcho<-c(1:25,1:17)
  
  cexLEG<-0.8 # editar cexLEG<-1
  
  #pcho <- 15:28
  #pcho <- c(18,15,17,0,1,2,5,6,8) #nuevo cambio de puntos puedo aumentar mas (comprobar)
  if(grises==TRUE)
  {co <- c("gray5","gray10","gray15","gray20","gray25","gray30","gray35","gray40","gray45","gray50","gray55","gray60","gray65","gray70")
  }else{co <- co}
  for (i in 1:nag) { ## bucle por temperatura
    agente <- subset(matri, matri[, 1] == not[i])
    
    if (test == "probit") {
      psp <- seq(0.01, 0.99, 0.01)
      pr <- qnorm(psp)
      p <- pnorm(pr)
      w <- (pr - intercepto[i])/slope
      lines(w, p*100, col = co[i], lwd = 1.8)
      #p50[i] <- (-intercepto[i])/slope
      if(agente[1,2]==intervalo & agente[1,5]==agente[1,4] & exis.val[i]==0){p50[i]=100}else{p50[i] <- (-intercepto[i])/slope}
    }else{
      ps <- seq(0, 10, 0.001)
      if (test == "cloglog") {
        p <- 1 - exp(-exp((intercepto[i] + slope * ps)))
        #p50[i] <- ( -(-log(-log(0.5))) - intercepto[i])/slope
        if( agente[1,2]==intervalo & agente[1,5]==agente[1,4] & exis.val[i]==0){p50[i]=100}else{p50[i] <- ( -(-log(-log(0.5))) - intercepto[i])/slope}
      }
      if (test == "logit") {
        p <- 1/(1 + exp(-(intercepto[i] + slope * ps)))
        #p50[i] <- ((log((1 - 0.5)/0.5)) - intercepto[i])/slope
        if( agente[1,2]==intervalo & agente[1,5]==agente[1,4] & exis.val[i]==0){p50[i]=100}else{p50[i] <- ((log((1 - 0.5)/0.5)) - intercepto[i])/slope}
      }
      lines(ps, p*100, col = co[i], lwd = 1.8)
    }
    points(agente[, 3], agente[, 5]*100/agente[,6], pch = pcho[i], cex = 1.5, col = co[i])
    minx <- min(matri[, 3])
    li[i] <- p50[i] - qt(0.975, length(matri[, 3]) - length(intercepto) -
                           length(slope)) * (1/slope)/sqrt(length(intercepto) + length(slope))
    ls[i] <- p50[i] + qt(0.975, length(matri[, 3]) - length(intercepto) -
                           length(slope)) * (1/slope)/sqrt(length(intercepto) + length(slope))
    
    lines(rbind(c(minx - 0.8, 50), c(p50[i], 50), c(p50[i], -100)), col = "gray50")
    lines(rbind(c(ls[i], 50), c(li[i], 50)), col = "black", lwd = 2)
    lines(rbind(c(ls[i], -0.03*100), c(li[i], -0.03*100)), col = "black", lwd = 2)
    lines(rbind(c(li[i], -0.03*100 - 0.02*100), c(li[i], -0.03*100 + 0.02*100)), col = "black", lwd = 2)
    lines(rbind(c(ls[i], -0.03*100 - 0.02*100), c(ls[i], -0.03*100 + 0.02*100)), col = "black", lwd = 2)
    lines(rbind(c(li[i], 0.5*100 - 0.02*100), c(li[i], 0.5*100 + 0.02*100)), col = "black", lwd = 2)
    lines(rbind(c(ls[i], 0.5*100 - 0.02*100), c(ls[i], 0.5*100 + 0.02*100)), col = "black", lwd = 2)
  }
  legend(lgx,lgy, not, pch = pcho, col = co, lty = 1.5, cex=cexLEG)#mean(matri[,5])#max(matri[,3])
  mat<-data.frame(x = (as.numeric(not)), y = (1/exp(p50)), Lower = (1/exp(ls)), Upper = (1/exp(li)))
  datao<-data.frame(x = (as.numeric(not)), y = c((1/exp(p50)),(1/exp(ls)),(1/exp(li))))
  #if( agente[1,2]==intervalo & length(p50[p50==100])>=1 ){p50[p50==100]=-Inf}  ## el valor de cero puede afectar algunas funciones cuando se hace el ajuste
  p50[p50==100]=-Inf
  
  ############## DM: Inicio: Lo siguiente se agrego para usarlo en los graficos de Development Time y omitir las T extremas donde no se desarrolla el insecto, de modo que el grafico represente mejor la realidad
  vv=exis.val==1; mat<-mat[vv,]; #p50<-p50[vv]
  vvv=c(vv,vv,vv); datao<-datao[vvv,];
  ############## DM: Fin
  
  salidas <- list(mat=mat,shapMi=datao,median=p50)
  return(salidas)
}

##############################################
# Generador de los valores Medios ponderados #
##############################################

dev.tim<-function(matri,tp)
{
 tmed<-c(1);n1<-c(1);acum1<-c(1)
 for(i in 1:length(tp))
 {
  mat1=matri[matri[,1]==tp[i],];n1[i]=nrow(mat1)
  #if(n1[i]!=1){acum1<-c(acum1,cumsum(mat1[,5]))}else{acum1<-c(acum1,mat1[1,4])}
  acum1<-c(acum1,cumsum(mat1[,5]))
  nn=rep(as.numeric(mat1[,2]),mat1[,5])
  if(length(nn) != 0){tmed[i]=median(nn)}else{tmed[i]=0}
 }
 acum1=acum1[-1]
 T=rep(tp,n1)
 return(list(acum1=acum1,T=T,tmed=tmed,n1=n1))
}

#DM
################################
# Arreglo para datos de cohorte sin machos  
################################

addmale<-function(datapre,tp,estadios)
{
Male<-data.frame(cbind(tp[1],1,sum(subset(datapre[[length(estadios)-2]][,4],datapre[[length(estadios)-2]][,1]==tp[1])),sum(subset(datapre[[length(estadios)-2]][,4],datapre[[length(estadios)-2]][,1]==tp[1]))))
for (i in 2:length(tp))
{
  Male<-rbind(Male,data.frame(cbind(tp[i],1,sum(subset(datapre[[length(estadios)-2]][,4],datapre[[length(estadios)-2]][,1]==tp[i])),sum(subset(datapre[[length(estadios)-2]][,4],datapre[[length(estadios)-2]][,1]==tp[i])))))
  }
return(Male)
}