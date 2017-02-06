descum<-function(vec){dc<-c(1);dc[1]=vec[1];for(i in 2:length(vec)){dc[i]=vec[i]-vec[i-1]};return(dc)}
####################
## Promedio especial

media<-function(vec){if(length(vec[is.na(vec)])==length(vec)){med=0}else{med= mean(vec,na.rm=T)};return(med)}

##########################################################################

oviposicion<-function(opc,datos,colum,mediana,estadios,tp=NULL)  ## cambio
{
	if(opc==1){## ultimo cambio
		data<-datos ## ultimo cambio
		not <- levels(factor(data[,1]))
		agente <- list(1)
		for (i in 1:length(not)) agente[i] <- list(subset(data, data[, 1] == not[i]))
		datos<-agente
		tp<-as.numeric(not)
		ovi<-sumf<-dias<-frecf<-list(1)
		for(i in 1:length(datos))
		{
			if(dim(datos[[i]])[1] == 1 & is.na(datos[[1]][1,colum]) ){ovi[[i]]=NA;sumf[[i]]=0;dias[[i]]=0;frecf[[i]]=0}else{
				ovi[i]<-list(t(datos[[i]][,colum:ncol(datos[[i]])])) ## seleccionan todas las columnas 
				sumf[i]<-list(apply(ovi[[i]],1,media))  ## promedio de huevos por cada dia
				dias[[i]]<-1:nrow(ovi[[i]])
				frecf[[i]]=cumsum(sumf[[i]]/sum(sumf[[i]],na.rm=T)) ## frecuencia relativa acumulada promedio
			}
		}
	}else
        {
		ovi<-sumf<-dias<-frecf<-list(1)
		for(i in 1:length(datos))
	        {
			if(ncol(datos[[i]]) == length(estadios)){ovi[[i]]=NA;sumf[[i]]=0;dias[[i]]=0;frecf[[i]]=0}else{
				ovi[i]<-list(t(datos[[i]][,colum:ncol(datos[[i]])])) ## seleccionan todas las columnas 
				sumf[i]<-list(apply(ovi[[i]],1,media))  ## promedio de huevos por cada dia
				dias[[i]]<-1:nrow(ovi[[i]])
				frecf[[i]]=cumsum(sumf[[i]]/sum(sumf[[i]],na.rm=T)) ## frecuencia relativa acumulada promedio
			}
		}
	}
	female<-cbind(T=tp[1],x=dias[[1]]/exp(mediana[1]),y=frecf[[1]])
	for(i in 2:length(datos))female<-rbind(female,cbind(T=tp[i],x=dias[[i]]/exp(mediana[i]),y=frecf[[i]]))
	rownames(female)=1:nrow(female)
	female<-data.frame(female)
	female[is.nan(female[,2]),2]<-0
	return(list(femal=female))
}

###################################################################################################################################################
pruebaovi<-function(modelm, ovifemal,inim,corrx,corry,mini,maxi,labx,laby,titulo)
{
	x<-ovifemal[,2]
	y<-ovifemal[,3]
	if(modelm==1)  form<-expression((1-exp(-(a*xlinea+b*xlinea^2+c*xlinea^3))))
	if(modelm==2)  form<-expression(pgamma(xlinea,a,b))
	if(modelm==3)  form<-expression(1/(1+exp(a+b*xlinea)))
	if(modelm==4)  form<-expression(1-exp(-a*xlinea^b))
	if(modelm==5)  form<-expression(1-exp(-((xlinea-a)/n)^b))
	if(modelm==6)  form<-expression(pweibull(xlinea,a,b))

	plot(ovifemal[,2],ovifemal[,3]*100,frame=F,pch=19,col=4,cex=1.3,xlim=corrx,ylim=corry,axes=F,xaxt = "n",xlab=labx,ylab=laby,main=titulo)
	axis(1, xaxp=c(corrx,5))
	axis(2,las=2)
	ini<-as.list(inim)
	for (i in names(ini))
	{
		temp <- ini[[i]]
		storage.mode(temp) <- "double"
		assign(i, temp)
	}
	xlinea<-seq(mini,maxi,length=1000)
	ylinea<-eval(form)
	lines(xlinea,ylinea*100,lwd=2,col=2)
	salidas<-list(ini=as.data.frame(ini))
	return(salidas)
}
###################################################################################################################################################
ovi_func <- function (modelm, ovifemal, alg, inim,pesos,weights)
{
	x<-ovifemal[,2]
	y<-ovifemal[,3]
	if(alg=="Newton")
	{
		if(modelm==1)
		{
			form<-y~(1-exp(-(a*x+b*x^2+c*x^3)))
			frm<-"O(E) = 1-exp(-aE-bE^2-cE^3))"
			if(pesos=="WLS") modelo<-nls(form,inim,data=ovifemal,weights=weights)
			if(pesos=="LS") modelo<-nls(form,inim,data=ovifemal)
			coefi<-as.data.frame(t(coef(modelo)))
			return(list(estimados=coefi,f=form,frm=frm))
		}
		if(modelm==2)
		{
			form<-y~pgamma(x,a,b)
			frm<-"O(E) = pgamma(E,a,b)"
			if(pesos=="WLS")  modelo<-nls(form,inim,data=ovifemal,weights=weights)
			if(pesos=="LS") modelo<-nls(form,inim,data=ovifemal)
			coefi<-as.data.frame(t(coef(modelo)))
			return(list(estimados=coefi,f=form,frm=frm))
		}
		if(modelm==3)
		{
			form<-y~1/(1+exp(a+b*x))
			frm<- "O(E) = 1/(1+exp(a+bE))"
			if(pesos=="WLS")modelo<-nls(form,inim,data=ovifemal,weights=weights)
			if(pesos=="LS")modelo<-nls(form,inim,data=ovifemal)
			coefi<-as.data.frame(t(coef(modelo)))
			return(list(estimados=coefi,frm=frm,f=form))
		}
		if(modelm==4) #para datos alrederor de cero, como heydi
		{
			form<-y~1-exp(-a*x^b)
			frm= "O(E) = 1-exp(-aE^b)"
			if(pesos=="WLS")  modelo<-nls(form,inim,data=ovifemal,weights=weights)
			if(pesos=="LS")  modelo<-nls(form,inim,data=ovifemal)
			coefi<-as.data.frame(t(coef(modelo)))
			return(list(estimados=coefi,frm=frm,f=form))
		}
		if(modelm==5) #para datos alrederor de cero, como heydi
		{
			form<-y~1-exp(-((x-a)/n)^b)
			frm= "O(E) = 1-exp(-((x-a)/n)^b)"
			if(pesos=="WLS")  modelo<-nls(form,inim,data=ovifemal,weights=weights)
			if(pesos=="LS")  modelo<-nls(form,inim,data=ovifemal)
			coefi<-as.data.frame(t(coef(modelo)))
			return(list(estimados=coefi,frm=frm,f=form))
		}
		if(modelm==6)
		{
			form<-y~pweibull(x,a,b)
			frm<-"O(E) = pweibull(E,a,b)"
			if(pesos=="WLS")  modelo<-nls(form,inim,data=ovifemal,weights=weights)
			if(pesos=="LS") modelo<-nls(form,inim,data=ovifemal)
			coefi<-as.data.frame(t(coef(modelo)))
			return(list(estimados=coefi,f=form,frm=frm))
		}

	}

	if(alg=="Marquardtr")
	{
		if(modelm==1)
		{
			x<-ovifemal[,2]
			y<-ovifemal[,3]
			ini<-as.list(inim)
			ind <- as.list(ini)
			for (i in names(ind)) {
				temp <- ini[[i]]
				storage.mode(temp) <- "double"
				assign(i, temp)
			}
			f <- function(x,a,b,c)
			{
				expr <- expression(1 - exp(-(a * x + b * x^2 + c * x^3)))
				eval(expr)
			}
			j <- function(x,a,b,c)
			{
				expr <- expression(1 - exp(-(a * x + b * x^2 + c * x^3)))
				c(eval(D(expr, "a")),eval(D(expr, "b")),eval(D(expr, "c")))
			}
			fcn     <- function(ini, x, y, fcall, jcall)
				(y - do.call("fcall", c(list(x = x), as.list(ini))))
			out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
			estimate<-as.data.frame(out$par)
			frm<-"O(E) = 1 - exp(-(a.x + b.x^2 + c.x^3)))"
		}

		if(modelm==2)
		{
			x<-ovifemal[,2]
			y<-ovifemal[,3]
			ini<-as.list(inim)
			ind <- as.list(ini)
			for (i in names(ind)) {
				temp <- ini[[i]]
				storage.mode(temp) <- "double"
				assign(i, temp)
			}
			f <- function(x,a,b)
			{
				expr <- expression(pgamma(x,a,b))
				eval(expr)
			}
			j <- function(x,a,b)
			{
				expr <- expression(pgamma(x,a,b))
				c(eval(D(expr, "a")),eval(D(expr, "b")))
			}
			fcn     <- function(ini, x, y, fcall, jcall)
				(y - do.call("fcall", c(list(x = x), as.list(ini))))
			out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
			estimate<-as.data.frame(out$par)
			frm<-"O(E) = pgamma(x,a,b)"
		}

		if(modelm==3)
		{
			x<-ovifemal[,2]
			y<-ovifemal[,3]
			ini<-as.list(inim)
			ind <- as.list(ini)
			for (i in names(ind)) {
				temp <- ini[[i]]
				storage.mode(temp) <- "double"
				assign(i, temp)
			}
			f <- function(x,a,b)
			{
				expr <- expression(1/(1+exp(a+b*x)))
				eval(expr)
			}
			j <- function(x,a,b)
			{
				expr <- expression(1/(1+exp(a+b*x)))
				c(eval(D(expr, "a")),eval(D(expr, "b")))
			}
			fcn     <- function(ini, x, y, fcall, jcall)
				(y - do.call("fcall", c(list(x = x), as.list(ini))))
			out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
			estimate<-as.data.frame(out$par)
			frm<-"O(E) = 1/(1+exp(a+b.x))"
		}


		if(modelm==4)
		{
			x<-ovifemal[,2]
			y<-ovifemal[,3]
			ini<-as.list(inim)
			ind <- as.list(ini)
			for (i in names(ind)) {
				temp <- ini[[i]]
				storage.mode(temp) <- "double"
				assign(i, temp)
			}
			f <- function(x,a,b)
			{
				expr <- expression(1-exp(-a*x^b))
				eval(expr)
			}
			j <- function(x,a,b)
			{
				expr <- expression(1-exp(-a*x^b))
				c(eval(D(expr, "a")),eval(D(expr, "b")))
			}
			fcn     <- function(ini, x, y, fcall, jcall)
				(y - do.call("fcall", c(list(x = x), as.list(ini))))
			out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
			estimate<-as.data.frame(out$par)
			frm<-"O(E) = 1-exp(-a.x^b)"
		}


		if(modelm==5)
		{
			x<-ovifemal[,2]
			y<-ovifemal[,3]
			ini<-as.list(inim)
			ind <- as.list(ini)
			for (i in names(ind)) {
				temp <- ini[[i]]
				storage.mode(temp) <- "double"
				assign(i, temp)
			}
			f <- function(x,a,b,n)
			{
				expr <- expression(1-exp(-((x-a)/n)^b))
				eval(expr)
			}
			j <- function(x,a,b,n)
			{
				expr <- expression(1-exp(-((x-a)/n)^b))
				c(eval(D(expr, "a")),eval(D(expr, "b")),eval(D(expr, "n")))
			}
			fcn     <- function(ini, x, y, fcall, jcall)
				(y - do.call("fcall", c(list(x = x), as.list(ini))))
			out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
			estimate<-as.data.frame(out$par)
			frm<-"O(E) = 1-exp(-((x-a)/n)^b)"
		}

		if(modelm==6)
		{
			x<-ovifemal[,2]
			y<-ovifemal[,3]
			ini<-as.list(inim)
			ind <- as.list(ini)
			for (i in names(ind)) {
				temp <- ini[[i]]
				storage.mode(temp) <- "double"
				assign(i, temp)
			}
			f <- function(x,a,b)
			{
				expr <- expression(pweibull(x,a,b))
				eval(expr)
			}
			j <- function(x,a,b)
			{
				expr <- expression(pweibull(x,a,b))
				c(eval(D(expr, "a")),eval(D(expr, "b")))
			}
			fcn     <- function(ini, x, y, fcall, jcall)
				(y - do.call("fcall", c(list(x = x), as.list(ini))))
			out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
			estimate<-as.data.frame(out$par)
			frm<-"O(E) = pweibull(x,a,b)"
		}

		stderro<-diag(ginv(out$hessian))
		return(list(estimados=estimate,f=f,frm=frm,stdovi=stderro))
	}
}

###################################################################################################################################################
coef_ovi<-function(modelm,estimor,stdovi,g,frm,ovifemal,alg,pesos,weights)
{
	x<-ovifemal[,2]
	y<-ovifemal[,3]
	ind <- as.list(estimor)
	for (i in names(ind))
	{
		temp <- estimor[[i]]
		storage.mode(temp) <- "double"
		assign(i, temp)
	}
	if(alg=="Newton")
	{
			if(pesos=="WLS") modelo<-nls(g,estimor,data=ovifemal,weights=weights)
			if(pesos=="LS") modelo<-nls(g,estimor,data=ovifemal)
			art=0.00000000000000000000000000001
			sqe<-sum((residuals(modelo))^2)+art
			qq<-summary(modelo)
			tabla<-qq$"parameters"
			stderror<-tabla[,2]  #Agrege DM
      aic<-AIC(modelo) #Quite ,k=length(estimor)  DM
			if(pesos=="WLS") r<-1-sum(residuals(modelo)^2)/sum(weights*(y-mean(y))^2)
			if(pesos=="LS") r<-1-sum(residuals(modelo)^2)/sum((y-mean(y))^2)
			r_ajus<- 1 - ((length(x) - 1) / (length(x) - length(estimor))) * (1-r)
			yl<-predict(modelo)
			if(pesos=="WLS") MSC<-log(sum(weights*(yl-mean(yl))^2)/sum(weights*(y-mean(y))^2))-2*length(estimor)/length(x)
			if(pesos=="LS") MSC<-log(sum((yl-mean(yl))^2)/sum((y-mean(y))^2))-2*length(estimor)/length(x)
			anva.1 <- c(length(coef(modelo))-1,length(x)-length(coef(modelo)),length(x)-1)
			anva.2 <- c((sum(y^2)-length(y)*mean(y)^2)-sqe,sqe,sum(y^2)-length(y)*mean(y)^2)
			anva.3 <- c(anva.2[1]/anva.1[1],anva.2[2]/anva.1[2],NA)
			anva.4 <- c(anva.3[1]/anva.3[2],NA,NA)
			anva.5 <- c(1-pf(anva.4[1],anva.1[1],anva.1[2]),NA,NA)
			anva   <- cbind(anva.1,round(anva.2,4),round(anva.3,4),round(anva.4,4),round(anva.5,4))
			rownames(anva) <- c("Model","Error","Total")
			colnames(anva) <- c("DF","SS","MS","Fc","p")
			cat("\nNONLINEAR REGRESSION MODEL\n")
			cat("\nMethod:", alg)
			cat("\nFormula: ", frm,"\n")
			cat("\nParameters\n")
			print(round(qq$"parameters",5))
			cat("\nAnalysis of Variance\n")
			print(anva,na.print = "")
			#n<-length(x); k<-length(estimor);  if(n/k<40) {aic<-aic+2*k*(k+1)/(n-k-1); nombre="AICc"} else nombre="AIC"  #Nuevo DM
      FRAMESEL<-data.frame(R2=round(r,4),R2_Adj=round(r_ajus,4),SSR=round(sqe,4),AIC=round(aic,4),MSC=round(MSC,4))
			#colnames(FRAMESEL)[4]<-nombre # Nuevo DM
      rownames(FRAMESEL)<-c("")
			cat("\nSelection criteria")
			print(t(FRAMESEL))
			para<-t(matrix(paste(round(estimor,5),"(","±",round(as.numeric(tabla[,2]),5),")")))
			colnames(para)<-names(estimor)
			param<-data.frame(frm,para,d.f.=paste(anva.1[1],",",anva.1[2]),F=round(anva.4[1],3),P.value=round(anva.5[1],3),FRAMESEL)

	}
	if(alg=="Marquardtr")
	{

		if(modelm==1)   {
			yl<-g(x,a,b,c)
			g<-y~ 1 - exp(-(a * x + b * x^2 + c * x^3))
		}
		if(modelm==2)   {
			yl<-g(x,a,b)
			g<-y~ pgamma(x,a,b)
		}
		if(modelm==3)   {
			yl<-g(x,a,b)
			g<-y~ 1/(1+exp(a+b*x))
		}
		if(modelm==4)   {
			yl<-g(x,a,b)
			g<-y~ 1-exp(-a*x^b)
		}
		if(modelm==5)   {
			yl<-g(x,a,b,n)
			g<-y~1-exp(-((x-a)/n)^b)
		}
		if(modelm==6)   {
			yl<-g(x,a,b)
			g<-y~ pweibull(x,a,b)
		}

		sqe <- sum((y-yl)^2)
		var <- (sum((y-yl)^2)/(length(x) - length(estimor)))
		stderror<-sqrt(var*stdovi)
		names(stderror)<-names(estimor) # Agregue DM
    tvalues<-estimor/stderror
		tvalta<-qt(0.025,length(x) - length(estimor))
		pvalues<-1-pt(as.numeric(tvalues),length(x) - length(estimor))
		r<-1-sqe/sum((y-mean(y))^2)
		r_ajus<- 1 - ((length(x) - 1) / (length(x) - length(estimor))) * (1-r)
		#AC<-length(x)*log(sqe/length(x))+2*length(estimor) #Agregue /length(x) DM
		AC<-length(x)*(log(2*pi*sqe/length(x))+1)+2*(length(estimor)+1) #Cambio segun Nonlinear Regression with R, pag 105. DM
    MSC<-log(sum((yl-mean(yl))^2)/sum((y-mean(y))^2))-2*length(estimor)/length(x)
		anva.1 <- c(length(estimor)-1,length(x)-length(estimor),length(x)-1)
		anva.2 <- c((sum(y^2)-length(y)*mean(y)^2)-sqe,sqe,sum(y^2)-length(y)*mean(y)^2)
		anva.3 <- c(anva.2[1]/anva.1[1],anva.2[2]/anva.1[2],NA)
		anva.4 <- c(anva.3[1]/anva.3[2],NA,NA)
		anva.5 <- c(1-pf(anva.4[1],anva.1[1],anva.1[2]),NA,NA)
		anva   <- cbind(anva.1,round(anva.2,4),round(anva.3,4),round(anva.4,4),round(anva.5,4))
		rownames(anva) <- c("Model","Error","Total")
		colnames(anva) <- c("DF","SS","MS","Fc","p")
		cat("\nNONLINEAR REGRESSION MODEL\n")
		cat("\nMethod:", alg)
		cat("\nFormula: ", frm,"\n")
		cat("\nParameters\n")
		estshap2<-data.frame(matrix(round(estimor,4)),round(stderror,5),formatC(round(as.numeric(tvalues),5)),round(pvalues,5))
		colnames(estshap2)<-c("Estimate","Std.Error","t value","Pr(>|t|)")
		rownames(estshap2)<-c(colnames(estimor))
		print(estshap2)
		cat("\nAnalysis of Variance\n")
		print(anva,na.print = "")
		#n<-length(x); k<-length(estimor); if(n/k<40) {AC<-AC+2*k*(k+1)/(n-k-1); nombre="AICc"} else nombre="AIC" #Nuevo DM
    FRAMESEL<-data.frame(R2=round(r,4),R2_Adj=round(r_ajus,4),SSR=round(sqe,4),AIC=round(AC,4),MSC=round(MSC,4))
		#colnames(FRAMESEL)[4]<-nombre #Nuevo DM
    rownames(FRAMESEL)<-c("")
		cat("\nSelection criteria")
		print(t(FRAMESEL))
		para<-t(matrix(paste(round(estimor,5),"(","±",round(stderror,5),")")))
		colnames(para)<-names(estimor)
		param<-data.frame(frm,para,d.f.=paste(anva.1[1],",",anva.1[2]),F=round(anva.4[1],3),P.value=round(anva.5[1],3),FRAMESEL)

	}
	df<-length(x)-length(ind)
	sdli<-sqrt(sqe/df)
	qto<-qt(0.025,df)
	salidas<-list(sdli=sdli,qto=qto,parovi=param,gg=g,frames=FRAMESEL,yl=yl, Std.Error=stderror)      # Agregue "Std.Error=stderror" DM)
	return(salidas)
}
###################################################################################################################################################
grafovi <- function(modelm, estimor, g, ovifemal, corrx=NULL,  mini, maxi, ldx,ldy,sdli,qto,limit,tam=c(1.8,1.2),labx=NULL, laby=NULL, titulo=NULL,grises=FALSE,xi){
        if(grises==TRUE)
	{colo = c("gray5","gray10","gray15","gray20","gray25","gray30","gray35","gray40","gray45","gray50","gray55","gray60","gray65")
	ccol=c("gray20","gray30")
	}else{colo <- 15:28;ccol=c(4,1)}
	x<-ovifemal[,2]
	y<-ovifemal[,3]
	estshap<-as.list(estimor)
	finalshap<-estshap
	for (i in names(finalshap)) {
		temp <- finalshap[[i]]
		storage.mode(temp) <- "double"
		assign(i, temp)
	}
	xl<-seq(mini,maxi,length=1000)
	if(modelm==1){  f<-function(x){1-exp(-(a*x+b*x^2+c*x^3))}}
	if(modelm==2){  f<-function(x){pgamma(x,a,b)}}
	if(modelm==3){  f<-function(x){1/(1+exp(a+b*x))}}
	if(modelm==4){  f<-function(x){1-exp(-a*x^b)}}
	if(modelm==5){  f<-function(x){1-exp(-((x-a)/n)^b)}}
	if(modelm==6){  f<-function(x){pweibull(x,a,b)}}
	yl<-f(xl)

	nag <- length(levels(factor(ovifemal[, 1])))
	not <- levels(factor(ovifemal[, 1]))
	agente <- subset(ovifemal, ovifemal[, 1] == not[1])
	
	
	scaleX<-0.5
	# ldx<-2.5
	# ldy<-70
	labx<-"normalized female age (days/median survival time)"
	laby<-"cumulative oviposition ratio (%)"
	corrx2=seq(corrx[1],corrx[2],scaleX) ## cambio
	MARGEN=c(5.2, 5, 5.5, 0.5)
	

	par(mar=MARGEN,family="serif",font=1,cex.axis=tam[1],cex.lab=tam[1])
  if(is.null(corrx)){corrx=c(0,max(ovifemal[,2])+max(ovifemal[,2])*0.1)} ## nuevo
	#corrx2=seq(corrx[1],corrx[2]+0.3,0.3) ## cambio
	plot(agente[, 2], agente[, 3]*100,frame=F,ylab=laby,
			xlab=labx,pch=19,cex=1.3,col=ccol[2],xlim=corrx,ylim=c(0,100),axes=F,xaxt = "n", main=titulo) ## 1
	axis(1, corrx2, cex.axis=1.6)  ## cambio
	axis(2,las=2, tck=-0.025, line=-1, cex.axis=1.6)
	lines(c(-1,xi),c(50,50),lwd=1,lty=2)
	lines(c(xi,xi),c(-10,50),lwd=1,lty=2)

	linf<-yl+sdli*qto
	lsup<-yl-sdli*qto
	if(limit=="yes"){
		lines(xl, linf*100, lwd=1,col=ccol[1],lty=2)  ## 4
		lines(xl, lsup*100, lwd=1,col=ccol[1],lty=2)
    	} ## 4
    	pcho=c(18,15,17,0,1,2,5,6,8)
	for(i in 1:nag){
		agente <- subset(ovifemal, ovifemal[, 1] == not[i])
		points(agente[, 2], agente[, 3]*100,pch=pcho[i],cex=1.3,col=colo[i]) ## colo
	}
	legend("bottomright",cex=tam[2],not,pch=pcho[1:nag],col=colo,lty = 3) ## colo
	lines(xl, yl*100, lwd=2,col=ccol[2]) ## 1
}


################################################
agenorm<-function(estimor,modelm,xini=0.1,ni=20)
{
 	ind <- as.list(estimor)
	for (i in names(ind))
	{
		temp <- estimor[[i]]
		storage.mode(temp) <- "double"
		assign(i, temp)
	}

	if(modelm==1){  f<-function(x){1-exp(-(a*x+b*x^2+c*x^3))-0.5}; dx1x <- deriv(~ 1-exp(-(a*x+b*x^2+c*x^3))-0.5, "x") }
	if(modelm==2){  f<-function(p){qgamma(p,a,b)}}
	if(modelm==3){  f<-function(x){1/(1+exp(a+b*x))-0.5}; dx1x <- deriv(~ 1/(1+exp(a+b*x))-0.5, "x") }
	if(modelm==4){  f<-function(x){1-exp(-a*x^b)-0.5}; dx1x <- deriv(~ 1-exp(-a*x^b)-0.5, "x") }
	if(modelm==5){  f<-function(x){1-exp(-((x-a)/n)^b)-0.5}; dx1x <- deriv(~ 1-exp(-((x-a)/n)^b)-0.5, "x") }
	if(modelm==6){  f<-function(p){qweibull(p,a,b)}}
	
	## Aplicamos Newton Raphson
        if(modelm==2 || modelm==6){x=f(0.5)}else
        {
		x=xini
		for(j in 1:ni)
		{
			de = eval(dx1x)
			xd = attr(de,"gradient")[,1]
			x  = x - f(x)/xd	
		}
	}

	return(list(xi=x))
}
################################################
################################
# Recalculo de valores iniciales
################################

recalc<-function(ini,niv=niv)
{
 vec<-list(1)
 for(i in 1:length(ini))
 {
   vec[[i]]<-ini[[i]]+ini[[i]]*runif(1,-(niv),niv)
 }
 names(vec)=names(ini)
 return(vec)
}

########################################3
###############################################################
# Funcion de que obtiene el tiempo de oviposicion de una hembra

tovip<-function(v1)
{
                v1=na.omit(v1);v1=v1[v1!=0];tovip=length(v1)
 return(tovip)
}

####################################################
# Genera los conteos para cada tiempo de oviposición

ovip.time<-function(dat)
{
 dat <- data.frame(matrix(as.numeric(as.matrix(dat)),nrow(dat),ncol(dat)))
 tmps=unique(c(dat[,1]))

 mat2=data.frame(Temp=NA,Dia=NA,Frec=NA,Frec.A=NA,N=NA,Median.W.Days=NA);n<-c(1)
 for(i in 1:length(tmps))
 {
                dat1=dat[dat[,1]==tmps[i],-1];tovc=apply(dat1,1,tovip);mat1=table(tovc)
                if(length(mat1) > 1){
                  if(names(mat1[1])=="0"){n[i]=nrow(dat1)-mat1[1];mat1=mat1[-1]}else{n[i]=nrow(dat1)} ## no considero los ceros
                  mmd=median(rep(as.numeric(names(mat1)),mat1))
                  tempo=data.frame(Temp=tmps[i],Dia=names(mat1),Frec=c(mat1),Frec.A=cumsum(mat1),N=n[i],Median.W.Days=mmd)
                  mat2=rbind(mat2,tempo)
                }
                
#                dat1=dat[dat[,1]==tmps[i],-1];tovc=apply(dat1,1,tovip);mat1=table(tovc)
#                if(names(mat1[1])=="0"){n[i]=nrow(dat1)-mat1[1];mat1=mat1[-1]}else{n[i]=nrow(dat1)} ## no considero los ceros
#                mmd=median(rep(as.numeric(names(mat1)),mat1))
#                tempo=data.frame(Temp=tmps[i],Dia=names(mat1),Frec=mat1,Frec.A=cumsum(mat1),N=n[i],Median.W.Days=mmd)
#                mat2=rbind(mat2,tempo)
 }
 mat2=mat2[-1,]
 mat2[,2]=as.numeric(mat2[,2]);mat2[,1]=as.factor(mat2[,1])

 colnames(mat2)=c("T","Day's","Freq","Cum.Freq","Sample","Median.W.Days")
 cat("\n")
 cat("\nTABLE OF FREQUENCY\n")
 print(mat2)

 return(list(mat2=mat2,tmps=tmps))
}

################################
rm.dead<-function(datos,muerte)
{
	la <- as.matrix(datos);n1=nrow(datos);n2=ncol(datos)
	la[la==muerte] <- NA
	la=as.numeric(la)
	datos=matrix(la,n1,n2)
	return(datos)
}

