summ<-function(vec){if(length(vec[is.na(vec)])==length(vec)){suma=NA}else(suma=sum(vec,na.rm=T));return(suma)}

taza<-function(opc,datos,tp,colum){## Todo esto funciona si la muerte esta codificada como "NA"
        if(opc==1){## hace que se creen sub grupos por cada temperatura
                data2<-datos
                not <- levels(factor(data2[,1]))
                agente <- list(1)
                for (i in 1:length(not)) agente[i] <- list(subset(data2, data2[, 1] == not[i]))
                datos<-agente
                tp<-as.numeric(not)
        }
        ovi<-list(1)
        if(opc==1){colum=2;for(i in 1:length(datos)) ovi[i]<-list(t(datos[[i]][,colum:ncol(datos[[i]])]))}else{
          for(i in 1:length(datos)) if(ncol(datos[[i]])>colum){ovi[i]<-list(t(datos[[i]][,(colum+1):ncol(datos[[i]])]))}else{ovi[i]<-list(t(matrix(NA,nrow(datos[[i]]),2)))}}

        sftab<-list(1)
        for(k in 1:length(datos)) sftab[k]<-list(apply(ovi[[k]],2,summ)) ## me deberia dar el numero total de huevos por cada hembra por temperatura
        novi<-sdovi<-meaovi<-c(1)
        todos<-list(1)

        for(i in 1:length(tp)){
                novi[i]<-length(na.omit(sftab[[i]]))
                if(novi[i]==0){todos[[i]]<-NA}else{todos[[i]]<-na.omit(sftab[[i]])}
                meaovi[i]<-mean(na.omit(sftab[[i]]))
                sdovi[i]<-sd(na.omit(sftab[[i]]))
        }
        meaovi[is.nan(meaovi)]=0

        todo<-cbind(tp[1],todos[[1]])
        for(i in 2:length(tp)) todo<-rbind(todo,cbind(tp[i],todos[[i]]))
        todo<-data.frame(todo)
        colnames(todo)<-c("x","y")
        tablaovi<-data.frame(x=tp,y=meaovi,sd=sdovi,n=novi)
        female<-data.frame(x=tp,y=meaovi)
        salida<-list(femal=tablaovi,todo=todo)
        return(salida)
}


taza_ant<-function(opc,datos,tp,colum)
{## Todo esto funciona si la muerte esta codificada como "NA"
        if(opc==1){## hace que se creen sub grupos por cada temperatura 
                data2<-datos
                not <- levels(factor(data2[,1]))
                agente <- list(1)
                for (i in 1:length(not)) agente[i] <- list(subset(data2, data2[, 1] == not[i]))
                datos<-agente
                tp<-as.numeric(not)
        }
        ovi<-list(1)
        if(opc==1){for(i in 1:length(datos)) ovi[i]<-list(t(datos[[i]][,colum:ncol(datos[[i]])]))}else{
        for(i in 1:length(datos)) ovi[i]<-list(t(datos[[i]][,(colum+1):ncol(datos[[i]])]))}

        sftab<-list(1)
        for(k in 1:length(datos)) sftab[k]<-list(apply(ovi[[k]],2,summ)) ## me deberia dar el numero total de huevos por cada hembra por temperatura
        novi<-sdovi<-meaovi<-c(1)
        todos<-list(1)

        for(i in 1:length(tp)){
                novi[i]<-length(na.omit(sftab[[i]]))
                if(novi[i]==0){todos[[i]]<-NA}else{todos[[i]]<-na.omit(sftab[[i]])}
                meaovi[i]<-mean(na.omit(sftab[[i]]))
                sdovi[i]<-sd(na.omit(sftab[[i]]))
        }
        meaovi[is.nan(meaovi)]=0

        todo<-cbind(tp[1],todos[[1]])
        for(i in 2:length(tp)) todo<-rbind(todo,cbind(tp[i],todos[[i]]))
        todo<-data.frame(todo)
        colnames(todo)<-c("x","y")
        tablaovi<-data.frame(x=tp,y=meaovi,sd=sdovi,n=novi)
        female<-data.frame(x=tp,y=meaovi)
        salida<-list(femal=tablaovi,todo=todo)
        return(salida)
}

####################################################
pr6mortal<-function(modelm,datm)
{
	x<-datm[,1]
	y<-datm[,2]
	if(modelm==1)  modelo<-lm(y~x+I(x^2))
	if(modelm==2)  modelo<-lm(y~x+I(sqrt(x)))
	if(modelm==3)  modelo<-lm(y~x+I(1/sqrt(x)))
	if(modelm==4)  modelo<-lm(y~x+I(1/x^2))
	if(modelm==5)  modelo<-lm(y~x+I(1/x))
	if(modelm==6)  modelo<-lm(y~x+I(log(x)))
	coefi<-round(as.numeric(coef(modelo)),3)
	for(i in 1:length(coefi))if(coefi[i]==0) coefi[i]<-0.001
	coefi<-c("a"=coefi[3],"b"=coefi[2],"c"=coefi[1])
	for (i in names(coefi))
	{
		temp <- coefi[[i]]
		storage.mode(temp) <- "double"
		assign(i, temp)
	}
	salidas<-list(ini=coefi)
	return(salidas)
}
###################################################################################################################################################
pruebamortal<-function(proc,modelm,datm,inim,corrx,corry,mini,maxi,labx, laby,titulo)
{
	if(modelm==1)  form<-expression(a*xlinea^2+b*xlinea+c)
	if(modelm==2)  form<-expression(a*sqrt(xlinea)+b*xlinea+c)
	if(modelm==3)  form<-expression(a*(1/sqrt(xlinea))+b*xlinea+c)
	if(modelm==4)  form<-expression(a*(1/xlinea^2)+b*xlinea+c)
	if(modelm==5)  form<-expression(a*(1/xlinea)+b*xlinea+c)
	if(modelm==6)  form<-expression(b*xlinea+a*log(xlinea)+c)
	if(modelm==7)  form<-expression(1/(1+a*exp(-b*((xlinea-c)/d)^2))) # log normal, 4 parameters
	if(modelm==8) form<-expression((a*exp(-b*((xlinea-xo)/c)^2)))
	if(modelm==9)  form<-expression(y0 + a * exp(-0.5 * ((xlinea-x0)/b)^2))
	if(modelm==10) form<-expression(y0 + a * exp(-0.5 * (log(abs(xlinea/x0))/b)^2))

	# nuevos modelos
	if(modelm==11) form<-expression(b1+b2*xlinea+b3*xlinea^d)
	if(modelm==12) form<-expression(exp(b1+b2*xlinea+b3*xlinea^2))
	if(modelm==13) form<-expression(1-(b4/(1+b5*exp(b1+b2*xlinea+b3*xlinea^2))))
	if(modelm==14) form<-expression(exp(b1+b2*xlinea+b3*sqrt(xlinea)))
	if(modelm==15) form<-expression(1-(b4/(1+b5*exp(b1+b2*xlinea+b3*sqrt(xlinea)))))
	if(modelm==16) form<-expression(exp(b1+b2*xlinea+b3*(1/sqrt(xlinea))))
	if(modelm==17) form<-expression(1-(b4/(1+b5*exp(b1+b2*xlinea+b3*(1/sqrt(xlinea))))))
	if(modelm==18) form<-expression(exp(b1+b2*xlinea+b3*(1/xlinea)))
	if(modelm==19) form<-expression(1-(b4/(1+b5*exp(b1+b2*xlinea+b3*(1/xlinea)))))
	if(modelm==20) form<-expression(exp(b1+b2*xlinea+b3*xlinea^d))
	if(modelm==21) form<-expression(1-(b4/(1+b5*exp(b1+b2*xlinea+b3*xlinea^d))))
	if(modelm==22) form<-expression(exp(b1+b2*xlinea+b3*log(xlinea)))
	if(modelm==23) form<-expression(1-(b4/(1+b5*exp(b1+b2*xlinea+b3*log(xlinea)))))
	if(modelm==24) form<-expression(1-rm*exp((-0.5)*(-(xlinea-Topt)/Troh)^2))
	if(modelm==25) form<-expression(1-rm*exp((-0.5)*(-(log(xlinea)-log(Topt))/Troh)^2))
#	if(modelm==26) form<-expression(1 - 1/(exp((1+exp(-(xlinea-Topt)/B))*(1+exp(-(Topt-xlinea)/B))*H)))
#	if(modelm==27) form<-expression(1 - 1/(exp((1+exp(-(xlinea-Tl)/B))*(1+exp(-(Th-xlinea)/B))*H)))
#	if(modelm==28) form<-expression(1 - 1/(exp((1+exp(-(xlinea-Topt)/Bl))*(1+exp(-(Topt-xlinea)/Bh))*H)))
#	if(modelm==29) form<-expression(1 - 1/(exp((1+exp(-(xlinea-Tl)/Bl))*(1+exp(-(Th-xlinea)/Bh))*H)))
	if(modelm==30) form<-expression(1 - H/(exp(1+exp(-(xlinea-Topt)/B))*(1+exp(-(Topt-xlinea)/B))))
	if(modelm==31) form<-expression(1 - H/(exp(1+exp(-(xlinea-Tl)/B))*(1+exp(-(Th-xlinea)/B))))
	if(modelm==32) form<-expression(1 - H/(exp(1+exp(-(xlinea-Topt)/Bl))*(1+exp(-(Topt-xlinea)/Bh))))
	if(modelm==33) form<-expression(1 - H/(exp(1+exp(-(xlinea-Tl)/Bl))*(1+exp(-(Th-xlinea)/Bh))))
	if(modelm==34) form<-expression(Bm + ((1-1/(1+exp((Hl/1.987)*((1/Tl)-(1/(xlinea+273.15))))+exp((Hh/1.987)*((1/Th)-(1/(xlinea+273.15))))))*(1-Bm))) #DM: se agrego 2 parentesis "Bm + (...)*"
#	if(modelm==35) form<-expression((1 - exp(-(exp(a1+b1*xlinea)))) + (1 - exp(-(exp(a2+b2*xlinea)))))
#	if(modelm==36) form<-expression((w-xlinea)^(-1))
	if(modelm==37) form<-expression(a1*exp(b1*xlinea) + a2*exp(b2*xlinea))
	if(modelm==38) form<-expression(a1*exp(b1*xlinea) + a2*exp(b2*xlinea)+c1)
	if(modelm==39) form<-expression(a*(abs(xlinea-b))^nn)
	if(modelm==40) form<-expression(a*xlinea*(xlinea-To)*(Tl-xlinea)^(1/d))
	if(modelm==41) form<-expression(exp(a*xlinea*(xlinea-To)*(Tl-xlinea)^(1/d)))
	if(modelm==42) form<-expression(a*((xlinea-Tmin)^n)*(Tmax-xlinea)^m)
	if(modelm==43) form<-expression(1/((Dmin/2) * (exp(k*(xlinea-Tp)) + exp(-(xlinea-Tp)*lamb))))
	if(modelm==44) form<-expression(a*(1-exp(-(xlinea-Tl)/B))*(1-exp(-(Th-xlinea)/B)))
	if(modelm==45) form<-expression(exp(a*(1-exp(-(xlinea-Tl)/Bl))*(1-exp(-(Th-xlinea)/Bh))))

	# fin de nuevos modelos


	if(proc=="mortal") plot(datm[,1],datm[,2]*100,frame=F,pch=19,col=4,cex=1.3,xlim=corrx,ylim=corry,axes=F,xaxt = "n",xlab=labx,ylab=laby,main=titulo)
	if(proc=="taza") plot(datm[,1],datm[,2],frame=F,pch=19,col=4,cex=1.3,xlim=corrx,ylim=corry,axes=F,xaxt = "n",xlab=labx,ylab=laby,main=titulo)
	axis(1, xaxp=c(corrx,5))
	axis(2,las=2)
	ini<-as.list(inim)
	for (i in names(ini))
	{
		temp <- ini[[i]]
		storage.mode(temp) <- "double"
		assign(i, temp)
	}
	xlinea<-seq(mini,maxi,length=100)
	ylinea<-eval(form)
	if(proc=="mortal") lines(xlinea,ylinea*100,lwd=2,col=2)
	if(proc=="taza") lines(xlinea,ylinea,lwd=2,col=2)
	salidas<-list(ini=as.data.frame(ini))
	return(salidas)
}

###################################################################################################################################################
dead_func <- function (proc,modelm, datm, alg, inim, pesos,weights){
	x<-datm[,1]
	y<-datm[,2]
	if(alg=="Newton"){
		if(modelm==1)
		{
			form<-y~a*x^2+b*x+c
			if(proc=="mortal") frm<- "m(T) = aT²+bT+c" else frm<- "f(T) = aT²+bT+c"
		}
		if(modelm==2)
		{
			form<-y~a*sqrt(x)+b*x+c
			if(proc=="mortal") frm<- "m(T) = a·sqrt(T)+bT+c" else frm<- "f(T) = a·sqrt(T)+bT+c"
		}
		if(modelm==3)
		{
			form<-y~a*(1/sqrt(x))+b*x+c
			if(proc=="mortal") frm<- "m(T) = a/sqrt(T)+bT+c" else frm<- "f(T) = a/sqrt(T)+bT+c"
		}
		if(modelm==4)
		{
			form<-y~a*(1/x^2)+b*x+c
			if(proc=="mortal") frm<- "m(T) = a/T²+bT+c" else frm<- "f(T) = a/T²+bT+c"
		}
		if(modelm==5)
		{
			form<-y~a*(1/x)+b*x+c
			if(proc=="mortal") frm<- "m(T) = a/T+bT+c" else frm<- "f(T) = a/T+bT+c"
		}
		if(modelm==6)
		{
			form<-y~b*x+a*log(x)+c
			if(proc=="mortal") frm<- "m(T) = a·ln(T)+bT+c" else frm<- "f(T) = a·ln(T)+bT+c"
		}
		if(pesos=="WLS") modelo<-nls(form,inim,data=datm,weights=weights)
		if(pesos=="LS") modelo<-nls(form,inim,data=datm)
		coefi<-coef(modelo)
		return(list(estimados=coefi,f=form,modelo=modelo,ecua=frm))
	}
	if(alg=="Marquardtr")
	{
		ini<-as.list(inim)
		for (i in names(ini)) {
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		if(modelm==9)
		{
			f <- function(x,a,b,y0,x0)
			{
				expr <- expression(y0 + a * exp(-0.5 * ((x-x0)/b)^2))
				eval(expr)
			}
			j <- function(x,a,b,y0,x0)
			{
				expr <- expression(y0 + a * exp(-0.5 * ((x-x0)/b)^2))
				c(eval(D(expr, "a")),eval(D(expr, "b")), eval(D(expr, "x0")), eval(D(expr, "y0")))
			}
			g<-y~y0 + a * exp(-0.5 * ((x-x0)/b)^2)
			if(proc=="mortal") frm<- "m(T) = y0+a·exp(-0.5((x-x0)/b)²)" else frm<- "f(T) = y0+a·exp(-0.5((x-x0)/b)²)"
		}
		if(modelm==7)
		{
			f <- function(x,a,b,c,d)
			{
				expr <- expression(1/(1+a*exp(-b*((x-c)/d)^2)))
				eval(expr)
			}
			j <- function(x,a,b,c,d)
			{
				expr <- expression(1/(1+a*exp(-b*((x-c)/d)^2)))
				c(eval(D(expr, "a")),eval(D(expr, "b")), eval(D(expr, "c")), eval(D(expr, "d")))
			}
			g<-y~1/(1+a*exp(-b*((x-c)/d)^2))
			if(proc=="mortal") frm<- "m(T) = 1/(1+a·exp(-b((x-c)/d)²))" else frm<- "f(T) = 1/(1+a·exp(-b((x-c)/d)²))"
		}
		if(modelm==8)
		{
			f <- function(x,a,b,c,xo)
			{
				expr <- expression((a*exp(-b*((x-xo)/c)^2)))
				eval(expr)
			}
			j <- function(x,a,b,c,xo)
			{
				expr <- expression((a*exp(-b*((x-xo)/c)^2)))
				c(eval(D(expr, "a")),eval(D(expr, "b")), eval(D(expr, "c")), eval(D(expr, "xo")))
			}
			g<-y~a*exp(-b*((x-xo)/c)^2)
			if(proc=="mortal") frm<- "m(T) = a·exp(-b((x-xo)/c)²)" else frm<- "f(T) = a·exp(-b((x-xo)/c)²)"
		}
		if(modelm==10){
			f <- function(x,y0,a,b,x0)
			{
				expr <- expression(y0 + a * exp(-0.5 * (log(abs(x/x0))/b)^2))
				eval(expr)
			}
			j <- function(x,y0,a,b,x0)
			{
				expr <- expression(y0 + a * exp(-0.5 * (log(abs(x/x0))/b)^2))
				c(eval(D(expr, "y0")),eval(D(expr, "a")), eval(D(expr, "b")), eval(D(expr, "x0")))
			}
			g<-y~y0 + a * exp(-0.5 * (log(abs(x/x0))/b)^2)
			if(proc=="mortal") frm<- "m(T) = y0+a·exp(-0.5(log(abs(x/x0))/b)²)" else frm<- "f(T) = y0+a·exp(-0.5(log(abs(x/x0))/b)²)"
		}

		if(modelm==11){
			f <- function(x,b1,b2,b3,d)
			{
				expr <- expression(b1+b2*x+b3*x^d)
				eval(expr)
			}
			j <- function(x,b1,b2,b3,d)
			{
				expr <- expression(b1+b2*x+b3*x^d)
				c(eval(D(expr, "b1")),eval(D(expr, "b2")), eval(D(expr, "b3")), eval(D(expr, "d")))
			}
			g<-y ~ b1+b2*x+b3*x^d
			if(proc=="mortal") frm<- "m(T) = b1+b2.x+b3.x^d " else frm<- "f(T) = b1+b2.x+b3.x^d"
		}


		if(modelm==12){
			f <- function(x,b1,b2,b3)
			{
				expr <- expression(exp(b1+b2*x+b3*x^2))
				eval(expr)
			}
			j <- function(x,b1,b2,b3)
			{
				expr <- expression(exp(b1+b2*x+b3*x^2))
				c(eval(D(expr, "b1")),eval(D(expr, "b2")), eval(D(expr, "b3")))
			}
			g<-y ~ exp(b1+b2*x+b3*x^2)
			if(proc=="mortal") frm<- "m(T) = exp(b1+b2.x+b3.x²) " else frm<- "f(T) = exp(b1+b2.x+b3.x²)"
		}

		if(modelm==13){
			f <- function(x,b1,b2,b3,b4,b5)
			{
				expr <- expression(1-(b4/(1+b5*exp(b1+b2*x+b3*x^2))))
				eval(expr)
			}
			j <- function(x,b1,b2,b3,b4,b5)
			{
				expr <- expression(1-(b4/(1+b5*exp(b1+b2*x+b3*x^2))))
				c(eval(D(expr, "b1")),eval(D(expr, "b2")), eval(D(expr, "b3")), eval(D(expr, "b4")), eval(D(expr, "b5")))
			}
			g<-y ~ 1-(b4/(1+b5*exp(b1+b2*x+b3*x^2)))
			if(proc=="mortal") frm<- "m(T) = 1-(b4/(1+b5.exp(b1+b2.x+b3.x²)))" else frm<- "f(T) = 1-(b4/(1+b5.exp(b1+b2.x+b3.x²)))"
		}

		if(modelm==14){
			f <- function(x,b1,b2,b3)
			{
				expr <- expression(exp(b1+b2*x+b3*sqrt(x)))
				eval(expr)
			}
			j <- function(x,b1,b2,b3)
			{
				expr <- expression(exp(b1+b2*x+b3*sqrt(x)))
				c(eval(D(expr, "b1")),eval(D(expr, "b2")), eval(D(expr, "b3")))
			}
			g<-y ~ exp(b1+b2*x+b3*sqrt(x))
			if(proc=="mortal") frm<- "m(T) = exp(b1+b2.x+b3.(x)^0.5)" else frm<- "f(T) = exp(b1+b2.x+b3.(x)^0.5)"
		}



		if(modelm==15){
			f <- function(x,b1,b2,b3,b4,b5)
			{
				expr <- expression(1-(b4/(1+b5*exp(b1+b2*x+b3*sqrt(x)))))
				eval(expr)
			}
			j <- function(x,b1,b2,b3,b4,b5)
			{
				expr <- expression(1-(b4/(1+b5*exp(b1+b2*x+b3*sqrt(x)))))
				c(eval(D(expr, "b1")),eval(D(expr, "b2")), eval(D(expr, "b3")), eval(D(expr, "b4")), eval(D(expr, "b5")))
			}
			g<-y ~ exp(1-(b4/(1+b5*exp(b1+b2*x+b3*sqrt(x)))))
			if(proc=="mortal") frm<- "m(T) = 1-(b4/(1+b5.exp(b1+b2.x+b3.(x)^0.5)))" else frm<- "f(T) = 1-(b4/(1+b5.exp(b1+b2.x+b3.(x)^0.5)))"
		}


		if(modelm==16){
			f <- function(x,b1,b2,b3)
			{
				expr <- expression(exp(b1+b2*x+b3*(1/sqrt(x))))
				eval(expr)
			}
			j <- function(x,b1,b2,b3)
			{
				expr <- expression(exp(b1+b2*x+b3*(1/sqrt(x))))
				c(eval(D(expr, "b1")),eval(D(expr, "b2")), eval(D(expr, "b3")))
			}
			g<-y ~ exp(b1+b2*x+b3*(1/sqrt(x)))
			if(proc=="mortal") frm<- "m(T) = exp(b1+b2.x+b3.(1/(x)^0.5))" else frm<- "f(T) = exp(b1+b2.x+b3.(1/(x)^0.5))"
		}



		if(modelm==17){
			f <- function(x,b1,b2,b3,b4,b5)
			{
				expr <- expression(1-(b4/(1+b5*exp(b1+b2*x+b3*(1/sqrt(x))))))
				eval(expr)
			}
			j <- function(x,b1,b2,b3,b4,b5)
			{
				expr <- expression(1-(b4/(1+b5*exp(b1+b2*x+b3*(1/sqrt(x))))))
				c(eval(D(expr, "b1")),eval(D(expr, "b2")), eval(D(expr, "b3")), eval(D(expr, "b4")), eval(D(expr, "b5")))
			}
			g<-y ~ 1-(b4/(1+b5*exp(b1+b2*x+b3*(1/sqrt(x)))))
			if(proc=="mortal") frm<- "m(T) = 1-(b4/(1+b5.exp(b1+b2.x+b3.(1/(x)^0.5))))" else frm<- "f(T) = 1-(b4/(1+b5.exp(b1+b2.x+b3.(1/(x)^0.5))))"
		}

		if(modelm==18){
			f <- function(x,b1,b2,b3)
			{
				expr <- expression(exp(b1+b2*x+b3*(1/x)))
				eval(expr)
			}
			j <- function(x,b1,b2,b3)
			{
				expr <- expression(exp(b1+b2*x+b3*(1/x)))
				c(eval(D(expr, "b1")),eval(D(expr, "b2")), eval(D(expr, "b3")))
			}
			g<-y ~ exp(b1+b2*x+b3*(1/x))
			if(proc=="mortal") frm<- "m(T) = exp(b1+b2.x+b3.(1/x))" else frm<- "f(T) = exp(b1+b2.x+b3.(1/x))"
		}



		if(modelm==19){
			f <- function(x,b1,b2,b3,b4,b5)
			{
				expr <- expression(1-(b4/(1+b5*exp(b1+b2*x+b3*(1/x)))))
				eval(expr)
			}
			j <- function(x,b1,b2,b3,b4,b5)
			{
				expr <- expression(1-(b4/(1+b5*exp(b1+b2*x+b3*(1/x)))))
				c(eval(D(expr, "b1")),eval(D(expr, "b2")), eval(D(expr, "b3")), eval(D(expr, "b4")), eval(D(expr, "b5")))
			}
			g<-y ~ 1-(b4/(1+b5*exp(b1+b2*x+b3*(1/x))))
			if(proc=="mortal") frm<- "m(T) = 1-(b4/(1+b5.exp(b1+b2.x+b3.(1/x))))" else frm<- "f(T) = 1-(b4/(1+b5.exp(b1+b2.x+b3.(1/x))))"
		}


		if(modelm==20){
			f <- function(x,b1,b2,b3,d)
			{
				expr <- expression(exp(b1+b2*x+b3*x^d))
				eval(expr)
			}
			j <- function(x,b1,b2,b3,d)
			{
				expr <- expression(exp(b1+b2*x+b3*x^d))
				c(eval(D(expr, "b1")),eval(D(expr, "b2")), eval(D(expr, "b3")),eval(D(expr, "d")))
			}
			g<-y ~ exp(b1+b2*x+b3*x^d)
			if(proc=="mortal") frm<- "m(T) = exp(b1+b2.x+b3.x^d)" else frm<- "f(T) = exp(b1+b2.x+b3.x^d)"
		}

		if(modelm==21){
			f <- function(x,b1,b2,b3,b4,b5,d)
			{
				expr <- expression(1-(b4/(1+b5*exp(b1+b2*x+b3*x^d))))
				eval(expr)
			}
			j <- function(x,b1,b2,b3,b4,b5,d)
			{
				expr <- expression(1-(b4/(1+b5*exp(b1+b2*x+b3*x^d))))
				c(eval(D(expr, "b1")),eval(D(expr, "b2")), eval(D(expr, "b3")), eval(D(expr, "b4")), eval(D(expr, "b5")),eval(D(expr, "d")))
			}
			g<-y ~ 1-(b4/(1+b5*exp(b1+b2*x+b3*x^d)))
			if(proc=="mortal") frm<- "m(T) = 1-(b4/(1+b5.exp(b1+b2.x+b3.x^d)))" else frm<- "f(T) = 1-(b4/(1+b5.exp(b1+b2.x+b3.x^d)))"
		}


		if(modelm==22){
			f <- function(x,b1,b2,b3)
			{
				expr <- expression(exp(b1+b2*x+b3*log(x)))
				eval(expr)
			}
			j <- function(x,b1,b2,b3)
			{
				expr <- expression(exp(b1+b2*x+b3*log(x)))
				c(eval(D(expr, "b1")),eval(D(expr, "b2")), eval(D(expr, "b3")))
			}
			g<-y ~ exp(b1+b2*x+b3*log(x))
			if(proc=="mortal") frm<- "m(T) = exp(b1+b2.x+b3.ln(x))" else frm<- "f(T) = exp(b1+b2.x+b3.ln(x))"
		}



		if(modelm==23){
			f <- function(x,b1,b2,b3,b4,b5)
			{
				expr <- expression(1-(b4/(1+b5*exp(b1+b2*x+b3*log(x)))))
				eval(expr)
			}
			j <- function(x,b1,b2,b3,b4,b5)
			{
				expr <- expression(1-(b4/(1+b5*exp(b1+b2*x+b3*log(x)))))
				c(eval(D(expr, "b1")),eval(D(expr, "b2")), eval(D(expr, "b3")), eval(D(expr, "b4")), eval(D(expr, "b5")))
			}
			g<-y ~ 1-(b4/(1+b5*exp(b1+b2*x+b3*log(x))))
			if(proc=="mortal") frm<- "m(T) = 1-(b4/(1+b5.exp(b1+b2.x+b3.ln(x))))" else frm<- "f(T) = 1-(b4/(1+b5.exp(b1+b2.x+b3.ln(x))))"
		}



		if(modelm==24){
			f <- function(x,rm,Topt,Troh)
			{
				expr <- expression(1-rm*exp((-0.5)*(-(x-Topt)/Troh)^2))
				eval(expr)
			}
			j <- function(x,rm,Topt,Troh)
			{
				expr <- expression(1-rm*exp((-0.5)*(-(x-Topt)/Troh)^2))
				c(eval(D(expr, "rm")),eval(D(expr, "Topt")), eval(D(expr, "Troh")))
			}
			g<-y ~ 1-rm*exp((-0.5)*(-(x-Topt)/Troh)^2)
			if(proc=="mortal") frm<- "m(T) = 1-rm.exp((-0.5).(-(x-Topt)/Troh)^2)" else frm<- "f(T) = 1-rm.exp((-0.5).(-(x-Topt)/Troh)^2)"
		}


		if(modelm==25){
			f <- function(x,rm,Topt,Troh)
			{
				expr <- expression(1-rm*exp((-0.5)*(-(log(x)-log(Topt))/Troh)^2))
				eval(expr)
			}
			j <- function(x,rm,Topt,Troh)
			{
				expr <- expression(1-rm*exp((-0.5)*(-(log(x)-log(Topt))/Troh)^2))
				c(eval(D(expr, "rm")),eval(D(expr, "Topt")), eval(D(expr, "Troh")))
			}
			g<-y ~ 1-rm*exp((-0.5)*(-(log(x)-log(Topt))/Troh)^2)
			if(proc=="mortal") frm<- "m(T) = 1-rm.exp((-0.5).(-(log(x)-log(Topt))/Troh)^2)" else frm<- "f(T) = 1-rm.exp((-0.5).(-(log(x)-log(Topt))/Troh)^2)"
		}


		if(modelm==30){
			f <- function(x,Topt,B,H)
			{
				expr <- expression(1 - H/(exp(1+exp(-(x-Topt)/B))*(1+exp(-(Topt-x)/B))))
				eval(expr)
			}
			j <- function(x,Topt,B,H)
			{
				expr <- expression(1 - H/(exp(1+exp(-(x-Topt)/B))*(1+exp(-(Topt-x)/B))))
				c(eval(D(expr, "Topt")),eval(D(expr, "B")), eval(D(expr, "H")))
			}
			g<-y ~ 1 - H/(exp(1+exp(-(x-Topt)/B))*(1+exp(-(Topt-x)/B)))
			if(proc=="mortal") frm<- "m(T) = 1 - H/(exp(1+exp(-(x-Topt)/B)).(1+exp(-(Topt-x)/B)))" else frm<- "f(T) = 1 - H/(exp(1+exp(-(x-Topt)/B)).(1+exp(-(Topt-x)/B)))"
		}


		if(modelm==31){
			f <- function(x,Tl,Th,B,H)
			{
				expr <- expression(1 - H/(exp(1+exp(-(x-Tl)/B))*(1+exp(-(Th-x)/B))))
				eval(expr)
			}
			j <- function(x,Tl,Th,B,H)
			{
				expr <- expression(1 - H/(exp(1+exp(-(x-Tl)/B))*(1+exp(-(Th-x)/B))))
				c(eval(D(expr, "Tl")),eval(D(expr, "Th")), eval(D(expr, "B")), eval(D(expr, "H")))
			}
			g<-y ~ 1 - H/(exp(1+exp(-(x-Tl)/B))*(1+exp(-(Th-x)/B)))
			if(proc=="mortal") frm<- "m(T) = 1 - H/(exp(1+exp(-(x-Tl)/B))*(1+exp(-(Th-x)/B)))" else frm<- "f(T) = 1 - H/(exp(1+exp(-(x-Tl)/B))*(1+exp(-(Th-x)/B)))"
		}

		if(modelm==32){
			f <- function(x,Topt,Bl,Bh,H)
			{
				expr <- expression(1 - H/(exp(1+exp(-(x-Topt)/Bl))*(1+exp(-(Topt-x)/Bh))))
				eval(expr)
			}
			j <- function(x,Topt,Bl,Bh,H)
			{
				expr <- expression(1 - H/(exp(1+exp(-(x-Topt)/Bl))*(1+exp(-(Topt-x)/Bh))))
				c(eval(D(expr, "Topt")),eval(D(expr, "Bl")),eval(D(expr, "Bh")), eval(D(expr, "H")))
			}
			g<-y ~ 1 - H/(exp(1+exp(-(x-Topt)/Bl))*(1+exp(-(Topt-x)/Bh)))
			if(proc=="mortal") frm<- "m(T) = 1 - H/(exp(1+exp(-(x-Topt)/Bl)).(1+exp(-(Topt-x)/Bh)))" else frm<- "f(T) = 1 - H/(exp(1+exp(-(x-Topt)/Bl)).(1+exp(-(Topt-x)/Bh))"
		}


		if(modelm==33){
			f <- function(x,Tl,Th,Bl,Bh,H)
			{
				expr <- expression(1 - H/(exp(1+exp(-(x-Tl)/Bl))*(1+exp(-(Th-x)/Bh))))
				eval(expr)
			}
			j <- function(x,Tl,Th,Bl,Bh,H)
			{
				expr <- expression(1 - H/(exp(1+exp(-(x-Tl)/Bl))*(1+exp(-(Th-x)/Bh))))
				c(eval(D(expr, "Tl")),eval(D(expr, "Th")),eval(D(expr, "Bl")),eval(D(expr, "Bh")), eval(D(expr, "H")))
			}
			g<-y ~ 1 - H/(exp(1+exp(-(x-Tl)/Bl))*(1+exp(-(Th-x)/Bh)))
			if(proc=="mortal") frm<- "m(T) = 1 - H/(exp(1+exp(-(x-Tl)/Bl)).(1+exp(-(Th-x)/Bh)))" else frm<- "f(T) = 1 - H/(exp(1+exp(-(x-Tl)/Bl)).(1+exp(-(Th-x)/Bh)))"
		}



		if(modelm==34){
			f <- function(x,Hl,Tl,Hh,Th,Bm)
			{
				expr <- expression(Bm + ((1-1/(1+exp((Hl/1.987)*((1/Tl)-(1/(x+273.15))))+exp((Hh/1.987)*((1/Th)-(1/(x+273.15))))))*(1-Bm))) #DM: se agrego 2 parentesis "Bm + (...)*"
				eval(expr)
			}
			j <- function(x,Hl,Tl,Hh,Th,Bm)
			{
				expr <- expression(Bm + ((1-1/(1+exp((Hl/1.987)*((1/Tl)-(1/(x+273.15))))+exp((Hh/1.987)*((1/Th)-(1/(x+273.15)))))*(1-Bm)))) #DM: se agrego 2 parentesis "Bm + (...)*"
				c(eval(D(expr, "Hl")),eval(D(expr, "Tl")),eval(D(expr, "Hh")),eval(D(expr, "Th")), eval(D(expr, "Bm")))
			}
			g<-y ~ Bm + ((1-1/(1+exp((Hl/1.987)*((1/Tl)-(1/(x+273.15))))+exp((Hh/1.987)*((1/Th)-(1/(x+273.15))))))*(1-Bm)) #DM: se agrego 2 parentesis "Bm + (...)*"
			if(proc=="mortal") frm<- "m(T) = Bm + ((1-1/(1+exp((Hl/1.987).((1/Tl)-(1/x)))+exp((Hh/1.987)*((1/Th)-(1/x))))).(1-Bm))" else frm<- "f(T) = Bm + ((1-1/(1+exp((Hl/1.987).((1/Tl)-(1/x)))+exp((Hh/1.987)*((1/Th)-(1/x))))).(1-Bm))" #DM: se agrego 2 parentesis "Bm + (...)*"
		}


		if(modelm==37){
			f <- function(x,a1,b1,a2,b2)
			{
				expr <- expression(a1*exp(b1*x) + a2*exp(b2*x))
				eval(expr)
			}
			j <- function(x,a1,b1,a2,b2)
			{
				expr <- expression(a1*exp(b1*x) + a2*exp(b2*x))
				c(eval(D(expr, "a1")),eval(D(expr, "b1")),eval(D(expr, "a2")),eval(D(expr, "b2")))
			}
			g<-y ~ a1*exp(b1*x) + a2*exp(b2*x)
			if(proc=="mortal") frm<- "m(T) = a1.exp(b1.x) + a2.exp(b2.x)" else frm<- "f(T) = a1.exp(b1.x) + a2.exp(b2.x)"
		}

		if(modelm==38){
			f <- function(x,a1,b1,a2,b2,c1)
			{
				expr <- expression(a1*exp(b1*x) + a2*exp(b2*x) + c1)
				eval(expr)
			}
			j <- function(x,a1,b1,a2,b2,c1)
			{
				expr <- expression(a1*exp(b1*x) + a2*exp(b2*x) + c1)
				c(eval(D(expr, "a1")),eval(D(expr, "b1")),eval(D(expr, "a2")),eval(D(expr, "b2")),eval(D(expr, "c1")))
			}
			g<-y ~ a1*exp(b1*x)+c1
			if(proc=="mortal") frm<- "m(T) = a1.exp(b1.x) + a2.exp(b2.x) + c1" else frm<- "f(T) = a1.exp(b1.x) + a2.exp(b2.x) + c1"
		}


		if(modelm==39){
			f <- function(x,a,b,nn)
			{
				expr <- expression(a*(abs(x-b))^nn)
				eval(expr)
			}
			j <- function(x,a,b,nn)
			{
				expr <- expression(a*(abs(x-b))^nn)
				c(eval(D(expr, "a")),eval(D(expr, "b")),eval(D(expr, "nn")))
			}
			g<-y ~ a*(abs(x-b))^nn
			if(proc=="mortal") frm<- "m(T) = a.(abs(x-b))^nn" else frm<- "f(T) = a.(abs(x-b))^nn"
		}



		if(modelm==40){
			f <- function(x,a,To,Tl,d)
			{
				expr <- expression(a*x*(x-To)*(Tl-x)^(1/d))
				eval(expr)
			}
			j <- function(x,a,b,nn)
			{
				expr <- expression(a*x*(x-To)*(Tl-x)^(1/d))
				c(eval(D(expr, "a")),eval(D(expr, "To")),eval(D(expr, "Tl")),eval(D(expr, "d")))
			}
			g<-y ~ a*x*(x-To)*(Tl-x)^(1/d)
			if(proc=="mortal") frm<- "m(T) = a.x.(x-To).(Tl-x)^(1/d)" else frm<- "f(T) = a.x.(x-To).(Tl-x)^(1/d)"
		}


		if(modelm==41){
			f <- function(x,a,To,Tl,d)
			{
				expr <- expression(exp(a*x*(x-To)*(Tl-x)^(1/d)))
				eval(expr)
			}
			j <- function(x,a,b,nn)
			{
				expr <- expression(exp(a*x*(x-To)*(Tl-x)^(1/d)))
				c(eval(D(expr, "a")),eval(D(expr, "To")),eval(D(expr, "Tl")),eval(D(expr, "d")))
			}
			g<-y ~ exp(a*x*(x-To)*(Tl-x)^(1/d))
			if(proc=="mortal") frm<- "m(T) = exp(a.x.(x-To).(Tl-x)^(1/d))" else frm<- "f(T) = exp(a.x.(x-To).(Tl-x)^(1/d))"
		}


		if(modelm==42){
			f <- function(x,a,Tmax, Tmin,n,m)
			{
				expr <- expression(a*((x-Tmin)^n)*(Tmax-x)^m)
				eval(expr)
			}
			j <- function(x,a,Tmax, Tmin,n,m)
			{
				expr <- expression(a*((x-Tmin)^n)*(Tmax-x)^m)
				c(eval(D(expr, "a")),eval(D(expr, "Tmax")),eval(D(expr, "Tmin")),eval(D(expr, "n")),eval(D(expr, "m")))
			}
			g<-y ~ a*((x-Tmin)^n)*(Tmax-x)^m
			if(proc=="mortal") frm<- "m(T) = a.((x-Tmin)^n).(Tmax-x)^m" else frm<- "f(T) = a.((x-Tmin)^n).(Tmax-x)^m"
		}

		if(modelm==43){
			f <- function(x,Dmin,k,Tp,lamb)
			{
				expr <- expression(1/((Dmin/2) * (exp(k*(x-Tp)) + exp(-(x-Tp)*lamb))))
				eval(expr)
			}
			j <- function(x,Dmin,k,Tp,lamb)
			{
				expr <- expression(1/((Dmin/2) * (exp(k*(x-Tp)) + exp(-(x-Tp)*lamb))))
				c(eval(D(expr, "Dmin")),eval(D(expr, "k")),eval(D(expr, "Tp")),eval(D(expr, "lamb")))
			}
			g<-y ~ 1/((Dmin/2) * (exp(k*(x-Tp)) + exp(-(x-Tp)*lamb)))
			if(proc=="mortal") frm<- "m(T) = 1/((Dmin/2) . (exp(k.(x-Tp)) + exp(-(x-Tp).lamb)))" else frm<- "f(T) = 1/((Dmin/2) . (exp(k.(x-Tp)) + exp(-(x-Tp).lamb)))"
		}


		if(modelm==44){
			f <- function(x,a,Tl, Th,B)
			{
				expr <- expression(a*(1-exp(-(x-Tl)/B))*(1-exp(-(Th-x)/B)))
				eval(expr)
			}
			j <- function(x,a,Tl, Th,B)
			{
				expr <- expression(a*(1-exp(-(x-Tl)/B))*(1-exp(-(Th-x)/B)))
				c(eval(D(expr, "a")),eval(D(expr, "Tl")),eval(D(expr, "Th")),eval(D(expr, "B")))
			}
			g<-y ~ a*(1-exp(-(x-Tl)/B))*(1-exp(-(Th-x)/B))
			if(proc=="mortal") frm<- "m(T) = a.(1-exp(-(x-Tl)/B)).(1-exp(-(Th-x)/B))" else frm<- "f(T) = a.(1-exp(-(x-Tl)/B)).(1-exp(-(Th-x)/B))"
		}

		if(modelm==45){
			f <- function(x,a,Tl, Th,Bl,Bh)
			{
				expr <- expression(exp(a*(1-exp(-(x-Tl)/Bl))*(1-exp(-(Th-x)/Bh))))
				eval(expr)
			}
			j <- function(x,a,Tl, Th,Bl,Bh)
			{
				expr <- expression(exp(a*(1-exp(-(x-Tl)/Bl))*(1-exp(-(Th-x)/Bh))))
				c(eval(D(expr, "a")),eval(D(expr, "Tl")),eval(D(expr, "Th")),eval(D(expr, "Bl")),eval(D(expr, "Bh")))
			}
			g<-y ~ exp(a*(1-exp(-(x-Tl)/Bl))*(1-exp(-(Th-x)/Bh)))
			if(proc=="mortal") frm<- "m(T) = exp(a.(1-exp(-(x-Tl)/Bl)).(1-exp(-(Th-x)/Bh)))" else frm<- "f(T) = exp(a.(1-exp(-(x-Tl)/Bl)).(1-exp(-(Th-x)/Bh)))"
		}


		fcn     <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), ini)))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		estimate<-as.data.frame(out$par)
		stdmor<-diag(ginv(out$hessian))
		return(list(estimados=estimate,f=g,modelo=frm,stdmort=stdmor,ecua=frm))
	}
}
###################################################################################################################################################
coef_mort<-function(proc,modelm,estimor,stdmortg,modelo,gg,datm,alg,pesos,weights,g,modelim)
{
	x<-datm[,1]
	y<-datm[,2]
	ind <- as.list(estimor)
	for (i in names(ind))
	{
		temp <- estimor[[i]]
		storage.mode(temp) <- "double"
		assign(i, temp)
	}
	if(alg=="Newton")
	{
		if(modelm==1 ||modelm==2 || modelm==3 || modelm==4 || modelm==5 || modelm==6)
		{
			yl<-predict(modelo)
			art=0.00000000000000000000000000001
			sqe <- sum(residuals(modelo)^2)+art
			qq<-summary(modelo)
			tabla<-qq$"parameters"
			stderror<-tabla[,2]  #Agrege DM
			aic<-AIC(modelo) #Quite ,k=length(estimor)  DM
			if(pesos=="WLS") r<-1-sum(residuals(modelo)^2)/sum(weights*(y-mean(y))^2)
			if(pesos=="LS")  r<-1-sum(residuals(modelo)^2)/sum((y-mean(y))^2)
			r_ajus<- 1 - ((length(x) - 1) / (length(x) - length(estimor))) * (1-r)
			if(pesos=="WLS") MSC<-log(sum(weights*(yl-mean(yl))^2)/sum(weights*(y-mean(y))^2))-2*length(estimor)/length(x)
			if(pesos=="LS") MSC<-log(sum((yl-mean(yl))^2)/sum((y-mean(y))^2))-2*length(estimor)/length(x)
			#var <- (sum((y-yl)^2)/(length(x) - length(estimor)))
			anva.1 <- c(length(coef(modelo))-1,length(x)-length(coef(modelo)),length(x)-1)
			anva.2 <- c((sum(y^2)-length(y)*mean(y)^2)-sqe,sqe,sum(y^2)-length(y)*mean(y)^2)
			anva.3 <- c(anva.2[1]/anva.1[1],anva.2[2]/anva.1[2],NA)
			anva.4 <- c(anva.3[1]/anva.3[2],NA,NA)
			anva.5 <- c(1-pf(anva.4[1],anva.1[1],anva.1[2]),NA,NA)
			anva   <- cbind(anva.1,round(anva.2,4),round(anva.3,4),round(anva.4,4),round(anva.5,4))
			rownames(anva) <- c("Model","Error","Total")
			colnames(anva) <- c("DF","SS","MS","Fc","p")
			if(proc=="mortal"){
				cat("MORTALITY FOR TEMPERATURE\n")
				dat <- data.frame(T = datm[,1], Mortality = datm[,2])}
			if(proc=="taza"){
				cat("FECUNDYTY FOR TEMPERATURE\n")
				dat <- data.frame(T = datm[,1], Fecundity = datm[,2])}
			print(dat)
			cat("\nNONLINEAR REGRESSION MODEL\n")
			cat("\nMethod:", alg)
			cat("\nFormula: ", g,"\n")
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
			param<-data.frame(g,para,d.f.=paste(anva.1[1],",",anva.1[2]),F=round(anva.4[1],3),P.value=round(anva.5[1],3),FRAMESEL)
		}
	}
	if(alg=="Marquardtr")
	{
		if(modelm==9)   {
			expre<-expression(y0 + a * exp(-0.5 * ((x-x0)/b)^2))
			yl<-eval(expre)
			gg<-y~y0 + a * exp(-0.5 * ((x-x0)/b)^2)
		}
		if(modelm==7){
			expre<-expression(1/(1+a*exp(-b*((x-c)/d)^2)))
			yl<-eval(expre)
			gg<-y~1/(1+a*exp(-b*((x-c)/d)^2))
		}
		if(modelm==8) {
			expre<-expression(a*exp(-b*((x-xo)/c)^2))
			yl<-eval(expre)
			gg<-y~a*exp(-b*((x-xo)/c)^2)
		}
		if(modelm==10) {
			expre<-expression(y0 + a * exp(-0.5 * (log(abs(x/x0))/b)^2))
			yl<-eval(expre)
			gg<-y~y0 + a * exp(-0.5 * (log(abs(x/x0))/b)^2)
		}

		if(modelm==11) {
			expre<-expression(b1+b2*x+b3*x^d)
			yl<-eval(expre)
			gg<-y~b1+b2*x+b3*x^d
		}

		if(modelm==12) {
			expre<-expression(exp(b1+b2*x+b3*x^2))
			yl<-eval(expre)
			gg<-y~exp(b1+b2*x+b3*x^2)
		}

		if(modelm==13) {
			expre<-expression(1-(b4/(1+b5*exp(b1+b2*x+b3*x^2))))
			yl<-eval(expre)
			gg<-y~1-(b4/(1+b5*exp(b1+b2*x+b3*x^2)))
		}


		if(modelm==14) {
			expre<-expression(exp(b1+b2*x+b3*sqrt(x)))
			yl<-eval(expre)
			gg<-y~exp(b1+b2*x+b3*sqrt(x))
		}


		if(modelm==15) {
			expre<-expression(1-(b4/(1+b5*exp(b1+b2*x+b3*sqrt(x)))))
			yl<-eval(expre)
			gg<-y~1-(b4/(1+b5*exp(b1+b2*x+b3*sqrt(x))))
		}

		if(modelm==16) {
			expre<-expression(exp(b1+b2*x+b3*(1/sqrt(x))))
			yl<-eval(expre)
			gg<-y~exp(b1+b2*x+b3*(1/sqrt(x)))
		}

		if(modelm==17) {
			expre<-expression(1-(b4/(1+b5*exp(b1+b2*x+b3*(1/sqrt(x))))))
			yl<-eval(expre)
			gg<-y~1-(b4/(1+b5*exp(b1+b2*x+b3*(1/sqrt(x)))))
		}

		if(modelm==18) {
			expre<-expression(exp(b1+b2*x+b3*(1/x)))
			yl<-eval(expre)
			gg<-y~exp(b1+b2*x+b3*(1/x))
		}

		if(modelm==19) {
			expre<-expression(1-(b4/(1+b5*exp(b1+b2*x+b3*(1/x)))))
			yl<-eval(expre)
			gg<-y~1-(b4/(1+b5*exp(b1+b2*x+b3*(1/x))))
		}

		if(modelm==20) {
			expre<-expression(exp(b1+b2*x+b3*x^d))
			yl<-eval(expre)
			gg<-y~exp(b1+b2*x+b3*x^d)
		}

		if(modelm==21) {
			expre<-expression(1-(b4/(1+b5*exp(b1+b2*x+b3*x^d))))
			yl<-eval(expre)
			gg<-y~1-(b4/(1+b5*exp(b1+b2*x+b3*x^d)))
		}

		if(modelm==22) {
			expre<-expression(exp(b1+b2*x+b3*log(x)))
			yl<-eval(expre)
			gg<-y~exp(b1+b2*x+b3*log(x))
		}

		if(modelm==23) {
			expre<-expression(1-(b4/(1+b5*exp(b1+b2*x+b3*log(x)))))
			yl<-eval(expre)
			gg<-y~1-(b4/(1+b5*exp(b1+b2*x+b3*log(x))))
		}

		if(modelm==24) {
			expre<-expression(1-rm*exp((-0.5)*(-(x-Topt)/Troh)^2))
			yl<-eval(expre)
			gg<-y~1-rm*exp((-0.5)*(-(x-Topt)/Troh)^2)
		}

		if(modelm==25) {
			expre<-expression(1-rm*exp((-0.5)*(-(log(x)-log(Topt))/Troh)^2))
			yl<-eval(expre)
			gg<-y~1-rm*exp((-0.5)*(-(log(x)-log(Topt))/Troh)^2)
		}

		if(modelm==30) {
			expre<-expression(1 - H/(exp(1+exp(-(x-Topt)/B))*(1+exp(-(Topt-x)/B))))
			yl<-eval(expre)
			gg<-y~1 - H/(exp(1+exp(-(x-Topt)/B))*(1+exp(-(Topt-x)/B)))
		}

		if(modelm==31) {
			expre<-expression(1 - H/(exp(1+exp(-(x-Tl)/B))*(1+exp(-(Th-x)/B))))
			yl<-eval(expre)
			gg<-y~1 - H/(exp(1+exp(-(x-Tl)/B))*(1+exp(-(Th-x)/B)))
		}

		if(modelm==32) {
			expre<-expression(1 - H/(exp(1+exp(-(x-Topt)/Bl))*(1+exp(-(Topt-x)/Bh))))
			yl<-eval(expre)
			gg<-y~1 - H/(exp(1+exp(-(x-Topt)/Bl))*(1+exp(-(Topt-x)/Bh)))
		}

		if(modelm==33) {
			expre<-expression(1 - H/(exp(1+exp(-(x-Tl)/Bl))*(1+exp(-(Th-x)/Bh))))
			yl<-eval(expre)
			gg<-y~1 - H/(exp(1+exp(-(x-Tl)/Bl))*(1+exp(-(Th-x)/Bh)))
		}

		if(modelm==34) {
			expre<-expression(Bm + ((1-1/(1+exp((Hl/1.987)*((1/Tl)-(1/(x+273.15))))+exp((Hh/1.987)*((1/Th)-(1/(x+273.15))))))*(1-Bm))) #DM: se agrego 2 parentesis "Bm + (...)*"
			yl<-eval(expre)
			gg<-y~Bm + ((1-1/(1+exp((Hl/1.987)*((1/Tl)-(1/(x+273.15))))+exp((Hh/1.987)*((1/Th)-(1/(x+273.15))))))*(1-Bm)) #DM: se agrego 2 parentesis "Bm + (...)*"
		}

		if(modelm==37) {
			expre<-expression(a1*exp(b1*x) + a2*exp(b2*x))
			yl<-eval(expre)
			gg<-y ~ a1*exp(b1*x) + a2*exp(b2*x)
		}

		if(modelm==38) {
			expre<-expression(a1*exp(b1*x) + a2*exp(b2*x) + c1)
			yl<-eval(expre)
			gg<-y ~ a1*exp(b1*x) + a2*exp(b2*x) + c1
		}


		if(modelm==39) {
			expre<-expression(a*(abs(x-b))^nn)
			yl<-eval(expre)
			gg<-y ~ a*(abs(x-b))^nn
		}


		if(modelm==40) {
			expre<-expression(a*x*(x-To)*(Tl-x)^(1/d))
			yl<-eval(expre)
			gg<-y ~ a*x*(x-To)*(Tl-x)^(1/d)
		}


		if(modelm==41) {
			expre<-expression(exp(a*x*(x-To)*(Tl-x)^(1/d)))
			yl<-eval(expre)
			gg<-y ~ exp(a*x*(x-To)*(Tl-x)^(1/d))
		}

		if(modelm==42) {
			expre<-expression(a*((x-Tmin)^n)*(Tmax-x)^m)
			yl<-eval(expre)
			gg<-y ~ a*((x-Tmin)^n)*(Tmax-x)^m
		}

		if(modelm==43) {
			expre<-expression(1/((Dmin/2) * (exp(k*(x-Tp)) + exp(-(x-Tp)*lamb))))
			yl<-eval(expre)
			gg<-y ~ 1/((Dmin/2) * (exp(k*(x-Tp)) + exp(-(x-Tp)*lamb)))
		}

		if(modelm==44) {
			expre<-expression(a*(1-exp(-(x-Tl)/B))*(1-exp(-(Th-x)/B)))
			yl<-eval(expre)
			gg<-y ~ a*(1-exp(-(x-Tl)/B))*(1-exp(-(Th-x)/B))
		}

		if(modelm==45) {
			expre<-expression(exp(a*(1-exp(-(x-Tl)/Bl))*(1-exp(-(Th-x)/Bh))))
			yl<-eval(expre)
			gg<-y ~ exp(a*(1-exp(-(x-Tl)/Bl))*(1-exp(-(Th-x)/Bh)))
		}

		sqe <- sum((y-yl)^2)
		if(length(x) == length(estimor)) stop("number of parameters = number of temperatures") #DM se agrego!
		var <- (sum((y-yl)^2)/(length(x) - length(estimor)))
		stderror<-sqrt(var*stdmortg)
		#if(length(x) == length(estimor)) stderror<-rep(NA,length(estimor)) #JC
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
		if(proc=="mortal"){
			cat("MORTALITY FOR TEMPERATURE\n")
			dat <- data.frame(T = datm[,1], Mortality = datm[,2])}
		if(proc=="taza"){
			cat("FECUNDYTY FOR TEMPERATURE\n")
			dat <- data.frame(T = datm[,1], Fecundity = datm[,2])}
		print(dat)
		cat("\nNONLINEAR REGRESSION MODEL\n")
		cat("\nMethod:", alg)
		cat("\nFormula: ", g,"\n")
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
		param<-data.frame(modelim,para,d.f.=paste(anva.1[1],",",anva.1[2]),F=round(anva.4[1],3),P.value=round(anva.5[1],3),FRAMESEL)

	}
	return(list(parmor=param,frames=FRAMESEL, Std.Error=stderror))      # Agregue "Std.Error=stderror" DM
}
###################################################################################################################################################
grafmort <- function(proc, modelm, estimor, g, datm, corrx=NULL, corry=NULL, mini, maxi, limit,tam,labx, laby,titulo,grises=FALSE, scaleX, scaleY){
	if(grises==TRUE){ccol=c("gray20","gray30")}else{ccol=c(4,2)}
	data<-datm
	x<-data[,1]
	estshap<-as.list(estimor)
	finalshap<-estshap
	for (i in names(finalshap)) {
		temp <- finalshap[[i]]
		storage.mode(temp) <- "double"
		assign(i, temp)
	}
	xl<-seq(mini,maxi,length=2000)
	if(modelm==1 ||modelm==2 || modelm==3 || modelm==4 || modelm==5 || modelm==6)
	{
		if(modelm==1)  fom<-y~a*x^2+b*x+c
		if(modelm==2)  fom<-y~a*sqrt(x)+b*x+c
		if(modelm==3)  fom<-y~a*(1/sqrt(x))+b*x+c
		if(modelm==4)  fom<-y~a*(1/x^2)+b*x+c
		if(modelm==5)  fom<-y~a*(1/x)+b*x+c
		if(modelm==6)  fom<-y~b*x+a*log(x)+c
		forex <- fom[[length(fom)]]
		ylexpx<-as.expression(forex)
		ylx<-eval(ylexpx)
		x<-xl
		ylexp<-as.expression(forex)
		yl<-eval(ylexp)
	}

	if(modelm==9) {
		exprex<-expression(y0 + a * exp(-0.5 * ((x-x0)/b)^2))
		ylx<-eval(exprex)
		expre<-expression(y0 + a * exp(-0.5 * ((xl-x0)/b)^2))
		yl<-eval(expre)
	}
	if(modelm==7) {
		exprex<-expression(1/(1+a*exp(-b*((x-c)/d)^2)))
		ylx<-eval(exprex)
		expre<-expression(1/(1+a*exp(-b*((xl-c)/d)^2)))
		yl<-eval(expre)
	}
	if(modelm==8) {
		exprex<-expression(a*exp(-b*((x-xo)/c)^2))
		ylx<-eval(exprex)
		expre<-expression(a*exp(-b*((xl-xo)/c)^2))
		yl<-eval(expre)
	}
	if(modelm==10) {
		exprex<-expression(y0 + a * exp(-0.5 * (log(abs(x/x0))/b)^2))
		ylx<-eval(exprex)
		expre<-expression(y0 + a * exp(-0.5 * (log(abs(xl/x0))/b)^2))
		yl<-eval(expre)
	}

	if(modelm==11) {
		exprex<-expression(b1+b2*x+b3*x^d)
		ylx<-eval(exprex)
		expre<-expression(b1+b2*xl+b3*xl^d)
		yl<-eval(expre)
	}


	if(modelm==12) {
		exprex<-expression(exp(b1+b2*x+b3*x^2))
		ylx<-eval(exprex)
		expre<-expression(exp(b1+b2*xl+b3*xl^2))
		yl<-eval(expre)
	}

	if(modelm==13) {
		exprex<-expression(1-(b4/(1+b5*exp(b1+b2*x+b3*x^2))))
		ylx<-eval(exprex)
		expre<-expression(1-(b4/(1+b5*exp(b1+b2*xl+b3*xl^2))))
		yl<-eval(expre)
	}

	if(modelm==14) {
		exprex<-expression(exp(b1+b2*x+b3*sqrt(x)))
		ylx<-eval(exprex)
		expre<-expression(exp(b1+b2*xl+b3*sqrt(xl)))
		yl<-eval(expre)
	}

	if(modelm==15) {
		exprex<-expression(1-(b4/(1+b5*exp(b1+b2*x+b3*sqrt(x)))))
		ylx<-eval(exprex)
		expre<-expression(1-(b4/(1+b5*exp(b1+b2*xl+b3*sqrt(xl)))))
		yl<-eval(expre)
	}

	if(modelm==16) {
		exprex<-expression(exp(b1+b2*x+b3*(1/sqrt(x))))
		ylx<-eval(exprex)
		expre<-expression(exp(b1+b2*xl+b3*(1/sqrt(xl))))
		yl<-eval(expre)
	}

	if(modelm==17) {
		exprex<-expression(1-(b4/(1+b5*exp(b1+b2*x+b3*(1/sqrt(x))))))
		ylx<-eval(exprex)
		expre<-expression(1-(b4/(1+b5*exp(b1+b2*xl+b3*(1/sqrt(xl))))))
		yl<-eval(expre)
	}

	if(modelm==18) {
		exprex<-expression(exp(b1+b2*x+b3*(1/x)))
		ylx<-eval(exprex)
		expre<-expression(exp(b1+b2*xl+b3*(1/xl)))
		yl<-eval(expre)
	}

	if(modelm==19) {
		exprex<-expression(1-(b4/(1+b5*exp(b1+b2*x+b3*(1/x)))))
		ylx<-eval(exprex)
		expre<-expression(1-(b4/(1+b5*exp(b1+b2*xl+b3*(1/xl)))))
		yl<-eval(expre)
	}

	if(modelm==20) {
		exprex<-expression(exp(b1+b2*x+b3*x^d))
		ylx<-eval(exprex)
		expre<-expression(exp(b1+b2*xl+b3*xl^d))
		yl<-eval(expre)
	}

	if(modelm==21) {
		exprex<-expression(1-(b4/(1+b5*exp(b1+b2*x+b3*x^d))))
		ylx<-eval(exprex)
		expre<-expression(1-(b4/(1+b5*exp(b1+b2*xl+b3*xl^d))))
		yl<-eval(expre)
	}

	if(modelm==22) {
		exprex<-expression(exp(b1+b2*x+b3*log(x)))
		ylx<-eval(exprex)
		expre<-expression(exp(b1+b2*xl+b3*log(xl)))
		yl<-eval(expre)
	}

	if(modelm==23) {
		exprex<-expression(1-(b4/(1+b5*exp(b1+b2*x+b3*log(x)))))
		ylx<-eval(exprex)
		expre<-expression(1-(b4/(1+b5*exp(b1+b2*xl+b3*log(xl)))))
		yl<-eval(expre)
	}

	if(modelm==24) {
		exprex<-expression(1-rm*exp((-0.5)*(-(x-Topt)/Troh)^2))
		ylx<-eval(exprex)
		expre<-expression(1-rm*exp((-0.5)*(-(xl-Topt)/Troh)^2))
		yl<-eval(expre)
	}

	if(modelm==25) {
		exprex<-expression(1-rm*exp((-0.5)*(-(log(x)-log(Topt))/Troh)^2))
		ylx<-eval(exprex)
		expre<-expression(1-rm*exp((-0.5)*(-(log(xl)-log(Topt))/Troh)^2))
		yl<-eval(expre)
	}

	if(modelm==30) {
		exprex<-expression(1 - H/(exp(1+exp(-(x-Topt)/B))*(1+exp(-(Topt-x)/B))))
		ylx<-eval(exprex)
		expre<-expression(1 - H/(exp(1+exp(-(xl-Topt)/B))*(1+exp(-(Topt-xl)/B))))
		yl<-eval(expre)
	}

	if(modelm==31) {
		exprex<-expression(1 - H/(exp(1+exp(-(x-Tl)/B))*(1+exp(-(Th-x)/B))))
		ylx<-eval(exprex)
		expre<-expression(1 - H/(exp(1+exp(-(xl-Tl)/B))*(1+exp(-(Th-xl)/B))))
		yl<-eval(expre)
	}

	if(modelm==32) {
		exprex<-expression(1 - H/(exp(1+exp(-(x-Topt)/Bl))*(1+exp(-(Topt-x)/Bh))))
		ylx<-eval(exprex)
		expre<-expression(1 - H/(exp(1+exp(-(xl-Topt)/Bl))*(1+exp(-(Topt-xl)/Bh))))
		yl<-eval(expre)
	}

	if(modelm==33) {
		exprex<-expression(1 - H/(exp(1+exp(-(x-Tl)/Bl))*(1+exp(-(Th-x)/Bh))))
		ylx<-eval(exprex)
		expre<-expression(1 - H/(exp(1+exp(-(xl-Tl)/Bl))*(1+exp(-(Th-xl)/Bh))))
		yl<-eval(expre)
	}

	if(modelm==34) {
		exprex<-expression(Bm + ((1-1/(1+exp((Hl/1.987)*((1/Tl)-(1/(x+273.15))))+exp((Hh/1.987)*((1/Th)-(1/(x+273.15))))))*(1-Bm))) #DM: se agrego 2 parentesis "Bm + (...)*"
		ylx<-eval(exprex)
		expre<-expression(Bm + ((1-1/(1+exp((Hl/1.987)*((1/Tl)-(1/(xl+273.15))))+exp((Hh/1.987)*((1/Th)-(1/(xl+273.15))))))*(1-Bm))) #DM: se agrego 2 parentesis "Bm + (...)*"
		yl<-eval(expre)
	}

	if(modelm==37) {
		exprex<-expression(a1*exp(b1*x) + a2*exp(b2*x))
		ylx<-eval(exprex)
		expre<-expression(a1*exp(b1*xl) + a2*exp(b2*xl))
		yl<-eval(expre)
	}

	if(modelm==38) {
		exprex<-expression(a1*exp(b1*x) + a2*exp(b2*x) + c1)
		ylx<-eval(exprex)
		expre<-expression(a1*exp(b1*xl) + a2*exp(b2*xl) + c1)
		yl<-eval(expre)
	}

	if(modelm==39) {
		exprex<-expression(a*(abs(x-b))^nn)
		ylx<-eval(exprex)
		expre<-expression(a*(abs(xl-b))^nn)
		yl<-eval(expre)
	}

	if(modelm==40) {
		exprex<-expression(a*x*(x-To)*(Tl-x)^(1/d))
		ylx<-eval(exprex)
		expre<-expression(a*xl*(xl-To)*(Tl-xl)^(1/d))
		yl<-eval(expre)
	}

	if(modelm==41) {
		exprex<-expression(exp(a*x*(x-To)*(Tl-x)^(1/d)))
		ylx<-eval(exprex)
		expre<-expression(exp(a*xl*(xl-To)*(Tl-xl)^(1/d)))
		yl<-eval(expre)
	}

	if(modelm==42) {
		exprex<-expression(a*((x-Tmin)^n)*(Tmax-x)^m)
		ylx<-eval(exprex)
		expre<-expression(a*((xl-Tmin)^n)*(Tmax-xl)^m)
		yl<-eval(expre)
	}

	if(modelm==43) {
		exprex<-expression(1/((Dmin/2) * (exp(k*(x-Tp)) + exp(-(x-Tp)*lamb))))
		ylx<-eval(exprex)
		expre<-expression(1/((Dmin/2) * (exp(k*(xl-Tp)) + exp(-(xl-Tp)*lamb))))
		yl<-eval(expre)
	}

	if(modelm==44) {
		exprex<-expression(a*(1-exp(-(x-Tl)/B))*(1-exp(-(Th-x)/B)))
		ylx<-eval(exprex)
		expre<-expression(a*(1-exp(-(xl-Tl)/B))*(1-exp(-(Th-xl)/B)))
		yl<-eval(expre)
	}

	if(modelm==45) {
		exprex<-expression(exp(a*(1-exp(-(x-Tl)/Bl))*(1-exp(-(Th-x)/Bh))))
		ylx<-eval(exprex)
		expre<-expression(exp(a*(1-exp(-(xl-Tl)/Bl))*(1-exp(-(Th-xl)/Bh))))
		yl<-eval(expre)
	}



	df<-length(data[,1])-length(estshap)
	sdli<-sqrt(sum((data[,2]-ylx)^2)/df)
	linf<-yl+sdli*qt(0.025,df)
	lsup<-yl-sdli*qt(0.025,df)
	
	labx<-expression(paste("temperature (", degree, "C)"))
	laby<-"eggs/female"
	MARGEN=c(5.2, 5, 5.5, 0.5)
	
	if(limit=="yes"){
		if(proc=="mortal"){
		  par(mar=MARGEN,family="serif",font=1,cex.axis=1.8,cex.lab=1.8)
		  if(is.null(corrx)){corrx=c(min(data[,1],0),max(data[,1])*1.2);corrx2=seq(0,10*round(corrx[2]/10,1),5)}else{corrx2=seq(corrx[1],corrx[2],scaleX)} ## cambio
			if(is.null(corry)){corry=c(0,100*(max(data[,2])*1.01));corry2=seq(0,round(max(corry)),10)}else{corry2=seq(corry[1],corry[2],scaleY)}              ## cambio

			plot(data[,1],data[,2]*100,frame=F,col=ccol[1],xlim=corrx,ylim=corry,xlab=labx,ylab=laby,pch=19,axes=F,xaxt = "n",main=titulo) ## 4
			axis(1, corrx2, cex.axis=1.6)  ## cambio
			axis(2, corry2,las=2, tck=-0.025, line=-1, cex.axis=1.6) ## cambio
			
			lines(xl, yl*100, lwd=2,col=ccol[2]) ## 2
			lines(xl, linf*100, lwd=1,col=ccol[1],lty=2) ## 4
			lines(xl, lsup*100, lwd=1,col=ccol[1],lty=2) ## 4
			#arrows(data[,1],(data[,2]-(data[,2]-data[,3])), data[,1],(data[,2]+(data[,4]-data[,2])), length=0.1,angle=90, code=3,col=ccol[1]) ## 4
			valx=xl[100*yl<=100];valx1=min(valx,na.rm=TRUE);valx2=max(valx,na.rm=TRUE)  #Se agrego na.rm=TRUE
			return(list(valxs=c(valx1,valx2)))
		}
		if(proc=="taza"){
		  par(mar=MARGEN,family="serif",font=1,cex.axis=1.8,cex.lab=1.8)
      if(is.null(corrx)){corrx=c(min(data[,1],0),max(data[,1])*1.2);corrx2=seq(0,10*round(corrx[2]/10,1),5)}else{corrx2=seq(corrx[1],corrx[2],scaleX)} ## cambio
			if(is.null(corry)){corry=c(0,max(data[,2])*1.01);corry2=seq(0,1,0.1)}else{corry2=seq(corry[1],corry[2],scaleY)}              ## cambio
			
			#plot(data[,1],data[,2],frame=F,col=ccol[1],xlim=corrx,ylim=corry,xlab=labx,ylab=laby,pch=19,axes=F,xaxt = "n",main=titulo) ## 4
			plot(data[,1],data[,2],frame=F,col=ccol[1],xlim=corrx,ylim=corry,xlab=labx,ylab="fecundity/female",pch=19,axes=F,xaxt = "n",main=titulo) ## 4
			axis(1, corrx2, cex.axis=1.6)  ## cambio
			axis(2, corry2,las=2, tck=-0.025, line=-0.45, cex.axis=1.6) ## cambio
			
			lines(xl, yl, lwd=2,col=ccol[2]) ## 2
			lines(xl, linf, lwd=1,col=ccol[1],lty=2) ## 4
			lines(xl, lsup, lwd=1,col=ccol[1],lty=2) ## 4
			#arrows(data[,1],(data[,2]-(data[,2]-data[,3])), data[,1],(data[,2]+(data[,4]-data[,2])), length=0.1,angle=90, code=3,col=4)
		}
	}
	if(limit=="no"){
		if(proc=="mortal"){
			par(cex=tam)
		        if(is.null(corrx)){corrx=c(min(data[,1],0),max(data[,1])*1.2);corrx2=seq(0,10*round(corrx[2]/10,1),5)}else{corrx2=seq(corrx[1],corrx[2],scaleX)} ## cambio
			if(is.null(corry)){corry=c(0,100*(max(data[,2])*1.1));corry2=seq(0,round(max(corry)),10)}else{corry2=seq(corry[1],corry[2],scaleY)}              ## cambio
			plot(data[,1],data[,2]*100,frame=F,col=ccol[1],xlim=corrx,ylim=corry,xlab=labx,ylab=laby,pch=19,axes=F,xaxt = "n",main=titulo) ## 4
			#axis(1, xaxp=c(corrx,5))
			#axis(2,las=2)
			axis(1, corrx2)  ## cambio
			axis(2, corry2,las=2) ## cambio
			lines(xl, yl*100, lwd=2,col=ccol[2]) ## 2
			valx=xl[100*yl<=100];valx1=min(valx,na.rm=TRUE);valx2=max(valx,na.rm=TRUE) #Se agrego na.rm=TRUE
			return(list(valxs=c(valx1,valx2)))
		}
		if(proc=="taza"){
			par(cex=tam)
		        if(is.null(corrx)){corrx=c(min(data[,1],0),max(data[,1])*1.2);corrx2=seq(0,10*round(corrx[2]/10,1),5)}else{corrx2=seq(corrx[1],corrx[2],scaleX)} ## cambio
			if(is.null(corry)){corry=c(0,max(data[,2])*1.1);corry2=seq(0,1,0.1)}else{corry2=seq(corry[1],corry[2],scaleY)}              ## cambio
			plot(data[,1],data[,2],frame=F,col=ccol[1],xlim=corrx,ylim=corry,xlab=labx,ylab=laby,pch=19,axes=F,xaxt = "n",main=titulo) ## 4
			#axis(1, xaxp=c(corrx,5))
			#axis(2,las=2)
			axis(1, corrx2)  ## cambio
			axis(2, corry2,las=2) ## cambio
			lines(xl, yl, lwd=2,col=ccol[2]) ## 2
		}
	}
}


################################
# Recalculo de valores iniciales
################################

recalc<-function(ini,niv=1)
{
	vec<-list(1)
	for(i in 1:length(ini))
	{
		vec[[i]]<-ini[[i]]+ini[[i]]*runif(1,-(niv),niv)
	}
	names(vec)=names(ini)
	return(vec)
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