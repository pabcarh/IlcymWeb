 
#############################################
#############################################
#############################################
prueba<-function(model,datashap,datao,ini,corrx,corry,punt,labx,laby,titulo, grises=FALSE){
   if(grises==TRUE){ccol=c("gray10","gray20","gray30")}else{ccol=c(4,1,2)}

   if(model==12){
		datlinea<-datashap
		#plot(datlinea[,1],datlinea[,2],frame=F,pch=19,col=ccol[1],cex=1.3,xlab=labx,ylab=laby,xlim=corrx,ylim=corry,axes=F,xaxt = "n",main=titulo)   ## 1
		#axis(1, xaxp=c(corrx,5))
		#axis(2,las=2)
		#arrows(datlinea[,1],(datlinea[,2]-(datlinea[,2]-datlinea[,3])), datlinea[,1],(datlinea[,2]+(datlinea[,4]-datlinea[,2])), length=0.1,angle=90, code=3,col=ccol[1]) ## 1
		punt<-punt
		#points(datlinea[,1][punt],datlinea[,2][punt],pch=19,col=ccol[2],cex=1.3) ## 2
		#arrows(datlinea[,1][punt],(datlinea[,2][punt]-(datlinea[,2][punt]-datlinea[,3][punt])), datlinea[,1][punt],(datlinea[,2][punt]+(datlinea[,4][punt]-datlinea[,2][punt])), length=0.1,angle=90, code=3,col=ccol[2])  ## 2

		if(length(datashap[,1])>=(length(ini)+1)){
			x<-datashap[,1]+273.15
			y<-datashap[,2]
			dataa<-data.frame(x = datlinea[punt,1]+273.15, y =datlinea[punt,2])
		}else{
			x<-datao[,1]+273.15
			y<-datao[,2]
			dataa<-data.frame(x = datlinea[punt,1]+273.15, y = c((datlinea[punt,2]),(datlinea[punt,4]),(datlinea[punt,3])))
		}
		coefi<-as.numeric(coef(lm(dataa[,2]~dataa[,1])))
		xlinea<-seq(0,max(dataa[,1],na.rm=TRUE),length=1000)
		ylinea<-coefi[1]+coefi[2]*(xlinea+273.15)
		#lines(xlinea,ylinea,col=ccol[2],lty=2,lwd=1)  ## 2
		
		
		################## nuevos modelos de sharpe de michelle


		if(model==12){
			ini<-as.list(ini)
			for (i in names(ini)){
				temp <- ini[[i]]
				storage.mode(temp) <- "double"
				assign(i, temp)
			}
			f <- function(x,Ha,Hl,Tl,Hh,Th){
				expr <- expression(((coefi[1]+coefi[2]*298.16) * (x/298.16) * exp((Ha/1.987) * ((1/298.16) - (1/x))))/
							(1 + exp((Hl/1.987) * ((1/Tl) - (1/x))) + exp((Hh/1.987) * ((1/Th) - (1/x)))))
				eval(expr)
			}
			p<-coefi[1]+coefi[2]*298.16
			lineashap<-seq(0,50,length=1000)
			ylineash<-f((lineashap+273.15),Ha, Hl,Tl,Hh,Th)
			salidas<-list(coefi=coefi,ini=as.data.frame(ini),p=p)
		}

		################## nuevos modelos de sharpe de michelle

		#lines(lineashap,ylineash,col=ccol[3],lwd=2)
		return(salidas)
	}else{
	 datlinea<-datashap
		#plot(datlinea[,1],datlinea[,2],frame=F,pch=19,col=ccol[1],cex=1.3,xlim=corrx,ylim=corry,axes=F,xaxt = "n",xlab=as.character(labx),ylab=laby) ## 1
		#axis(1, xaxp=c(corrx,5), main=titulo)
		#axis(2,las=2)
		#arrows(datlinea[,1],(datlinea[,2]-(datlinea[,2]-datlinea[,3])), datlinea[,1],(datlinea[,2]+(datlinea[,4]-datlinea[,2])), length=0.1,angle=90, code=3,col=ccol[1]) ## 1
		lineashap<-seq(0,50,length=1000)
		ini<-as.list(ini)
		for (i in names(ini)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}

		################## nuevos modelos de sharpe de michelle

		if(model==11)   {
			lineashap<-lineashap+273.15
			expre<-expression((p * (lineashap/298.16) * exp((Ha/1.987) * ((1/298.16) - (1/lineashap))))/
							(1 + exp((Hl/1.987) * ((1/Tl) - (1/lineashap))) + exp((Hh/1.987) * ((1/Th) - (1/lineashap)))))
			ylineash<-eval(expre)
                        coefi=NULL
		}

		if(model==13)   {
			lineashap<-lineashap+273.15
			expre<-expression((p * (lineashap/298.16) * exp((Ha/1.987) * ((1/298.16) - (1/lineashap))))/
							(1 + exp((Hl/1.987) * ((1/Tl) - (1/lineashap)))))
			ylineash<-eval(expre)
                        coefi=NULL
		}


		if(model==14)   {
			lineashap<-lineashap+273.15
			expre<-expression((p * (lineashap/298.16) * exp((Ha/1.987) * ((1/298.16) - (1/lineashap))))/
							(1 + exp((Hh/1.987) * ((1/Th) - (1/lineashap)))))
			ylineash<-eval(expre)
                        coefi=NULL
		}


		################## fin nuevos modelos de sharpe de michelle

		if(model==24){
			f <- function(x,Inter,Slop){
				expr <- expression(Inter + Slop*x)
				eval(expr)
			}
			ylineash<-f(lineashap,Inter,Slop)
                        coefi=NULL
		}


		if(model==25){
			f <- function(x,b1,b2){
				expr <- expression(b1*exp(b2*x))
				eval(expr)
			}
			ylineash<-f(lineashap,b1,b2)
                        coefi=NULL
		}

    if(model==26){
			f <- function(x,sy,b,Tb,DTb){
				expr <- expression(sy*exp(b*(x-Tb)-exp(b*(x-Tb)/DTb)))
				eval(expr)
			}
			ylineash<-f(lineashap,sy,b,Tb,DTb)
                        coefi=NULL
		}

		if(model==27){
			f <- function(x,sy,b,Tb){
				expr <- expression(sy*exp(b*(x-Tb)))
				eval(expr)
			}
			ylineash<-f(lineashap,sy,b,Tb)
                        coefi=NULL
		}

		if(model==28){
			f <- function(x,b,Tmin){
				expr <- expression(exp(b*(x-Tmin))-1)
				eval(expr)
			}
			ylineash<-f(lineashap,b,Tmin)
                        coefi=NULL
		}

		if(model==29){
			f <- function(x,b,Tb){
				expr <- expression(b*(x-Tb)^2)
				eval(expr)
			}
			ylineash<-f(lineashap,b,Tb)
                        coefi=NULL
		}

		if(model==30){
			f <- function(x,k,a,b){
				expr <- expression(k/(1+exp(a-b*x))) #DM: Se cambio "+b*x" por "-b*x"
				eval(expr)
			}
			ylineash<-f(lineashap,k,a,b)
                        coefi=NULL
		}


		if(model==31){
			f <- function(x,R,Tm,To){
				expr <- expression(R*exp((-1/2)*((x-Tm)/To))^2) #DM: Se agrego "^2"
				eval(expr)
			}
			ylineash<-f(lineashap,R,Tm,To)
                        coefi=NULL
		}

		if(model==32){
			f <- function(x,a,b,c,d){
				expr <- expression(a*exp((-1/2)*(abs(x-b)/c)^d)) #DM: Se cambio "abs((x-b)/c)" por "abs(x-b)/c"
				eval(expr)
			}
			ylineash<-f(lineashap,a,b,c,d)
                        coefi=NULL
		}

		if(model==33){
			f <- function(x,Rmax,k1,k2,Topc){
				expr <- expression(Rmax*(exp(k1+k2*Topc))/(1+exp(k1+k2*x)))
				eval(expr)
			}
			ylineash<-f(lineashap,Rmax,k1,k2,Topc)
                        coefi=NULL
		}


		if(model==34){
			f <- function(x,Tb,Tmax,d,Y,v){
				expr <- expression(Y*((x-Tb)^2/((x-Tb)+d^2)-exp(-(Tmax-(x-Tb))/v))) #DM: Se elevo al cuadrado en el numerador y se quito en el denominador
				eval(expr)
			}
			ylineash<-f(lineashap,Tb,Tmax,d,Y,v)
                        coefi=NULL
		}



		if(model==35){
			f <- function(x,Tl, p, dt){
				expr <- expression(exp(p*x)-exp(p*Tl-(Tl-x)/dt)) #DM: Se omitio un menos "-(p" por "p"  y se invirtio "(x-Tl))/dt" por "(Tl-x)/dt"
				eval(expr)
			}
			ylineash<-f(lineashap,Tl, p, dt)
                        coefi=NULL
		}



		if(model==44){
			f <- function(x,a1,b1,c1,d1,f1,g1){
				expr <- expression(x*exp(a1-b1/x)/(1 + exp(c1-d1/x) + exp(f1-g1/x)))
				eval(expr)
			}
			ylineash<-f(lineashap,a1,b1,c1,d1,f1,g1)
                        coefi=NULL
		}



		if(model==46){
			f <- function(x,Dmin,Topt,K){
				expr <- expression(2/(Dmin*(exp(K*(x-Topt)) + exp((-K)*(x-Topt)))))
				eval(expr)
			}
			ylineash<-f(lineashap,Dmin,Topt,K)
                        coefi=NULL
		}



		if(model==47){
			f <- function(x,c,a,b,Tm){
				expr <- expression(2*c/(a^(x-Tm) + b^(Tm-x)))
				eval(expr)
			}
			ylineash<-f(lineashap,c,a,b,Tm)
                        coefi=NULL
		}


		if(model==48){
			f <- function(x,a0,a1,a2,a3){
				expr <- expression(a0+a1*x+a2*x^2+a3*x^3)
				eval(expr)
			}
			ylineash<-f(lineashap,a0,a1,a2,a3)
                        coefi=NULL
		}


		if(model==49){
			f <- function(x,k,a,b,c,Tmin,Tmax,r){
				expr <- expression(k*(1-exp((-a)*(x-Tmin)))*(1-exp(b*(x-Tmax)))/(1+exp((-r)*(x-c))))
				eval(expr)
			}
			ylineash<-f(lineashap,k,a,b,c,Tmin,Tmax,r)
                        coefi=NULL
		}

		## funciones adaptadas a senescencia

		if(model==50){
			f <- function(x,c1,k1,k2){
				expr <- expression(c1/(1+exp(k1+k2*x)))
				eval(expr)
			}
			ylineash<-f(lineashap,c1,k1,k2)
                        coefi=NULL
		}


		if(model==51){
			f <- function(x,c1,c2,k1,k2,To){
				expr <- expression(c1/(1+exp(k1+k2*x)) + c2/(1+exp(k1+k2*(2*To-x))))
				eval(expr)
			}
			ylineash<-f(lineashap,c1,c2,k1,k2,To)
                        coefi=NULL
		}



		if(model==52){
			f <- function(x,sy,b,Tmin,Tmax,Dtb){
				expr <- expression(sy*exp(b*(x-Tmin)-exp(b*Tmax - (Tmax-(x-Tmin))/DTb)))
				eval(expr)
			}
			ylineash<-f(lineashap,sy,b,Tmin,Tmax,Dtb)
                        coefi=NULL
		}

		if(model==53){
			f <- function(x,alph,k,b,Tmin,Tmax,Dt){
				expr <- expression(alph*(1/(1+k*exp(-b*(x-Tmin))) - exp(-(Tmax-(x-Tmin))/Dt)))
				eval(expr)
			}
			ylineash<-f(lineashap,alph,k,b,Tmin,Tmax,Dt)
                        coefi=NULL
		}

		if(model==54){
			f <- function(x,alph,k,b,Tmax,Dt){
				expr <- expression(alph*(1/(1+k*exp(-b*x)) - exp(-(Tmax-x)/Dt)))
				eval(expr)
			}
			ylineash<-f(lineashap,alph,k,b,Tmax,Dt)
                        coefi=NULL
		}

		if(model==55){
			f <- function(x,trid,Tmax,Dt){
				expr <- expression(trid*( (x^2)/(x^2+D)  - exp(-(Tmax-x)/Dt))) #Cambio de "x/Dt" por "x)/Dt"
				eval(expr)
			}
			ylineash<-f(lineashap,trid,Tmax,Dt)
                        coefi=NULL
		}

		if(model==56){
			f <- function(x,trid,Tmax,Tmin,D,Dt,Smin){
				expr <- expression(trid*(((x-Tmin)^2)/((x-Tmin)^2 + D) - exp(-(Tmax-(x-Tmin))/Dt)) + Smin)
				eval(expr)
			}
			ylineash<-f(lineashap,trid,Tmax,Tmin,D,Dt,Smin)
                        coefi=NULL
		}

		if(model==57){
			f <- function(x,rm,Topt,Troh){ #DM: se quito Smin
				expr <- expression(rm*exp(-(0.5)*(-(x-Topt)/Troh)^2))
				eval(expr)
			}
			ylineash<-f(lineashap,rm,Topt,Troh) #DM: se quito Smin
                        coefi=NULL
		}

		if(model==58){
			f <- function(x,Tl, p, dt,lamb){
				expr <- expression(exp(p*x)-exp(p*Tl-(Tl-x)/dt) + lamb) #DM: Se cambió "(p*Tl-(Tl-x))/dt)" por "p*Tl-(Tl-x)/dt)"  
				eval(expr)
			}
			ylineash<-f(lineashap,Tl, p, dt,lamb)
                        coefi=NULL
		}

		if(model==59){
			f <- function(x,c1,a,b){
				expr <- expression(c1/(1+exp(a+b*x)))
				eval(expr)
			}
			ylineash<-f(lineashap,c1,a,b)
                        coefi=NULL
		}


		#if(model==11 || model==13 || model==14) {lines(lineashap-273.15,ylineash,col=ccol[3],lwd=2)}else{lines(lineashap,ylineash,col=ccol[3],lwd=2)} ##  3
		salidas<-list(ini=as.data.frame(ini),coefi=coefi)                                                  
		return(salidas)

  }
}

#############################################
#############################################
#############################################
shape<-function(model,datashap,datao,ini,coefi){

	################## nuevos modelos de sharpe de michelle


  	if(model==11) {
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]+273.15
			y<-datashap[,2]
		}else{
			x<-datao[,1]+273.15
			y<-datao[,2]
		}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x,p,Ha, Hl,Tl,Hh,Th){

			expr <- expression((p * (x/298.16) * exp((Ha/1.987) * ((1/298.16) - (1/x))))/
							(1 + exp((Hl/1.987) * ((1/Tl) - (1/x))) + exp((Hh/1.987) * ((1/Th) - (1/x)))))
			eval(expr)
		}
		j <- function(x,p,Ha, Hl,Tl,Hh,Th){
			expr <- expression((p * (x/298.16) * exp((Ha/1.987) * ((1/298.16) - (1/x))))/
							(1 + exp((Hl/1.987) * ((1/Tl) - (1/x))) + exp((Hh/1.987) * ((1/Th) - (1/x)))))
			c(eval(D(expr, "p")),eval(D(expr, "Ha"  )), eval(D(expr, "Hl" )),
					eval(D(expr, "Tl" )), eval(D(expr, "Hh" )), eval(D(expr, "Th" )))
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,p=p,stderro=stderro))
	}


 	if(model==12) {
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]+273.15
			y<-datashap[,2]
		}else{
			x<-datao[,1]+273.15
			y<-datao[,2]
		}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x, Ha, Hl,Tl,Hh,Th){
			expr <- expression(((coefi[1]+coefi[2]*298.16) * (x/298.16) * exp((Ha/1.987) * ((1/298.16) - (1/x))))/
							(1 + exp((Hl/1.987) * ((1/Tl) - (1/x))) + exp((Hh/1.987) * ((1/Th) - (1/x)))))
			eval(expr)
		}
		j <- function(x, Ha, Hl,Tl,Hh,Th){
			expr <- expression(((coefi[1]+coefi[2]*298.16) * (x/298.16) * exp((Ha/1.987) * ((1/298.16) - (1/x))))/
							(1 + exp((Hl/1.987) * ((1/Tl) - (1/x))) + exp((Hh/1.987) * ((1/Th) - (1/x)))))
			c(eval(D(expr, "Ha"  )), eval(D(expr, "Hl" )),
					eval(D(expr, "Tl" )), eval(D(expr, "Hh" )), eval(D(expr, "Th" )))
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,p=(coefi[1]+coefi[2]*298.16),stderro=stderro))
	}



  	if(model==13) {
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]+273.15
			y<-datashap[,2]
		}else{
			x<-datao[,1]+273.15
			y<-datao[,2]
		}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x,p,Ha, Hl,Tl){

			expr <- expression((p * (x/298.16) * exp((Ha/1.987) * ((1/298.16) - (1/x))))/
							(1 + exp((Hl/1.987) * ((1/Tl) - (1/x)))))
			eval(expr)
		}
		j <- function(x,p,Ha, Hl,Tl){
			expr <- expression((p * (x/298.16) * exp((Ha/1.987) * ((1/298.16) - (1/x))))/
							(1 + exp((Hl/1.987) * ((1/Tl) - (1/x)))))
			c(eval(D(expr, "p")),eval(D(expr, "Ha"  )), eval(D(expr, "Hl" )),
					eval(D(expr, "Tl" )))
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,p=p,stderro=stderro))
	}

  	if(model==14) {
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]+273.15
			y<-datashap[,2]
		}else{
			x<-datao[,1]+273.15
			y<-datao[,2]
		}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x,p,Ha, Hh,Th){

			expr <- expression((p * (x/298.16) * exp((Ha/1.987) * ((1/298.16) - (1/x))))/
							(1 + exp((Hh/1.987) * ((1/Th) - (1/x)))))
			eval(expr)
		}
		j <- function(x,p,Ha, Hh,Th){
			expr <- expression((p * (x/298.16) * exp((Ha/1.987) * ((1/298.16) - (1/x))))/
							(1 + exp((Hh/1.987) * ((1/Th) - (1/x)))))
			c(eval(D(expr, "p")),eval(D(expr, "Ha"  )), eval(D(expr, "Hh" )), eval(D(expr, "Th" )))
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))                                    
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,p=p,stderro=stderro))
	}


	################## fin nuevos modelos de sharpe de michelle


	##  Lineal
	if(model==24)  {
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]
			y<-datashap[,2]}else{
			x<-datao[,1]
			y<-datao[,2]}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x,Inter,Slop){
			expr <- expression(Inter + Slop*x)
			eval(expr)
		}
		j <- function(x,Inter,Slop){
			expr <- expression(Inter + Slop*x)
			c(eval(D(expr, "Inter")),eval(D(expr, "Slop")))
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,stderro=stderro))
	}

  ##  exponencial simple
	if(model==25)  {
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]
			y<-datashap[,2]}else{
			x<-datao[,1]
			y<-datao[,2]}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x,b1,b2){
			expr <- expression(b1*exp(b2*x))
			eval(expr)
		}
		j <- function(x,b1,b2){
			expr <- expression(b1*exp(b2*x))
			c(eval(D(expr, "b1")),eval(D(expr, "b2")))
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,stderro=stderro))
	}

	##  Tb Model (Logan)
	if(model==26)  {
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]
			y<-datashap[,2]}else{
			x<-datao[,1]
			y<-datao[,2]}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x,sy,b,Tb,DTb){
			expr <- expression(sy*exp(b*(x-Tb)-exp(b*(x-Tb)/DTb)))
			eval(expr)
		}
		j <- function(x,sy,b,Tb,DTb){
			expr <- expression(sy*exp(b*(x-Tb)-exp(b*(x-Tb)/DTb)))
			c(eval(D(expr, "sy")),eval(D(expr, "b")),eval(D(expr, "Tb")),eval(D(expr, "DTb")))
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,stderro=stderro))
	}

	##  Exponential Model (Logan)
	if(model==27)  {
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]
			y<-datashap[,2]}else{
			x<-datao[,1]
			y<-datao[,2]}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x,sy,b,Tb){
			expr <- expression(sy*exp(b*(x-Tb)))
			eval(expr)
		}
		j <- function(x,sy,b,Tb){
			expr <- expression(sy*exp(b*(x-Tb)))
			c(eval(D(expr, "sy")),eval(D(expr, "b")),eval(D(expr, "Tb")))
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,stderro=stderro))
	}

  ##  Exponential Tb (Logan)
	if(model==28){
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]
			y<-datashap[,2]}else{
			x<-datao[,1]
			y<-datao[,2]}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x,b,Tmin){
			expr <- expression(exp(b*(x-Tmin))-1)
			eval(expr)
		}
		j <- function(x,b,Tmin){
			expr <- expression(exp(b*(x-Tmin))-1)
			c(eval(D(expr, "b")),eval(D(expr, "Tmin")))
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,stderro=stderro))
	}

	##  Square root model of Ratkowsky
	if(model==29){
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]
			y<-datashap[,2]}else{
			x<-datao[,1]
			y<-datao[,2]}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x,b,Tb){
			expr <- expression(b*(x-Tb)^2)
			eval(expr)
		}
		j <- function(x,b,Tb){
			expr <- expression(b*(x-Tb)^2)
			c(eval(D(expr, "b")),eval(D(expr, "Tb")))
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,stderro=stderro))
	}

	#Davidson
	if(model==30){
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]
			y<-datashap[,2]}else{
			x<-datao[,1]
			y<-datao[,2]}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x,k,a,b){
			expr <- expression(k/(1+exp(a-b*x))) #DM: Se cambio "+b*x" por "-b*x"
			eval(expr)
		}
		j <- function(x,k,a,b){
			expr <- expression(k/(1+exp(a-b*x))) #DM: Se cambio "+b*x" por "-b*x"
			c(eval(D(expr, "k")),eval(D(expr, "a")),eval(D(expr, "b")))
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,stderro=stderro))
	}


	#Pradham - 1
	if(model==31){
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]
			y<-datashap[,2]}else{
			x<-datao[,1]
			y<-datao[,2]}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x,R,Tm,To){
			expr <- expression(R*exp((-1/2)*((x-Tm)/To))^2) #DM: Se agrego "^2"
			eval(expr)
		}
		j <- function(x,R,Tm,To){
			expr <- expression(R*exp((-1/2)*((x-Tm)/To))^2) #DM: Se agrego "^2"
			c(eval(D(expr, "R")),eval(D(expr, "Tm")),eval(D(expr, "To")))
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,stderro=stderro))
	}

	#Angilletta Jr.
	if(model==32){
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]
			y<-datashap[,2]}else{
			x<-datao[,1]
			y<-datao[,2]}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x,a,b,c,d){
			expr <- expression(a*exp((-1/2)*(abs(x-b)/c)^d)) #DM: Se cambio "abs((x-b)/c)" por "abs(x-b)/c"
			eval(expr)
		}
		j <- function(x,a,b,c,d){
			expr <- expression(a*exp((-1/2)*(abs(x-b)/c)^d)) #DM: Se cambio "abs((x-b)/c)" por "abs(x-b)/c"
			c(eval(D(expr, "a")),eval(D(expr, "b")),eval(D(expr, "c")),eval(D(expr, "d")))
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,stderro=stderro))
	}

	#Stinner 2
	if(model==33){
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]
			y<-datashap[,2]}else{
			x<-datao[,1]
			y<-datao[,2]}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x,Rmax,k1,k2,Topc){
			expr <- expression(Rmax*(exp(k1+k2*Topc))/(1+exp(k1+k2*x)))
			eval(expr)
		}
		j <- function(x,Rmax,k1,k2,Topc){
			expr <- expression(Rmax*(exp(k1+k2*Topc))/(1+exp(k1+k2*x)))
			c(eval(D(expr, "Rmax")),eval(D(expr, "k1")),eval(D(expr, "k2")),eval(D(expr, "Topc")))
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,stderro=stderro))
	}

	#Hilbert
	if(model==34){
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]
			y<-datashap[,2]}else{
			x<-datao[,1]
			y<-datao[,2]}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x,Tb,Tmax,d,Y,v){
			expr <- expression(Y*((x-Tb)^2/((x-Tb)+d^2)-exp(-(Tmax-(x-Tb))/v))) #DM: Se quito el cuadrado en el denominador
			eval(expr)
		}
		j <- function(x,Tb,Tmax,d,Y,v){
			expr <- expression(Y*((x-Tb)^2/((x-Tb)+d^2)-exp(-(Tmax-(x-Tb))/v))) #DM: Se quito el cuadrado en el denominador
			c(eval(D(expr, "Tb")),eval(D(expr, "Tmax")),eval(D(expr, "d")),eval(D(expr, "Y")),eval(D(expr, "v")))
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,stderro=stderro))
	}
	#Lactin 2
	if(model==35){
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]
			y<-datashap[,2]}else{
			x<-datao[,1]
			y<-datao[,2]}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x,Tl, p, dt){
			expr <- expression(exp(p*x)-exp(p*Tl-(Tl-x)/dt)) #DM: Se omitio un menos "-(p" por "p"  y se invirtio "(x-Tl))/dt" por "(Tl-x)/dt"
			eval(expr)
		}
		j <- function(x,Tl, p, dt){
			expr <- expression(exp(p*x)-exp(p*Tl-(Tl-x)/dt)) #DM: Se omitio un menos "-(p" por "p"  y se invirtio "(x-Tl))/dt" por "(Tl-x)/dt"
			c(eval(D(expr, "Tl")),eval(D(expr, "p")),eval(D(expr, "dt")))
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,stderro=stderro))
	}


	#Kontodimas-3
	if(model==44){
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]
			y<-datashap[,2]}else{
			x<-datao[,1]
			y<-datao[,2]}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x,a1,b1,c1,d1,f1,g1){
			expr <- expression(x*exp(a1-b1/x)/(1 + exp(c1-d1/x) + exp(f1-g1/x)))
			eval(expr)
		}
		j <- function(x,a1,b1,c1,d1,f1,g1){
			expr <- expression(x*exp(a1-b1/x)/(1 + exp(c1-d1/x) + exp(f1-g1/x)))
			c(eval(D(expr, "a1")),eval(D(expr, "b1")),eval(D(expr, "c1")),eval(D(expr, "d1")),eval(D(expr, "f1")),eval(D(expr, "g1")))
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,stderro=stderro))
	}


	#Janish-1
	if(model==46){
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]
			y<-datashap[,2]}else{
			x<-datao[,1]
			y<-datao[,2]}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x,Dmin,Topt,K){
			expr <- expression(2/(Dmin*(exp(K*(x-Topt)) + exp((-K)*(x-Topt)))))
			eval(expr)
		}
		j <- function(x,Dmin,Topt,K){
			expr <- expression(2/(Dmin*(exp(K*(x-Topt)) + exp((-K)*(x-Topt)))))
			c(eval(D(expr, "Dmin")),eval(D(expr, "Topt")),eval(D(expr, "K")))
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,stderro=stderro))
	}

	#Janish-2
	if(model==47){
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]
			y<-datashap[,2]}else{
			x<-datao[,1]
			y<-datao[,2]}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x,c,a,b,Tm){
			expr <- expression(2*c/(a^(x-Tm) + b^(Tm-x)))
			eval(expr)
		}
		j <- function(x,c,a,b,Tm){
			expr <- expression(2*c/(a^(x-Tm) + b^(Tm-x)))
			c(eval(D(expr, "c")),eval(D(expr, "a")),eval(D(expr, "b")),eval(D(expr, "Tm")))
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,stderro=stderro))
	}

	#Tanigoshi
	if(model==48){
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]
			y<-datashap[,2]}else{
			x<-datao[,1]
			y<-datao[,2]}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x,a0,a1,a2,a3){
			expr <- expression(a0+a1*x+a2*x^2+a3*x^3)
			eval(expr)
		}
		j <- function(x,a0,a1,a2,a3){
			expr <- expression(a0+a1*x+a2*x^2+a3*x^3)
			c(eval(D(expr, "a0")),eval(D(expr, "a1")),eval(D(expr, "a2")),eval(D(expr, "a3")))
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,stderro=stderro))
	}

	#Wang-Lan-Ding
	if(model==49){
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]
			y<-datashap[,2]}else{
			x<-datao[,1]
			y<-datao[,2]}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x,k,a,b,c,Tmin,Tmax,r){
			expr <- expression(k*(1-exp((-a)*(x-Tmin)))*(1-exp(b*(x-Tmax)))/(1+exp((-r)*(x-c))))
			eval(expr)
		}
		j <- function(x,k,a,b,c,Tmin,Tmax,r){
			expr <- expression(k*(1-exp((-a)*(x-Tmin)))*(1-exp(b*(x-Tmax)))/(1+exp((-r)*(x-c))))
			c(eval(D(expr, "k")),eval(D(expr, "a")),eval(D(expr, "b")),eval(D(expr, "c")),eval(D(expr, "Tmin")),eval(D(expr, "Tmax")),eval(D(expr, "r")))
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,stderro=stderro))
	}


	## modelos adaptados para senescencia

	#Stinner-3
	if(model==50){
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]
			y<-datashap[,2]}else{
			x<-datao[,1]
			y<-datao[,2]}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x,c1,k1,k2){
			expr <- expression(c1/(1+exp(k1+k2*x)))
			eval(expr)
		}
		j <- function(x,c1,k1,k2){
			expr <- expression(c1/(1+exp(k1+k2*x)))
			c(eval(D(expr, "c1")),eval(D(expr, "k1")),eval(D(expr, "k2")))
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,stderro=stderro))
	}


	#Stinner-4
	if(model==51){
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]
			y<-datashap[,2]}else{
			x<-datao[,1]
			y<-datao[,2]}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x,c1,c2,k1,k2,To){
			expr <- expression(c1/(1+exp(k1+k2*x)) + c2/(1+exp(k1+k2*(2*To-x))))
			eval(expr)
		}
		j <- function(x,c1,c2,k1,k2,To){
			expr <- expression(c1/(1+exp(k1+k2*x)) + c2/(1+exp(k1+k2*(2*To-x))))
			c(eval(D(expr, "x")),eval(D(expr, "c1")),eval(D(expr, "c2")),eval(D(expr, "k1")),eval(D(expr, "k2")),eval(D(expr, "To")))
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,stderro=stderro))
	}

	#Logan-3
	if(model==52){
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]
			y<-datashap[,2]}else{
			x<-datao[,1]
			y<-datao[,2]}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x,sy,b,Tmin,Tmax,DTb){
			expr <- expression(sy*exp(b*(x-Tmin)-exp(b*Tmax - (Tmax-(x-Tmin))/DTb)))
			eval(expr)
		}
		j <- function(x,sy,b,Tmin,Tmax,DTb){
			expr <- expression(sy*exp(b*(x-Tmin)-exp(b*Tmax - (Tmax-(x-Tmin))/DTb)))
			c(eval(D(expr, "sy")),eval(D(expr, "b")),eval(D(expr, "Tmin")),eval(D(expr, "Tmax")),eval(D(expr, "DTb")))
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,stderro=stderro))
	}

	#Logan-4
	if(model==53){
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]
			y<-datashap[,2]}else{
			x<-datao[,1]
			y<-datao[,2]}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x,alph,k,b,Tmin,Tmax,Dt){
			expr <- expression(alph*(1/(1+k*exp(-b*(x-Tmin))) - exp(-(Tmax-(x-Tmin))/Dt)))
			eval(expr)
		}
		j <- function(x,alph,k,b,Tmin,Tmax,Dt){
			expr <- expression(alph*(1/(1+k*exp(-b*(x-Tmin))) - exp(-(Tmax-(x-Tmin))/Dt)))
			c(eval(D(expr, "alph")),eval(D(expr, "k")),eval(D(expr, "b")),eval(D(expr, "Tmin")),eval(D(expr, "Tmax")),eval(D(expr, "Dt")))
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,stderro=stderro))
	}

	#Logan-5
	if(model==54){
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]
			y<-datashap[,2]}else{
			x<-datao[,1]
			y<-datao[,2]}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x,alph,k,b,Tmax,Dt){
			expr <- expression(alph*(1/(1+k*exp(-b*x)) - exp(-(Tmax-x)/Dt)))
			eval(expr)
		}
		j <- function(x,alph,k,b,Tmax,Dt){
			expr <- expression(alph*(1/(1+k*exp(-b*x)) - exp(-(Tmax-x)/Dt)))
			c(eval(D(expr, "alph")),eval(D(expr, "k")),eval(D(expr, "b")),eval(D(expr, "Tmax")),eval(D(expr, "Dt")))
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,stderro=stderro))
	}

	#Hilber y logan 2
	if(model==55){
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]
			y<-datashap[,2]}else{
			x<-datao[,1]
			y<-datao[,2]}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x,trid,D,Tmax,Dt){
			expr <- expression(trid*( (x^2)/(x^2+D)  - exp(-(Tmax-x)/Dt))) #Cambio de "x/Dt" por "x)/Dt"
			eval(expr)
		}
		j <- function(x,trid,D,Tmax,Dt){
			expr <- expression(trid*( (x^2)/(x^2+D)  - exp(-(Tmax-x)/Dt))) #Cambio de "x/Dt" por "x)/Dt"
			c(eval(D(expr, "trid")),eval(D(expr, "Tmax")),eval(D(expr, "Dt")))
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,stderro=stderro))
	}

	#Hilber y logan 3
	if(model==56){
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]
			y<-datashap[,2]}else{
			x<-datao[,1]
			y<-datao[,2]}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x,trid,Tmax,Tmin,D,Dt,Smin){
			expr <- expression(trid*(((x-Tmin)^2)/((x-Tmin)^2 + D) - exp(-(Tmax-(x-Tmin))/Dt)) + Smin)
			eval(expr)
		}
		j <- function(x,trid,Tmax,Tmin,D,Dt,Smin){
			expr <- expression(trid*(((x-Tmin)^2)/((x-Tmin)^2 + D) - exp(-(Tmax-(x-Tmin))/Dt)) + Smin)
			c(eval(D(expr, "trid")),eval(D(expr, "Tmax")),eval(D(expr, "Tmin")),eval(D(expr, "D")),eval(D(expr, "Dt")),eval(D(expr, "Smin")))
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,stderro=stderro))
	}

	#Taylor
	if(model==57){
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]
			y<-datashap[,2]}else{
			x<-datao[,1]
			y<-datao[,2]}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x,rm,Topt,Troh){ #DM: se quito Smin
			expr <- expression(rm*exp(-(0.5)*(-(x-Topt)/Troh)^2))
			eval(expr)
		}
		j <- function(x,rm,Topt,Troh){ #DM: se quito Smin
			expr <- expression(rm*exp(-(0.5)*(-(x-Topt)/Troh)^2))
			c(eval(D(expr, "rm")),eval(D(expr, "Topt")),eval(D(expr, "Troh"))) #DM: se quito Smin
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,stderro=stderro))
	}

	#Lactin 3
	if(model==58){
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]
			y<-datashap[,2]}else{
			x<-datao[,1]
			y<-datao[,2]}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x,Tl, p, dt,lamb){
			expr <- expression(exp(p*x)-exp(p*Tl-(Tl-x)/dt) + lamb) #DM: Se cambió "(p*Tl-(Tl-x))/dt)" por "p*Tl-(Tl-x)/dt)"
			eval(expr)
		}
		j <- function(x,Tl, p, dt,lamb){
			expr <- expression(exp(p*x)-exp(p*Tl-(Tl-x)/dt) + lamb) #DM: Se cambió "(p*Tl-(Tl-x))/dt)" por "p*Tl-(Tl-x)/dt)"
			c(eval(D(expr, "Tl")),eval(D(expr, "p")),eval(D(expr, "Dt")),eval(D(expr, "lamb")))
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,stderro=stderro))
	}

	#Sigmoid or logistic
	if(model==59){
		if(length(datashap[,1])>(length(ini)+1)){
			x<-datashap[,1]
			y<-datashap[,2]}else{
			x<-datao[,1]
			y<-datao[,2]}
		ini<-as.list(ini)
		ind <- as.list(ini)
		for (i in names(ind)){
			temp <- ini[[i]]
			storage.mode(temp) <- "double"
			assign(i, temp)
		}
		f <- function(x,c1,a,b){
			expr <- expression(c1/(1+exp(a+b*x)))
			eval(expr)
		}
		j <- function(x,c1,a,b){
			expr <- expression(c1/(1+exp(a+b*x)))
			c(eval(D(expr, "c1")),eval(D(expr, "a")),eval(D(expr, "b")))
		}
		fcn <- function(ini, x, y, fcall, jcall)
			(y - do.call("fcall", c(list(x = x), as.list(ini))))
		out <- nls.lm(par = ini, fn = fcn,fcall = f, jcall = j,x = x, y = y)
		stderro<-diag(ginv(out$hessian))
		estimate<-as.data.frame(out$par)
		return(list(estimados=estimate,f=f,stderro=stderro))
	}



}

#############################################
#############################################
#############################################
modelcomp<-function(test,model,matri,parametros,shap.estimados,coefi){
	slope<-parametros[length(parametros)]
	est1<-cbind(shap.estimados)
	est1<-as.list(est1)
	for (i in names(est1)) {
		temp <- est1[[i]]
		storage.mode(temp) <- "double"
		assign(i, temp)
	}
	kelvin<-matri[,1]+273.15
	celsius<-matri[,1]
	time<-matri[,2]
	des1<-matri[,5]/matri[,6]
	inteest1<-parametros[1:(length(parametros)-1)]
	slop<-parametros[length(parametros)]
	if(test=="logit"){
		
		if(model==11){
			meta <- function(celsius,time,Tmin,b)
			{
				expr <- expression(1/(1+exp(-(log(time*b*(celsius-Tmin))*slope))))
				eval(expr)
			}
			jmeta<- function(celsius,time,Tmin,b)
			{
				expr <- expression(1/(1+exp(-(log(time*b*(celsius-Tmin))*slope))))
				c(eval(D(expr, "Tmin" )), eval(D(expr, "b" )))
			}
			fcn     <- function(est1, celsius,time, des1, fcall, jcall)
				(des1 - do.call("fcall", c(list(celsius = celsius,time=time), as.list(est1))))
			metamodel <- nls.lm(par = est1, fn = fcn,fcall = meta, jcall = jmeta,celsius = celsius,time=time,des1 = des1)
		}
		
		if(model==12)
		{
			meta <- function(celsius,time,b1,b2,b3,b4,b5)
			{
				expr <- expression(1/(1+exp(-(log(time*(b1*10^(((((celsius-b3)/(b3-b2)-(1/(1+0.28*b4+0.72*log(1+b4))))+exp(b4*((celsius-b3)/(b3-b2)-(1/(1+0.28*b4+0.72*log(1+b4))))))/((1+b4)/(1+1.5*b4+0.39*b4^2)))^2)*(1-b5+b5*((((celsius-b3)/(b3-b2)- (1/(1+0.28*b4+0.72*log(1+b4))))+exp(b4*((celsius-b3)/(b3-b2)- (1/(1+0.28*b4+0.72*log(1+b4))))))/((1+b4)/(1+1.5*b4+0.39*b4^2)))^2)))*slope))))
				eval(expr)
			}
			jmeta<- function(celsius,time,b1,b2,b3,b4,b5)
			{
				expr <- expression(1/(1+exp(-(log(time*(b1*10^(((((celsius-b3)/(b3-b2)-(1/(1+0.28*b4+0.72*log(1+b4))))+exp(b4*((celsius-b3)/(b3-b2)-(1/(1+0.28*b4+0.72*log(1+b4))))))/((1+b4)/(1+1.5*b4+0.39*b4^2)))^2)*(1-b5+b5*((((celsius-b3)/(b3-b2)- (1/(1+0.28*b4+0.72*log(1+b4))))+exp(b4*((celsius-b3)/(b3-b2)- (1/(1+0.28*b4+0.72*log(1+b4))))))/((1+b4)/(1+1.5*b4+0.39*b4^2)))^2)))*slope))))
				c(eval(D(expr, "b1")), eval(D(expr, "b2")), eval(D(expr, "b3"  )), eval(D(expr, "b4" )),
						eval(D(expr, "b5" )))
			}
			fcn     <- function(est1, celsius,time, des1, fcall, jcall)
				(des1 - do.call("fcall", c(list(celsius = celsius,time=time), as.list(est1))))
			metamodel <- nls.lm(par = est1, fn = fcn,
					fcall = meta, jcall = jmeta,
					celsius = celsius,time=time,des1 = des1)
		}
		if(model==13)
		{
			meta <- function(celsius,time,Y,Tmax, p, v)
			{
				expr <- expression(1/(1+exp(-(log(time*(Y*(exp(p*celsius)-exp(p*Tmax-(Tmax-celsius)/v))))*slope))))
				eval(expr)
			}
			jmeta<- function(celsius,time,Y,Tmax, p, v)
			{
				expr <- expression(1/(1+exp(-(log(time*(Y*(exp(p*celsius)-exp(p*Tmax-(Tmax-celsius)/v))))*slope))))
				c(eval(D(expr, "Y")),eval(D(expr, "Tmax")), eval(D(expr, "p")), eval(D(expr, "v")))
			}
			fcn     <- function(est1, celsius,time, des1, fcall, jcall)
				(des1 - do.call("fcall", c(list(celsius = celsius,time=time), as.list(est1))))
			metamodel <- nls.lm(par = est1, fn = fcn,
					fcall = meta, jcall = jmeta,
					celsius = celsius,time=time,des1 = des1)
		}
		if(model==14)
		{
			meta <- function(celsius,time,alfa,k,Tmax, p, v)
			{
				expr <- expression(1/(1+exp(-(log(time*(alfa*((1/(1+k*exp(-p*celsius)))-exp(-(Tmax-celsius)/v))))*slope))))
				eval(expr)
			}
			jmeta<- function(celsius,time,alfa,k,Tmax, p, v)
			{
				expr <- expression(1/(1+exp(-(log(time*(alfa*((1/(1+k*exp(-p*celsius)))-exp(-(Tmax-celsius)/v))))*slope))))
				c(eval(D(expr, "alfa")),eval(D(expr, "k")),eval(D(expr, "Tmax")), eval(D(expr, "p")), eval(D(expr, "v")))
			}
			fcn     <- function(est1, celsius,time, des1, fcall, jcall)
				(des1 - do.call("fcall", c(list(celsius = celsius,time=time), as.list(est1))))
			metamodel <- nls.lm(par = est1, fn = fcn,
					fcall = meta, jcall = jmeta,
					celsius = celsius,time=time,des1 = des1)
		}
		
	}
	if(test=="probit"){
		
		if(model==11){
			meta <- function(celsius,time,Tmin,b)
			{
				expr <- expression(pnorm(log(time*b*(celsius-Tmin))*slope))
				eval(expr)
			}
			jmeta<- function(celsius,time,Tmin,b)
			{
				expr <- expression(pnorm(log(time*b*(celsius-Tmin))*slope))
				c(eval(D(expr, "Tmin" )), eval(D(expr, "b" )))
			}
			fcn     <- function(est1, celsius,time, des1, fcall, jcall)
				(des1 - do.call("fcall", c(list(celsius = celsius,time=time), as.list(est1))))
			metamodel <- nls.lm(par = est1, fn = fcn,
					fcall = meta, jcall = jmeta,
					celsius = celsius,time=time,des1 = des1)
		}
		
		if(model==12){
			meta <- function(celsius,time,b1,b2,b3,b4,b5)
			{
				expr <- expression(pnorm(log(time*(b1*10^(((((celsius-b3)/(b3-b2)-(1/(1+0.28*b4+0.72*log(1+b4))))+exp(b4*((celsius-b3)/(b3-b2)-(1/(1+0.28*b4+0.72*log(1+b4))))))/((1+b4)/(1+1.5*b4+0.39*b4^2)))^2)*(1-b5+b5*((((celsius-b3)/(b3-b2)- (1/(1+0.28*b4+0.72*log(1+b4))))+exp(b4*((celsius-b3)/(b3-b2)- (1/(1+0.28*b4+0.72*log(1+b4))))))/((1+b4)/(1+1.5*b4+0.39*b4^2)))^2)))*slope))
				eval(expr)
			}
			jmeta<- function(celsius,time,b1,b2,b3,b4,b5)
			{
				expr <- expression(pnorm(log(time*(b1*10^(((((celsius-b3)/(b3-b2)-(1/(1+0.28*b4+0.72*log(1+b4))))+exp(b4*((celsius-b3)/(b3-b2)-(1/(1+0.28*b4+0.72*log(1+b4))))))/((1+b4)/(1+1.5*b4+0.39*b4^2)))^2)*(1-b5+b5*((((celsius-b3)/(b3-b2)- (1/(1+0.28*b4+0.72*log(1+b4))))+exp(b4*((celsius-b3)/(b3-b2)- (1/(1+0.28*b4+0.72*log(1+b4))))))/((1+b4)/(1+1.5*b4+0.39*b4^2)))^2)))*slope))
				c(eval(D(expr, "b1")), eval(D(expr, "b2")), eval(D(expr, "b3"  )), eval(D(expr, "b4" )),
						eval(D(expr, "b5" )))
			}
			fcn     <- function(est1, celsius,time, des1, fcall, jcall)
				(des1 - do.call("fcall", c(list(celsius = celsius,time=time), as.list(est1))))
			metamodel <- nls.lm(par = est1, fn = fcn,
					fcall = meta, jcall = jmeta,
					celsius = celsius,time=time,des1 = des1)
		}
		
		if(model==13){
			meta <- function(celsius,time,Y,Tmax, p, v)
			{
				expr <- expression(pnorm(log(time*(Y*(exp(p*celsius)-exp(p*Tmax-(Tmax-celsius)/v))))*slope))
				eval(expr)
			}
			jmeta<- function(celsius,time,Y,Tmax, p, v)
			{
				expr <- expression(pnorm(log(time*(Y*(exp(p*celsius)-exp(p*Tmax-(Tmax-celsius)/v))))*slope))
				c(eval(D(expr, "Y")),eval(D(expr, "Tmax")), eval(D(expr, "p")), eval(D(expr, "v")))
			}
			fcn     <- function(est1, celsius,time, des1, fcall, jcall)
				(des1 - do.call("fcall", c(list(celsius = celsius,time=time), as.list(est1))))
			metamodel <- nls.lm(par = est1, fn = fcn,
					fcall = meta, jcall = jmeta,
					celsius = celsius,time=time,des1 = des1)
		}
		if(model==14){
			meta <- function(celsius,time,alfa,k,Tmax, p, v)
			{
				expr <- expression(pnorm(log(time*(alfa*((1/(1+k*exp(-p*celsius)))-exp(-(Tmax-celsius)/v))))*slope))
				eval(expr)
			}
			jmeta<- function(celsius,time,alfa,k,Tmax, p, v)
			{
				expr <- expression(pnorm(log(time*(alfa*((1/(1+k*exp(-p*celsius)))-exp(-(Tmax-celsius)/v))))*slope))
				c(eval(D(expr, "alfa")),eval(D(expr, "k")),eval(D(expr, "Tmax")), eval(D(expr, "p")), eval(D(expr, "v")))
			}
			fcn     <- function(est1, celsius,time, des1, fcall, jcall)
				(des1 - do.call("fcall", c(list(celsius = celsius,time=time), as.list(est1))))
			metamodel <- nls.lm(par = est1, fn = fcn,
					fcall = meta, jcall = jmeta,
					celsius = celsius,time=time,des1 = des1)
		}
		
	}
	if(test=="cloglog"){
		
		if(model==11){
			meta <- function(celsius,time,Tmin,b)
			{
				expr <- expression(1-exp(-exp(-log(-log(0.5)))+((log(time*b*(celsius-Tmin))*slope))))
				eval(expr)
			}
			jmeta<- function(celsius,time,Tmin,b)
			{
				expr <- expression(1-exp(-exp(-log(-log(0.5)))+((log(time*b*(celsius-Tmin))*slope))))
				c(eval(D(expr, "Tmin" )), eval(D(expr, "b" )))
			}
			fcn     <- function(est1, celsius,time, des1, fcall, jcall)
				(des1 - do.call("fcall", c(list(celsius = celsius,time=time), as.list(est1))))
			metamodel <- nls.lm(par = est1, fn = fcn,
					fcall = meta, jcall = jmeta,
					celsius = celsius,time=time,des1 = des1)
		}
		if(model==12){
			meta <- function(celsius,time,b1,b2,b3,b4,b5)
			{
				expr <- expression(1-exp(-exp(-log(-log(0.5)))+((log(time*(b1*10^(((((celsius-b3)/(b3-b2)-(1/(1+0.28*b4+0.72*log(1+b4))))+exp(b4*((celsius-b3)/(b3-b2)-(1/(1+0.28*b4+0.72*log(1+b4))))))/((1+b4)/(1+1.5*b4+0.39*b4^2)))^2)*(1-b5+b5*((((celsius-b3)/(b3-b2)- (1/(1+0.28*b4+0.72*log(1+b4))))+exp(b4*((celsius-b3)/(b3-b2)- (1/(1+0.28*b4+0.72*log(1+b4))))))/((1+b4)/(1+1.5*b4+0.39*b4^2)))^2)))*slope))))
				eval(expr)
			}
			jmeta<- function(celsius,time,b1,b2,b3,b4,b5)
			{
				expr <- expression(1-exp(-exp(-log(-log(0.5)))+((log(time*(b1*10^(((((celsius-b3)/(b3-b2)-(1/(1+0.28*b4+0.72*log(1+b4))))+exp(b4*((celsius-b3)/(b3-b2)-(1/(1+0.28*b4+0.72*log(1+b4))))))/((1+b4)/(1+1.5*b4+0.39*b4^2)))^2)*(1-b5+b5*((((celsius-b3)/(b3-b2)- (1/(1+0.28*b4+0.72*log(1+b4))))+exp(b4*((celsius-b3)/(b3-b2)- (1/(1+0.28*b4+0.72*log(1+b4))))))/((1+b4)/(1+1.5*b4+0.39*b4^2)))^2)))*slope))))
				c(eval(D(expr, "b1")), eval(D(expr, "b2")), eval(D(expr, "b3"  )), eval(D(expr, "b4" )),
						eval(D(expr, "b5" )))
			}
			fcn     <- function(est1, celsius,time, des1, fcall, jcall)
				(des1 - do.call("fcall", c(list(celsius = celsius,time=time), as.list(est1))))
			metamodel <- nls.lm(par = est1, fn = fcn,
					fcall = meta, jcall = jmeta,
					celsius = celsius,time=time,des1 = des1)
		}
		if(model==13){
			meta <- function(celsius,time,Y,Tmax, p, v)
			{
				expr <- expression(1-exp(-exp(-log(-log(0.5)))+((log(time*(Y*(exp(p*celsius)-exp(p*Tmax-(Tmax-celsius)/v))))*slope))))
				eval(expr)
			}
			jmeta<- function(celsius,time,Y,Tmax, p, v)
			{
				expr <- expression(1-exp(-exp(-log(-log(0.5)))+((log(time*(Y*(exp(p*celsius)-exp(p*Tmax-(Tmax-celsius)/v))))*slope))))
				c(eval(D(expr, "Y")),eval(D(expr, "Tmax")), eval(D(expr, "p")), eval(D(expr, "v")))
			}
			fcn     <- function(est1, celsius,time, des1, fcall, jcall)
				(des1 - do.call("fcall", c(list(celsius = celsius,time=time), as.list(est1))))
			metamodel <- nls.lm(par = est1, fn = fcn,
					fcall = meta, jcall = jmeta,
					celsius = celsius,time=time,des1 = des1)
		}
		if(model==14){
			meta <- function(celsius,time,alfa,k,Tmax, p, v)
			{
				expr <- expression(1-exp(-exp(-log(-log(0.5)))+((log(time*(alfa*((1/(1+k*exp(-p*celsius)))-exp(-(Tmax-celsius)/v))))*slope))))
				eval(expr)
			}
			jmeta<- function(celsius,time,alfa,k,Tmax, p, v)
			{
				expr <- expression(1-exp(-exp(-log(-log(0.5)))+((log(time*(alfa*((1/(1+k*exp(-p*celsius)))-exp(-(Tmax-celsius)/v))))*slope))))
				c(eval(D(expr, "alfa")),eval(D(expr, "k")),eval(D(expr, "Tmax")), eval(D(expr, "p")), eval(D(expr, "v")))
			}
			fcn     <- function(est1, celsius,time, des1, fcall, jcall)
				(des1 - do.call("fcall", c(list(celsius = celsius,time=time), as.list(est1))))
			metamodel <- nls.lm(par = est1, fn = fcn,
					fcall = meta, jcall = jmeta,
					celsius = celsius,time=time,des1 = des1)
		}

	}
	stderro<-diag(ginv(metamodel$hessian))
	estimate<-as.data.frame(metamodel$par)
	slope<-slope
	estimados<-as.data.frame(metamodel$par)
	meta<-meta
	ind <- as.list(estimate)
	for (i in names(ind))
	{
		temp <- ind[[i]]
		storage.mode(temp) <- "double"
		assign(i, temp)
	}
	
	if(model==11 || model==12 || model==13 || model==14)
	{salida<-list(estimados=estimate,slope=slope,estmshape=estimados,meta=meta,stderro=stderro)}

	return(salida)
}

#################################################
#################################################
#################################################
grafshape<-function(model,estshap,datashap,qtt,sdli,corrx=NULL,corry=NULL,mini,maxi,coefi,limit,tam,labx=NULL,laby=NULL, titulo=NULL,grises=FALSE, scaleY, scaleX,est, estadios){
	if(grises==TRUE){ccol=c("gray20","gray30")}else{ccol=c(4,2)}

	data<-datashap

	estshap<-as.list(estshap)

	finalshap<-estshap

	for (i in names(finalshap)) {

		temp <- finalshap[[i]]

		storage.mode(temp) <- "double"

		assign(i, temp)

	}



	xl<-seq(mini,maxi,length=200)


	## nuevos modelos sharpe

	if(model==11){

		xl<-xl+273.15

		expre<-expression((p * (xl/298.16) * exp((Ha/1.987) * ((1/298.16) - (1/xl))))/(1 + exp((Hl/1.987) * ((1/Tl) - (1/xl))) + exp((Hh/1.987) * ((1/Th) - (1/xl)))))

		yl<-eval(expre)

	}


	if(model==12){

		xl<-xl+273.15

		expre<-expression(((coefi[1]+coefi[2]*298.16) * (xl/298.16) * exp((Ha/1.987) * ((1/298.16) - (1/xl))))/(1 + exp((Hl/1.987) * ((1/Tl) - (1/xl))) + exp((Hh/1.987) * ((1/Th) - (1/xl)))))

		yl<-eval(expre)

	}

	if(model==13){

		xl<-xl+273.15

		expre<-expression((p * (xl/298.16) * exp((Ha/1.987) * ((1/298.16) - (1/xl))))/(1 + exp((Hl/1.987) * ((1/Tl) - (1/xl)))))

		yl<-eval(expre)

	}

	if(model==14){

		xl<-xl+273.15

		expre<-expression((p * (xl/298.16) * exp((Ha/1.987) * ((1/298.16) - (1/xl))))/(1 + exp((Hh/1.987) * ((1/Th) - (1/xl)))))

		yl<-eval(expre)

	}




	## fin de nuevos modelos sharpe


	if(model==24) {

		expre<-expression(Inter + Slop*xl)

		yl<-eval(expre)

	}



	if(model==25) {

		expre<-expression(b1*exp(b2*xl))

		yl<-eval(expre)

	}



	if(model==26) {

		expre<-expression(sy*exp(b*(xl-Tb)-exp(b*(xl-Tb)/DTb)))

		yl<-eval(expre)

	}



	if(model==27) {

		expre<-expression(sy*exp(b*(xl-Tb)))

		yl<-eval(expre)

	}



	if(model==28) {

		expre<-expression(exp(b*(xl-Tmin))-1)

		yl<-eval(expre)

	}



	if(model==29) {

		expre<-expression(b*(xl-Tb)^2)

		yl<-eval(expre)

	}



	if(model==30) {

		expre<-expression(k/(1+exp(a-b*xl))) #DM: Se cambio "+b*xl" por "-b*xl"

		yl<-eval(expre)

	}

	if(model==31) {

		expre<-expression(R*exp((-1/2)*((xl-Tm)/To))^2) #DM: Se agrego "^2"

		yl<-eval(expre)

	}

	if(model==32) {

		expre<-expression(a*exp((-1/2)*(abs(xl-b)/c)^d)) #DM: Se cambio "abs((x-b)/c)" por "abs(x-b)/c"

		yl<-eval(expre)

	}

	if(model==33) {

		expre<-expression(Rmax*(exp(k1+k2*Topc))/(1+exp(k1+k2*xl)))

		yl<-eval(expre)

	}

	if(model==34) {

		expre<-expression(Y*((xl-Tb)^2/((xl-Tb)+d^2)-exp(-(Tmax-(xl-Tb))/v))) #DM: Se quito al cuadrado en el denominador

		yl<-eval(expre)

	}

	if(model==35) {

		expre<-expression(exp(p*xl)-exp(p*Tl-(Tl-xl)/dt)) #DM: Se omitio un menos "-(p" por "p"  y se invirtio "(x-Tl))/dt" por "(Tl-x)/dt"

		yl<-eval(expre)

	}

	if(model==44) {

		expre<-expression(xl*exp(a1-b1/xl)/(1 + exp(c1-d1/xl) + exp(f1-g1/xl)))

		yl<-eval(expre)

	}

	if(model==46) {

		expre<-expression(2/(Dmin*(exp(K*(xl-Topt)) + exp((-K)*(xl-Topt)))))

		yl<-eval(expre)

	}

	if(model==47) {

		expre<-expression(2*c/(a^(xl-Tm) + b^(Tm-xl)))

		yl<-eval(expre)

	}

	if(model==48) {

		expre<-expression(a0+a1*xl+a2*xl^2+a3*xl^3)

		yl<-eval(expre)

	}

	if(model==49) {

		expre<-expression(k*(1-exp((-a)*(xl-Tmin)))*(1-exp(b*(xl-Tmax)))/(1+exp((-r)*(xl-c))))

		yl<-eval(expre)

	}


	## funciones adaptadas a senescencia

	if(model==50) {

		expre<-expression(c1/(1+exp(k1+k2*xl)))

		yl<-eval(expre)

	}

	if(model==51) {

		expre<-expression(c1/(1+exp(k1+k2*xl)) + c2/(1+exp(k1+k2*(2*To-xl))))

		yl<-eval(expre)

	}

	if(model==52) {

		expre<-expression(sy*exp(b*(xl-Tmin)-exp(b*Tmax - (Tmax-(xl-Tmin))/DTb)))

		yl<-eval(expre)

	}

	if(model==53) {

		expre<-expression(alph*(1/(1+k*exp(-b*(xl-Tmin))) - exp(-(Tmax-(xl-Tmin))/Dt)))

		yl<-eval(expre)

	}

	if(model==54) {

		expre<-expression(alph*(1/(1+k*exp(-b*xl)) - exp(-(Tmax-xl)/Dt)))

		yl<-eval(expre)

	}

	if(model==55) {

		expre<-expression(trid*( (xl^2)/(xl^2+D)  - exp(-(Tmax-xl)/Dt))) #Cambio de "x/Dt" por "x)/Dt"

		yl<-eval(expre)

	}

	if(model==56) {

		expre<-expression(trid*(((xl-Tmin)^2)/((xl-Tmin)^2 + D) - exp(-(Tmax-(xl-Tmin))/Dt)) + Smin)

		yl<-eval(expre)

	}

	if(model==57) {

		expre<-expression(rm*exp(-(0.5)*(-(xl-Topt)/Troh)^2))

		yl<-eval(expre)

	}

	if(model==58) {

		expre<-expression(exp(p*xl)-exp(p*Tl-(Tl-x)/dt) + lamb) #DM: Se cambió "(p*Tl-(Tl-x))/dt)" por "p*Tl-(Tl-x)/dt)"  

		yl<-eval(expre)

	}

	if(model==59) {

		expre<-expression(c1/(1+exp(a+b*xl)))

		yl<-eval(expre)

	}


	labx<-expression(paste("temperature (", degree, "C)"))
	laby<-"senescence (1/day)"
	MARGEN=c(5.2, 5, 5.5, 0.5)
	
	if(is.null(corrx)){corrx=c(min(data[,1],0),max(data[,1])+max(data[,1])*0.2);corrx2=seq(0,10*round(corrx[2]/10,1),5)}else{corrx2=seq(corrx[1],corrx[2],scaleX)} ## cambio

	if(is.null(corry)){corry=c(0,max(data[,2],yl)+max(data[,2])*0.2);corry2=seq(0,round(max(corry),1),0.1)}else{corry2=seq(corry[1],corry[2],scaleY)}              ## cambio

	par(mar=MARGEN,family="serif",font=1,cex.axis=1.8,cex.lab=1.8)
	
	plot(data[,1],data[,2],frame=F,col=ccol[1],pch=19,xlim=corrx,ylim=corry,xlab=labx,ylab=laby,axes=F,xaxt = "n", main=titulo) ## 4
	n2= length(data[,1])
	#posic <- (1:7)[max(dataa[,2])==dataa[,2]][1] # excluye el ultimo valor para la regresion
		#dataaa <- dataa[1:posic,] # excluye el ultimo valor para la regresion

	posic=(1:n2)[max(data[,2])==data[,2]][1]
	modr= lm(data[1:posic,2]~data[1:posic,1])
	if((1:length(estadios))[estadios==est]<length(estadios)-1){
  	abline(modr,col=ccol[2],lty=2,lwd=2)
	}
	axis(1, corrx2, cex.axis=1.6)  ## cambio
	axis(2, corry2,las=2, tck=-0.025, line=-1.0, cex.axis=1.6) ## cambio
	


	linf<-yl+sdli*qtt

	lsup<-yl-sdli*qtt

	if(limit=="yes"){

		if(model==11 || model==12 || model==13 || model==14){  ############ solo aumente el tipo de modelo

			lines(xl-273.15, yl, lwd=2,col=ccol[2]) ## 2

			lines(xl-273.15, linf, lwd=1,col=ccol[1],lty=2)

			lines(xl-273.15, lsup, lwd=1,col=ccol[1],lty=2)

		}else{

			lines(xl, yl, lwd=2,col=ccol[2]) ## 2

			lines(xl, linf, lwd=1,col=ccol[1],lty=2)

			lines(xl, lsup, lwd=1,col=ccol[1],lty=2)

		}}

	if(limit=="no") if(model==11 || model==12 || model==13 || model==14) lines(xl-273.15, yl, lwd=2,col=ccol[2]) else lines(xl, yl, lwd=2,col=ccol[2]) ## 2 ..... solo aumente el tipo de modelo

	arrows(data[,1],(data[,2]-(data[,2]-data[,3])), data[,1],(data[,2]+(data[,4]-data[,2])), length=0.1,angle=90, code=3,col=ccol[1]) ## 4

#	if(limit=="yes"){

#		if(model==1 || model==2) {

#			lines(xl-273.15, linf, lwd=1,col=ccol[1],lty=2) ## 4

#			lines(xl-273.15, lsup, lwd=1,col=ccol[1],lty=2) ## 4

#			yli<-coefi[1]+coefi[2]*(xl)

#			lines(xl-273.15, yl, lwd=2,col=ccol[2]) ## 2

#			#lines(xl-273.15, yli, col=ccol[2],lty=2,lwd=2) ## 2

#		}}

	if(limit=="no"){

		if(model==12 ) {

			yli<-coefi[1]+coefi[2]*(xl)

			lines(xl-273.15, yl, lwd=2,col=ccol[2]) ## 2

			#lines(xl-273.15, yli, col=ccol[2],lty=2,lwd=2) ## 2

		}}


	if(model==12) {
		if(model==12){To=298.16}
		points(To-273.15,coefi[1]+coefi[2]*To,pch=22,cex=2)

	}

}

#############################################
#############################################
#############################################
# calculos de p-values cuando la hipotesis es bilateral

pvalores<-function(v,x,estshap){pval=pt(v,length(x)-length(estshap));if(v<0){pval=2*pval}else{pval=2*(1-pval)};return(pval)}


#############################################
#############################################
#############################################
stat_develop<-function(model,datashap,datao,estshap,coefi,stderro){
		ecua=c("Sharpe & DeMichele 1","Sharpe & DeMichele 2","Sharpe & DeMichele 3","Sharpe & DeMichele 4","Sharpe & DeMichele 5","Sharpe & DeMichele 6","Sharpe & DeMichele 7","Sharpe & DeMichele 8","Sharpe & DeMichele 9","Sharpe & DeMichele 10","Sharpe & DeMichele 11","Sharpe & DeMichele 12","Sharpe & DeMichele 13","Sharpe & DeMichele 14","Deva 1","Deva no lineal o Higgis","Logan 1","Logan 2","Briere 1","Briere 2","Stinner","Hilber y logan","Latin 2 ","Linear","Exponential simple","Tb Model (Logan)","Exponential Model (Logan)","Exponential Tb (Logan)","Square root model of Ratkowsky","Davidson","Pradham - 1","Angilletta Jr.","Stinner 2","Hilbert","Lactin 2","Anlytis-1","Anlytis-2","Anlytis-3","Allahyari","Briere 3","Briere 4","Kontodimas-1","Kontodimas-2","Kontodimas-3","Ratkowsky 2","Janish-1 ","Janish-2","Tanigoshi","Wang-Lan-Ding","Stinner-3","Stinner-4","Logan-3","Logan-4","Logan-5","Hilber y logan 2","Hilber y logan 3","Taylor","Lactin 3","Sigmoid or Logistic")
                if(length(datashap[,1])>(length(estshap)+1)){
                                x<-datashap[,1]
                                y<-datashap[,2]
                    }else{
                                  x<-datao[,1]
                                  y<-datao[,2]
                    }
                ind <- as.list(estshap)
                for (i in names(ind))
                {
                                temp <- estshap[[i]]
                                storage.mode(temp) <- "double"
                                assign(i, temp)
                }

		## nuevos modelos sharpe


                if(model==11){
                                x<-x+273.15
				expre<-expression((p * (x/298.16) * exp((Ha/1.987) * ((1/298.16) - (1/x))))/(1 + exp((Hl/1.987) * ((1/Tl) - (1/x))) + exp((Hh/1.987) * ((1/Th) - (1/x)))))
                                yl<-eval(expre)
                                gg<-"r(T) = (p . (T/298.16) . exp((Ha/1.987) . ((1/298.16) - (1/T))))/(1 + exp((Hl/1.987) . ((1/Tl) - (1/T))) + exp((Hh/1.987) . ((1/Th) - (1/T))))"
                                ggg<-y ~ (p * (x/298.16) * exp((Ha/1.987) * ((1/298.16) - (1/x))))/(1 + exp((Hl/1.987) * ((1/Tl) - (1/x))) + exp((Hh/1.987) * ((1/Th) - (1/x))))
                }

                if(model==12){
                                x<-x+273.15
				expre<-expression(((coefi[1]+coefi[2]*298.16) * (x/298.16) * exp((Ha/1.987) * ((1/298.16) - (1/x))))/(1 + exp((Hl/1.987) * ((1/Tl) - (1/x))) + exp((Hh/1.987) * ((1/Th) - (1/x)))))
                                yl<-eval(expre)
                                gg<-"r(T) = (p . (T/298.16) . exp((Ha/1.987) . ((1/298.16) - (1/T))))/(1 + exp((Hl/1.987) . ((1/Tl) - (1/T))) + exp((Hh/1.987) . ((1/Th) - (1/T))))"
                                ggg<-y ~ (p * (x/298.16) * exp((Ha/1.987) * ((1/298.16) - (1/x))))/(1 + exp((Hl/1.987) * ((1/Tl) - (1/x))) + exp((Hh/1.987) * ((1/Th) - (1/x))))
                }


                if(model==13){
                                x<-x+273.15
				expre<-expression((p * (x/298.16) * exp((Ha/1.987) * ((1/298.16) - (1/x))))/(1 + exp((Hl/1.987) * ((1/Tl) - (1/x)))))
                                yl<-eval(expre)
                                gg<-"r(T) = (p . (T/298.16) . exp((Ha/1.987) . ((1/298.16) - (1/T))))/(1 + exp((Hl/1.987) . ((1/Tl) - (1/T))))"
                                ggg<-y ~ (p * (x/298.16) * exp((Ha/1.987) * ((1/298.16) - (1/x))))/(1 + exp((Hl/1.987) * ((1/Tl) - (1/x))))
                }


                if(model==14){
                                x<-x+273.15
				expre<-expression((p * (x/298.16) * exp((Ha/1.987) * ((1/298.16) - (1/x))))/(1 + exp((Hh/1.987) * ((1/Th) - (1/x)))))
                                yl<-eval(expre)
                                gg<-"r(T) = (p . (T/298.16) . exp((Ha/1.987) . ((1/298.16) - (1/T))))/(1 + exp((Hh/1.987) . ((1/Th) - (1/T))))"
                                ggg<-y ~ (p * (x/298.16) * exp((Ha/1.987) * ((1/298.16) - (1/x))))/(1 + exp((Hh/1.987) * ((1/Th) - (1/x))))
                }




		## fin de nuevos modelos sharpe



                if(model==24){
                                expre<-expression(Inter + Slop*x)
                                yl<-eval(expre)
                                gg<-"r(T) = Inter + Slop.T"
                }

                if(model==25){
                                expre<-expression(b1*exp(b2*x))
                                yl<-eval(expre)
                                gg<-"r(T) = b1.exp(b2.T)"
                }

                if(model==26){
                                expre<-expression(sy*exp(b*(x-Tb)-exp(b*(x-Tb)/DTb)))
                                yl<-eval(expre)
                                gg<-"r(T) = sy.exp(b.(T-Tb)-exp(b.(T-Tb)/DTb))"
                }

                if(model==27){
                                expre<-expression(sy*exp(b*(x-Tb)))
                                yl<-eval(expre)
                                gg<-"r(T) = sy.exp(b.(T-Tb))"
                }

                if(model==28){
                                expre<-expression(exp(b*(x-Tmin))-1)
                                yl<-eval(expre)
                                gg<-"r(T) = exp(b.(T-Tmin))-1"
                }

                if(model==29){
                                expre<-expression(b*(x-Tb)^2)
                                yl<-eval(expre)
                                gg<-"r(T) = b.(T-Tb)²"
                }


                if(model==30){                                      
                                expre<-expression(k/(1+exp(a-b*x))) #DM: Se cambio "+b*x" por "-b*x"
                                yl<-eval(expre)
                                gg<-"r(T) = k/(1+exp(a-b*T))"
                }

                if(model==31){
                                expre<-expression(R*exp((-1/2)*((x-Tm)/To))^2) #DM: Se agrego "^2"
                                yl<-eval(expre)
                                gg<-"r(T) = R.exp((-1/2).((T-Tm)/To))^2"
                }

                if(model==32){
                                expre<-expression(a*exp((-1/2)*(abs(x-b)/c)^d)) #DM: Se cambio "abs((x-b)/c)" por "abs(x-b)/c"
                                yl<-eval(expre)
                                gg<-"r(T) = a.exp((-1/2).(abs(T-b)/c)^d)"
                }

                if(model==33){
                                expre<-expression(Rmax*(exp(k1+k2*Topc))/(1+exp(k1+k2*x)))
                                yl<-eval(expre)
                                gg<-"r(T) = Rmax.(exp(k1+k2.Topc))/(1+exp(k1+k2.T)"
                }

                if(model==34){
                                expre<-expression(Y*((x-Tb)^2/((x-Tb)+d^2)-exp(-(Tmax-(x-Tb))/v))) #DM: Se quito el cuadrado en el denominador
                                yl<-eval(expre)
                                gg<-"r(T) = Y.((T-Tb)^2/((T-Tb)+d^2)-exp(-(Tmax-(T-Tb))/v))"
                }

                if(model==35){
                                expre<-expression(exp(p*x)-exp(p*Tl-(Tl-x)/dt)) #DM: Se omitio un menos "-(p" por "p"  y se invirtio "(x-Tl))/dt" por "(Tl-x)/dt"
                                yl<-eval(expre)
                                gg<-"r(T) = exp(p.T)-exp(p.Tl-(Tl-T)/dt)"
                }                                 

                if(model==44){
                                expre<-expression(x*exp(a1-b1/x)/(1 + exp(c1-d1/x) + exp(f1-g1/x)))
                                yl<-eval(expre)
                                gg<-"r(T) = T.exp(a1-b1/T)/(1 + exp(c1-d1/T) + exp(f1-g1/T))"
                }

                if(model==46){
                                expre<-expression(2/(Dmin*(exp(K*(x-Topt)) + exp((-K)*(x-Topt)))))
                                yl<-eval(expre)
                                gg<-"r(T) = 2/(Dmin.(exp(K.(T-Topt)) + exp((-K).(T-Topt))))"
                }

                if(model==47){
                                expre<-expression(2*c/(a^(x-Tm) + b^(Tm-x)))
                                yl<-eval(expre)
                                gg<-"r(T) = 2.c/(a^(T-Tm) + b^(Tm-T))"
                }

                if(model==48){
                                expre<-expression(a0+a1*x+a2*x^2+a3*x^3)
                                yl<-eval(expre)
                                gg<-"r(T) = a0+a1.T+a2.T^2+a3.T^3"
                }

                if(model==49){
                                expre<-expression(k*(1-exp((-a)*(x-Tmin)))*(1-exp(b*(x-Tmax)))/(1+exp((-r)*(x-c))))
                                yl<-eval(expre)
                                gg<-"r(T) = k.(1-exp((-a).(T-Tmin))).(1-exp(b.(T-Tmax)))/(1+exp((-r).(T-c)))"
                }

		## funciones adaptadas a la senescencia

                if(model==50){
                                expre<-expression(c1/(1+exp(k1+k2*x)))
                                yl<-eval(expre)
                                gg<-"r(T) = c1/(1+exp(k1+k2.x))"
                }

                if(model==51){
                                expre<-expression(c1/(1+exp(k1+k2*x)) + c2/(1+exp(k1+k2*(2*To-x))))
                                yl<-eval(expre)
                                gg<-"r(T) = c1/(1+exp(k1+k2.x)) + c2/(1+exp(k1+k2.(2.To-x)))"
                }

                if(model==52){
                                expre<-expression(sy*exp(b*(x-Tmin)-exp(b*Tmax - (Tmax-(x-Tmin))/DTb)))
                                yl<-eval(expre)
                                gg<-"r(T) = sy.exp(b.(x-Tmin)-exp(b.Tmax - (Tmax-(x-Tmin))/DTb))"
                }

                if(model==53){
                                expre<-expression(alph*(1/(1+k*exp(-b*(x-Tmin))) - exp(-(Tmax-(x-Tmin))/Dt)))
                                yl<-eval(expre)
                                gg<-"r(T) = alph.(1/(1+k.exp(-b*(x-Tmin))) - exp(-(Tmax-(x-Tmin))/Dt))"
                }

                if(model==54){
                                expre<-expression(alph*(1/(1+k*exp(-b*x)) - exp(-(Tmax-x)/Dt)))
                                yl<-eval(expre)
                                gg<-"r(T) = alph.(1/(1+k.exp(-b.x)) - exp(-(Tmax-x)/Dt))"
                }

                if(model==55){
                                expre<-expression(trid*( (x^2)/(x^2+D)  - exp(-(Tmax-x)/Dt))) #Cambio de "x/Dt" por "x)/Dt"
                                yl<-eval(expre)
                                gg<-"r(T) = trid.( (x²)/(x²+D)  - exp(-(Tmax-x)/Dt))"
                }

                if(model==56){
                                expre<-expression(trid*(((x-Tmin)^2)/((x-Tmin)^2 + D) - exp(-(Tmax-(x-Tmin))/Dt)) + Smin)
                                yl<-eval(expre)
                                gg<-"r(T) = trid.(((x-Tmin)²)/((x-Tmin)² + D) - exp(-(Tmax-(x-Tmin))/Dt)) + Smin"
                }

                if(model==57){
                                expre<-expression(rm*exp(-(0.5)*(-(x-Topt)/Troh)^2))
                                yl<-eval(expre)
                                gg<-"r(T) = rm.exp(-(0.5).(-(x-Topt)/Troh)²)"
                }

                if(model==58){
                                expre<-expression(exp(p*x)-exp(p*Tl-(Tl-x)/dt) + lamb) #DM: Se cambió "(p*Tl-(Tl-x))/dt)" por "p*Tl-(Tl-x)/dt)"
                                yl<-eval(expre)
                                gg<-"r(T) = exp(p.x)-exp(p.Tl-(Tl-x)/dt) + lamb"
                }

                if(model==59){
                                expre<-expression(c1/(1+exp(a+b*x)))
                                yl<-eval(expre)
                                gg<-"r(T) = c1/(1+exp(a+b.x))"
                }

                art=0.00000000000000000000000000001
                 sqe <- sum((y-yl)^2)+art
                df<-length(y)-length(estshap)
                sdli<-sqrt(sqe/df)
                qtt<-qt(0.025,df)
                var <- (sum((y-yl)^2)/(length(x) - length(estshap)))
                stderror<-sqrt(var*stderro)
                tvalues<-estshap/stderror;tvals2=data.frame(t(tvalues))
                tvalta<-qt(0.025,length(x) - length(estshap))
                #pvalues<-1-pt(as.numeric(tvalues),length(x) - length(estshap))
                pvalues<-apply(tvals2,1,pvalores,x,estshap)

                anva.1 <- c(length(estshap)-1,length(x)-length(estshap),length(x)-1)
                anva.2 <- c((sum(y^2)-length(y)*mean(y)^2)-sqe,sqe,sum(y^2)-length(y)*mean(y)^2)
                anva.3 <- c(anva.2[1]/anva.1[1],anva.2[2]/anva.1[2],NA)
                anva.4 <- c(anva.3[1]/anva.3[2],NA,NA)
                anva.5 <- c(1-pf(anva.4[1],anva.1[1],anva.1[2]),NA,NA)
                anva   <- cbind(anva.1,round(anva.2,4),round(anva.3,4),round(anva.4,4),round(anva.5,4))
                rownames(anva) <- c("Model","Error","Total")
                colnames(anva) <- c("DF","SS","MS","Fc","p")
                r<-1-sqe/sum((y-mean(y))^2)
                r_ajus<- 1 - ((length(x) - 1) / (length(x) - length(estshap))) * (1-r)
                #AC<-length(x)*log(sqe/length(x))+2*length(estshap) #Agregue /length(x) DM
                #AC<-length(x)*(log(2*pi*sqe/length(x))+1)+2*(length(estshap)+1) #Cambio segun Nonlinear Regression with R, pag 105. DM
                AC<-length(datashap[,1])*(log(2*pi*sqe/length(datashap[,1]))+1)+2*(length(estshap)+1) #Cambio segun Nonlinear Regression with R, pag 105. DM
#                n<-length(x); k<-length(estshap); if(n/k<40) {AC<-AC+2*k*(k+1)/(n-k-1); nombre="AICc"} else nombre="AIC" #Nuevo DM
                MSC<-log(sum((yl-mean(yl))^2)/sum((y-mean(y))^2))-2*length(estshap)/length(x)
                FRAMESEL<-data.frame(R2=round(r,3),R2_Adj=round(r_ajus,3),SSR=round(sqe,4),AIC=round(AC,3),MSC=round(MSC,3))
 #               colnames(FRAMESEL)[4]<-nombre #Nuevo DM
                rownames(FRAMESEL)<-c("")
                cat("\nNONLINEAR REGRESSION MODEL\n")
                cat("\nModel name:",ecua[model])
                cat("\nMethod:", "Marquardtr")
                cat("\nFormula: ", gg,"\n")
                cat("\n")
                cat("Parameters\n")
                if(model==1 ){
                                estshap<-data.frame(p=(coefi[1]+((Hl-Hh)/(1.987*log(-Hl/Hh)+(Hl/Tl)-(Hh/Th)))*coefi[2]),To=((Hl-Hh)/(1.987*log(-Hl/Hh)+(Hl/Tl)-(Hh/Th))),estshap)   ## pone todos los parametros incluyendo "p" siempre
                                estshap2<-data.frame(matrix(round(estshap,4)),c("","",round(stderror,5)),c("","",formatC(round(as.numeric(tvalues),5))),c("","",pvalues))  ## tiene la misma forma solo fijate en los espacios, pues solo se considera el error para los valores estimados.
                                rownames(estshap2)<-c(colnames(estshap))  ## el mismo
                                colnames(estshap2)<-c("Estimate","Std.Error","t value","Pr(>|t|)")  ## el mismo
                                para<-t(matrix(paste(round(estshap[3:7],2),"(","±",formatC(stderror,5),")")))  ## aqui lo q puede cambiar son los indices de estshap
                                colnames(para)<-names(estshap[3:7])  ## aqui lo q puede cambiar son los indices de estshap
                                param<-data.frame(Model="Sharpe 1",p=coefi[1]+((Hl-Hh)/((1.987*log(-Hl/Hh)+(Hl/Tl)-(Hh/Th)))*coefi[2]),To=((Hl-Hh)/(1.987*log(-Hl/Hh)+(Hl/Tl)-(Hh/Th))),para,d.f.=paste(anva.1[1],",",anva.1[2]),F=round(anva.4[1],3),P.value=round(anva.5[1],3),FRAMESEL,T_Threshold=-coefi[1]/coefi[2]-273.15) ## cambia la formula del estimador para "p" y/o "To"
                                print(estshap2)

                }
                if(model==12){
				if(model==12){To=298.16}
                                estshap<-as.matrix(cbind(p=(coefi[1]+(To)*coefi[2]),estshap))
                                estshap2<-data.frame(matrix(round(estshap,4)),c("",round(stderror,5)),c("",formatC(round(as.numeric(tvalues),5))),c("",pvalues))
                                colnames(estshap2)<-c("Estimate","Std.Error","t value","Pr(>|t|)")
                                rownames(estshap2)<-c(colnames(estshap))
                                para<-t(matrix(paste(round(estshap[2:7],2),"(","±",formatC(stderror,5),")")))
                                Sharpe=paste("Sharpe",model)
                                colnames(para)<-names(estshap[2:7])
                                param<-data.frame(Model=Sharpe,p=(coefi[1]+(To)*coefi[2]),para,d.f.=paste(anva.1[1],",",anva.1[2]),F=round(anva.4[1],3),P.value=round(anva.5[1],3),FRAMESEL,T_Threshold=-coefi[1]/coefi[2]-273.15)
                                print(estshap2)

                }

                if(model!=12){  ## falta poner los decimales para el pvalue
                                estshap2<-data.frame(matrix(round(estshap,4)),round(stderror,5),formatC(round(as.numeric(tvalues),5)),pvalues)
                                colnames(estshap2)<-c("Estimate","Std.Error","t value","Pr(>|t|)")
                                rownames(estshap2)<-c(colnames(estshap))
                                para<-t(matrix(paste(round(estshap,2),"(","±",formatC(stderror,5),")")))
                                colnames(para)<-names(estshap)
                                if(model==3) param<-data.frame(Model="Sharpe 3",para,d.f.=paste(anva.1[1],",",anva.1[2]),F=round(anva.4[1],3),P.value=round(anva.5[1],3),FRAMESEL)
                                if(model==4) param<-data.frame(Model="Sharpe 4",para,d.f.=paste(anva.1[1],",",anva.1[2]),F=round(anva.4[1],3),P.value=round(anva.5[1],3),FRAMESEL)
                                if(model==5) param<-data.frame(Model="Sharpe 5",para,d.f.=paste(anva.1[1],",",anva.1[2]),F=round(anva.4[1],3),P.value=round(anva.5[1],3),FRAMESEL)
                                if(model==6) param<-data.frame(Model="Sharpe 6",para,d.f.=paste(anva.1[1],",",anva.1[2]),F=round(anva.4[1],3),P.value=round(anva.5[1],3),FRAMESEL)
                                if(model==10) param<-data.frame(Model="Sharpe 10",para,d.f.=paste(anva.1[1],",",anva.1[2]),F=round(anva.4[1],3),P.value=round(anva.5[1],3),FRAMESEL)
                                if(model==11) param<-data.frame(Model="Sharpe 11",para,d.f.=paste(anva.1[1],",",anva.1[2]),F=round(anva.4[1],3),P.value=round(anva.5[1],3),FRAMESEL)
                                if(model==13) param<-data.frame(Model="Sharpe 13",para,d.f.=paste(anva.1[1],",",anva.1[2]),F=round(anva.4[1],3),P.value=round(anva.5[1],3),FRAMESEL)
                                if(model==14) param<-data.frame(Model="Sharpe 14",para,d.f.=paste(anva.1[1],",",anva.1[2]),F=round(anva.4[1],3),P.value=round(anva.5[1],3),FRAMESEL)

                if(model!=11 & model!=13 & model!=14) param<-data.frame(Model=gg,para,d.f.=paste(anva.1[1],",",anva.1[2]),F=round(anva.4[1],3),P.value=round(anva.5[1],3),FRAMESEL)
                                print(estshap2)
                }

                cat("\n")
                cat("Analysis of variance\n")
                print(anva,na.print = "")
                cat("\nSelection criteria")
                print(t(FRAMESEL))
                if ( length(estshap)!=length(stderror) ) { stderror<-c(rep(NA,(length(estshap)-length(stderror))),stderror)}  ###Agregue DM
                names(stderror)<-names(estshap) #Agrege DM
                if (model==11 || model==12 || model==13 || model==14){
                                salidas<-list(ecuacion=gg,parmer=estshap,frames=FRAMESEL,q=qtt,sdli=sdli,param=param,ecuaci=ggg,Std.Error=stderror)    #Agrege "Std.Error=stderror" DM
                }else{
                                salidas<-list(ecuacion=gg,parmer=estshap,frames= FRAMESEL,q=qtt,sdli=sdli,param=param,ecuaci=formula(paste("y~",as.character(expre))),Std.Error=stderror) # Agregue "Std.Error=stderror" DM
                }
                return(salidas)
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
