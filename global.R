#########################################################################################################################
# ILCYM Functions
#source('developmentTime.r')
#source('dev_rate_new.r')

#########################################################################################################################
# List-Nav-Choose Functions
source("chooser.R")

#########################################################################################################################
# Variables
globalpath <- "no project selected"
#graphMult <- 37


#########################################################################################################################
# Interface Components - Estadios
ImputEstadios<-function(estadios,ModSelected)
{
  estadiosInm<-estadios[-(length(estadios)-0:1)]
  estadiosAdul<-estadios[(length(estadios)-1:0)]
  estadiosFem<-estadios[(length(estadios)-1)]
  #estadios1<-estadios[-length(estadios)]
  
  ImEs<-switch(ModSelected,
               "Development Time" = selectInput("estadio", h4("Current Stage evaluated:"),choices = estadios),
               "Development Rate" = selectInput("estadio", h4("Current Stage evaluated:"),choices = estadiosInm),
               "Mortality" = selectInput("estadio", h4("Current Stage evaluated:"),choices = estadiosInm),
               "Senescence" = selectInput("estadio", h4("Current Stage evaluated:"),choices = estadiosAdul),
               "Total Oviposition" = selectInput("estadio", h4("Current Stage evaluated:"),choices = estadiosFem),
               "Relative Oviposition" = selectInput("estadio", h4("Current Stage evaluated:"),choices = estadiosFem),
               "Transmission Rate" = fileInput('transmfile', h4("Transmission dataset"),accept=c('text/csv','text/comma-separated-values,text/plain','.csv'), multiple = FALSE)
  )
  print(ImEs)
}

#########################################################################################################################
# Interface Components - Modelos

ImputModelList<-function(ModSelected)
{
  ImML<-switch(ModSelected,
               "Development Time" = selectInput("modelo", h4("Choose the final distribution model:"),choices = c("logit", "probit","cloglog")),
               "Development Rate" = chooserInput("modelo", "Available", "Selected", c("Shape & DeMichele 1","Shape & DeMichele 2","Shape & DeMichele 3","Shape & DeMichele 4","Shape & DeMichele 5","Shape & DeMichele 6","Shape & DeMichele 7","Shape & DeMichele 8","Shape & DeMichele 9","Shape & DeMichele 10","Shape & DeMichele 11","Shape & DeMichele 12","Shape & DeMichele 13","Shape & DeMichele 14","Deva 2","Logan 1","Briere 1","Stinner 1","Stinner 2","Stinner 3","Stinner 4","Latin 2","Kontodimas 1","Janish 1","Janish 2","Ratkowsky 1","Ratkowsky 2","Hilber y logan 1","Hilber y logan 2","Exponential simple","Tb Model","Exponential Model","Davidson","Pradham 1","Allahyari","Tanigoshi","Taylor"), c(), size = 12, multiple = TRUE),
               "Mortality" = chooserInput("modelo", "Available", "Selected", c("Quadratic", "Gaussian","Taylor","Wang 1","Wang 2","Wang 3","Wang 4","Wang 5","Wang 6","Wang 7","Wang 8","Wang 9","Wang 10","Gompertz-Makeham","Sharpe","Weibull","Janisch & Analytis"), c(), size = 12, multiple = TRUE),
               "Senescence" = chooserInput("modelo", "Available", "Selected", c("Shape & DeMichele 11","Shape & DeMichele 12","Shape & DeMichele 13","Shape & DeMichele 14","Exponential simple","Tb Model","Exponential Model","Ratkowsky","Pradham","Davidson","Janish-1","Tanigoshi","Wang-Lan-Ding","Stinner-3","Stinner-4","Logan-3","Hilber y logan 3"), c(), size = 12, multiple = TRUE),
               "Total Oviposition" = chooserInput("modelo", "Available", "Selected", c("Quadratic", "Gaussian","Taylor","Wang 5","Wang 6","Wang 7","Wang 8","Wang 9","Wang 10","Sharpe","Gompertz-Makeham","Weibull","Janisch & Analytis"), c(), size = 12, multiple = TRUE),
               "Relative Oviposition" = chooserInput("modelo", "Available", "Selected", c("Gamma", "Weibull","Exponential modified 1","Exponential modified 2","Exponential modified 3","Exponential modified 4"), c(), size = 12, multiple = TRUE),
               "Transmission Rate" = chooserInput("modelo", "Available", "Selected", c("Shape & DeMichele 1","Shape & DeMichele 2","Shape & DeMichele 3","Shape & DeMichele 4","Shape & DeMichele 5","Shape & DeMichele 6","Shape & DeMichele 7","Shape & DeMichele 8","Shape & DeMichele 9","Shape & DeMichele 10","Shape & DeMichele 11","Shape & DeMichele 12","Shape & DeMichele 13","Shape & DeMichele 14","Deva 2","Logan 1","Briere 1","Stinner 1","Stinner 2","Stinner 3","Stinner 4","Latin 2","Kontodimas 1","Janish 1","Janish 2","Ratkowsky 1","Ratkowsky 2","Hilber y logan 1","Hilber y logan 2","Exponential simple","Tb Model","Exponential Model","Davidson","Pradham 1","Allahyari","Tanigoshi","Taylor"), c(), size = 12, multiple = TRUE)
  )
  print(ImML)
}

ImputModelListNX<-function(ModSelected)
{
  ImML<-switch(ModSelected,
               "Development Time" = sliderInput("nx", h4("Range of the x axis:"),min = 0, max = 10, value = c(0,4), step= 0.5),
               "Development Rate" = sliderInput("nx", h4("Range of the x axis:"),min = 0, max = 100, value = c(0,45), step= 5),
               "Mortality" = sliderInput("nx", h4("Range of the x axis:"),min = 0, max = 100, value = c(0,45), step= 5),
               "Senescence" = sliderInput("nx", h4("Range of the x axis:"),min = 0, max = 100, value = c(0,45), step= 5),
               "Total Oviposition" = sliderInput("nx", h4("Range of the x axis:"),min = 0, max = 100, value = c(0,45), step= 5),
               "Relative Oviposition" = sliderInput("nx", h4("Range of the x axis:"),min = 0, max = 10, value = c(0,4), step= 0.5),
               "Transmission Rate" = sliderInput("nx", h4("Range of the x axis:"),min = 0, max = 100, value = c(0,45), step= 5),
  )
  print(ImML)
}


ImputModelListNY<-function(ModSelected)
{
  ImML<-switch(ModSelected,
               "Development Time" = sliderInput("ny", h4("Range of the y axis:"),min = 0, max = 100, value = c(0,100), step= 5),
               "Development Rate" = sliderInput("ny", h4("Range of the y axis:"),min = 0, max = 3, value = c(0,1), step= 0.1),
               "Mortality" = sliderInput("ny", h4("Range of the y axis in %:"),min = 0, max = 100, value = c(0,100), step= 5),
               "Senescence" = sliderInput("ny", h4("Range of the y axis:"),min = 0, max = 3, value = c(0,1), step= 0.1),
               "Total Oviposition" = sliderInput("ny", h4("Range of the y axis:"),min = 0, max = 1000, value = c(0,200), step= 10),
               "Relative Oviposition" = sliderInput("ny", h4("Range of the y axis:"),min = 0, max = 100, value = c(0,100), step= 10),
               "Transmission Rate" = sliderInput("ny", h4("Range of the y axis:"),min = 0, max = 3, value = c(0,1), step= 0.1),
  )
  print(ImML)
}


#########################################################################################################################
# Special Functions to Display


# Define a server for the Shiny app
get_plot_output_list <- function(input_n) {
  # Insert plot output objects the list
  plot_output_list <- lapply(1:input_n, function(i) {
    plotname <- paste("plot", i, sep="")
    plot_output_object <- plotOutput(plotname, height = 300, width = 400)
  })
  do.call(tagList, plot_output_list) # needed to display properly.
  
  return(plot_output_list)
}

#########################################################################################################################
# Development Rate models

DRmodels <- function(IDname,datashap)
{
  model <- switch(IDname,"Shape & DeMichele 1"=1,"Shape & DeMichele 2"=2,"Shape & DeMichele 3"=3,"Shape & DeMichele 4"=4,"Shape & DeMichele 5"=5,"Shape & DeMichele 6"=6,"Shape & DeMichele 7"=7,"Shape & DeMichele 8"=8,"Shape & DeMichele 9"=9,"Shape & DeMichele 10"=10,"Shape & DeMichele 11"=11,"Shape & DeMichele 12"=12,"Shape & DeMichele 13"=13,"Shape & DeMichele 14"=14,"Deva 2"=16,"Logan 1"=17,"Briere 1"=19,"Stinner 1"=21,"Stinner 2"=33,"Stinner 3"=50,"Stinner 4"=51,"Latin 2"=23,"Kontodimas 1"=42,"Janish 1"=46,"Janish 2"=47,"Ratkowsky 1"=29,"Ratkowsky 2"=45,"Hilber y logan 1"=22,"Hilber y logan 2"=55,"Exponential simple"=25,"Tb Model"=26,"Exponential Model"=27,"Davidson"=30,"Pradham 1"=31,"Allahyari"=39,"Tanigoshi"=48,"Taylor"=57)
  if(model==1)
  {
    ####  Shape&DeMichele sin p y To
    xha1<-log(datashap[,2]);xha=xha1[xha1 != -100]
    yha<-(1/(datashap[,1]+273.15))[xha1 != -100]
    beta<-as.numeric(coef(lm(xha~yha))[2])
    xli<-(datashap[,1]+273.15)
    yli<-datashap[,2]
    cof<-as.numeric(coef(lm(yli~xli)))
    ini <- list(Ha = -beta*1.987,Tl = -cof[1]/cof[2]+2, Hl =-100000 ,Hh =200000, Th =300)
  }

  if(model==2)
  {
    ####  Shape&DeMichele con To
    yha<-1/(datashap[,1]+273.15)
    xha<-log(datashap[,2])
    beta<-as.numeric(coef(lm(xha~yha))[2])
    xli<-(datashap[,1]+273.15)
    yli<-datashap[,2]
    cof<-as.numeric(coef(lm(yli~xli)))
    ini <- list(To=median(datashap[,1])+273.15,Ha = -beta*1.987,Tl = -cof[1]/cof[2]+1, Hl =-10000 ,Hh =200000, Th =as.numeric(subset(datashap,max(datashap[,2])<=datashap[,2]))[1]+273.15+2)
  }

  if(model==3)
  {
    ####  Shape&DeMichele con To y p
    yha<-1/(datashap[,1]+273.15)
    xha<-log(datashap[,2])
    beta<-as.numeric(coef(lm(xha~yha))[2])
    xli<-(datashap[,1]+273.15)
    yli<-datashap[,2]
    cof<-as.numeric(coef(lm(yli~xli)))
    ini <- list(p=median(datashap[,2]),To=median(datashap[,1])+273.15,Ha = -beta*1.987,Tl = -cof[1]/cof[2]+1, Hl =-10000 ,Hh =200000, Th =as.numeric(subset(datashap,max(datashap[,2])<=datashap[,2]))[1]+273.15+2)
  }
  
  if(model==4)
  {
    ####  Shape&DeMichele solo con To, p y Ha
    yha<-1/(datashap[,1]+273.15)
    xha<-log(datashap[,2])
    beta<-as.numeric(coef(lm(xha~yha))[2])
    xli<-(datashap[,1]+273.15)
    yli<-datashap[,2]
    cof<-as.numeric(coef(lm(yli~xli)))
    p=cof[1]+cof[2]*(median(datashap[,1])+273.15)
    ini <- list(p=p,To=median(datashap[,1])+273.15,Ha = -beta* 1.987)
  }
  
  if(model==5)
  {
    ####  Shape&DeMichele con To, p, Ha, Hl, Tl
    yha<-1/(datashap[,1]+273.15)
    xha<-log(datashap[,2])
    beta<-as.numeric(coef(lm(xha~yha))[2])
    xli<-(datashap[,1]+273.15)
    yli<-datashap[,2]
    cof<-as.numeric(coef(lm(yli~xli)))
    ini <- list(p=median(datashap[,2]),To=median(datashap[,1])+273.15,Ha = -beta* 1.987,Tl =-cof[1]/cof[2] +2, Hl =-100000)
  }
  
  if(model==6)
  {
    ####  Shape&DeMichele con To, p, Ha, Hh, Th
    yha<-1/(datashap[,1]+273.15)
    xha<-log(datashap[,2])
    beta<-as.numeric(coef(lm(xha~yha))[2])
    xli<-(datashap[,1]+273.15)
    yli<-datashap[,2]
    cof<-as.numeric(coef(lm(yli~xli)))
    p=cof[1]+cof[2]*(median(datashap[,1])+273.15)
    ini <- list(p=p,To=median(datashap[,1])+273.15,Ha = -beta* 1.987,Hh =200000, Th =as.numeric(subset(datashap,max(datashap[,2])<=datashap[,2]))[1]+273.15)
  }

  if(model==7)
  {
    ####  Shape&DeMichele con To, Ha
    yha<-1/(datashap[,1]+273.15)
    xha<-log(datashap[,2])
    beta<-as.numeric(coef(lm(xha~yha))[2])
    xli<-(datashap[,1]+273.15)
    yli<-datashap[,2]
    cof<-as.numeric(coef(lm(yli~xli)))
    ini <- list(To=median(datashap[,1])+273.15,Ha = -beta* 1.987) ## aqui en la version pasada ingresaba el p
  }
  
  if(model==8)
  {
    ####  Shape&DeMichele con To, Ha, Hl, Tl
    yha<-1/(datashap[,1]+273.15)
    xha<-log(datashap[,2])
    beta<-as.numeric(coef(lm(xha~yha))[2])
    xli<-(datashap[,1]+273.15)
    yli<-datashap[,2]
    cof<-as.numeric(coef(lm(yli~xli)))
    ini <- list(To=median(datashap[,1])+273.15,Ha = -beta*1.987,Tl = -cof[1]/cof[2]+1, Hl =-10000)
  }
  
  if(model==9)
  {
    ####  Shape&DeMichele con To, Ha, Hh, Th      
    yha<-1/(datashap[,1]+273.15)
    xha<-log(datashap[,2])
    beta<-as.numeric(coef(lm(xha~yha))[2])
    xli<-(datashap[,1]+273.15)
    yli<-datashap[,2]
    cof<-as.numeric(coef(lm(yli~xli)))
    p=cof[1]+cof[2]*(median(datashap[,1])+273.15)
    ini <- list(To=median(datashap[,1])+273.15,Ha = -beta* 1.987,Hh =200000, Th =as.numeric(subset(datashap,max(datashap[,2])<=datashap[,2]))[1]+273.15)
  }
  
  if(model==10)
  {
    ####  Shape&DeMichele con p, Ha, Hl, Tl      
    yha<-1/(datashap[,1]+273.15)
    xha<-log(datashap[,2])
    beta<-as.numeric(coef(lm(xha~yha))[2])
    xli<-(datashap[,1]+273.15)
    yli<-datashap[,2]
    cof<-as.numeric(coef(lm(yli~xli)))
    ini <- list(p=median(datashap[,2]),Ha = -beta* 1.987,Tl =-cof[1]/cof[2] +2, Hl =-100000)
  }

  if(model==11)
  {
    ####  Shape&DeMichele con p,Ha, Hl,Tl,Hh,Th      
    yha<-1/(datashap[,1]+273.15)
    xha<-log(datashap[,2])
    beta<-as.numeric(coef(lm(xha~yha))[2])
    xli<-(datashap[,1]+273.15)
    yli<-datashap[,2]
    cof<-as.numeric(coef(lm(yli~xli)))
    ini <- list(p=median(datashap[,2]),Ha = -beta* 1.987,Tl =-cof[1]/cof[2] +2, Hl =-100000 ,Hh =200000, Th =as.numeric(subset(datashap,max(datashap[,2])<=datashap[,2]))[1]+273.15)
  }
  
  if(model==12)
  {
    ####  Shape&DeMichele con Ha, Hl,Tl,Hh,Th      
    yha<-1/(datashap[,1]+273.15)
    xha<-log(datashap[,2])
    beta<-as.numeric(coef(lm(xha~yha))[2])
    xli<-(datashap[,1]+273.15)
    yli<-datashap[,2]
    cof<-as.numeric(coef(lm(yli~xli)))
    ini <- list(Ha = -beta*1.987,Tl = -cof[1]/cof[2]+2, Hl =-100000 ,Hh =200000, Th =as.numeric(subset(datashap,max(datashap[,2])<=datashap[,2]))[1]+273.15)
  }
  
  if(model==13)
  {
    ####  Shape&DeMichele con p,Ha, Hl,Tl      
    yha<-1/(datashap[,1]+273.15)
    xha<-log(datashap[,2])
    beta<-as.numeric(coef(lm(xha~yha))[2])
    xli<-(datashap[,1]+273.15)
    yli<-datashap[,2]
    cof<-as.numeric(coef(lm(yli~xli)))
    ini <- list(p=median(datashap[,2]),Ha = -beta*1.987,Tl = -cof[1]/cof[2]+2, Hl =-100000)
  }

  if(model==14)
  {
    ####  Shape&DeMichele con p,Ha, Hh,Th      
    yha<-1/(datashap[,1]+273.15)
    xha<-log(datashap[,2])
    beta<-as.numeric(coef(lm(xha~yha))[2])
    xli<-(datashap[,1]+273.15)
    yli<-datashap[,2]
    cof<-as.numeric(coef(lm(yli~xli)))
    ini <- list(p=median(datashap[,2]),Ha = -beta* 1.987,Hh =200000, Th =as.numeric(subset(datashap,max(datashap[,2])<=datashap[,2]))[1]+273.15)
  }
  
  if(model==16)
  {
    ####  Deva2 b1,b2,b3,b4,b5
    ini <- list(b1=0.0001630595,b2=-74.79061,b3=-38.76691,b4=-3.6201e-05,b5=-0.3245052 )
  }
  
  if(model==17)
  {
    ####  Logan1 Y,Tmax, p, v
    ini <- list(Y=min(datashap[,2]),Tmax = max(datashap[,1]), p = median(datashap[,2]), v=3.5)
  }  

  if(model==19)
  {
    ####  Briere1 aa,To,Tmax
    ini <- list(aa=0.00003,To=min(datashap[,1])-4,Tmax =max(datashap[,1])+90)
  }  

  if(model==21)
  {
    ####  Stinner1 Rmax,Topc,k1,k2
    xstinner<-c(min(datashap[,1]),max(datashap[,1]))
    ystinner<-c(log(as.numeric(subset(datashap,max(datashap[,2])<=datashap[,2]))[1]/min(datashap[,2])-1),log(as.numeric(subset(datashap,max(datashap[,2])<=datashap[,2]))[1]/max(datashap[,2])-1))
    ks<-as.numeric(coef(lm(ystinner~xstinner)))
    ini=c(Rmax=max(datashap[,2]),k1=ks[1],k2=ks[2],Topc=as.numeric(subset(datashap,max(datashap[,2])<=datashap[,2]))[1])
  }
  
  if(model==33)
  {
    ####  Stinner2 Rmax,k1,k2,Topc
    xstinner<-c(min(datashap[,1]),max(datashap[,1]))
    ystinner<-c(log(as.numeric(subset(datashap,max(datashap[,2])<=datashap[,2]))[1]/min(datashap[,2])-1),log(as.numeric(subset(datashap,max(datashap[,2])<=datashap[,2]))[1]/max(datashap[,2])-1))
    ks<-as.numeric(coef(lm(ystinner~xstinner)))
    ini=c(Rmax=max(datashap[,2]),k1=ks[1],k2=ks[2],Topc=as.numeric(subset(datashap,max(datashap[,2])<=datashap[,2]))[1])
  }
  
  if(model==50)
  {
    ####  Stinner3 c1,k1,k2
    ini=c(c1=11.7,k1=5.783,k2=-0.0516)
  }  
  
  if(model==51)
  {
    ####  Stinner4 c1,c2,k1,k2,To
    ini=c(c1=-0.962,c2=0.2545,k1=-3.91,k2=0.54,To=7.77)
  }

  if(model==23)
  {
    ####  Latin2 Tl, p, dt,L
    ini <- list(Tl = max(datashap[,1]), p = min(datashap[,2]), dt= 0.8,L=-1)
  }
  
  if(model==42)
  {
    ####  Kontodimas1 aa,Tmin,Tmax
    ini=c(aa=0.00013,Tmin=min(datashap[,1])-4, Tmax=max(datashap[,1])+1)
  }
  
  if(model==46)
  {
    ####  Janish1 Dmin,Topt,K
    ini=c(Dmin=2.8,Topt=mean(datashap[,1]),K=0.17)
  }

  if(model==47)
  {
    ####  Janish2 c,a,b,Tm
    ini=c(c=0.2151377,a=4.97447,b=1.077405,Tm=max(datashap[,1]))
  }
  
  if(model==29)
  {
    ####  Ratkowsky1 b,Tb
    ini=c(b=0.000008487,Tb=-150.881)
  }

  if(model==45)
  {
    ####  Ratkowsky2 aa,Tmin,Tmax,b
    ini=c(aa=0.00065,Tmin=min(datashap[,1])-1,Tmax=max(datashap[,1])+38,b=0.09)
  }
  
  if(model==22)
  {
    ####  Hilber y logan 1  d,Y,Tmax, v
    ini <- list(d=2*(max(datashap[,1])-min(datashap[,1])),Y=3*max(datashap[,2]),Tmax = max(datashap[,1]), v=0.01)
  }
  
  if(model==55)
  {
    ####  Hilber y logan 2  trid,D,Tmax,Dt
    ini=c(trid=10.145,D=12457,Tmax=24.698,Dt=1.3818)
  }
  
  if(model==25)
  {
    ####  Exponential simple  b1,b2
    ini=c(b1=0.019,b2=0.086)
  }
  
  if(model==26)
  {
    ####  Tb Model  sy,b,Tb,DTb
    ini=c(sy=0.32,b=0.0924,Tb=max(datashap[,1]),DTb=0.084)
  }

  if(model==27)
  {
    ####  Exponential Model  sy,b,Tb
    ini=c(sy=0.2567,b=0.086,Tb=min(datashap[,1]))
  }
  
  if(model==30)
  {
    ####  Davidson  k,a,b
    ini=c(k=0.5,a=1,b=-0.05)
  }
  
  if(model==31)
  {
    ####  Pradham1  R,Tm,To
    ini=c(R=0.3,Tm=mean(datashap[,1]),To=(-1)*min(datashap[,1]))
  }
  
  if(model==39)
  {
    ####  Allahyari  P,Tmax, Tmin,n,m
    ini=c(P=1.2,Tmax=max(datashap[,1]), Tmin=min(datashap[,1])-9,n=1,m=0.5)
  }

  if(model==48)
  {
    ####  Tanigoshi  P,Tmax, Tmin,n,m
    xTan<-datashap[,1]
    yTan<-datashap[,2]
    ks<-as.numeric(coef(lm(yTan~xTan+I(xTan^2)+I(xTan^3))))
    ini=c(a0=ks[1],a1=ks[2],a2=ks[3],a3=ks[4])
  }
  
  if(model==57)
  {
    ####  Taylor  rm,Topt,Troh
    ini=c(rm=0.339,Topt=21.81,Troh=7.897)
  }
  
  rm(IDname);rm(datashap)

  Objs0<-ls()
  Objs<-list()
  for (k in 1:length(Objs0)){
    Objs[[k]] <- get(Objs0[k])
  }
  return(list(Objs=Objs,NamesO=Objs0))
}

#########################################################################################################################
# Senescence models

SNCmodels <- function(IDname,datashap)
{
  model <- switch(IDname,"Shape & DeMichele 11"=11,"Shape & DeMichele 12"=12,"Shape & DeMichele 13"=13,"Shape & DeMichele 14"=14,"Exponential simple"=25,"Tb Model"=26,"Exponential Model"=27,"Ratkowsky"=29,"Pradham"=31,"Davidson"=30,"Janish-1"=46,"Tanigoshi"=48,"Wang-Lan-Ding"=49,"Stinner-3"=50,"Stinner-4"=51,"Logan-3"=52,"Hilber y logan 3"=56)
  
  if(model==11)
  {
    ####  Shape&DeMichele con p,Ha, Hl,Tl,Hh,Th      
    yha<-1/(datashap[,1]+273.15)
    xha<-log(datashap[,2])
    beta<-as.numeric(coef(lm(xha~yha))[2])
    xli<-(datashap[,1]+273.15)
    yli<-datashap[,2]
    cof<-as.numeric(coef(lm(yli~xli)))
    ini <- list(p=median(datashap[,2]),Ha = -beta* 1.987,Tl =-cof[1]/cof[2] +2, Hl =-100000 ,Hh =200000, Th =as.numeric(subset(datashap,max(datashap[,2])<=datashap[,2]))[1]+273.15)
  }
  
  if(model==12)
  {
    ####  Shape&DeMichele con Ha, Hl,Tl,Hh,Th      
    yha<-1/(datashap[,1]+273.15)
    xha<-log(datashap[,2])
    beta<-as.numeric(coef(lm(xha~yha))[2])
    xli<-(datashap[,1]+273.15)
    yli<-datashap[,2]
    cof<-as.numeric(coef(lm(yli~xli)))
    ini <- list(Ha = -beta* 1.987, Hl =170 , Tl =-cof[1]/cof[2] -300, Hh =200000, Th =-400)
  }
  
  if(model==13)
  {
    ####  Shape&DeMichele con p,Ha, Hl,Tl      
    yha<-1/(datashap[,1]+273.15)
    xha<-log(datashap[,2])
    beta<-as.numeric(coef(lm(xha~yha))[2])
    xli<-(datashap[,1]+273.15)
    yli<-datashap[,2]
    cof<-as.numeric(coef(lm(yli~xli)))
    ini <- list(p=median(datashap[,2]),Ha = -beta* 1.987, Hl =-100000 ,Tl =-cof[1]/cof[2] +2)
  }
  
  if(model==14)
  {
    ####  Shape&DeMichele con p,Ha, Hh,Th      
    yha<-1/(datashap[,1]+273.15)
    xha<-log(datashap[,2])
    beta<-as.numeric(coef(lm(xha~yha))[2])
    xli<-(datashap[,1]+273.15)
    yli<-datashap[,2]
    cof<-as.numeric(coef(lm(yli~xli)))
    ini <- list(p=median(datashap[,2]),Ha = -beta* 1.987, Hh =200000, Th =515)
  }
  
  if(model==25)
  {
    # exponential simple
    ini=c(b1=0.019,b2=0.086)
  }
  
  if(model==26)
  {
    # Tb Model (Logan)
    ini=c(sy=0.09107,b=0.1005,Tb=max(datashap[,1])-15,DTb=-0.035753)
  }  
  
  if(model==27)
  {
    # Exponential Model (Logan)
    ini=c(sy=0.0468,b=0.06779,Tb=min(datashap[,1])-8)
  }  
  
  if(model==29)
  {
    # Square root model of Ratkowsky
    ini=c(b=0.000008487,Tb=-150.881)
  }
  
  if(model==31)
  {
    #Pradham - 1
    ini=c(R=0.3,Tm=mean(datashap[,1]),To=(-1)*min(datashap[,1]))
  }
  
  if(model==30)
  {
    #Davidson
    ini=c(k=0.5,a=1,b=-0.05)
  }  
  
  if(model==46)
  {
    #Janish-1 
    ini=c(Dmin=4.93248,Topt=mean(datashap[,1])+24.8,K=0.05937)
  }
  
  if(model==48)
  {
    #Tanigoshi
    xTan<-datashap[,1]
    yTan<-datashap[,2]
    ks<-as.numeric(coef(lm(yTan~xTan+I(xTan^2)+I(xTan^3))))
    ini=c(a0=ks[1],a1=ks[2],a2=ks[3],a3=ks[4])
  }
  
  if(model==49)
  {
    #Wang-Lan-Ding
    ini=c(k=3.78,a=-0.03943,b=0.00765,c=-1.219,Tmin=min(datashap[,1])+7.2,Tmax=max(datashap[,1])-18.2,r=11.772)
  }
  
  if(model==50)
  {
    #Stinner-3
    ini=c(c1=11.7,k1=5.783,k2=-0.0516)
  }
  
  if(model==51)
  {
    #Stinner-4
    ini=c(c1=0.76492,c2=0.5269,k1=-3.772,k2=0.3887,To=20.01)
  }
  
  #"Logan-3"=52,"Hilber y logan 3"=56
  if(model==52)
  {
    #Logan-3
    ini=c(sy=0.102089,b=0.073,Tmin=11.232,Tmax=-72.172,DTb=-0.3124)
  }
  
  if(model==56)
  {
    #Hilber y logan 3
    ini=c(trid=49.88,Tmax=38.567,Tmin=13.668,D=80706,Dt=0.01722,Smin=0.11573)
  }
  
  rm(IDname);rm(datashap)
  
  Objs0<-ls()
  Objs<-list()
  for (k in 1:length(Objs0)){
    Objs[[k]] <- get(Objs0[k])
  }
  return(list(Objs=Objs,NamesO=Objs0))
}

#########################################################################################################################
# Mortality models

MRmodels <- function(IDname,datm)
{
  modelm <- switch(IDname,"Quadratic"=1, "Gaussian"=8,"Taylor"=24,"Wang 1"=26,"Wang 2"=27,"Wang 3"=28,"Wang 4"=29,"Wang 5"=30,"Wang 6"=31,"Wang 7"=32,"Wang 8"=33,"Wang 9"=44,"Wang 10"=45,"Sharpe"=34,"Gompertz-Makeham"=38,"Weibull"=39,"Janisch & Analytis"=43)
  
  if(modelm==1)
  {
    # Quadratic
    inicial<-pr6mortal(modelm,datm)
    inim<-inicial$ini
  }
  
  if(modelm==8)
  {
    # Gaussian
    inim<-c(a=0.2,b=-17485,xo=18,c=-1400)
  }
  
  if(modelm==24)
  {
    # Taylor
    inim=c(rm=0.807,Topt=25.87,Troh=9.2)
  }
  
  if(modelm==26)
  {
    # Wang 1
    inim=c(Topt=25.53,B=4.338,H=0.06)
  }
  
  if(modelm==27)
  {
    # Wang 2
    inim=c(Tl=25.5,Th=25.53,B=4.338,H=0.061)
  }
  
  if(modelm==28)
  {
    # Wang 3
    inim=c(Topt=27.9,Bl=5.01,Bh=3.15,H=0.06)
  }
  
  if(modelm==29)
  {
    # Wang 4
    inim=c(Tl=23.47,Th=30.16,Bl=4.75,Bh=2.82,H=0.119)
  }
  
  if(modelm==30)
  {
    # Wang 5
    inim=c(Topt=17.55,B=5.78,H=10.1)
  }  
  
  if(modelm==31)
  {
    # Wang 6
    inim=c(Tl=14.04,Th=37.23,B=5.388,H=2.75)
  }  
  
  if(modelm==32)
  {
    # Wang 7
    inim=c(Topt=16.93,Bl= 3.07,Bh=4.69,H=9.64)
  }
  
  if(modelm==33)
  {
    # Wang 8
    inim=c(Tl=13.33,Th=36.06,Bl=4.7,Bh=2.96,H=2.39)
  }
  
  if(modelm==44)
  {
    # Wang 9
    inim=c(a=0.2,Tl=1, Th=70,B=-50)
  }  
  
  if(modelm==45)
  {
    # Wang 10
    inim=c(a=-22.23, Tl=12.4, Th=61.68,Bl=3,Bh=12)
  }
  
  if(modelm==34)
  {
    # Sharpe
    inim=c(Hl=-60000,Tl=287,Hh=100000,Th=308,Bm=0.193)
  }

  if(modelm==38)
  {
    # Gompertz-Makeham
    inim=c(a1=8e-08,b1=0.5,a2=29,b2=-0.337,c1=0.001)
  }

  if(modelm==39)
  {
    # Weibull
    inim=c(a=0.005,b=24,nn=2)
  }
  
  if(modelm==43)
  {
    # Janisch & Analytis
    inim=c(Dmin=0.3,k=0.088,Tp=-15.5,lamb=0.116)
  }

  rm(IDname)
  
  Objs0<-ls()
  Objs<-list()
  for (k in 1:length(Objs0)){
    Objs[[k]] <- get(Objs0[k])
  }
  return(list(Objs=Objs,NamesO=Objs0))
}



#########################################################################################################################
# Total oviposition models

TOmodels <- function(IDname,datm)
{
  modelm <- switch(IDname,"Quadratic"=1, "Gaussian"=8,"Taylor"=24,"Wang 5"=30,"Wang 6"=31,"Wang 7"=32,"Wang 8"=33,"Wang 9"=44,"Wang 10"=45,"Sharpe"=34,"Gompertz-Makeham"=38,"Weibull"=39,"Janisch & Analytis"=43)
  
  if(modelm==1)
  {
    # Quadratic
    inicial<-pr6mortal(modelm,datm)
    inim<-inicial$ini
  }
  
  if(modelm==8)
  {
    # Gaussian
    inim=c(a=43.172,b=0.0085,xo=21.4,c=0.568)
  }
  
  if(modelm==24)
  {
    # Taylor
    inim=c(rm=-40.27,Topt=22.8,Troh=5.88)
  }
  
  if(modelm==30)
  {
    # Wang 5
    inim=c(Topt=18.55,B=4.78,H=-530.1)
  }  
  
  if(modelm==31)
  {
    # Wang 6
    inim=c(Tl=18.6,Th=9.52,B=5.388,H=-1723.1)
  }  
  
  if(modelm==32)
  {
    # Wang 7
    inim=c(Topt=18.93,Bl= 3.07,Bh=4.69,H=-530.1)
  }
  
  if(modelm==33)
  {
    # Wang 8
    inim=c(Tl=18.6,Th=19.52,Bl=5.7,Bh=4.56,H=-528.1)
  }
  
  if(modelm==44)
  {
    # Wang 9
    inim=c(a=15300,Tl=10.52, Th=31.9,B=221)
  }  
  
  if(modelm==45)
  {
    # Wang 10
    inim=c(a=25.5, Tl=31.78, Th=4.24,Bl=305.6,Bh=10.3)
  }
  
  if(modelm==34)
  {
    # Sharpe
    inim=c(Hl=-753722,Tl=289.6,Hh=209230,Th=300,Bm=14.50282)
  }
  
  if(modelm==38)
  {
    # Gompertz-Makeham
    inim=c(a1=7940.902,b1=-0.0066,a2=-3570.6,b2=-0.0193,c1=-4488.5)
  }
  
  if(modelm==39)
  {
    # Weibull
    inim=c(a=84.21,b=21.95,nn=-0.97)
  }
  
  if(modelm==43)
  {
    # Janisch & Analytis
    inim=c(Dmin=0.04,k=-1.2716,Tp=15.9,lamb=-0.06)
  }
  
  rm(IDname)
  
  Objs0<-ls()
  Objs<-list()
  for (k in 1:length(Objs0)){
    Objs[[k]] <- get(Objs0[k])
  }
  return(list(Objs=Objs,NamesO=Objs0))
}

#########################################################################################################################
# Relative oviposition models

ROmodels <- function(IDname)
{
  modelm <- switch(IDname,"Gamma"=2, "Weibull"=6,"Exponential modified 1"=1,"Exponential modified 2"=3,"Exponential modified 3"=4,"Exponential modified 4"=5)
  
  if(modelm==2)
  {
    # Gamma
    inim<-c(a = 1, b = 2)
  }
  
  if(modelm==6)
  {
    # Weibull
    inim<-c(a = 1, b = 0.3)
  }
  
  if(modelm==1)
  {
    # Exponential modified 1
    inim<-c(a = 3.3788, b = 1.0881, c = -0.2060)
  }
  
  if(modelm==3)
  {
    # Exponential modified 2
    inim<-c(a = 1, b = -5)
  }  
  
  if(modelm==4)
  {
    # Exponential modified 3
    inim<-c(a = 3, b = 1)
  }  
  
  if(modelm==5)
  {
    # Exponential modified 4
    inim<-c(a = -0.6, b = 6,n = 0.9)
  }
  
  
  rm(IDname)
  
  Objs0<-ls()
  Objs<-list()
  for (k in 1:length(Objs0)){
    Objs[[k]] <- get(Objs0[k])
  }
  return(list(Objs=Objs,NamesO=Objs0))
}

#########################################################################################################################
# Development Time procedure

DevTime <- function(path,est,modelo,PosModel,OPTplot=TRUE,nx,ny,saveSelec=FALSE)
{
  #source("D:/_BK-D/Pablo/R archivos/shiny-Examples/041-dynamic-ui-ILCYM/lib/developmentTime.r")
  #path<-"D:/LH/Paratrioza-Cohort-2016"
  datos<-list(1)
  load(paste(path,"/PhenologySims.RData",sep=""))
  load(paste(path,"/PhenologyStats.RData",sep=""))
  estadios1<-params$estadios
  
  for(i in 1:length(estadios1)) # CAMBIO
  {
    datos[[i]]<-read.table(paste(path,"/Data/",estadios1[i],".txt",sep="")) # CAMBIO
  }
  
  estadios<-estadios1
  estadiosINMD<-estadios[1:(length(estadios)-2)]
  estadiosADLT<-estadios[(length(estadios)-1):length(estadios)]
  
  intervalo<-1
  temperatura<-temp(datos,estadios,est)
  tp<-temperatura$temperature
  opc=1
  
  distri<-devath(opc,datos,estadios,est,tp,intervalo=intervalo,modelo)
  matri<-distri$matriz
  slope <- distri$Slope; names(slope)<-"slope"
  parametros<-distri$parametros
  test<-distri$model
  exis.val <- distri$exis.val
  
  if(saveSelec){modelo=""}
  if(OPTplot){
    plot<-plot.devath (matri,parametros,test,c(nx[1],nx[2]),0.1,90,0.8,labx="Development time (Ln-days)",laby="Accumulated frequency (%)",titulo=modelo,grises=FALSE,intervalo=intervalo,exis.val=exis.val)
    if(est==estadios[(length(estadios))-1]){
      mediana<-plot$median
    }
  }
  if(saveSelec){
    report$fenologia[[1]][[(1:length(estadios))[estadios==est]]]<-distri$tabest
    if(length(estadiosINMD[est==estadiosINMD]) >= 1){
      params$hfeno$distri_dv[[(1:length(estadios))[estadios==est]]] <- test
      params$hfeno$slope_dv[[(1:length(estadios))[estadios==est]]] <-  slope
      params$hfeno$StEr_DTime[[(1:length(estadios))[estadios==est]]] <- slope
    }else{
      if(est==estadios[(length(estadios))-1]){
        params$hfeno$distri_snh <- test
        params$hfeno$slope_snh <- slope
        params$hfeno$StEr_DTimeH <- slope
        report$mediana<-mediana
        report$tp<-tp
      }else{
        params$hfeno$distri_snm <- test
        params$hfeno$slope_snm <- slope
        params$hfeno$StEr_DTimeM <- slope
      }
    }
    save(report, file = paste(path,"/PhenologyStats.RData",sep=""))
    save(params, file = paste(path,"/PhenologySims.RData",sep=""))
  }
  rm(list=ls())
}

#########################################################################################################################
# Development Rate procedure

DevRate <- function(path,est,modelo,PosModel,OPTplot=TRUE,nx,ny,saveSelec=FALSE)
{
  load(paste(path,"/PhenologySims.RData",sep=""))
  load(paste(path,"/PhenologyStats.RData",sep=""))
  estadios<-params$estadios
  
  
  datashap0 <- report$fenologia[[1]][[(1:length(estadios))[estadios==est]]][,c(1,5,6,7)] # putting only the necessesary
  datashap <- data.frame(x=datashap0[,1],y=1/datashap0[,2],Lower=1/datashap0[,4],Upper=1/datashap0[,3])
  datashap2 <- datashap; colnames(datashap2)=c("x","y","y","y")
  datao=rbind(datashap2[,c(1,2)],datashap2[,c(1,3)],datashap2[,c(1,4)])
  
  IDname<-modelo[PosModel] # al final se va a editar
  Temp<-DRmodels(IDname,datashap) ## Verificar
  for(i in 1:length(Temp$NamesO)){assign((Temp$NamesO)[i],(Temp$Objs)[[i]])}
  
  #jpeg(paste(path,"/temporal.jpg",sep=""))
  shapprueba<-prueba(model,datashap,datao,ini,corrx=c(0,55),corry=c(0,0.9),punt=1:6,labx="X",laby="Y",titulo="Title",grises=T)
  #dev.off()
  
  ini<-shapprueba$ini
  coefi<-shapprueba$coefi
  p<-shapprueba$p
  
  shap<-shape(model,datashap,datao,ini,coefi)
  estshap<-shap$estimados
  g<-shap$f
  p<-shap$p
  stderro<-shap$stderro
  
  sol_develop<-stat_develop(model,datashap,datao,estshap,coefi,stderro)    
  qtt<-sol_develop$q
  sdli<-sol_develop$sdli
  
  #ny<-c(0,1)
  TempNY<-seq(ny[1],ny[2],length.out=round((ny[2]-ny[1])/(0.1*(ny[2]-0))))
  scaleY<-TempNY[2]-TempNY[1];Scales<-c(0.01,0.02,0.04,0.05,0.1,0.2,0.4,0.5)
  DEuq<-(scaleY-Scales)^2
  scaleY<-Scales[DEuq==min(DEuq)]
    
  # scaleY<-round(scaleY,2)
  # TempNY2<-seq(ny[1],ny[2],scaleY)
  # ny[2]<-TempNY2[length(TempNY2)]
  if(saveSelec){IDname=""}
  if(OPTplot){
    plot_shape<-grafshape(model,estshap,datashap,qtt,sdli,corrx=c(nx[1],nx[2]), corry=c(ny[1],ny[2]),mini=0,maxi=100,coefi,limit="yes",1,labx=expression(paste("temperature (", degree, "C)")), titulo=IDname,laby="Development rate (1/days)", grises=FALSE, scaleY=scaleY, scaleX=5, est=est, estadios=estadios)
  }
  
  if(saveSelec){
    report$fenologia$DaveRate[[(1:length(estadios))[estadios==est]]]<-datashap
    params$hfeno$fdv_dv[[(1:length(estadios))[estadios==est]]] <- sol_develop$ecuaci
    tempPAR<-sol_develop$parmer;tempPAR2<-sol_develop$parmer
    if(is.matrix(tempPAR) || is.data.frame(tempPAR)){tempPAR2<-c(as.matrix(tempPAR[1,]));names(tempPAR2)<-colnames(tempPAR);rm(tempPAR)}
    params$hfeno$pdv_dv[[(1:length(estadios))[estadios==est]]] <- tempPAR2
    params$hfeno$StEr_DRate[[(1:length(estadios))[estadios==est]]] <- sol_develop$Std.Error
    params$modelim[(1:length(estadios))[estadios==est]]<-model
    
    save(report, file = paste(path,"/PhenologyStats.RData",sep=""))
    save(params, file = paste(path,"/PhenologySims.RData",sep=""))
  }
  rm(list=ls())
}


#########################################################################################################################
# Mortality procedure

DevMort <- function(path,est,modelo,PosModel,OPTplot=TRUE,nx,ny,saveSelec=FALSE)
{
  load(paste(path,"/PhenologySims.RData",sep=""))
  load(paste(path,"/PhenologyStats.RData",sep=""))
  estadios<-params$estadios
  
  opc<-1
  datos<-list(1)
  
  for(i in 1:length(estadios)) # CAMBIO
  {
    datos[[i]]<-read.table(paste(path,"/Data/",estadios[i],".txt",sep="")) # CAMBIO
  }
  
  temperaturas  <-  temp(datos, estadios, est)
  tp<-temperaturas$temperature
  temps<-length(tp)
  
  mort<-mortality(opc, datos, estadios, est, tp)
  datm<-mort$datam
  
  IDname<-modelo[PosModel]
  Temp<-MRmodels(IDname,datm)
  for(i in 1:length(Temp$NamesO)){assign((Temp$NamesO)[i],(Temp$Objs)[[i]])}
  
  labx=expression(paste("temperature (", degree, "C)"));laby="mortality (%)";titulo=IDname
  
#   jpeg(paste(path,"/temporal.jpg",sep=""))
#   pbmortal<-pruebamortal("mortal",modelm,datm,inim,corrx=c(0,50),corry=c(0,100),mini=-5,maxi=55,labx,laby,titulo)
#   dev.off()

  limit<-"yes"
#   inim<-pbmortal$ini
  
  ## Escojemos el algoritmo
  if(modelm < 7){alg<-"Newton"}
  if(modelm > 6){alg<-"Marquardtr"}
  
  pesos<-"LS"
  fmort<-dead_func("mortal",modelm, datm, alg, inim, pesos,weights)
  estimor<-fmort$estimados
  g<-fmort$ecua
  gg<-fmort$f
  stdmortg <-  fmort$stdmort
  modelo<- fmort$modelo
  modelim<-fmort$modelo
  
  sol_mort<-coef_mort("mortal",modelm,estimor,stdmortg,modelo,gg,datm,alg,pesos,weights,g,modelim=modelim)
  
  if(saveSelec){titulo=""}
  if(OPTplot){
    plot_mort<-grafmort("mortal",modelm,estimor,g,datm,corrx=c(nx[1],nx[2]),corry=c(0,100),mini=0,maxi=60,limit,1,labx,laby,titulo)
  }
  
  if(saveSelec){
    report$fenologia$Mortality[[(1:length(estadios))[estadios==est]]]<-datm
    
    params$hfeno$mortal[[(1:length(estadios))[estadios==est]]] <- gg
    params$hfeno$pmortal[[(1:length(estadios))[estadios==est]]] <- estimor
    params$hfeno$StEr_Mort[[(1:length(estadios))[estadios==est]]] <- sol_mort$Std.Error
    
    save(report, file = paste(path,"/PhenologyStats.RData",sep=""))
    save(params, file = paste(path,"/PhenologySims.RData",sep=""))
  }
  rm(list=ls())
}

#########################################################################################################################
# Senescence procedure

DevSenes <- function(path,est,modelo,PosModel,OPTplot=TRUE,nx,ny,saveSelec=FALSE)
{
  
  load(paste(path,"/PhenologySims.RData",sep=""))
  load(paste(path,"/PhenologyStats.RData",sep=""))
  estadios<-params$estadios
  
  datashap0 <- report$fenologia[[1]][[(1:length(estadios))[estadios==est]]][,c(1,5,6,7)] # putting only the necessesary
  datashap <- data.frame(x=datashap0[,1],y=1/datashap0[,2],Lower=1/datashap0[,4],Upper=1/datashap0[,3])
  datashap2 <- datashap; colnames(datashap2)=c("x","y","y","y")
  datao=rbind(datashap2[,c(1,2)],datashap2[,c(1,3)],datashap2[,c(1,4)])
  
  IDname<-modelo[PosModel]
  Temp<-SNCmodels(IDname,datashap) ## Verificar
  for(i in 1:length(Temp$NamesO)){assign((Temp$NamesO)[i],(Temp$Objs)[[i]])}

  #jpeg(paste(path,"/temporal.jpg",sep=""))
  shapprueba<-prueba(model,datashap,datao,ini,corrx=c(0,55),corry=c(0,0.9),punt=1:6,labx="X",laby="Y",titulo="Title",grises=T)
  #dev.off()
  
  ini<-shapprueba$ini
  coefi<-shapprueba$coefi
  p<-shapprueba$p
  
  shap<-shape(model,datashap,datao,ini,coefi)
  estshap<-shap$estimados
  g<-shap$f
  p  <-  shap$p
  stderro<-shap$stderro
  
  sol_develop<-stat_develop(model,datashap,datao,estshap,coefi,stderro)    
  qtt<-sol_develop$q
  sdli<-sol_develop$sdli
  
  TempNY<-seq(ny[1],ny[2],length.out=round((ny[2]-ny[1])/(0.1*(ny[2]-0))))
  scaleY<-TempNY[2]-TempNY[1];Scales<-c(0.01,0.02,0.04,0.05,0.1,0.2,0.4,0.5)
  DEuq<-(scaleY-Scales)^2
  scaleY<-Scales[DEuq==min(DEuq)]

  if(saveSelec){IDname=""}
  if(OPTplot){
    plot_shape<-grafshape(model,estshap,datashap,qtt,sdli,corrx=c(nx[1],nx[2]), corry=c(ny[1],ny[2]),mini=0,maxi=100,coefi,limit="yes",1,labx=expression(paste("temperature (", degree, "C)")), titulo=IDname,laby="Development rate (1/days)", grises=FALSE, scaleY=scaleY, scaleX=5, est=est, estadios=estadios)
  }
  
  if(saveSelec){
    report$fenologia$Senes<-datashap
    
    if(est==estadios[(length(estadios))-1]){
      params$hfeno$fh_h <- sol_develop$ecuaci
      params$hfeno$pfh_h <- c(as.matrix(estshap)[1,])
      params$hfeno$StEr_DRateH <- sol_develop$Std.Error
      params$modelm[1]<-model
    }else{
      params$hfeno$fm_m <- sol_develop$ecuaci
      params$hfeno$pfm_m <- c(as.matrix(sol_develop$parmer)[1,])
      params$hfeno$StEr_DRateM <- sol_develop$Std.Error
      params$modelm[2]<-model
    }
    
    save(report, file = paste(path,"/PhenologyStats.RData",sep=""))
    save(params, file = paste(path,"/PhenologySims.RData",sep=""))
  }
  rm(list=ls()) 
}


#########################################################################################################################
# Total oviposition procedure

DevTotOvi <- function(path,est,modelo,PosModel,OPTplot=TRUE,nx,ny,saveSelec=FALSE)
{
  load(paste(path,"/PhenologySims.RData",sep=""))
  load(paste(path,"/PhenologyStats.RData",sep=""))
  estadios<-params$estadios
  OvipFile<-params$ovip
  opc<-1
  datos<-list(1)
  
  for(i in 1:length(estadios)) # CAMBIO
  {
    datos[[i]]<-read.table(paste(path,"/Data/",estadios[i],".txt",sep="")) # CAMBIO
  }
  
  dato<-read.table(paste(path,"/Data/",OvipFile[1],".txt",sep=""))
  
  temperaturas  <-  temp(datos, estadios, est)
  tp<-temperaturas$temperature
  colum=2
  taz<-taza(opc,dato,tp,colum)
  
  datfemal<-taz$femal
  dattodo<-taz$todo
  datm<-datfemal
  
  IDname<-modelo[PosModel]
  Temp<-TOmodels(IDname,datm)
  for(i in 1:length(Temp$NamesO)){assign((Temp$NamesO)[i],(Temp$Objs)[[i]])}
  
  labx=expression(paste("temperature (", degree, "C)"));laby="total oviposition";titulo=IDname
  
#   jpeg(paste(path,"/temporal.jpg",sep=""))
#   pbmortal<-pruebamortal("taza",modelm,datm,inim,corrx=c(0,50),corry=c(0,200),mini=-5,maxi=55,labx,laby,titulo)
#   dev.off()
  
  limit<-"yes"
#   inim<-pbmortal$ini
  
  ## Escojemos el algoritmo
  if(modelm < 7){alg<-"Newton"}
  if(modelm > 6){alg<-"Marquardtr"}
  
  pesos<-"LS"
  fmort<-dead_func("taza",modelm, datm, alg, inim, pesos,weights)
  estimor<-fmort$estimados
  g<-fmort$ecua
  gg<-fmort$f
  stdmortg <-  fmort$stdmort
  modelo<- fmort$modelo
  modelim<-fmort$modelo
  
  sol_mort<-coef_mort("taza",modelm,estimor,stdmortg,modelo,gg,datm,alg,pesos,weights,g=g,modelim=modelim)
  #ny=c(0,480)
  TempNY<-seq(ny[1],ny[2],length.out=round((ny[2]-ny[1])/(0.1*(ny[2]-0))))
  scaleY<-TempNY[2]-TempNY[1];Scales<-c(2,5,10,20,40,50,100,200)
  DEuq<-(scaleY-Scales)^2
  scaleY<-Scales[DEuq==min(DEuq)]

  if(saveSelec){titulo=""}
  
  if(OPTplot){
    plot_mort<-grafmort("taza",modelm,estimor,g,datm,corrx=c(nx[1],nx[2]),corry=c(ny[1],ny[2]),mini=0,maxi=60,limit,1,labx,laby,titulo,scaleY=scaleY)
  }
    
  if(saveSelec){
    report$fenologia$OvipTotal<-datm
    
    params$hfeno$ftazaeh_h <- gg
    params$hfeno$ptazaeh_h <- c(as.matrix(estimor)[1,])
    params$hfeno$StEr_ToviH <- sol_mort$Std.Error
    
    save(report, file = paste(path,"/PhenologyStats.RData",sep=""))
    save(params, file = paste(path,"/PhenologySims.RData",sep=""))
  }
  rm(list=ls())   
}


#########################################################################################################################
# Relative oviposition procedure

RelOvi <- function(path,est,modelo,PosModel,OPTplot=TRUE,nx,ny,saveSelec=FALSE)
{
  load(paste(path,"/PhenologySims.RData",sep=""))
  load(paste(path,"/PhenologyStats.RData",sep=""))
  estadios<-params$estadios
  OvipFile<-params$ovip
  mediana<-report$mediana
  tp<-report$tp
  opc<-1
  #dato<-read.table("D:/LH/Paratrioza-Cohort-2016/Data/Oviposition.txt")
  dato<-read.table(paste(path,"/Data/",OvipFile[1],".txt",sep=""))
  dato<-dato[!is.na(match(dato[,1],tp)),]
  colum=2
  
  
  ovi<-oviposicion(opc,dato,colum,mediana, estadios, tp)
  ovifemal<-ovi$femal
  corrx=c(nx[1],nx[2]); corry=c(0,100); mini<-0; maxi<-nx[2]
  labx="Normalized female age (days / median survival time)"
  laby="Cumulative oviposition rate (%)"
  
  IDname<-modelo[PosModel]
  titulo=IDname;legx=1;legy=60
  Temp<-ROmodels(IDname)
  for(i in 1:length(Temp$NamesO)){assign((Temp$NamesO)[i],(Temp$Objs)[[i]])}
  
  limit<-"yes"
  alg<-"Marquardtr"

#   jpeg(paste(path,"/temporal.jpg",sep=""))
#   pbovi<-pruebaovi(modelm, ovifemal,inim,corrx,corry,mini,maxi,labx,laby,titulo)
#   dev.off()
  
#   inim<-pbovi$ini
  
  pesos<-"LS"
  weights<-1
  ovip<-ovi_func(modelm, ovifemal, alg, inim,pesos,weights)
  estimor<-ovip$estimados
  g<-ovip$f
  frm<-ovip$frm
  stdovi  <-  ovip$stdovi
  xi=agenorm(estimor,modelm,xini=0.1,ni=20)$xi
  
  sol_ovi<-coef_ovi(modelm,estimor,stdovi,g,frm,ovifemal,alg,pesos,weights)
  frsel<-sol_ovi$frames
  qto=sol_ovi$qto
  sdli=sol_ovi$sdli
  table5  <-sol_ovi$parovi
  if(saveSelec){titulo=""}

  if(OPTplot){
    plot_ovi<-grafovi(modelm,estimor,g,ovifemal,corrx,mini,maxi,legx,legy,sdli,qto,limit,tam=c(1.6,0.8),labx,laby,titulo,xi=xi)
  }
    
  if(saveSelec){
    report$fenologia$OvipTotal<-ovifemal
    
    params$hfeno$fovih_h <- sol_ovi$gg
    params$hfeno$povih_h <- c(as.matrix(estimor)[1,])
    params$hfeno$StEr_RoviH <- sol_ovi$Std.Error
    params$xi <- xi
    
    save(report, file = paste(path,"/PhenologyStats.RData",sep=""))
    save(params, file = paste(path,"/PhenologySims.RData",sep=""))
  }
  
  rm(list=ls())  
}


#########################################################################################################################
# Transmission Rate procedure

TransmRate <- function(path,Namflucfile,modelo,PosModel,OPTplot=TRUE,nx,ny,saveSelec=FALSE) ### Falta terminar este Codigo!!!!!
{
  #source("D:/_BK-D/Pablo/R archivos/shiny-Examples/041-dynamic-ui-ILCYM/lib/dev_rate_new.r")
  #path<-"D:/LH/Paratrioza-Cohort-2016"
  load(paste(path,"/PhenologySims.RData",sep=""))
  load(paste(path,"/PhenologyStats.RData",sep=""))
  estadios<-params$estadios
  
  #Namflucfile<-input$transmfile
  datashap0<-read.table(Namflucfile$datapath[1])

  #datashap0<-read.table("D:/LH/Transmission Files/TransmissionData.txt")
  
  #datashap0 <- report$fenologia[[1]][[(1:length(estadios))[estadios==est]]][,c(1,5,6,7)] # putting only the necessesary
  #datashap <- data.frame(x=datashap0[,1],y=1/datashap0[,2],Lower=1/datashap0[,4],Upper=1/datashap0[,3])
  datashap <- data.frame(x=datashap0[,1],y=datashap0[,2],Lower=datashap0[,2],Upper=datashap0[,2])
  datashap2 <- datashap; colnames(datashap2)=c("x","y","y","y")
  datao=rbind(datashap2[,c(1,2)],datashap2[,c(1,3)],datashap2[,c(1,4)])
  
  IDname<-modelo[PosModel] # al final se va a editar
  #IDname<-"Tb Model"
  Temp<-DRmodels(IDname,datashap) ## Verificar
  for(i in 1:length(Temp$NamesO)){assign((Temp$NamesO)[i],(Temp$Objs)[[i]])}
  
  #jpeg(paste(path,"/temporal.jpg",sep=""))
  shapprueba<-prueba(model,datashap,datao,ini,corrx=c(0,55),corry=c(0,0.9),punt=1:6,labx="X",laby="Y",titulo="Title",grises=T)
  #dev.off()
  
  ini<-shapprueba$ini
  coefi<-shapprueba$coefi
  p<-shapprueba$p
  
  shap<-shape(model,datashap,datao,ini,coefi)
  estshap<-shap$estimados
  g<-shap$f
  p<-shap$p
  stderro<-shap$stderro
  
  sol_develop<-stat_develop(model,datashap,datao,estshap,coefi,stderro)    
  qtt<-sol_develop$q
  sdli<-sol_develop$sdli
  
  #ny<-c(0,1)
  TempNY<-seq(ny[1],ny[2],length.out=round((ny[2]-ny[1])/(0.1*(ny[2]-0))))
  scaleY<-TempNY[2]-TempNY[1];Scales<-c(0.01,0.02,0.04,0.05,0.1,0.2,0.4,0.5)
  DEuq<-(scaleY-Scales)^2
  scaleY<-Scales[DEuq==min(DEuq)]
  
  # if(saveSelec){IDname=""}
  #nx<-c(0,50)
  est<-estadios[length(estadios)-1]
  if(OPTplot){
    plot_shape<-grafshape(model,estshap,datashap,qtt,sdli,corrx=c(nx[1],nx[2]), corry=c(ny[1],ny[2]),mini=0,maxi=100,coefi,limit="yes",1,labx=expression(paste("temperature (", degree, "C)")), titulo=IDname,laby="Development rate (1/days)", grises=FALSE, scaleY=scaleY, scaleX=5, est=est, estadios=estadios)
  }

  if(saveSelec){
    report$fenologia$TransmRate<-datashap
    params$transm$ftr_tr <- sol_develop$ecuaci
    tempPAR<-sol_develop$parmer;tempPAR2<-sol_develop$parmer
    if(is.matrix(tempPAR) || is.data.frame(tempPAR)){tempPAR2<-c(as.matrix(tempPAR[1,]));names(tempPAR2)<-colnames(tempPAR);rm(tempPAR)}
    params$transm$ptr_tr <- tempPAR2
    params$transm$StEr_TRate <- sol_develop$Std.Error
    params$modeltransm<-model

    save(report, file = paste(path,"/PhenologyStats.RData",sep=""))
    save(params, file = paste(path,"/PhenologySims.RData",sep=""))
  }
  rm(list=ls())
}


#########################################################################################################################
# Deterministic simulation procedure - Constant temperature

DeteSim <- function(path,N,M,Vtempt,Intv,OPTplot=TRUE)
{
  load(paste(path,"/PhenologySims.RData",sep=""))
  hfeno<-params$hfeno
  xi<-params$xi
  
  #temps<-28:35
  temps<-seq(Vtempt[1],Vtempt[2],Intv)
  reps <- rep(1,length(temps))
  
  steps<-48
  #modelim=c(modelim,modelm)
  sexratio<-0.5 # se puede extraer OJOOOOO
  isFixed=TRUE
  pars<-simultemp(N,sexratio, isFixed,temp=temps, reps=reps, xi, steps,poli=1, params)
  
  direc=paste(path,"/DeterministicSimulation","/",sep="")
  
  sink(paste(path,"/DeterministicSimulation/","ParametersEstimated",".txt",sep=""))
  print(pars)
  sink()
  
  if(OPTplot){
    jpeg(paste(path,"/DeterministicSimulation/","ParametersEstimated",".jpg",sep=""), width = 9, height = 10, units = 'in', res = 500)
    graphsimulp(pars=pars,direc=direc, ecua=FALSE, nomec=FALSE, ejex=NA, ejey=NA, tecu=1.1, grises=FALSE, tit=TRUE, ax=1, ay=1)
    dev.off()
    
    result=graphsimulp(pars=pars,direc=direc, ecua=FALSE, nomec=FALSE, ejex=NA, ejey=NA, tecu=1.1, grises=FALSE, tit=TRUE, ax=1, ay=1)
  }
  
  rm(list=ls())  
}

#########################################################################################################################
# Deterministic simulation procedure - Fluctuating temperature

DeteSimFluc <- function(path,numIni,Table,OPTplot=TRUE,xlegend=300)
{
  #source('D:/_BK-D/Pablo/R archivos/shiny-Examples/041-dynamic-ui-ILCYM/lib/dete_sim.r')
  load(paste(path,"/PhenologySims.RData",sep=""))
  #Table<-read.table("D:/_BK-D/Pablo/cip/ilcym/PTM/simulacion geografica - temperatura fluctuante/temp fluctuante.txt")
  Table ## OK
  numIni ## OK
  
  modelim<-params$modelim
  modelm<-params$modelm
  estadios<-params$estadios
  hfeno<-params$hfeno
  xi<-params$xi
  steps<-48
  
  M<-366
  sexratio<-0.5 # extraer
  
  Rs=rep(sexratio,365)
  modelim=c(modelim,modelm)
  cuadro <- matrizA(estadios, hfeno, Table, steps,modelim)
  ageclases <- AgeClases(cuadro, estadios, hfeno)
  estadiosAges <- ageclases$estadiosAges
  oviFreq <- ageclases$oviFreq
  ageClassMatriz <- ageclases$ageClassMatriz
  
  Day <- 1
  severalYear=FALSE
  Steps=364
  
  simu<-simulation2(Day, estadiosAges, oviFreq, ageClassMatriz, hfeno, cuadro, estadios,numIni,sexratio, Rs, Steps,severalYear=FALSE)
  matrizOut<-simu$matrizOut
  matrizOutDead<-simu$matrizOutDead
  
  sink(paste(path,"/DeterministicSimulation/","LifeFrequencyEstimatedFluctuating",".txt",sep=""))
  print(matrizOut)
  sink()
  
  if(OPTplot){
    corrx1=0
    corrx2=400
    lgx=xlegend
    lgy=10
    labx="Age (days)"
    laby="Individuals"
    titulo="Age-specific survival 1st year"
    grafSimDete(matrizOut, estadios, labx,laby,titulo,lgx,lgy,corrx1,corrx2)
  }
  
  rm(list=ls())
}

#########################################################################################################################
# Validation procedure with Fluctuating Life Table

DeteSimVal <- function(path,vidalife,Table,OPTplot=TRUE,xlegend=300)
{
  #source('D:/_BK-D/Pablo/R archivos/shiny-Examples/041-dynamic-ui-ILCYM/lib/validate.r')
  load(paste(path,"/PhenologySims.RData",sep=""))
  #load('D:/LH/PhthorimaeaOperculella-Cohort-Nuevo/PhenologySims.RData')
  #Table<-read.table("D:/LH/Data validacion de PTM/temp fluctuante.txt") # 
  Table ## OK
  #vidalife <- read.table("D:/LH/Data validacion de PTM/tabla de vida 1.txt") # datos de tabla de vida
  N<-ncol(vidalife) ## OK
  
  modelim<-params$modelim
  modelm<-params$modelm
  estadios<-params$estadios
  hfeno<-params$hfeno
  xi<-params$xi
  steps<-48
  modelim=c(modelim,modelm)
  
  cuadro <- matrizA(estadios, hfeno, Table, steps,modelim) # funcion
  M<-nrow(vidalife)
  sexratio<-0.5 # extraer
  
  Rs=rep(sexratio,nrow(vidalife)) # Corregir!!!!!!!!!
  
  nhem2<-function(vec,Fem){n4=length(vec[vec==Fem]);if(n4!=0){n4=1};return(n4)}
  nfem2 = sum(apply(vidalife,2,nhem2,estadios[length(estadios)-1])) ## numero de hembras obtenidas en la tabla de vida
  
  se <- function(x) sd(x)/sqrt(length(x))
  Tablelife <-life.table2(vidalife, estadios, ovi=TRUE)$life.table
  p<-PARAlife (N,estadios,Tablelife,Rs2 = NULL,ovipo = NULL, nfem2 = nfem2)$parametro
  
  sink(paste(path,"/ModelValidation/","ValidationModel",".txt",sep=""))
  VIDA(vidalife,Tablelife,p, cuadro, estadios, N, M, hfeno, Rs)
  sink()
  
  if(OPTplot){
    corrx=c(0,100)
    corry=c(0,100)
    #lgx=60
    lgx=xlegend
    lgy=100
    labx="Age (days)"
    laby="Individuals"
    titulo="Model validation"
    graffvida(Tablelife,cuadro,estadios,N,M,hfeno,labx,laby,corrx,corry,lgx,lgy, Rs)
  }
  
  rm(list=ls())  
}

#########################################################################################################################
# Geographic simulation procedure
GeoSimFluc <- function(dir,Index=NULL,brks=NULL,HistRange=FALSE)
{
  if(!is.null(Index)){nombreINDX <- switch(Index,"ERI"="ERI.asc","GI"="GI.asc","AI"="AI.asc")}
  if(HistRange){
    par(mfrow=c(1,3))
    
    ERI = readAsciiGrid(paste(dir,"/","ERI.asc",sep=""))  ### extraemos la variable de altura 
    cap=raster(ERI)
    rng <- c(minValue(cap), maxValue(cap))
    hist(cap,main="ERI",xlab=paste("min=",rng[1]," and max=",rng[2],sep=""))
    rm(ERI);rm(cap)
    
    GI = readAsciiGrid(paste(dir,"/","GI.asc",sep=""))  ### extraemos la variable de altura 
    cap=raster(GI)
    rng <- c(minValue(cap), maxValue(cap))
    hist(cap,main="GI",xlab=paste("min=",rng[1]," and max=",rng[2],sep=""))
    rm(GI);rm(cap)
    
    AI = readAsciiGrid(paste(dir,"/","AI.asc",sep=""))  ### extraemos la variable de altura 
    cap=raster(AI)
    rng <- c(minValue(cap), maxValue(cap))
    hist(cap,main="AI",xlab=paste("min=",rng[1]," and max=",rng[2],sep=""))
    rm(AI);rm(cap)
  }else{
    INDX = readAsciiGrid(paste(dir,"/",nombreINDX,sep=""))  ### extraemos la variable de altura 
    cap=raster(INDX)
    rng <- c(minValue(cap), maxValue(cap))
    if(Index=="ERI"){
      brks <- as.numeric(as.character(unlist(strsplit(brks, ","))))
      cols <- c("gray70",brewer.pal(9, "Greens")[8:5],brewer.pal(8, "YlOrRd")[2:5],"#8C510A",brewer.pal(8, "YlOrRd")[7])
      ZLIM <- c(0,1)
    }else{
      brks <- as.numeric(as.character(unlist(strsplit(brks, ","))))
      cols <- c("gray70","aquamarine4","chartreuse2","yellow","darkorange2","#8C510A","firebrick2")
      ZLIM <- c(0,rng[2])
    }
    #png(paste(dir,"/",Index,".png",sep=""), width = 14, height = 10, units = 'in', res = 300)
    data(wrld_simpl)
    plot(cap, breaks=brks, col=cols,zlim=ZLIM,axis.args=list(cex.axis=1.2),legend.width=1.2)
    plot(wrld_simpl, lwd=0.8, col="transparent",add=TRUE)
    #dev.off()
    rm(INDX);rm(cap)
  }
}

#########################################################################################################################
# Geographic simulation procedure for Transmission

GeoSimFlucT <- function(dir,Index=NULL,brks=NULL,HistRange=FALSE)
{
  if(!is.null(Index)){nombreINDX <- switch(Index,"PT"="ITT.asc","PAT"="PTP.asc")}
  if(HistRange){
    par(mfrow=c(1,2))

    PT = readAsciiGrid(paste(dir,"/","ITT.asc",sep=""))  ### extraemos la variable de altura
    cap=raster(PT)
    rng <- c(minValue(cap), maxValue(cap))
    hist(cap,main="PT",xlab=paste("min=",rng[1]," and max=",rng[2],sep=""))
    rm(PT);rm(cap)

    PAT = readAsciiGrid(paste(dir,"/","PTP.asc",sep=""))  ### extraemos la variable de altura
    cap=raster(PAT)
    rng <- c(minValue(cap), maxValue(cap))
    hist(cap,main="PAT",xlab=paste("min=",rng[1]," and max=",rng[2],sep=""))
    rm(PAT);rm(cap)

  }else{
    INDX = readAsciiGrid(paste(dir,"/",nombreINDX,sep=""))  ### extraemos la variable de altura
    cap=raster(INDX)
    rng <- c(minValue(cap), maxValue(cap))
    if(Index=="PT"){
      brks <- as.numeric(as.character(unlist(strsplit(brks, ","))))
      cols <- c("gray70",brewer.pal(9, "Greens")[8:5],brewer.pal(8, "YlOrRd")[2:5],"#8C510A",brewer.pal(8, "YlOrRd")[7])
      ZLIM <- c(0,1)
    }else{
      brks <- as.numeric(as.character(unlist(strsplit(brks, ","))))
      cols <- c("gray70","aquamarine4","chartreuse2","yellow","darkorange2","#8C510A","firebrick2")
      ZLIM <- c(0,rng[2])
    }
    #png(paste(dir,"/",Index,".png",sep=""), width = 14, height = 10, units = 'in', res = 300)
    data(wrld_simpl)
    plot(cap, breaks=brks, col=cols,zlim=ZLIM,axis.args=list(cex.axis=1.2),legend.width=1.2)
    plot(wrld_simpl, lwd=0.8, col="transparent",add=TRUE)
    #dev.off()
    rm(INDX);rm(cap)
  }
}
