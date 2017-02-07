###############################################################
## Non Linear Modelling #######################################

library(MASS)
setwd("D:/_BK-D/Pablo/cip/Heidy/Transmision")
source("functions-modified.R")
# dataset of the transmision

    # x1<-c(10, 12,  15, 16, 18,  20,	25)
    # y1<-c(0.02, 0.14,  0.64, 0.19,	0.04,	0.04,	0.03)
#     x<-c(10, 12,  15, 20,  25)
#     y<-c(0.02, 0.14,  0.64,	0.04,	0.03)

    # x<-c(10, 12,  13.5,15, 16, 20,  25)
    # y<-c(0.02, 0.14,  0.69,0.64, 0.19,  0.04,	0.03)

    # x<-c(12,  15, 18,  20,	25,10,12,14,15,16,18,20,25,10,12,14,15,16,18,20,25)
    # y<-c(0.14,  0.64, 0.04,	0.04,	0.03,0.04,0.1,0.4,0.66,0.14,0.06,0.06,0.02,0,0.18,0.38,0.62,0.24,0.02,0.02,0.04)

    # x<-c(10,12,14,15,16,18,20,25,10,12,14,15,16,18,20,25) # 2 repeticiones
    # y<-c(0.04,0.1,0.4,0.66,0.14,0.06,0.06,0.02,0,0.18,0.38,0.62,0.24,0.02,0.02,0.04)

    xX<-c(10,12,14,15,16,18,20,25,10,12,14,15,16,18,20,25)
    yY<-c(0.04,0.1,0.4,0.66,0.14,0.06,0.06,0.02,0,0.18,0.38,0.62,0.24,0.02,0.02,0.04)


    x<-c(10,12,14,15,18,20,25,10,12,14,15,18,20,25,15,15,15)
    y<-c(0.04,0.1,0.4,0.66,0.06,0.06,0.02,0,0.18,0.38,0.62,0.02,0.02,0.04,0.66,0.66,0.665)
    
    xl<-c(0,max(x)+5) # se puede editar
    yl<-c(0,max(y)+0.3) # se puede editar


# non linear equation

    #Janish-1 
    #model<-46
    Fyx=as.formula(paste("y ~ 2/(Dmin*(exp(K*(x-Topt)) + exp((-K)*(x-Topt))))")) ## ecuacion
    Ival=list(Dmin=4.8,Topt=quantile(x,0.25),K=0.47)

    # Tb Model (Logan) sy*exp(b*(x-Tb)-exp(b*(x-Tb)/DTb))
    #model<-26
    # Fyx=as.formula(paste("y ~ sy*exp(b*(x-Tb)-exp(b*(x-Tb)/DTb))")) ## ecuacion
    # Ival=list(sy=0.85,b=0.0924,Tb=mean(x)-2,DTb=0.084)

    #model<-shape 3
    # Fyx=as.formula(paste("y ~ (p * (x/(To)) * exp((Ha/1.987) * ((1/To) - (1/x))))/(1 + exp((Hl/1.987) * ((1/Tl) - (1/x))) + exp((Hh/1.987) * ((1/Th) - (1/x))))")) ## ecuacion
    # Ival=list(p=0.1143,To=295.383, Ha=13994.98, Tl=283.2025,Hl=-901531.8,Hh=146384.1,Th=305.9398)


    # Fyx=as.formula(paste("y ~ 2*c/(a^(x-Tm) + b^(Tm-x))")) ## ecuacion
    # Ival=list(c=0.4821, a=41.3314, b=1.7439, Tm=15.6102)
    
    
# assignment of the variables according to the order of the column that has the data table

    out <- nls(Fyx, start = Ival,trace = FALSE)
    SE<-(summary(out))[3]$sigma
    dff<-(summary(out))[4]$df[2]


    sal <- coef(out)
    for (i in names(sal))
    {
      temp <- sal[i]
      storage.mode(temp) <- "double"
      assign(i, temp)
    }


    ff <- function(x){eval(Fyx[[3]])}
    ffup <- function(x){eval(Fyx[[3]])+SE*qt(0.975,dff)}
    fflo <- function(x){eval(Fyx[[3]])-SE*qt(0.975,dff)}

    #png("Grafico_NonLinear.png", width = 12, height = 10, units = 'in', res = 300) # para el grafico
    plot(xX,yY,col=1,pch=19, ylim=yl,xlim=xl)
    curve(ff,add=TRUE,col=2,lwd=1)
    curve(ffup,add=TRUE,col=2,lwd=2,lty=3)
    curve(fflo,add=TRUE,col=2,lwd=2,lty=3)
    
    print(summary(out))
    
    Dfinal2=data.frame(x,y,Ajustado=round(predict(out),4));colnames(Dfinal2)[2]<-"Observado"
    print(Dfinal2)
    #dev.off()

#####################################################################
# Spacial Prediction ################################################

# charging the phenology of Bemisia afer
load("D:/ILCYM/workspace/TrialeurodesVaporariumProject/PhenologySims.RData")

# libraries
library(sp)
library(maptools)
library(rgdal)
library(maps)
library(doRNG)

# states and features

modelim<-params$modelim
modelm<-params$modelm
estadios<-params$estadios
hfeno<-params$hfeno


# coordinates area
# ilon=c(-179.9167,179.91668544)
# ilat=c(-59.91666666,89.91667)
ilon=c(-117.89747,-34.02247) # south america 2.5 minutes
ilat=c(-56.91415,33.29419) # south america 2.5 minutes


# path of the FLT files
# dir1 = "D:/_BK-D/MAPAS/datos mundo - 2000 - 10 mints/Tmin/"
# dir2 = "D:/_BK-D/MAPAS/datos mundo - 2000 - 10 mints/Tmax/"

dir1 = "D:/_BK-D/MAPAS/SouthAmericaFromMexico-2000-2.5min/Tmin/"
dir2 = "D:/_BK-D/MAPAS/SouthAmericaFromMexico-2000-2.5min/Tmax/"

# sub division in areas when there are problems with RAM memory
R=8

dir.out="D:/_BK-D/Pablo/cip/Heidy/Transmision/Sudamerica-2000/" # donde guardaremos los resultados - don't forget "/"
#dir.out="D:/_BK-D/Pablo/cip/Heidy/Transmision/Mundo-2000/" # donde guardaremos los resultados - don't forget "/"
name.out <- "results"

# temperature filters
filtro=c(-5,45)

# object of states are arranged
modelim=c(modelim,modelm)
if(length(estadios)>length(modelim)){modelim = c(modelim,15)}

# generating the raster
file<-zone.div(dir1,dir2,ilon,ilat,R,dir.out,name.out=name.out,out,filtro)
# map of total mortality index
# IMT = readAsciiGrid(paste(dir.out,"ITT.asc",sep=""))
# spplot(IMT[c("ITT.asc")])
