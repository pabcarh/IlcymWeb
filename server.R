library(shiny)
library(minpack.lm)
library(MASS)
#library(shinyBS)


shinyServer(function(input, output, session) {
  observe({
    if (input$do == 0)
      return()
    isolate({
      path<-input$directory
      namePJ<-input$namePJ
      path = choose.dir(getwd(), "Choose a suitable folder")
      path <- gsub("\\\\", "/",path)
      PJdescrip<-data.frame(Names=c("Project Name:","Species Name:","Author:","Date:","Fixed Rate:"),
                            Detail=c(namePJ,input$nameESP,input$nameAUT,input$nameDATE,input$nameRATE))
      dir.create(paste(path,"/",namePJ,sep=""))
      dir.create(paste(path,"/",namePJ,"/Data",sep=""))
      dir.create(paste(path,"/",namePJ,"/DevelopmentRate",sep=""))
      dir.create(paste(path,"/",namePJ,"/DevelopmentTime",sep=""))
      dir.create(paste(path,"/",namePJ,"/Mortality",sep=""))
      dir.create(paste(path,"/",namePJ,"/RelativeOviposition",sep=""))
      dir.create(paste(path,"/",namePJ,"/Senescence",sep=""))
      dir.create(paste(path,"/",namePJ,"/TotalOviposition",sep=""))
      dir.create(paste(path,"/",namePJ,"/DeterministicSimulation",sep=""))
      dir.create(paste(path,"/",namePJ,"/GeographicalSimulation",sep=""))
      dir.create(paste(path,"/",namePJ,"/ModelValidation",sep=""))
      dir.create(paste(path,"/",namePJ,"/Transmission",sep=""))
      dir.create(paste(path,"/",namePJ,"/TransmissionSimulation",sep=""))
      updateTextInput(session, "directory", value=path)
      write.table(PJdescrip,paste(path,"/",namePJ,"/",namePJ,".ilcym",sep=""),row.names = FALSE,col.names = FALSE,sep="\t")
    })
  })
  
  datasetInput <- reactive({
    datos<-list(1)
    inFile <- input$file1
    path<-input$directory
    PATH <- input$file1$name
    
    if (!is.null(PATH))
    {
      for(i in 1:length(inFile$datapath)) # CAMBIO
      {
        datos[[i]]<-read.table(inFile$datapath[i])
        write.table(datos[[i]],paste(path,"/",input$namePJ,"/Data/",PATH[i],sep=""),row.names = FALSE,col.names = FALSE)
      }
      return(PATH)
    }else{PATH<-"no data";return(PATH)}
  })
  
  output$choices <- renderUI({
    PATH<-datasetInput()
    if (is.null(PATH)){
      return(NULL)
    }else{
      chooserInput("mychooser", "Available", "Selected",
                   PATH, c(), size = 12, multiple = TRUE
      )
    }
  })
  
  observeEvent(input$doU, {
    StatesIni<- input$mychooser$left
    States<- input$mychooser$right
    
    style <- isolate(input$style)
    withProgress(message = 'Upload the dataset', style = style, value = 0.1, {
      Sys.sleep(0.4)
      
      if (is.null(States) & !length(StatesIni)==0){
        if(StatesIni == "no data"){cat("Please select your cohort data to process")}
      }else{
        path<-input$directory
        namePJ<-input$namePJ
        estadios0<-gsub(".txt","",States)
        estadios1<-estadios0[-length(estadios0)]
        params<-list();params$estadios<-estadios1
        params$ovip<-estadios0[length(estadios0)]
        report<-list();report$estadios<-estadios0
        TempParams<-list()
        save(params, file = paste(path,"/",namePJ,"/PhenologySims.RData",sep=""))
        save(report, file = paste(path,"/",namePJ,"/PhenologyStats.RData",sep=""))
        #save(TempParams, file = paste(path,"/",namePJ,"/TempParams.RData",sep=""))
        #cat("The files will be copied in the project folder")
        updateTextInput(session, "directory2", value=paste(path,"/",namePJ,sep=""))
        incProgress(0.5,message = "creating workspace")
      }
      setProgress(1)
    })
  })
  
  output$choicesMOD1 <- renderUI({
    ModSelected<-input$modules
    path<-input$directory2
    
    Modul1<-switch(ModSelected,
                 "Development Time" = "images/GrayScale_time-icon3.png",
                 "Development Rate" = "images/GrayScale_drateicon2.png",
                 "Mortality" = "images/GrayScale_moralityicon2.png",
                 "Senescence" = "images/GrayScale_senesicon3.png",
                 "Total Oviposition" = "images/GrayScale_totalicon.png",
                 "Relative Oviposition" = "images/GrayScale_relativeicon2.png",
                 "Transmission Rate" = "images/GrayScale_transmRate.png"
    )
    
    if (path != "no project selected"){
      print(tags$img(src=Modul1,height="20",width="20"))
    }else{
      return(NULL)
    }
    
  })
  
  ############################################################################
  # Model Builder Inputs
  
  observe({
    if (input$do2 == 0)
      return()
    isolate({
      path<-input$directory2
      path = choose.dir(getwd(), "Choose a suitable folder")
      path <- gsub("\\\\", "/",path)
      updateTextInput(session, "directory2", value=path)
      globalpath <<- path
    })
    
  })
  
  output$choicesMB1 <- renderUI({
    ModSelected<-input$modules
    path<-input$directory2
    if (path != "no project selected" & !is.null(path)){
      load(paste(path,"/PhenologySims.RData",sep=""))
      estadios<-gsub(".txt","",params$estadios)
      estadios1<-estadios
      ImputEstadios(estadios,ModSelected)
    }else{
      return(NULL)
    }
    
  })
  
  output$choicesMB2 <- renderUI({
    ModSelected<-input$modules
    path<-input$directory2
    if (path != "no project selected" & !is.null(path)){
      ImputModelList(ModSelected)
    }else{
      return(NULL)
    }
    
  })
  
  output$choicesMB3 <- renderUI({
    ModSelected<-input$modules
    path<-input$directory2
    if (path != "no project selected" & !is.null(path)){
      ImputModelListNX(ModSelected)
    }else{
      return(NULL)
    }
    
  })
  
  output$choicesMB4 <- renderUI({
    ModSelected<-input$modules
    path<-input$directory2
    if (path != "no project selected" & !is.null(path)){
      ImputModelListNY(ModSelected)
    }else{
      return(NULL)
    }
    
  })
  
  output$choicesMB5 <- renderUI({
    ModSelected<-input$modules
    path<-input$directory2
    modelo1 <- switch(ModSelected,"Development Time"=input$modelo,"Development Rate"=input$modelo$right,"Mortality"=input$modelo$right,"Senescence"=input$modelo$right,"Total Oviposition"=input$modelo$right,"Relative Oviposition"=input$modelo$right,"Transmission Rate"=input$modelo$right)
    
    if (length(modelo1) == 1 & path != "no project selected" & !is.null(path)){
      actionButton("doS", "Save",icon=icon("floppy-o"))
    }else{
      return(NULL)
    }
    
  })
  
  output$graphsMB1 <- renderUI({
    ModSelected<-input$modules
    path<-input$directory2
    
    modelo1 <- switch(ModSelected,"Development Time"=input$modelo,"Development Rate"=input$modelo$right,"Mortality"=input$modelo$right,"Senescence"=input$modelo$right,"Total Oviposition"=input$modelo$right,"Relative Oviposition"=input$modelo$right,"Transmission Rate"=input$modelo$right)

    if (length(modelo1) == 1 & path != "no project selected"){
      plotOutput("plot")
    }else{
      n <- length(modelo1)
      get_plot_output_list(n)
    }
    
  })
  
  output$graphsMB2 <- renderUI({
    ModSelected<-input$modules
    path<-input$directory2
    if (path != "no project selected"){
      verbatimTextOutput("summary")
    }else{
      return(NULL)
    }
    
  })
  
  ############################################################################
  # Model Builder Procesing modules
  
  output$plot <- renderPlot({
    #system.file(package="grDevices")
    #grDevices:::rversion()
    #withProgress(message = 'Generating Plot', detail = "part 0", value = 0, {
    if (input$doM == 0)
      return()
    isolate({
      
      ModSelected<-input$modules
      #saveSelec<-input$saveSelec
  
      if (ModSelected == "Development Time"){
        source('lib/developmentTime.r')
        #incProgress(0.1, detail = "Reading development time functions")
        
        path<-input$directory2
        est<-req(input$estadio)
        modelo<-req(input$modelo)
        nx<-req(input$nx)
        ny<-req(input$ny)
        PosModel<-1
        
          DevTime(path,est,modelo,PosModel,OPTplot=TRUE,nx,ny)

        #updateCheckboxInput(session, "saveSelec", value = FALSE)
      }
      
      if (ModSelected == "Development Rate"){
        source('lib/dev_rate_new.r')
        
        path<-input$directory2
        est<-req(input$estadio)
        modelo<-req(input$modelo$right)
        nx<-req(input$nx)
        ny<-req(input$ny)
        PosModel<-1
  
          DevRate(path,est,modelo,PosModel,OPTplot=TRUE,nx,ny)
        #updateCheckboxInput(session, "saveSelec", value = FALSE)
      }
      
      if (ModSelected == "Mortality"){
        source('lib/mortality.r')
        
        path<-input$directory2
        est<-input$estadio
        modelo<-input$modelo$right
        nx<-input$nx
        ny<-input$ny
        PosModel<-1
        
          DevMort(path,est,modelo,PosModel,OPTplot=TRUE,nx,ny)
        #updateCheckboxInput(session, "saveSelec", value = FALSE)
      }
      
      if (ModSelected == "Senescence"){
        source('lib/senescence.r')
        
        path<-input$directory2
        est<-input$estadio
        modelo<-input$modelo$right
        nx<-input$nx
        ny<-input$ny
        PosModel<-1
        
          DevSenes(path,est,modelo,PosModel,OPTplot=TRUE,nx,ny)
        #updateCheckboxInput(session, "saveSelec", value = FALSE)
      }
      
      if (ModSelected == "Total Oviposition"){
        source('lib/totalOviposition.r')
        
        path<-input$directory2
        est<-input$estadio
        modelo<-input$modelo$right
        nx<-input$nx
        ny<-input$ny
        PosModel<-1
        
          DevTotOvi(path,est,modelo,PosModel,OPTplot=TRUE,nx,ny)
        #updateCheckboxInput(session, "saveSelec", value = FALSE)
      }
      
      if (ModSelected == "Relative Oviposition"){
        source('lib/relativeOviposition.r')
        
        path<-input$directory2
        est<-input$estadio
        modelo<-input$modelo$right
        nx<-input$nx
        ny<-input$ny
        PosModel<-1
        
          RelOvi(path,est,modelo,PosModel,OPTplot=TRUE,nx,ny)
        #updateCheckboxInput(session, "saveSelec", value = FALSE)
      }
      
      if (ModSelected == "Transmission Rate"){
        source('lib/dev_rate_new.r')
        
        path<-input$directory2
        modelo<-req(input$modelo$right)
        nx<-req(input$nx)
        ny<-req(input$ny)
        PosModel<-1
        Namflucfile<-req(input$transmfile)
        
        TransmRate(path,Namflucfile,modelo,PosModel,OPTplot=TRUE,nx,ny)
        #updateCheckboxInput(session, "saveSelec", value = FALSE)
      }
      
    
    }) # ending progress
    
  }, height = 560, width = 800)
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    
    ModSelected<-input$modules
    #saveSelec<-input$saveSelec
    
    if (input$doM == 0)
      return()
    isolate({

      if (ModSelected == "Development Time"){
        source('lib/developmentTime.r')
        
        path<-input$directory2
        est<-input$estadio
        modelo<-input$modelo
        nx<-input$nx
        ny<-input$ny
        PosModel<-1
        
          DevTime(path,est,modelo,PosModel,OPTplot=FALSE,nx,ny)

        #updateCheckboxInput(session, "saveSelec", value = FALSE)
      }
      
      if (ModSelected == "Development Rate"){
        source('lib/dev_rate_new.r')
        
        path<-input$directory2
        est<-input$estadio
        modelo<-input$modelo$right
        nx<-input$nx
        ny<-input$ny
        PosModel<-1
        
          DevRate(path,est,modelo,PosModel,OPTplot=FALSE,nx,ny)

        #updateCheckboxInput(session, "saveSelec", value = FALSE)
      }
      
      if (ModSelected == "Mortality"){
        source('lib/mortality.r')
        
        path<-input$directory2
        est<-input$estadio
        modelo<-input$modelo$right
        nx<-input$nx
        ny<-input$ny
        PosModel<-1
        
          DevMort(path,est,modelo,PosModel,OPTplot=FALSE,nx,ny)

        #updateCheckboxInput(session, "saveSelec", value = FALSE)
      }
      
      if (ModSelected == "Senescence"){
        source('lib/senescence.r')
        
        path<-input$directory2
        est<-input$estadio
        modelo<-input$modelo$right
        nx<-input$nx
        ny<-input$ny
        PosModel<-1
        
          DevSenes(path,est,modelo,PosModel,OPTplot=FALSE,nx,ny)

        #updateCheckboxInput(session, "saveSelec", value = FALSE)
      }
      
      if (ModSelected == "Total Oviposition"){
        source('lib/totalOviposition.r')
        
        path<-input$directory2
        est<-input$estadio
        modelo<-input$modelo$right
        nx<-input$nx
        ny<-input$ny
        PosModel<-1
        
          DevTotOvi(path,est,modelo,PosModel,OPTplot=FALSE,nx,ny)

        #updateCheckboxInput(session, "saveSelec", value = FALSE)
      }
      
      if (ModSelected == "Relative Oviposition"){
        source('lib/relativeOviposition.r')
        
        path<-input$directory2
        est<-input$estadio
        modelo<-input$modelo$right
        nx<-input$nx
        ny<-input$ny
        PosModel<-1
        
          RelOvi(path,est,modelo,PosModel,OPTplot=FALSE,nx,ny)

        #updateCheckboxInput(session, "saveSelec", value = FALSE)
      }
      
      if (ModSelected == "Transmission Rate"){
        source('lib/dev_rate_new.r')
        
        path<-input$directory2
        est<-input$estadio
        modelo<-input$modelo$right
        nx<-input$nx
        ny<-input$ny
        PosModel<-1
        Namflucfile<-req(input$transmfile)
        
          TransmRate(path,Namflucfile,modelo,PosModel,OPTplot=FALSE,nx,ny)
        
        #updateCheckboxInput(session, "saveSelec", value = FALSE)
      }
      
    })
    
  })

  # Generate a summary and plot of the data
  observeEvent(input$doS, {
      ModSelected<-input$modules
      #saveSelec<-input$saveSelec
      style <- isolate(input$style)
      withProgress(message = 'Upload the dataset', style = style, value = 0.1, {
          Sys.sleep(0.35)
        incProgress(0.5,message = "saving")
          
        if (ModSelected == "Development Time"){
          source('lib/developmentTime.r')
          #incProgress(0.1, detail = "Reading development time functions")
          
          path<-input$directory2
          est<-req(input$estadio)
          modelo<-req(input$modelo)
          nx<-req(input$nx)
          ny<-req(input$ny)
          PosModel<-1
          saveSelec<-TRUE
          #jpeg(paste(path,"/DevelopmentTime/",est,".jpg",sep=""),width = 800, height = 480)
          jpeg(paste(path,"/DevelopmentTime/",est,".jpg",sep=""), width = 10, height = 5, units = 'in', res = 400)
          DevTime(path,est,modelo,PosModel,OPTplot=TRUE,nx,ny,saveSelec=saveSelec)
          dev.off()
          sink(paste(path,"/DevelopmentTime/",est,".txt",sep=""))
          DevTime(path,est,modelo,PosModel,OPTplot=FALSE,nx,ny)
          sink()
          
        }
        
        if (ModSelected == "Development Rate"){
          source('lib/dev_rate_new.r')
          
          path<-input$directory2
          est<-req(input$estadio)
          modelo<-req(input$modelo$right)
          nx<-req(input$nx)
          ny<-req(input$ny)
          PosModel<-1
          saveSelec<-TRUE
          jpeg(paste(path,"/DevelopmentRate/",est,".jpg",sep=""), width = 7, height = 6, units = 'in', res = 400)
          DevRate(path,est,modelo,PosModel,OPTplot=TRUE,nx,ny,saveSelec=saveSelec)
          dev.off()
          sink(paste(path,"/DevelopmentRate/",est,".txt",sep=""))
          DevRate(path,est,modelo,PosModel,OPTplot=FALSE,nx,ny)
          sink()
        }
        
        if (ModSelected == "Mortality"){
          source('lib/mortality.r')
          
          path<-input$directory2
          est<-input$estadio
          modelo<-input$modelo$right
          nx<-input$nx
          ny<-input$ny
          PosModel<-1
          saveSelec<-TRUE
          jpeg(paste(path,"/Mortality/",est,".jpg",sep=""), width = 7, height = 6, units = 'in', res = 400)
          DevMort(path,est,modelo,PosModel,OPTplot=TRUE,nx,ny,saveSelec=saveSelec)
          dev.off()
          sink(paste(path,"/Mortality/",est,".txt",sep=""))
          DevMort(path,est,modelo,PosModel,OPTplot=FALSE,nx,ny)
          sink()
          
        }
        
        if (ModSelected == "Senescence"){
          source('lib/senescence.r')
          
          path<-input$directory2
          est<-input$estadio
          modelo<-input$modelo$right
          nx<-input$nx
          ny<-input$ny
          PosModel<-1
          saveSelec<-TRUE
          jpeg(paste(path,"/Senescence/",est,".jpg",sep=""), width = 7, height = 6, units = 'in', res = 400)
          DevSenes(path,est,modelo,PosModel,OPTplot=TRUE,nx,ny,saveSelec=saveSelec)
          dev.off()
          sink(paste(path,"/Senescence/",est,".txt",sep=""))
          DevSenes(path,est,modelo,PosModel,OPTplot=FALSE,nx,ny)
          sink()
        }
        
        if (ModSelected == "Total Oviposition"){
          source('lib/totalOviposition.r')
          
          path<-input$directory2
          est<-input$estadio
          modelo<-input$modelo$right
          nx<-input$nx
          ny<-input$ny
          PosModel<-1
          saveSelec<-TRUE
          jpeg(paste(path,"/TotalOviposition/",est,".jpg",sep=""), width = 7, height = 6, units = 'in', res = 400)
          DevTotOvi(path,est,modelo,PosModel,OPTplot=TRUE,nx,ny,saveSelec=saveSelec)
          dev.off()
          sink(paste(path,"/TotalOviposition/",est,".txt",sep=""))
          DevTotOvi(path,est,modelo,PosModel,OPTplot=FALSE,nx,ny)
          sink()
        }
        
        if (ModSelected == "Relative Oviposition"){
          source('lib/relativeOviposition.r')
          
          path<-input$directory2
          est<-input$estadio
          modelo<-input$modelo$right
          nx<-input$nx
          ny<-input$ny
          PosModel<-1
          saveSelec<-TRUE
          jpeg(paste(path,"/RelativeOviposition/",est,".jpg",sep=""), width = 7, height = 6, units = 'in', res = 400)
          RelOvi(path,est,modelo,PosModel,OPTplot=TRUE,nx,ny,saveSelec=saveSelec)
          dev.off()
          sink(paste(path,"/RelativeOviposition/",est,".txt",sep=""))
          RelOvi(path,est,modelo,PosModel,OPTplot=FALSE,nx,ny)
          sink()
        }
        
        if (ModSelected == "Transmission Rate"){
          source('lib/dev_rate_new.r')
          
          path<-input$directory2
          modelo<-req(input$modelo$right)
          nx<-req(input$nx)
          ny<-req(input$ny)
          PosModel<-1
          Namflucfile<-req(input$transmfile)
          saveSelec<-TRUE

          TransmRate(path,Namflucfile,modelo,PosModel,OPTplot=TRUE,nx,ny,saveSelec=saveSelec)
          
          jpeg(paste(path,"/Transmission/","TranssRate",".jpg",sep=""), width = 7, height = 6, units = 'in', res = 400)
          TransmRate(path,Namflucfile,modelo,PosModel,OPTplot=TRUE,nx,ny,saveSelec=saveSelec)
          dev.off()
          sink(paste(path,"/Transmission/","TranssRate",".txt",sep=""))
          TransmRate(path,Namflucfile,modelo,PosModel,OPTplot=FALSE,nx,ny,saveSelec=saveSelec)
          sink()
        }

        setProgress(1)
      })
  })
  
  
  
  N=37 # el numero maximo de modelos para un modulo (se puede generar una variable de entorno)
  for(i in 1:37)
  {
    #system.file(package="grDevices")
    #grDevices::rversion()
    # observeEvent(input$doM,{
    capo1<-as.expression(substitute(output$ploti <-renderPlot({source('lib/developmentTime.r');
                                                               if (input$doM == 0){return()}else{isolate({
                                                               ModSelected<-input$modules;
                                                               style <- isolate(input$style);
                                                               withProgress(message = 'Processing the modeling', style = style, value = 0.1, {
                                                               path<-input$directory2;
                                                               #est<-req(input$estadio);
                                                               if(ModSelected!="Transmission Rate"){est<-req(input$estadio)}else{est<-"Adult"};
                                                               modelo<-req(input$modelo$right);
                                                               nx<-req(input$nx);
                                                               ny<-req(input$ny);
                                                               #Namflucfile<-req(input$transmfile);
							       if(ModSelected=="Transmission Rate"){Namflucfile<-req(input$transmfile)};
                                                               PosModel<-imas;OPTplot<-TRUE;
                                                               
                                                               switch(ModSelected,
                                                                                  "Development Rate" = source('lib/dev_rate_new.r'),
                                                                                  "Mortality" = source('lib/mortality.r'),
                                                                                  "Senescence" = source('lib/senescence.r'),
                                                                                  "Total Oviposition" = source('lib/totalOviposition.r'),
                                                                                  "Relative Oviposition" = source('lib/relativeOviposition.r'),
                                                                                  "Transmission Rate" = source('lib/dev_rate_new.r')
                                                               );
                                                               incProgress(0.5,message = "estimating model parameters")
                                                               #sink("D:/BORRAR-1.txt");print(ModSelected);print(Namflucfile);sink();
                                                               ObjsR<-  switch(ModSelected,
                                                                               "Development Time" = DevTime(path,est,modelo,PosModel,OPTplot=TRUE,nx,ny),
                                                                               "Development Rate" = DevRate(path,est,modelo,PosModel,OPTplot=TRUE,nx,ny),
                                                                               "Mortality" = DevMort(path,est,modelo,PosModel,OPTplot=TRUE,nx,ny),
                                                                               "Senescence" = DevSenes(path,est,modelo,PosModel,OPTplot=TRUE,nx,ny),
                                                                               "Total Oviposition" = DevTotOvi(path,est,modelo,PosModel,OPTplot=TRUE,nx,ny),
                                                                               "Relative Oviposition" = RelOvi(path,est,modelo,PosModel,OPTplot=TRUE,nx,ny),
                                                                               "Transmission Rate" = TransmRate(path,Namflucfile,modelo,PosModel,OPTplot=TRUE,nx,ny)
                                                               );
                                                               #sink("D:/BORRAR-1.txt");print(ModSelected);print(modelo);sink();
                                                               setProgress(1)
                                                               })})}
                                                               }),list(ploti = paste("plot",i,sep=""),imas = i)))
    eval(capo1)
  }

  # observeEvent(input$doM, {
  #   ModSelected<-input$modules
  #   path<-input$directory2
  #   est<-req(input$estadio)
  #   modelo<-req(input$modelo$right)
  #   nx<-req(input$nx)
  #   ny<-req(input$ny)
  #   NloopModls<-length(modelo)
  #   
  #   for(i in 1:NloopModls)
  #   {
  #     #system.file(package="grDevices")
  #     #grDevices::rversion()
  #     capo1<-as.expression(substitute(output$ploti <-renderPlot({source('lib/developmentTime.r');
  #         ModSelected<-input$modules;
  #         style <- isolate(input$style);
  #         withProgress(message = 'Processing the modeling', style = style, value = 0.1, {
  #           PosModel<-imas;OPTplot<-TRUE;
  #           switch(ModSelected,
  #                  "Development Rate" = source('lib/dev_rate_new.r'),
  #                  "Mortality" = source('lib/mortality.r'),
  #                  "Senescence" = source('lib/senescence.r'),
  #                  "Total Oviposition" = source('lib/totalOviposition.r'),
  #                  "Relative Oviposition" = source('lib/relativeOviposition.r')
  #           );
  #           incProgress(0.5,message = "estimating model parameters")
  #           ObjsR<-  switch(ModSelected,
  #                           "Development Time" = DevTime(path,est,modelo,PosModel,OPTplot=TRUE,nx,ny),
  #                           "Development Rate" = DevRate(path,est,modelo,PosModel,OPTplot=TRUE,nx,ny),
  #                           "Mortality" = DevMort(path,est,modelo,PosModel,OPTplot=TRUE,nx,ny),
  #                           "Senescence" = DevSenes(path,est,modelo,PosModel,OPTplot=TRUE,nx,ny),
  #                           "Total Oviposition" = DevTotOvi(path,est,modelo,PosModel,OPTplot=TRUE,nx,ny),
  #                           "Relative Oviposition" = RelOvi(path,est,modelo,PosModel,OPTplot=TRUE,nx,ny)
  #           );
  #           setProgress(1)
  #         })
  #     }),list(ploti = paste("plot",i,sep=""),imas = i)))
  #     eval(capo1)
  #   }
  # })
  ##########################################################################################################
  # Phenology progress
  output$viewprogress <- renderTable({
    # path  <- "D:/LH/Paratrioza-Cohort-2016"
    path<-input$directory2
    load(paste(path,"/PhenologySims.RData",sep=""))
    estadios<-gsub(".txt","",params$estadios)
    estadios1<-estadios
    if (path != "no project selected" & !is.null(path)){
      n0<-length(estadios1)
      Totaloutputs<-n0*2 + (n0-2)*2 + 4 + (n0-2)*2 + 2 + 2
      e1<-gsub(".txt","",list.files(paste(path,"/DevelopmentTime",sep=""),pattern = ".txt"))
      e2<-gsub(".txt","",list.files(paste(path,"/DevelopmentRate",sep=""),pattern = ".txt"))
      e3<-gsub(".txt","",list.files(paste(path,"/Senescence",sep=""),pattern = ".txt"))
      e4<-gsub(".txt","",list.files(paste(path,"/Mortality",sep=""),pattern = ".txt"))
      e5<-gsub(".txt","",list.files(paste(path,"/TotalOviposition",sep=""),pattern = ".txt"))
      e6<-gsub(".txt","",list.files(paste(path,"/RelativeOviposition",sep=""),pattern = ".txt"))
      
      MatF<-as.data.frame(matrix(NA,length(estadios1),6))
      MatF$V1[match(e1,estadios1)]<-"x"
      MatF$V2[match(e2,estadios1)]<-"x"
      MatF$V3[match(e3,estadios1)]<-"x"
      MatF$V4[match(e4,estadios1)]<-"x"
      MatF$V5[match(e5,estadios1)]<-"x"
      MatF$V6[match(e6,estadios1)]<-"x"
      colnames(MatF)<-c("Dev. Time","Dev. Rate","Senescence","Mortality","Total Ovip.","Relative Ovip.")
      rownames(MatF)<-estadios1
      print(MatF,na.print="")
    }else{
      return(NULL)
    }
    
  },rownames = TRUE, na ="")
  
  output$progressplot <- renderPlot({
    # path  <- "D:/LH/Paratrioza-Cohort-2016"
    path<-input$directory2
    load(paste(path,"/PhenologySims.RData",sep=""))
    estadios<-gsub(".txt","",params$estadios)
    estadios1<-estadios
    if (path != "no project selected" & !is.null(path)){
      #path<-"D:/ILCYM - Atlas/Paratrioza-Cohort-2016/"
      n0<-length(estadios1)
      Totaloutputs<-n0*2 + (n0-2)*2 + 4 + (n0-2)*2 + 2 + 2
      e1<-gsub(".txt","",list.files(paste(path,"/DevelopmentTime",sep=""),pattern = ".txt"))
      e2<-gsub(".txt","",list.files(paste(path,"/DevelopmentRate",sep=""),pattern = ".txt"))
      e3<-gsub(".txt","",list.files(paste(path,"/Senescence",sep=""),pattern = ".txt"))
      e4<-gsub(".txt","",list.files(paste(path,"/Mortality",sep=""),pattern = ".txt"))
      e5<-gsub(".txt","",list.files(paste(path,"/TotalOviposition",sep=""),pattern = ".txt"))
      e6<-gsub(".txt","",list.files(paste(path,"/RelativeOviposition",sep=""),pattern = ".txt"))
      
      PlotP<-c(length(e1)/n0,length(e2)/(n0-2),length(e3)/2,length(e4)/(n0-2),length(e5)/1,length(e6)/1)*100
      names(PlotP)<-c("DevelopmentTime","DevelopmentRate","Senescence","Mortality","TotalOviposition","RelativeOviposition")
      par(mar = c(5, 12, 0.2, 2))
      barplot(PlotP,horiz=TRUE,las=1, xlab=c("Percentage of progress (%)"), xlim=c(0,101))
      
    }else{
      return(NULL)
    }
    
  })
  
  ##########################################################################################################
  # Phenology summary

  output$report  <- renderUI({
    src <- normalizePath('report.Rmd')
    owd <- setwd(tempdir())
    on.exit(setwd(owd))
    knitr::opts_knit$set(root.dir = owd)
    #phenology objects
    path<-input$directory2
    load(paste(path,"/PhenologySims.RData",sep=""))
    params<-params
    tagList(
      HTML(knitr::knit2html(text = readLines(src), fragment.only = TRUE)),
      # typeset LaTeX math
      tags$script(HTML("if (hljs) $('#report pre code').each(function(i, e) {
                          hljs.highlightBlock(e)
                       });"))
    )
    
    
  })
  
  
  ##########################################################################################################
  # Constant Simulation
  
  output$consimplot <- renderPlot({
    if (input$do3 == 0)
      return()
    isolate({
      
      style <- isolate(input$style)
      withProgress(message = 'Simulating', style = style, value = 0.1, {
        
        incProgress(0.3,message = "processing")
        Sys.sleep(0.35) 
        source('lib/dete_sim.r')
        #source("D:/_BK-D/Pablo/R archivos/shiny-Examples/041-dynamic-ui-ILCYM/lib/dete_sim.r")
        path<-input$directory2
        N<-input$Ninsect
        M<-input$Ndays
        Vtempt<-input$Vtempt
        Intv<-input$Intv
        incProgress(0.6,message = "processing")
        if (path != "no project selected" & !is.null(path)){
          DeteSim(path,N,M,Vtempt,Intv,OPTplot=TRUE)
          incProgress(0.8,message = "processing")
          Sys.sleep(0.35)
        }else{
          return()
        }
        setProgress(1)
        #updateTextInput(session, "directory2", value=path)
      })
    })
  }, height = 800, width = 720)
  
  
  ##########################################################################################################
  # Fluctuating Simulation
  
  output$consimplot2 <- renderPlot({
    if (input$do4 == 0)
      return()
    isolate({
      style <- isolate(input$style)
      withProgress(message = 'Simulating', style = style, value = 0.1, {
          incProgress(0.3,message = "processing")
          Sys.sleep(0.35) 
        source('lib/dete_sim.r')
        path<-input$directory2
        numIni<-input$Ninsect2
        Namflucfile<-input$flucfile
        Table<-read.table(Namflucfile$datapath[1])
        xlegend<-input$xlegend
        incProgress(0.6,message = "processing")
        if (path != "no project selected" & !is.null(path)){
          DeteSimFluc(path,numIni,Table,OPTplot=TRUE,xlegend=xlegend)
          incProgress(0.8,message = "processing")
        }else{
          return()
        }
        setProgress(1)
        #updateTextInput(session, "directory2", value=path)
      })
    })
  }, height = 400, width = 800)

  
  ##########################################################################################################
  # Validation
  
  output$validation <- renderPlot({
    if (input$do8 == 0)
      return()
    isolate({
      style <- isolate(input$style)
      withProgress(message = 'Validating', style = style, value = 0.1, {
        incProgress(0.3,message = "processing")
        Sys.sleep(0.35)
        #source('D:/_BK-D/Pablo/R archivos/shiny-Examples/backup/041-dynamic-ui-ILCYM/lib/validate.r')
        source('lib/validate.r')
        path<-input$directory2
        Namflucfile<-input$flucfileVal
        Table<-read.table(Namflucfile$datapath[1])
        
        NamflucfileLT<-input$flucfileLT
        vidalife <-read.table(NamflucfileLT$datapath[1])

        xlegend<-input$xlegendVal
        incProgress(0.6,message = "processing")
        if (path != "no project selected" & !is.null(path)){
          DeteSimVal(path,vidalife,Table,OPTplot=TRUE,xlegend=xlegend)
          incProgress(0.8,message = "processing")
        }else{
          return()
        }
        setProgress(1)
        #updateTextInput(session, "directory2", value=path)
      })
    })
  }, height = 400, width = 800)
  
  

  ##########################################################################################################
  # Geographic Simulation
  
  observe({
    if (input$do5 == 0)
      return()
    isolate({
      pathclim<-input$PathClimDat
      pathclim = choose.dir(getwd(), "Choose the folder of Tmin and Tmax")
      pathclim <- gsub("\\\\", "/",pathclim)
      updateTextInput(session, "PathClimDat", value=pathclim)
    })
    
  })
  
  observe({
    if (input$do6 == 0)
      return()
    isolate({
      style <- isolate(input$style)
      withProgress(message = 'Simulating', style = style, value = 0.1, {
        Sys.sleep(0.35) 
          
        library(sp)
        library(maptools)
        library(rgdal)
        source('lib/geo_sim_M.r')
        #source('D:/_BK-D/Pablo/R archivos/shiny-Examples/041-dynamic-ui-ILCYM/lib/geo_sim_M.r')
        
        
        pathclim<-input$PathClimDat
        #pathclim<-"D:/_BK-D/Pablo/R archivos/shiny-Examples/FLT-Peru-10min-2000/"
        R<-input$DivExtent
        #R<-2
        NameInd<-input$NameFolderIndex
        #NameInd<-"Risk Index"
        path<-input$directory2
        #path<-"D:/LH/"
        namePJ<-input$namePJ
        #namePJ<-"Paratrioza-Cohort-2016"
        load(paste(path,"/PhenologySims.RData",sep=""))
        
        modelim<-params$modelim
        modelm<-params$modelm
        estadios<-params$estadios
        hfeno<-params$hfeno
        xi<-params$xi
        steps<-4
        
        dir1 = paste(pathclim,"/Tmin/",sep="")
        dir2 = paste(pathclim,"/Tmax/",sep="")
        temp<-readGDAL(list.files(dir1,pattern=".flt",full.names = TRUE)[1])
        
        ilon<-c(bbox(temp)[1,1],bbox(temp)[1,2])
        ilat<-c(bbox(temp)[2,1],bbox(temp)[2,2])
        rm(temp)
        
        dir.create(paste(path,"/GeographicalSimulation/",NameInd,sep=""))
        dir.out=paste(path,"/GeographicalSimulation/",NameInd,"/",sep="")
        
        name.out <- "mundo-2000"
        modelim=c(modelim,modelm)
        filtro=c(-5,45)
        incProgress(0.2,message = "processing")
        file<-zone.div(dir1,dir2,ilon,ilat,R,dir.out,name.out=name.out,method=method,modelim=modelim,modelm=modelm,estadios=estadios,xi=xi,steps=steps, filtro=filtro,hfeno=hfeno)
        
        updateTextInput(session, "NameFolderIndex", value="Ready")
        setProgress(1)
      })
    })
    
  })
  
  output$geosimplot0 <- renderPlot({
    if (input$NameFolderIndex != "Ready"){
      return()
    }else{
      library(maptools)
      library(raster)
      library(colorspace)
      library(RColorBrewer)
      
      path<-input$directory2
      namePJ<-input$namePJ
      borrar=file.info(list.dirs(paste(path,"/",namePJ,"/GeographicalSimulation/",sep=""),full.names=TRUE))
      dir<-(row.names(subset(borrar,ctime==max(borrar$ctime))))[1] # el mas actual
      #GeoSimFluc(dir,Index=NULL,brks=NULL,HistRange=FALSE)#### OJOOOOOOO
      if (path != "no project selected" & !is.null(path)){
        GeoSimFluc(dir,HistRange=TRUE)#### OJOOOOOOO
      }else{
        return()
      }
    }
  }, height = 400, width = 800)
  
  output$geosimplot1 <- renderPlot({
    if (input$do7 == 0)
      return()
    isolate({
      library(maptools)
      library(raster)
      library(colorspace)
      library(RColorBrewer)
      
      path<-input$directory2
      namePJ<-input$namePJ
      borrar=file.info(list.dirs(paste(path,"/",namePJ,"/GeographicalSimulation/",sep=""),full.names=TRUE))
      dir<-(row.names(subset(borrar,ctime==max(borrar$ctime))))[1] # the most updated folder
      Index<-"ERI"
      brks<-input$rangeERI
      if (path != "no project selected" & !is.null(path)){
        GeoSimFluc(dir,Index=Index,brks=brks)
      }else{
        return()
      }
    })
  }, height = 600, width = 800)
  
  output$geosimplot2 <- renderPlot({
    if (input$do7 == 0)
      return()
    isolate({
      library(maptools)
      library(raster)
      library(colorspace)
      library(RColorBrewer)
      
      path<-input$directory2
      namePJ<-input$namePJ
      borrar=file.info(list.dirs(paste(path,"/",namePJ,"/GeographicalSimulation/",sep=""),full.names=TRUE))
      dir<-(row.names(subset(borrar,ctime==max(borrar$ctime))))[1] # el mas actual
      Index<-"GI"
      brks<-input$rangeGI
      #GeoSimFluc(dir,Index=NULL,brks=NULL,HistRange=FALSE)#### OJOOOOOOO
      if (path != "no project selected" & !is.null(path)){
        GeoSimFluc(dir,Index=Index,brks=brks)
      }else{
        return()
      }
    })
  }, height = 600, width = 800)
  
  output$geosimplot3 <- renderPlot({
    if (input$do7 == 0)
      return()
    isolate({
      library(maptools)
      library(raster)
      library(colorspace)
      library(RColorBrewer)
      
      path<-input$directory2
      namePJ<-input$namePJ
      borrar=file.info(list.dirs(paste(path,"/",namePJ,"/GeographicalSimulation/",sep=""),full.names=TRUE))
      dir<-(row.names(subset(borrar,ctime==max(borrar$ctime))))[1] # el mas actual
      Index<-"AI"
      brks<-input$rangeAI
      #GeoSimFluc(dir,Index=NULL,brks=NULL,HistRange=FALSE)#### OJOOOOOOO
      if (path != "no project selected" & !is.null(path)){
        GeoSimFluc(dir,Index=Index,brks=brks)
      }else{
        return()
      }
    })
  }, height = 600, width = 800)
  
  ##########################################################################################################
  # Geographic Simulation for Transmission indices
  
  observe({
    if (input$do9 == 0)
      return()
    isolate({
      pathclim<-input$PathClimDatT
      pathclim = choose.dir(getwd(), "Choose the folder of Tmin and Tmax")
      pathclim <- gsub("\\\\", "/",pathclim)
      updateTextInput(session, "PathClimDatT", value=pathclim)
    })
    
  })
  
  observe({
    if (input$do10 == 0)
      return()
    isolate({
      style <- isolate(input$style)
      withProgress(message = 'Simulating', style = style, value = 0.1, {
        Sys.sleep(0.35) 
        
        library(sp)
        library(maptools)
        library(rgdal)
        source('lib/geo_transmission.R')
        #source('D:/_BK-D/Pablo/R archivos/shiny-Examples/041-dynamic-ui-ILCYM/lib/geo_transmission.R')
        
        
        pathclim<-input$PathClimDatT
        #pathclim<-"D:/_BK-D/Pablo/R archivos/shiny-Examples/FLT-Peru-10min-2000/"
        R<-input$DivExtent
        #R<-2
        NameInd<-input$NameFolderIndexT
        #NameInd<-"Risk Index"
        path<-input$directory2
        #path<-"D:/LH/Paratrioza-Cohort-2016"
        # namePJ<-input$namePJ
        #namePJ<-"Paratrioza-Cohort-2016"
        load(paste(path,"/PhenologySims.RData",sep=""))
        load(paste(path,"/PhenologyStats.RData",sep=""))
        
        modelim<-params$modelim
        modelm<-params$modelm
        estadios<-params$estadios
        hfeno<-params$hfeno
        xi<-params$xi
        steps<-4
        
        x<-report$fenologia$TransmRate$x
        y<-report$fenologia$TransmRate$y
        temporal<-do.call("as.list",list(params$transm$ptr_tr))
        out <- nls(params$transm$ftr_tr, start = temporal, trace = FALSE)
        rm(x);rm(y)
        
        dir1 = paste(pathclim,"/Tmin/",sep="")
        dir2 = paste(pathclim,"/Tmax/",sep="")
        temp<-readGDAL(list.files(dir1,pattern=".flt",full.names = TRUE)[1])
        
        ilon<-c(bbox(temp)[1,1],bbox(temp)[1,2])
        ilat<-c(bbox(temp)[2,1],bbox(temp)[2,2])
        rm(temp)
        
        dir.create(paste(path,"/TransmissionSimulation/",NameInd,sep=""))
        dir.out=paste(path,"/TransmissionSimulation/",NameInd,"/",sep="")
        
        name.out <- "mundo-2000"
        modelim=c(modelim,modelm)
        filtro=c(-5,45)
        incProgress(0.2,message = "processing")
        file<-zone.div(dir1,dir2,ilon,ilat,R,dir.out,name.out=name.out,out=out,method=method,modelim=modelim,modelm=modelm,estadios=estadios,xi=xi,steps=steps, filtro=filtro,hfeno=hfeno)
        #file<-zone.div(dir1,dir2,ilon,ilat,R,dir.out,name.out=name.out,out,filtro)
        updateTextInput(session, "NameFolderIndexT", value="Ready")
        setProgress(1)
      })
    })
    
  })
  
  output$geosimplot0T <- renderPlot({
    if (input$NameFolderIndexT != "Ready"){
      return()
    }else{
      library(maptools)
      library(raster)
      library(colorspace)
      library(RColorBrewer)

      path<-input$directory2
      #namePJ<-input$namePJ
      borrar=file.info(list.dirs(paste(path,"/TransmissionSimulation/",sep=""),full.names=TRUE))
      dir<-(row.names(subset(borrar,ctime==max(borrar$ctime))))[1] # el mas actual
      #GeoSimFluc(dir,Index=NULL,brks=NULL,HistRange=FALSE)#### OJOOOOOOO
      
      if (path != "no project selected" & !is.null(path)){
        GeoSimFlucT(dir,HistRange=TRUE)#### OJOOOOOOO
      }else{
        return()
      }
      
    }
  }, height = 400, width = 800)
  
  
  
  output$geosimplot1T <- renderPlot({
    if (input$do11 == 0)
      return()
    isolate({
      library(maptools)
      library(raster)
      library(colorspace)
      library(RColorBrewer)
      
      path<-input$directory2
      #namePJ<-input$namePJ
      borrar=file.info(list.dirs(paste(path,"/TransmissionSimulation/",sep=""),full.names=TRUE))
      dir<-(row.names(subset(borrar,ctime==max(borrar$ctime))))[1] # the most updated folder
      Index<-"PT"
      brks<-input$rangePT
      if (path != "no project selected" & !is.null(path)){
        GeoSimFlucT(dir,Index=Index,brks=brks)
      }else{
        return()
      }
    })
  }, height = 600, width = 800)
  
  output$geosimplot2T <- renderPlot({
    if (input$do11 == 0)
      return()
    isolate({
      library(maptools)
      library(raster)
      library(colorspace)
      library(RColorBrewer)
      
      path<-input$directory2
      #namePJ<-input$namePJ
      borrar=file.info(list.dirs(paste(path,"/TransmissionSimulation/",sep=""),full.names=TRUE))
      dir<-(row.names(subset(borrar,ctime==max(borrar$ctime))))[1] # el mas actual
      Index<-"PAT"
      brks<-input$rangePAT
      #GeoSimFluc(dir,Index=NULL,brks=NULL,HistRange=FALSE)#### OJOOOOOOO
      if (path != "no project selected" & !is.null(path)){
        GeoSimFlucT(dir,Index=Index,brks=brks)
      }else{
        return()
      }
    })
  }, height = 600, width = 800)
  
})
