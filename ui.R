library(shiny)
#library(shinythemes)
#theme = "bootstrap.min.css"
# se puede ingresar el theme de otra forma 
shinyUI(navbarPage(theme = "bootstrap.min.css",title=tags$b("ILCYM  Web",tags$img(src="images/Phthorimaea_operculella-Gray-2.png",height="25",width="35")),
                   tabPanel("Project",
                            fluidRow(column(6,wellPanel(
                              selectInput("Opcion", h4("Select an option"), choices = c("Select a Project"="SePr", "New Project"="CrPr"))
                            ))),
                            br(),
                            
                            conditionalPanel(
                              'input.Opcion === "CrPr"',
                              column(4,wellPanel(
                                h4('1. Project details'),
                                textInput("namePJ", "Project Name:", value = ""),
                                textInput("nameESP", "Species Name:", value = ""),
                                textInput("nameAUT", "Author:", value = ""),
                                textInput("nameDATE", "Date:", value = Sys.Date()),
                                numericInput("nameRATE", "Fixed Rate:", value = 0.5,min=0,max=1,step=0.05),
                                textInput("directory", "Project location", value = path.expand("~/")),
                                #tags$h5('Select your Directory:'),
                                #actionButton("do", "Browse and Save")
                                actionButton("do", "Browse and Save",icon=icon("folder-o"))
                               )
                              ),
                              column(7,wellPanel(
                                fileInput('file1', h4('2. Load data file'),
                                          accept=c('text/csv', 
                                                   'text/comma-separated-values,text/plain', 
                                                   '.csv'), multiple = TRUE
                                          
                                ),
                                helpText("Note: Select your files names according to next order: Egg, Larvae, Pupae, Female, Male and oviposition"),
                                uiOutput("choices")
                               )
                              ),
                              actionButton("doU", "Upload",icon=icon("upload"), class = "btn-primary")
                            ),
                            
                            conditionalPanel(
                              'input.Opcion === "SePr"',
                              column(4,
                                textInput("directory2", h4('1. Project location'), value = "no project selected"),
                                actionButton("do2", "Open Project",icon=icon("folder-open"), class = "btn-primary")
                              ),
                              column(6,
                                selectInput('modules', h4('2. Select a variable'), c("Development Time","Development Rate","Mortality","Senescence","Total Oviposition","Relative Oviposition","Transmission Rate"), multiple=FALSE, selectize=FALSE),
                                uiOutput("choicesMOD1")
                              )
                            ),
                            br()
                            
                   ),
                   tabPanel("Modelling",
                            sidebarLayout(
                              sidebarPanel(
                                fluidRow(column(10,wellPanel(
                                  uiOutput("choicesMB1")
                                ))
                                ),    
                                
                                fluidRow(column(10,wellPanel(
                                  uiOutput("choicesMB2")
                                ))
                                ),
                                
                                fluidRow(column(10,wellPanel(
                                  uiOutput("choicesMB3")
                                ))
                                ),
                                
                                fluidRow(column(10,wellPanel(
                                  uiOutput("choicesMB4")
                                ))
                                ),
                                br(),
                                actionButton("doM", "OK",icon=icon("flash"), class = "btn-primary"),
                                br(),
                                fluidRow(column(6,wellPanel(
                                  uiOutput("choicesMB5")
                                )))
                              ),
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Plot of Model", uiOutput("graphsMB1")),
                                  tabPanel("Summary", uiOutput("graphsMB2"))
                                )
                              )
                            )
                            
                   ),
                   navbarMenu("Compilation",
                              tabPanel("Phenology progress",icon=icon("align-left"),
                                column(6,wellPanel(tableOutput("viewprogress")
                                )),
                                fluidRow(column(6,
                                  plotOutput("progressplot")
                                ))
                              ),
                              tabPanel("Phenology Summary",icon=icon("file-text"),
                                uiOutput('report')
                              )
                   ),
                   navbarMenu("Simulation",
                              tabPanel("Constant Simulation",icon=icon("bar-chart"),
                                       sidebarLayout(
                                         sidebarPanel(
                                           wellPanel(
                                               h4("Constant Simulation acording ILCYM 3.0"),
                                               numericInput("Ninsect", "Number of insect", value = 100,min=0,max=500,step=20),
                                               numericInput("Ndays", "Number of days", value = 365,min=0,max=365,step=5),
                                               sliderInput("Vtempt", "Range of temperatures", min = 0, max = 50, value = c(10,30), step = 1),
                                               numericInput("Intv", "Interval length", value = 5,min=0,max=5,step=0.5),
                                               tags$head(tags$script(src = "message-handler.js")),
                                               actionButton("do3", "Estimate",icon=icon("refresh"))
                                           )
                                         ),
                                         mainPanel(
                                           column(12,
                                                  plotOutput("consimplot")
                                           )
                                         )
                                       )
                              ),
                              tabPanel("Fluctuating Simulation",icon=icon("area-chart"),
                                       sidebarLayout(
                                         sidebarPanel(
                                           wellPanel(
                                             h4("Fluctuating Simulation acording ILCYM 3.0"),
                                             numericInput("Ninsect2", "Number of insect", value = 100,min=0,max=500,step=20),
                                             fileInput('flucfile', 'Load fluctuating temperature file',
                                                       accept=c('text/csv', 
                                                                'text/comma-separated-values,text/plain', 
                                                                '.csv'), multiple = FALSE
                                                       
                                             ),
                                             sliderInput("xlegend", "x coordinate of the legend", min = 0, max = 360, value = 300, step = 20),
                                             tags$head(tags$script(src = "message-handler.js")),
                                             actionButton("do4", "Estimate",icon=icon("refresh"))
                                           )
                                         ),
                                         mainPanel(
                                           column(12,
                                                  plotOutput("consimplot2")
                                           )
                                         )
                                       )
                              ),
                              tabPanel("Validation",icon=icon("line-chart"),
                                       sidebarLayout(
                                         sidebarPanel(
                                           wellPanel(
                                             h4("Model validation acording ILCYM 3.0"),
                                             fileInput('flucfileVal', 'Load fluctuating temperature file',
                                                       accept=c('text/csv', 
                                                                'text/comma-separated-values,text/plain', 
                                                                '.csv'), multiple = FALSE
                                                       
                                             ),
                                             fileInput('flucfileLT', 'Load fluctuating Life Table',
                                                       accept=c('text/csv', 
                                                                'text/comma-separated-values,text/plain', 
                                                                '.csv'), multiple = FALSE
                                                       
                                             ),
                                             sliderInput("xlegendVal", "x coordinate of the legend", min = 0, max = 360, value = 60, step = 20),
                                             tags$head(tags$script(src = "message-handler.js")),
                                             actionButton("do8", "Estimate",icon=icon("refresh"))
                                           )
                                         ),
                                         mainPanel(
                                           column(12,
                                                  plotOutput("validation")
                                           )
                                         )
                                       )
                              ),
                              tabPanel("Geographical Simulation",icon=icon("globe"),
                                sidebarLayout(
                                  sidebarPanel(
                                    wellPanel(
                                      h3("Risk Indices according ILCYM 3.0"),
                                      textInput("PathClimDat", "FLT files location", value = "no data"),
                                      actionButton("do5", "Load",icon=icon("database")),
                                      br()
                                    ),
                                    conditionalPanel(
                                      'input.PathClimDat !== "no data"',
                                      fluidRow(column(12,wellPanel(
                                        numericInput("DivExtent", "Number of the extent division", value = 8,min=1,max=20,step=1),
                                        textInput("NameFolderIndex", "Folder name", value = "Risk Index"),
                                        br(),
                                        actionButton("do6", "Generate raster",icon=icon("flash"))
                                      ))
                                      )
                                    ),
                                    conditionalPanel(
                                      'input.NameFolderIndex === "Ready"',
                                      fluidRow(column(12,wellPanel(
                                        textInput("rangeERI", "Establisment Index needs to define intervals:", value = "0,0.1,0.20,0.3,0.4,0.5,0.6,0.7,0.80,0.9,0.95,1"),
                                        br(),
                                        textInput("rangeGI", "Generation Index needs to define intervals:", value = "0,1,2,3,4,5,6,8"),
                                        br(),
                                        textInput("rangeAI", "Activity Index needs to define intervals:", value = "0,1,2,3,4,5,6,8"),
                                        br(),
                                        actionButton("do7", "Generate Risk Maps",icon=icon("flash"))
                                      ))
                                      )
                                    ),
                                    br()
                                  ),
                                  mainPanel(
                                    tabsetPanel(
                                      tabPanel("Raster Summary", plotOutput("geosimplot0")),
                                      tabPanel("Establisment Index", plotOutput("geosimplot1")),
                                      tabPanel("Generation Index", plotOutput("geosimplot2")),
                                      tabPanel("Activity Index", plotOutput("geosimplot3"))
                                    )
                                  )
                                )
                              ),
                              tabPanel("Geographical Simulation for Transmission",icon=icon("map"),
                                       sidebarLayout(
                                         sidebarPanel(
                                           wellPanel(
                                             h3("Transmission Indices according ILCYM 3.0"),
                                             textInput("PathClimDatT", "FLT files location", value = "no data"),
                                             actionButton("do9", "Load",icon=icon("database")),
                                             br()
                                           ),
                                           conditionalPanel(
                                             'input.PathClimDatT !== "no data"',
                                             fluidRow(column(12,wellPanel(
                                               numericInput("DivExtentT", "Number of the extent division", value = 8,min=1,max=20,step=1),
                                               textInput("NameFolderIndexT", "Folder name", value = "Risk Index"),
                                               br(),
                                               actionButton("do10", "Generate raster",icon=icon("flash"))
                                             ))
                                             )
                                           ),
                                           conditionalPanel(
                                             'input.NameFolderIndexT === "Ready"',
                                             fluidRow(column(12,wellPanel(
                                               textInput("rangePT", "Potential Transmission:", value = "0,0.1,0.20,0.3,0.4,0.5,0.6,0.7,0.80,0.9,0.95,1"),
                                               br(),
                                               textInput("rangePAT", "Potential Activity of Transmission:", value = "0,1,2,3,4,5,6,8"),
                                               br(),
                                               actionButton("do11", "Generate Maps",icon=icon("flash"))
                                             ))
                                             )
                                           )
                                           ,
                                           br()
                                         ),
                                         mainPanel(
                                           tabsetPanel(
                                             tabPanel("Raster Summary", plotOutput("geosimplot0T")),
                                             tabPanel("Potential Transmission", plotOutput("geosimplot1T")),
                                             tabPanel("Potential Activity of Transmission", plotOutput("geosimplot2T"))
                                           )
                                         )
                                       )
                              )
                   )
))
