#UI para Shiny con Tareas
if(!require(shiny)) install.packages("shiny")
if(!require(shinydashboard)) install.packages("shinydashboard")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(DT)) install.packages("DT")
if(!require(Rcpp)) install.packages("Rcpp")
if(!require(markdown)) install.packages("markdown")
if(!require(dplyr)) install.packages("dplyr")
library(shiny)
library(shinydashboard)
library(ggplot2)
library(Rcpp)
library(DT)

data4 <- read.csv(file="cheese.csv", header=T)
data5 <- data4[, !names(data4) %in% c("id","taste")]

dashboardPage(
  dashboardHeader(
    title = "Estadistica computacional",
    #    title = tags$a(href='http://mycompanyishere.com',
    #                   tags$img(src='logo_itam_70.png')),
    titleWidth = "900px"
    ),
  skin = "green",
  dashboardSidebar(
    sidebarMenu(
      menuItem("Numeros aleatorios", tabName = "random", icon = icon("black-tie")),
      menuItem("Integracion via Monte Carlo", tabName = "montecarlo", icon = icon("magic")),
      menuItem("MCMC", tabName = "MCMC", icon = icon("line-chart")),
      menuItem("Extras", tabName = "extras", icon = icon("gift")),
      textInput("seed", "Semilla:", "109152")
      #menuItem("About", tabName = "about", icon = icon("user"))
    )
  ),
  dashboardBody(
    #tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    ## slide
    tabItems(
      tabItem(tabName = "random",
              h2("Tarea 01"),
              tabsetPanel(
                tabPanel("Instrucciones",
                         box(
                           width = 15,
                           includeMarkdown("md/hw01.md")
                         )
                ),
                tabPanel("Teoria",
                         box(
                           withMathJax(),
                           width = 15,
                           includeMarkdown("md/teo01.md")
                         )
                ),
                tabPanel( "Ejemplo",
                          fluidRow(
                            box(title = "Parametros", "",
                                sliderInput("nsims","Numeros a simular",
                                            min = 20, max = 1000,
                                            value = 50)
                                ),
                            box(title = "Parametros", "",
                                numericInput("lambda",
                                             "lambda:",
                                             value = 1,step=.1),
                                numericInput("nbin",
                                             "nbin:",
                                             value = 10,step=1),
                                downloadButton("downloadData", "Descargar")
                                
                                # sliderInput("nbin","Numero de elementos en el histograma",
                                #             min = 20, max = 1000,
                                #             value = 50)   
                            ) 
                          ),
                          # ,
                          # box(title = "Box title", "Box content",
                              
                          h3("Pruebas de bondad de ajuste"),
                          #h5("Kolmogorov-Smirnov y Chi-Square Test"),
                          fluidRow(column(5, verbatimTextOutput("ksTest")),
                                   column(5, verbatimTextOutput("chiTest"))
                          ),
                          
                          
                          sidebarLayout(
                            sidebarPanel(
                              fluidRow(radioButtons("radioBtn", "Tipo de distribucion:",
                                                    c("Uniforme (GCL)"         = "UNIF",
                                                      "Exponencial (Fnc-Inv)"  = "EXP",
                                                      "Normal (Box-Muller)"    = "NORM",
                                                      "Geometrica"             = "GEOM")))
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                tabPanel("histograma", plotOutput("hist")),
                                tabPanel("qqPlot",     plotOutput("qqPlot"))
                              ),
                              verbatimTextOutput("stats")
                            )
                          )
                          )
                )
              ),
      
      ##slide menu new element  
      tabItem(tabName = "montecarlo",
              h2("Tarea 02"),
              tabsetPanel(
                tabPanel("Instrucciones",
                           box(
                             width = 15,
                             includeMarkdown("md/hw02.md")
                           )
                  ),
                  tabPanel("Teoriaa",
                           box(
                             withMathJax(),
                             width = 15,
                             includeMarkdown("md/teo02.md")
                           )
                  ),
                  tabPanel( "Ejemplo",
                            fluidRow(
                              ##aqui va el shiny
                              fluidRow(
                                box(title = "Parametros", "",
                                    textInput(inputId="inpFunc", label="Funcion a evaluar",
                                              value="function (x) {4/(1+x^2)}"),
                                    
                                    sliderInput(inputId = "lmts", label="Limites de la integral",
                                                max=10, min=0, value=c(0,1)),
                                    
                                    sliderInput(inputId = "alfa", label="Intervalo de confianza",
                                                max=0.1, min=0.01, value=0.05, step=0.01),
                                    
                                    sliderInput("n", 
                                                "Numero de puntos aleatorios:", 
                                                value = 100,
                                                min = 2, 
                                                max = 1000)
                                ),
                                box(title = "Parametros", "",
                                    tabsetPanel(type = "tabs", 
                                                tabPanel("Mont eCarlo", plotOutput("plot")), 
                                                tabPanel("Intervalos", plotOutput("intervals")),
                                                tabPanel("Trapecio vs MC", dataTableOutput("comparation"))
                                    )
                                    
                                    # sliderInput("nbin","Numero de elementos en el histograma",
                                    #             min = 20, max = 1000,
                                    #             value = 50)   
                                ) 
                              )
                              )
                  )
                )
      ),## fin slide menu
      
      ##slide menu new element  
      tabItem(tabName = "MCMC",
              h2("Tarea 04"),
              tabsetPanel(
                tabPanel("Generalidades",
                         box(
                           width = 15,
                           includeMarkdown("md/hw4-6.md")
                         )
                ),
                
                tabPanel("Teoria",
                         box(
                           withMathJax(),
                           width = 15,
                           "falta"
                           #includeMarkdown("md/teo02.md")
                         )
                ),
                tabPanel("Tarea 04",
                         sidebarLayout(
                           sidebarPanel(
                             checkboxGroupInput("cVariables4", h3("Variables"),
                                                choices = names(data4)),
                             
                             sliderInput("s_a4", "a -> Unif ", min=1, max=10, value=c(5,8)),
                             sliderInput("s_b4", "b <- Norm", min=1, max=10, value=5),
                             sliderInput("s_sigma4", "sigma -> Unif", min=1, max=10, value=c(5, 6))
                           ),
                           
                           mainPanel(
                             tabsetPanel(type = "tabs", 
                                         tabPanel("datos",
                                                  fluidRow(
                                                    column(8, plotOutput("plot_data4")),
                                                    column(12, DT::dataTableOutput("table4"))
                                                  ) 
                                         ),
                                         tabPanel("distribuciones aPriori",
                                                  fluidRow(
                                                    column(4, plotOutput("plot_hist_A4")),
                                                    column(4, plotOutput("plot_hist_B4")),
                                                    column(4, plotOutput("plot_hist_Sd4")),
                                                    column(4, plotOutput("plot_hist_Total4"))
                                                  )
                                         )
                             )
                           )
                         )
                         # box(
                         #   column(1,checkboxGroupInput("cVariables", h3("Variables"),
                         #                               choices = names(data4)))
                         # ),
                         # box(
                         #   
                         #   sliderInput("s_a", "a -> Unif ", min=1, max=10, value=c(5,8)),
                         #   sliderInput("s_b", "b <- Norm", min=1, max=10, value=5),
                         #   sliderInput("s_sigma", "sigma -> Unif", min=1, max=10, value=c(5, 6)),
                         #   withMathJax(),
                         #   #width = 15,
                         #   includeMarkdown("md/hw04.md")
                         # ),
                         # box(
                         #   
                         #   tabsetPanel(type = "tabs", 
                         #               tabPanel("datos",
                         #                        fluidRow(
                         #                          column(8, plotOutput("plot_data")),
                         #                          column(12, DT::dataTableOutput("table"))
                         #                        ) 
                         #               ),
                         #               tabPanel("distribuciones aPriori",
                         #                        fluidRow(
                         #                          column(4, plotOutput("plot_hist_A")),
                         #                          column(4, plotOutput("plot_hist_B")),
                         #                          column(4, plotOutput("plot_hist_Sd")),
                         #                          column(4, plotOutput("plot_hist_Total"))
                         #                        )
                         #               )
                         #   )
                         # )
                ),
                # tabPanel("Tarea 05",
                #          
                #          sidebarPanel(
                #            checkboxGroupInput("cVariables", h3("Variables"),
                #                               choices = names(data5)),
                #            numericInput("nCadenas", "cadenas a simular", value=1, min=1, max=10, step=1),
                #            sliderInput("sLongitud", "longitud de cadenas", min=10000, max=1000000, value=1000),
                #            sliderInput("sBurnin", "Burnin", min=10, max=10000, value=5000),
                #            actionButton("button", "Calcula MCMC"),  #Calcula MCMC
                #            
                #            h4("Parametros aPriori"),
                #            sliderInput("s_a", "a -> Unif ", min=1, max=10, value=c(5,8)),
                #            sliderInput("s_b", "b <- Norm", min=1, max=10, value=5),
                #            sliderInput("s_sigma", "sigma -> Unif", min=1, max=10, value=c(5, 6))
                #          ),
                #          
                #          mainPanel(
                #            tabsetPanel(type="tabs",
                #                        tabPanel("Datos", 
                #                                 fluidRow(
                #                                   column(8, plotOutput("plot_data")),
                #                                   column(12, DT::dataTableOutput("table"))
                #                                 )
                #                        ),
                #                        tabPanel("distribuciones aPriori",
                #                                 fluidRow(
                #                                   column(4, plotOutput("plot_hist_A")),
                #                                   column(4, plotOutput("plot_hist_B")),
                #                                   column(4, plotOutput("plot_hist_Sd")),
                #                                   column(4, plotOutput("plot_hist_Total"))
                #                                 )
                #                        ),
                #                        tabPanel("Parametros de la regresion",
                #                                 fluidRow(
                #                                   column(4, plotOutput("hist_posteriori_A")),
                #                                   column(4, plotOutput("hist_posteriori_B")),
                #                                   column(4, plotOutput("hist_posteriori_Sd")),
                #                                   column(4, plotOutput("plot_posteriori_A")),
                #                                   column(4, plotOutput("plot_posteriori_B")),
                #                                   column(4, plotOutput("plot_posteriori_Sd"))
                #                                 )
                #                        ),
                #                        tabPanel("Multiples cadenas",
                #                                 fluidRow(
                #                                   column(4, verbatimTextOutput("summary")),
                #                                   column(4, plotOutput("regresionCalc")),
                #                                   column(12, DT::dataTableOutput("cadenasMCMC"))
                #                                 )),
                #                        tabPanel("Convergencia de MCMC's", 
                #                                 plotOutput("pConvergencia_A"),
                #                                 plotOutput("pConvergencia_B"),
                #                                 plotOutput("pConvergencia_Sd"))
                #            ))
                # ),
                tabPanel("Tarea 5",
                         sidebarLayout(
                           sidebarPanel(
                             checkboxGroupInput("cVariables", h3("Variables"),
                                                choices = names(data5)),
                             numericInput("nCadenas", "cadenas a simular", value=1, min=1, max=10, step=1),
                             sliderInput("sLongitud", "longitud de cadenas", min=10000, max=1000000, value=1000),
                             sliderInput("sBurnin", "Burnin", min=10, max=10000, value=5000),
                             actionButton("button", "Calcula MCMC"),  #Calcula MCMC
                             
                             h4("Parametros aPriori"),
                             sliderInput("s_a", "a -> Unif ", min=1, max=10, value=c(5,8)),
                             sliderInput("s_b", "b <- Norm", min=1, max=10, value=5),
                             sliderInput("s_sigma", "sigma -> Unif", min=1, max=10, value=c(5, 6))
                           ),
                           
                           mainPanel(
                             tabsetPanel(type="tabs",
                                         tabPanel("datos",
                                                  fluidRow(
                                                    column(8, plotOutput("plot_data")),
                                                    column(12, DT::dataTableOutput("table"))
                                                  )
                                         ),
                                         tabPanel("a Priori y posteriori",
                                                  fluidRow(
                                                    column(3, plotOutput("plot_hist_A")),
                                                    column(3, plotOutput("plot_hist_B")),
                                                    column(3, plotOutput("plot_hist_Sd")),
                                                    column(3, plotOutput("plot_hist_Afinal")),
                                                    column(3, plotOutput("plot_hist_Bfinal")),
                                                    column(3, plotOutput("plot_hist_Sdfinal"))
                                                    
                                                  )
                                         ),
                                         tabPanel("Parametros de la regresion",
                                                  fluidRow(
                                                    column(4, plotOutput("hist_posteriori_A")),
                                                    column(4, plotOutput("hist_posteriori_B")),
                                                    column(4, plotOutput("hist_posteriori_Sd")),
                                                    column(4, plotOutput("plot_posteriori_A")),
                                                    column(4, plotOutput("plot_posteriori_B")),
                                                    column(4, plotOutput("plot_posteriori_Sd"))
                                                  )
                                         ),
                                         tabPanel("Multiples cadenas",
                                                  fluidRow(
                                                    column(6, plotOutput("regresionCalc")),
                                                    column(6, plotOutput("autocorrelacionCalc")),
                                                    column(12, DT::dataTableOutput("cadenasMCMC"))
                                                  )),
                                         tabPanel("Convergencia de MCMC's",
                                                  plotOutput("pConvergencia_A"),
                                                  plotOutput("pConvergencia_B"),
                                                  plotOutput("pConvergencia_Sd"))
                             )
                           )
                         )
                )
              )
      ),## fin slide menu
      
      tabItem(tabName = "extras",
              h2("Extras"), 
              tabsetPanel(
               tabPanel( "Paradoja del cumpleanios",icon = icon("birthday-cake"),
                         h3("Cual es la probabilidad de que dos personas en una fiesta cumplan anios el mismo dia?"),
                         fluidRow(
                           # box(
                           # )
                         ),
                         
                         
                         box(
                           title =  "# de Personas en la fiesta",
                           width = 10,
                           numericInput(inputId = "personascumpleanos",
                                        label = "??Cuantas personas hay en la fiesta?",
                                        value = 10
                           ),
                           verbatimTextOutput("probabilidad_cumpleanos")
                         )),
               
               tabPanel( "Faltantes", icon = icon("asterisk"),
                         fluidRow(
                           
                            box( "Faltantes", "Tarea 6, agregar liga pdf (tarea 3), agregar liga video"
                            )
                         ))
               
        ))
    )
  )
)