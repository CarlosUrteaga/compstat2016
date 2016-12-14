#UI para Shiny con Tareas

library(shiny)
library(shinydashboard)
library(ggplot2)
library(Rcpp)


dashboardPage(
  dashboardHeader(
    title = "Estad??stica computacional",
    #    title = tags$a(href='http://mycompanyishere.com',
    #                   tags$img(src='logo_itam_70.png')),
    titleWidth = "900px"
    ),
  skin = "green",
  dashboardSidebar(
    sidebarMenu(
      menuItem("N??meros aleatorios", tabName = "random", icon = icon("black-tie")),
      menuItem("Integracion via Monte Carlo", tabName = "montecarlo", icon = icon("magic")),
      menuItem("MCMC", tabName = "MCMC", icon = icon("line-chart")),
      menuItem("Extras", tabName = "extras", icon = icon("gift"))
      
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
                                
                                # sliderInput("nbin","N??mero de elementos en el histograma",
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
                              fluidRow(radioButtons("radioBtn", "Tipo de distribuci??n:",
                                                    c("Uniforme (GCL)"         = "UNIF",
                                                      "Exponencial (Fnc-Inv)"  = "EXP",
                                                      "Normal (Box-M??ller)"    = "NORM",
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
                                box(title = "Par??metros", "",
                                    tabsetPanel(type = "tabs", 
                                                tabPanel("Mont eCarlo", plotOutput("plot")), 
                                                tabPanel("Intervalos", plotOutput("intervals")),
                                                tabPanel("Trapecio vs MC", dataTableOutput("comparation"))
                                    )
                                    
                                    # sliderInput("nbin","N??mero de elementos en el histograma",
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
                         box(
                           withMathJax(),
                           width = 15,
                           includeMarkdown("md/hw04.md")
                         )
                ),
                tabPanel("Tarea 05",
                         box(
                           withMathJax(),
                           width = 15,
                           includeMarkdown("md/hw05.md")
                         )
                ),
                tabPanel("Tarea 06",
                         box(
                           withMathJax(),
                           width = 15,
                           includeMarkdown("md/hw06.md")
                         )
                )
              )
      ),## fin slide menu
      
      tabItem(tabName = "extras",
              h2("Extras"), 
              tabsetPanel(
                tabPanel( "M??todo de la Funci??n Inversa", icon = icon("asterisk"),
                fluidRow(
                  
               
               )),
               tabPanel( "Paradoja del cumplea??os",icon = icon("birthday-cake"),
                         h3("??Cu??l es la probabilidad de que dos personas en una fiesta cumplan a??os el mismo dia?"),
                         fluidRow(
                           # box(
                           # )
                         ),
                         
                         box(
                           withMathJax(),
                           width = 15,
                           includeMarkdown("md/paradox02.md")
                         ))
               
        ))
    )
  )
)