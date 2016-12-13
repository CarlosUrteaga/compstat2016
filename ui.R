#UI para Shiny con Tareas

library(shiny)
library(shinydashboard)
library(ggplot2)
library(Rcpp)


dashboardPage(
  dashboardHeader(
    title = "Estadística computacional",
    #    title = tags$a(href='http://mycompanyishere.com',
    #                   tags$img(src='logo_itam_70.png')),
    titleWidth = "900px"
    ),
  skin = "green",
  dashboardSidebar(
    sidebarMenu(
      menuItem("Números aleatorios", tabName = "random", icon = icon("black-tie")),
      menuItem("Integracion via Monte Carlo", tabName = "montecarlo", icon = icon("magic")),
      menuItem("MCMC", tabName = "MCMC", icon = icon("line-chart")),
      menuItem("Paradoja de Cumpleaños", tabName = "birthdayparadox", icon = icon("birthday-cake"))
      
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
                tabPanel("Teoría",
                         box(
                           withMathJax(),
                           width = 15,
                           includeMarkdown("md/teo01.md")
                         )
                ),
                tabPanel( "Ejemplo",
                          fluidRow(
                            ##aqui va el shiny
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
                  tabPanel("Teoría",
                           box(
                             withMathJax(),
                             width = 15,
                             includeMarkdown("md/teo02.md")
                           )
                  ),
                  tabPanel( "Ejemplo",
                            fluidRow(
                              ##aqui va el shiny
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
                tabPanel("Parte 04",
                         box(
                           withMathJax(),
                           width = 15,
                           includeMarkdown("md/hw04.md")
                         )
                ),
                tabPanel("Parte 05",
                         box(
                           withMathJax(),
                           width = 15,
                           includeMarkdown("md/hw05.md")
                         )
                ),
                tabPanel("Parte 06",
                         box(
                           withMathJax(),
                           width = 15,
                           includeMarkdown("md/hw06.md")
                         )
                )
              )
      ),## fin slide menu
      
      # tabName = "random",
      # h2("Tarea 01"),
      # tabsetPanel(
      #   tabPanel( "Método de la Función Inversa",
      tabItem(tabName = "birthdayparadox",
              h2("Paradoja del cumpleaños"),
              tabsetPanel(
                tabPanel( "Método de la Función Inversa",
                h3("¿Cuál es la probabilidad de que dos personas en una fiesta cumplan años el mismo dia?"),
                fluidRow(
                  box(
                   title =  "# de Personas en la fiesta",
                   width = 10,
                   numericInput(inputId = "personascumpleanos",
                                 label = "¿Cuantas personas hay en la fiesta?",
                                value = 10
                   ),
                   verbatimTextOutput("probabilidad_cumpleanos")
                 )
               ) ),
               tabPanel("Teoría",
                        box(
                          withMathJax(),
                          width = 15,
                          includeMarkdown("md/paradox02.md")
                        )
               )
                
        )),
      #===== elemento menu (3)
      tabItem(tabName = "Extras",
              h2("Tareas extras"),
              tabsetPanel(
                tabPanel( "Método de la Función Inversa",
                          h3(""),
                          fluidRow(
                            box(
                            )
                          ) ),
                tabPanel("Teoría",
                         box(
                           withMathJax(),
                           width = 15,
                           includeMarkdown("md/paradox02.md")
                         )
                )
                
              ))
    )
  )
)