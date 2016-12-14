#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

data4 <- read.csv(file="cheese.csv", header=T)

n <- dim(data)[1]
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  data <- reactive({
    lambda = input$lambda
    switch(input$radioBtn,
           #generador de congruencia lineal, distribuci??n uniforme
           UNIF = {
             LCG(input$nsims)
             # sapply(aux[2:input$num], function(x, a=22695477, c=1, m=2**32){
             #   return(((a*x + c) %% m) / m)
             #   })
           },
           
           #Box-M??ller
           NORM = { #rnorm(input$num)
             u1 <- runif(input$nsims)   #R2 <- -2*log(u1)
             u2 <- runif(input$nsims)   #theta <- 2*pi*u2
             sqrt(-2*log(u1))*cos(2*pi*u2)
           },
           #funci??n inversa
           EXP  = {
             sapply(seq(1, input$nsims), function(x, lambda=input$lambda){
               u <- runif(length(x))
               return(-log(1-u)/lambda)
             })
           },
           
           GEOM = {
             sapply(seq(1, input$nsims), function(x, prob=0.5){
               u <- runif(length(x))
               return(log(u)/log(1-prob))
             })
           })
  })
  
  kolmogorovTest <- reactive({
    switch(input$radioBtn,
           UNIF = ks.test(data(), "punif"),
           NORM = ks.test(data(), "pnorm"),
           EXP  = ks.test(data(), "pexp"),
           GEOM = ks.test(data(), rgeom(input$nsims, prob=0.5)))
  })
  
  chiTest <- reactive({
    switch(input$radioBtn,
           UNIF = {breaks <- seq(0,1, length.out = input$nsims/10)
           o <- table(cut(data(), breaks=breaks))
           p <- diff(punif(breaks))
           chisq.test(o, p=p, rescale.p=T)},
           NORM = {breaks <- c(seq(-5,5, length.out = input$nsims/10))
           o <- table(cut(data(), breaks = breaks))
           p <- diff(pnorm(breaks))
           chisq.test(o, p=p, rescale.p = T)},
           EXP  = {breaks <- c(seq(0,10, length.out = input$nsims/10))
           o <- table(cut(data(), breaks = breaks))
           p <- diff(pexp(breaks))
           chisq.test(o, p=p, rescale.p = T)},
           GEOM = {breaks <- c(seq(0,10, by=1))
           o <- table(cut(data(), breaks = breaks))
           p <- diff(pgeom(breaks, prob=0.5))
           chisq.test(o, p=p, rescale.p = T)}
    )
  })
  
  output$ksTest <- renderPrint({
    kolmogorovTest()
  })
  
  output$chiTest <- renderPrint({
    chiTest()
  })
  
  output$stats <- renderPrint({
    summary(data())
  })
  
  output$hist <- renderPlot({
    h <- hist(data(), breaks = input$nbin, plot=F)
    d <- density(data())
    hist(data(), breaks = input$nbin,
         main= isolate(input$title))
    lines(x=d$x, y=d$y*length(data())*diff(h$breaks)[1], ldw=2)
  })
  
  output$qqPlot <- renderPlot({
    par(mfrow=c(1,2))
    switch(input$radioBtn,
           UNIF = {q1 = qunif(seq(0,1,0.01)); 
           q2 = quantile(data(), seq(0,1,0.01));
           plot(q1, q2, main="Q-Q Plot", ylab = "CLG", xlab="qunif")},
           NORM = {q1 = qnorm(seq(0,1,0.01)); 
           q2 = quantile(data(), seq(0,1,0.01));
           plot(q1, q2, main="Q-Q Plot", ylab="rnorm", xlab="qnorm")},
           EXP  = {q1 = qexp(seq(0,1,0.01));  
           q2 = quantile(data(), seq(0,1,0.01));
           plot(q1, q2, main="Q-Q Plot", ylab = "expInv", xlab="qexp")},
           GEOM = {q1 = qgeom(seq(0,1,0.01), prob=0.5);  
           q2 = quantile(data(), seq(0,1,0.01));
           plot(q1, q2, main="Q-Q Plot", ylab = "geomInv", xlab="qgeom")}
    )
    plot(data()[1:length(data())-1], data()[2:length(data())], main = "Secuencia en n??meros")
    #qplot(data()[-length(data())], data()[-1], main = "Secuencia en n??meros")
  })
  output$distPlot <- renderPlot({
    
    if (input$tab == "random") {
    }    

    
  })
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$radioBtn, '.csv', sep='') },
    content = function(file) {
      write.csv(data(), file)
    }
  )
  output$plot <- renderPlot({
    fun1 <- eval(parse(text = input$inpFunc))
    
    nn <- input$n
    from <- input$lmts[1]
    to <- input$lmts[2]
    to2 <- to - from
    
    u1 <- runif(nn, from, to)
    aux <- fun1(u1)
    aux[is.nan(aux)] <- 0
    #to2*mean(aux)
    
    curve(fun1, from=input$lmts[1], to=input$lmts[2], main=paste("MonteCarlo: ", to2*mean(aux)))
    
    aux <- fun1(seq(from, to))
    aux[is.nan(aux)] <- 0
    mn <- min(aux)
    mx <- max(aux)
    points(u1, runif(input$n, mn, mx))
  })
  
  output$intervals <- renderPlot({
    upper <- {}
    lower <- {}
    mC <- {}
    nn <- input$n
    
    fun1 <- eval(parse(text = input$inpFunc))
    
    for (i in seq(1,nn)){
      u1 <- runif(i, input$lmts[1], input$lmts[2])
      to2 <- input$lmts[2] - input$lmts[1]
      aux <- to2*mean(fun1(u1))
      mC[i]  <- aux
      
      s = sd(mC)
      error <- qnorm(1-input$alfa)*s/(sqrt(i))
      left <- aux-error
      right <- aux+error
      
      lower[i] <- left
      upper[i] <- right
    }
    
    plot(upper[2:nn], type="l", log="x",col="gray", main=paste("Simulaci??n MonteCarlo: ", aux))
    lines(mC[2:nn], col="black")
    lines(lower[2:nn],  col="gray")
    abline(h=aux, col="red")
    grid()
  })
  
  output$comparation <- renderDataTable({
    fun1 <- eval(parse(text = input$inpFunc))
    aux <- mcIntervals(input$n, input$lmts[1], input$lmts[2], fun=fun1, alfa=input$alfa)
    aux
  })
  ## la gacha
  data4 <- read.csv(file="cheese.csv", header=T)
  
    n <- dim(data4)[1]
    
    dataInput <- reactive({
      if(is.null(input$cVariables))
        return()
      aux <- data4[, input$cVariables]
      aux
    })
    
    priori_a <- reactive({
      runif(n, min=input$s_a[1], max = input$s_a[2])
    })
    priori_b <- reactive({
      rnorm(n, mean=0, sd = input$s_b)
    })
    priori_sd <- reactive({
      runif(n, min=input$s_sigma[1], max = input$s_sigma[2])
    })
    
    nmes <- renderText({
      input$cVariables
    })
    
    output$table <- DT::renderDataTable(DT::datatable({
      if(is.null(input$cVariables))
        return()
      else 
        return(dataInput())
      
    }))
    
    output$plot_data4 <- renderPlot({
      if(is.null(input$cVariables))
        return()
      else{
        return(plot(dataInput(), main="Grafica de dispersion"))
      }
    })
    
    output$plot_hist_A <- renderPlot({
      hist(priori_a())
    })
    
    output$plot_hist_B <- renderPlot({
      hist(priori_b())
    })
    
    output$plot_hist_Sd <- renderPlot({
      hist(priori_sd())
    })
    
    output$plot_hist_Total <- renderPlot({
      hist(priori_a() * priori_b() * priori_sd(), main="distribucin a priori")
    })

})
# funciones auxiliares
LCG <- function(nsim, M = 2^32, a = 22695477, c = 1, seed = 110104){
  X = c(seed, numeric(nsim-1)) # Aparta memoria
  for(i in 1:(nsim-1)) X[i+1] <- ((a*X[i] + c)%% M) # Aplica GenradorCongruenciaLineal
  return(X/M) # Aplica transformacion
}
mcIntervals <- function(nn, from, to, fun, alfa=0.05){
  lower <- {}
  upper <- {}
  mC <- {}
  trapecio <- {}
  
  for (n in 1:nn){
    aux <- (to-from)*mean(sapply(runif(n, from, to), fun))
    mC <- c(mC, aux)
    S <- sd(mC)
    error <- qnorm(1-alfa)*S/sqrt(length(mC))
    lo <- aux-error
    up <- aux+error
    lower <- c(lower, lo)
    upper <- c(upper, up)
    
    aux <- trapezoid(n, from, to, fun)
    trapecio <- c(trapecio, aux)
  }
  aux <- data.frame(lowerLimit=lower, monteCarlo=mC, upperLimit=upper, trapecio=trapecio)
  return(aux)
}
trapezoid <- function(n, a, b, FUN){
  dim <- length(a)
  x <- seq(a[1], b[1], (b[1]-a[1])/n)
  if(dim == 1){
    fi <- sapply(x, FUN)
  } else{
    fi <- sapply(x, function(x){
      trapezoid(n, a[-1], b[-1], function(y) FUN(c(x,y)))
    })
  }
  return(((b[1]-a[1])/(2*n))*sum(fi[-1]+fi[-(n+1)]))
}

