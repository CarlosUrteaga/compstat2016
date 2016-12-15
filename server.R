#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
seed <- 109152
data4 <- read.csv(file="cheese.csv", header=T)
Taste <- data4$taste
data5 <- data4[, !names(data4) %in% c("id","taste")]

Rcpp::sourceCpp("markovsin.cpp")
n1 <- dim(data)[1]
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  data <- reactive({
    seed = input$seed
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
  #data4 <- read.csv(file="cheese.csv", header=T)
  
    n4 <- dim(data4)[1]
    
    dataInput4 <- reactive({
      if(is.null(input$cVariables4))
        return()
      data4[, input$cVariables4]
    })
    
    priori_a4 <- reactive({
      runif(n4, min=input$s_a4[1], max = input$s_a4[2])
    })
    priori_b4 <- reactive({
      rnorm(n4, mean=0, sd = input$s_b4)
    })
    priori_sd4 <- reactive({
      runif(n4, min=input$s_sigma4[1], max = input$s_sigma4[2])
    })
    
    nmes <- renderText({
      input$cVariables4
    })
    #--Variables aPriori
    
    output$table4 <- DT::renderDataTable(DT::datatable({
      if(is.null(input$cVariables4))
        return()
      else 
        return(dataInput4())
      
    }))
    
    output$plot_data4 <- renderPlot({
      if(is.null(input$cVariables4))
        return()
      else{
        return(plot(dataInput4(), main="Grafica de dispersion"))
      }
    })
    
    output$plot_hist_A4 <- renderPlot({
      hist(priori_a4())
    })
    
    output$plot_hist_B4 <- renderPlot({
      hist(priori_b4())
    })
    
    output$plot_hist_Sd4 <- renderPlot({
      hist(priori_sd4())
    })
    
    output$plot_hist_Total4 <- renderPlot({
      hist(priori_a4() * priori_b4() * priori_sd4(), main="distribucion a priori")
    })
    ## mega horrible
    
    
    ##########################################################
    ############---Regresion Bayesiana: MCMC---##############
    n5 <- dim(data5)[1]
    resultado <- {}
    ############################################################
    ####################--datos de entrada--##############################
    dataInput <- reactive({
      if(is.null(input$cVariables))
        return()
      aux <- cbind(Taste, data5[, input$cVariables])
      aux
    })
    
    ############################################################
    ####################--grafica de entrada--#########################
    output$table <- DT::renderDataTable(DT::datatable({
      if(is.null(input$cVariables))
        return()
      else
        return(dataInput())
      
    }))
    
    output$plot_data <- renderPlot({
      if(is.null(input$cVariables))
        return()
      else{
        return(plot(dataInput(), main="Grafica de dispersion para quesos"))
      }
    })
    
    output$plot_hist_A <- renderPlot({
      priori_a <- runif(n5, min=input$s_a[1], max = input$s_a[2])
      hist(priori_a)
    })
    
    output$plot_hist_B <- renderPlot({
      priori_b <- rnorm(n5, mean=0, sd = input$s_b)
      hist(priori_b)
    })
    
    output$plot_hist_Sd <- renderPlot({
      priori_sd <- runif(n5, min=input$s_sigma[1], max = input$s_sigma[2])
      hist(priori_sd)
    })
    
    ############################################################
    ####################--MCMC--##############################
    chain <- reactive({
      if(is.null(input$cVariables))
        return()
      else{
        theta0 <- c(1,1,1)
        temp <- dataInput()
        chain <- runMCMC(x=data5[, input$cVariables[1]], y=Taste, startValue=theta0, iterations=input$sLongitud)
        return(data.frame(a=chain[,1], b=chain[,2], sd=chain[,3]))
      }
    })
    
    output$table <- DT::renderDataTable(DT::datatable({
      if(is.null(input$cVariables))
        return()
      else {
        return(dataInput())
      }
      
    }))
    
    output$Graph1 <- renderPlot({
      if(is.null(input$cVariables))
        return()
      else{
        return(plot(dataInput()))
      }
    })
    
    output$gPriori <- renderPlot({
      #aux <- runMCMC(x, y, theta0, 10000)
      hist(chain()[,1], title=paste("MCMC"))
    })
    
    ####################################################
    ############---Calcula MCMC con botòn---################
    df <- eventReactive(input$button, {              #dataframe with multiple chains
      if(is.null(input$cVariables))
        return()
      else{
        theta0 <- c(1,1,1)
        temp <- dataInput()
        chain <- runMCMC(x=temp[,2], y=Taste, startValue=theta0, iterations=input$sLongitud)
        chain <- chain[-(1:input$sBurnin),]
        chain <- data.frame(a=chain[,1], b=chain[,2], sd=chain[,3])
        if(input$nCadenas > 1){
          for (i in 2:input$nCadenas){
            aux <- theta0 + round(10*runif(1))
            aux2 <- runMCMC(x=temp[,2], y=Taste, startValue=aux, iterations=input$sLongitud)
            aux2 <- aux2[-(1:input$sBurnin),]
            aux2 <- data.frame(a=aux2[,1], b=aux2[,2], sd=aux2[,3])
            chain <- cbind(chain, aux2)
          }
        }
        return(chain)
      }
      
    })
    
    output$cadenasMCMC <- DT::renderDataTable(DT::datatable({
      if(is.null(df()))
        return()
      else
        return(df())
    }))
    
    
    ##############################################################################
    output$hist_posteriori_A <- renderPlot({
      if(is.null(df()))
        return()
      else{
        return({AA = mean(df()[,1])
        hist(df()[,1], main=paste("Posterior A: ", AA))
        abline(v=AA, col="red")
        })
      }
    })
    output$hist_posteriori_B <- renderPlot({
      if(is.null(df()))
        return()
      else{
        return({BB = mean(df()[,2])
        hist(df()[,2], main=paste("Posterior B: ", BB))
        abline(v=BB, col="red")
        })
      }
    })
    output$hist_posteriori_Sd <- renderPlot({
      if(is.null(df()))
        return()
      else{
        return({Ssd = mean(df()[,3])
        hist(df()[,3], main=paste("Posterior A: ", Ssd))
        abline(v=Ssd, col="red")
        })
      }
    })
    output$plot_posteriori_A <- renderPlot({
      if(is.null(df()))
        return()
      else{
        return({AA = mean(df()[-(1:input$sBurnin),1])
        plot(df()[,1], type="l", main=paste("Posterior A: ", AA))
        abline(h=AA, col="red")
        })
      }
    })
    output$plot_posteriori_B <- renderPlot({
      if(is.null(df()))
        return()
      else{
        return({BB = mean(df()[-(1:input$sBurnin),2])
        plot(df()[,2], type="l", main=paste("Posterior B: ", BB))
        abline(h=BB, col="red")
        })
      }
    })
    output$plot_posteriori_Sd <- renderPlot({
      if(is.null(df()))
        return()
      else{
        return({Ssd = mean(df()[-(1:input$sBurnin),3])
        plot(df()[,3], type="l", main=paste("Posterior Sd: ", Ssd))
        abline(h=Ssd, col="red")
        })
        
      }
    })
    
    output$plot_hist_Afinal <- renderPlot({
      if(is.null(df()))
        return()
      else{
        return({AA = mean(df()[-(1:input$sBurnin),1])
        h <- hist(df()[,1], plot=F, breaks=10)
        d <- density(df()[,1])
        hist(df()[,1], main=paste("Posterior A: ", AA),  breaks=10)
        abline(v=AA, col="red")
        lines(x=d$x, y=d$y*length(df()[,1])*diff(h$breaks)[1], ldw=2)
        })
      }
    })
    output$plot_hist_Bfinal <- renderPlot({
      if(is.null(df()))
        return()
      else{
        return({
          BB = mean(df()[-(1:input$sBurnin),2])
          
          h <- hist(df()[,2], plot=F, breaks=10)
          d <- density(df()[,2])
          hist(df()[,2], main=paste("Posterior B: ", BB),  breaks=10)
          abline(v=BB, col="red")
          lines(x=d$x, y=d$y*length(df()[,2])*diff(h$breaks)[1], ldw=2)
        })
      }
    })
    output$plot_hist_Sdfinal <- renderPlot({
      if(is.null(df()))
        return()
      else{
        return({Ssd = mean(df()[-(1:input$sBurnin),3])
        h <- hist(df()[,3], plot=F, breaks=10)
        d <- density(df()[,3])
        hist(df()[,3], main=paste("Posterior sd: ", Ssd),  breaks=10)
        abline(v=Ssd, col="red")
        lines(x=d$x, y=d$y*length(df()[,3])*diff(h$breaks)[1], ldw=2)
        })
      }
    })
    
    ##############################################################################
    output$regresionCalc <- renderPlot({
      if(is.null(input$cVariables))
        return()
      else
        return({
          plot(dataInput()[,2], dataInput()[,1], main="Regresiòn lineal", xlab="Taste", ylab="Acido")
          lines(dataInput()[,2], dataInput()[,2]*mean(df()[,1]) + mean(df()[,2]), col="blue")
        })
    })
    
    output$autocorrelacionCalc <- renderPlot({
      if(is.null(input$cVariables))
        return()
      else
        return({
          pacf(df()[,1], lag.max = NULL, plot = TRUE, na.action = na.fail, main="Auto-correlación")
          #acf(df()[,1], lag.max = NULL, type = c("correlation", "covariance", "partial"), plot = TRUE, na.action = na.fail, demean = TRUE)
        })
    })
    ##############################################################################
    output$pConvergencia_A <- renderPlot({
      if(is.null(df()))
        return()
      else{
        par(mfrow=(c(1,1)))
        return({plot(df()[,1], type = "l", main="Parametro A", ylab="A", xlab="iteraciones + Burnin")
          if(input$nCadenas > 1){
            for(i in 2:input$nCadenas){
              lines(df()[,i*3-2], col=i)
            }
          }
        })
      }
    })
    output$pConvergencia_B <- renderPlot({
      if(is.null(df()))
        return()
      else{
        par(mfrow=(c(1,1)))
        return({plot(df()[,2], type = "l", main="Parametro B", ylab="B", xlab="iteraciones + Burnin")
          if(input$nCadenas > 1){
            for(i in 2:input$nCadenas){
              lines(df()[,i*3-1], col=i)
            }
          }
        })
      }
    })
    output$pConvergencia_Sd <- renderPlot({
      if(is.null(df()))
        return()
      else{
        par(mfrow=(c(1,1)))
        return({plot(df()[,3], type = "l", main="Parametro Sd", ylab="Sd", xlab="iteraciones + Burnin")
          if(input$nCadenas > 1){
            for(i in 2:input$nCadenas){
              lines(df()[,i*3], col=i)
            }
          }
        })
      }
    })
    
  ## birthday 
    output$probabilidad_cumpleanos <-renderText({
      per = input$personascumpleanos
      prob = 1
      for (i in 1:per){
        prob = prob*((365-i)/365)
      }
      y <- round(((1-prob)*100),3)
      x <- paste("Si hay ", per, "personas en una fiesta, hay ", y,"% de probabilidades de que dos personas cumplanios el mismo dia")
      x
    })
})
# funciones auxiliares
LCG <- function(nsim, M = 2^32, a = 22695477, c = 1, seed = seed){
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

