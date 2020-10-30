#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(TTR)


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  data <- read.csv("data.csv", header=TRUE,sep="," )
  data$dt <- as.Date(data$data)
  data$totale_casi_su_tamponi <- data$totale_casi /data$tamponi
  data$ic_su_ospedalizzati <- ifelse(data$totale_ospedalizzati >0,data$terapia_intensiva / data$totale_ospedalizzati, 0)
  
  shift <- function(x, n){
    c(x[-(seq(n))], rep(NA, n))
  }
  
#  trailingDays <- 28
 # data$mortailita_su_21_giorni <- data$deceduti /  shift(data$totale_casi,trailingDays)
  output$summaryPlot <- renderPlot({
    region <- input$regione
    misura <- "totale_positivi"
    
    if(region == "ITALIA") {
      regData <-  aggregate(. ~ dt, data, sum, na.action = na.pass)
    } else {
      regData <- data[data$denominazione_regione == region,]
    }
    
    projd <- regData[, c("dt","totale_positivi","totale_ospedalizzati", "terapia_intensiva")]
    
    
    plot(projd$dt,projd$totale_positivi, type="l", col="purple", lwd=4, main=paste( "Positivi/ospedalizzati/t.intensiva", region, "Logaritmico", sep=" - "), ylab="casi/osp./t.intensiva", 
         xlab="Date", axes=TRUE, log="y")
    lines(projd$dt, projd$totale_ospedalizzati, col="black", lwd=4)
    lines(projd$dt, projd$terapia_intensiva, col="red", lwd=3)
    abline(h=0,col="gray")
    
    
    
    
    
  })
  
  output$linePlot <- renderPlot({
    
    region <- input$regione
    misura <- input$misura
    
    if(region == "ITALIA") {
      regData <-  aggregate(. ~ dt, data, sum, na.action = na.pass)
      regData$totale_casi_su_tamponi <-  regData$totale_casi / regData$tamponi 
      regData$ic_su_ospedalizzati <- ifelse(regData$totale_ospedalizzati >0,regData$terapia_intensiva / regData$totale_ospedalizzati, 0)
      
    } else {
      regData <- data[data$denominazione_regione == region,]
    }
  
    projd <- regData[, c("dt",misura)]
    names(projd) <- c("dt", "misura")
    
    myder <- function(df) {
      data.frame( "misura" = tail(df$misura, -1) - head(df$misura,-1), "dt" = tail(df$dt,-1))
    }
    
    der1 <- myder(projd)
    # ma1 <- TTR::EMA(der1$misura, n=7,  ratio=2/(5+1))
    ma1 <- TTR::SMA(der1$misura, n=7)

   # dev.new(width = 550, height = 330, unit = "px")
    
    der2 <- myder(der1)
    # ma2 <- TTR::EMA(der2$misura, n=7,  ratio=2/(5+1))
    ma2 <- TTR::SMA(der2$misura, n=7)
    layout(matrix(1:4, 2,2))
    
    # metrica richiesta, alto a sinistra
    plot(projd$dt,projd$misura, type="l", col="purple", lwd=5, main=paste(misura, region, sep=" - "), ylab=misura, 
         xlab="Date", axes=TRUE)
    
    grid(NULL, NULL, col = "lightgray", lty = "dotted",
         lwd = par("lwd"), equilogs = TRUE)
    abline(v=as.Date("03/03/2020","%d/%m/%Y"))  
    
    
    # (2)  metrics richiesta, logartmica, basso a sinistra
    plot(projd$dt,projd$misura, type="l", col="purple", lwd=5, main=paste(misura, region, "Logaritmico", sep=" - "), ylab=misura, 
         xlab="Date", axes=TRUE, log="y")
    
    grid(NULL, NULL, col = "lightgray", lty = "dotted",
         lwd = par("lwd"), equilogs = TRUE)
    abline(v=as.Date("03/03/2020","%d/%m/%Y"))  
    
    # (3) derivata prima, alto a destra
    plot(der1$dt, der1$misura,pch=12, col="red",type="l", cex=2, main="Velocità",  
         ylab=paste(misura, " - differenza giorno precedente"))
    lines(der1$dt, ma1, col="black", lty=2)
    abline(h=0,col="gray")
    abline(v=as.Date("03/03\ fg/2020","%d/%m/%Y"))  
    
    # (4) derivata seconda, basso a destra
    # der2$dt,der2$misura,  col=ifelse(der2$misura<0, "green", "red")
    plot(der2$dt, ma2,  col="black", cex=3, main="Accelerazione, media su 7 giorni", xaxs="i",
         type="l",
         xlab="Data",
         ylab=paste(misura, "/giorni^2"))
    #lines(der2$dt, ma2, col="black", lty=2)
    abline(h=0, col="gray")
    abline(v=as.Date("03/03/2020","%d/%m/%Y"))  
    
  },  height = 1000, width = 1000)
  
})
