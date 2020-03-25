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
   
  output$linePlot <- renderPlot({
    data <- read.csv("data.csv", header=TRUE,sep="," )
    data$dt <- as.Date(data$data)
    
    region <- input$regione
    misura <- input$misura
    
    if(region == "ITALIA") {
      regData <-  aggregate(. ~ dt, data, sum)
    } else {
      regData <- data[data$denominazione_regione == region,]
    }
  
    projd <- regData[, c("dt",misura)]
    names(projd) <- c("dt", "misura")
    
    myder <- function(df) {
      data.frame( "misura" = tail(df$misura, -1) - head(df$misura,-1), "dt" = tail(df$dt,-1))
    }
    
    der1 <- myder(projd)
    ma1 <- TTR::EMA(der1$misura, n=7,  ratio=2/(5+1))

    
    der2 <- myder(der1)
    ma2 <- TTR::EMA(der2$misura, n=7,  ratio=2/(5+1))
    layout(matrix(1:4, 2, 2))
    
    plot(projd$dt,projd$misura, type="l", col="purple", lwd=5, main=paste(misura, region, sep=" - "), ylab=misura, 
         xlab="Date", axes=TRUE)
    
    grid(NULL, NULL, col = "lightgray", lty = "dotted",
         lwd = par("lwd"), equilogs = TRUE)
    
    plot(der2$dt,der2$misura,  col=ifelse(der2$misura<0, "green", "red"), cex=3, main="Accelerazione",
         xlab="Verde: Decelerazione\nRosso: Accelerazione",
         ylab=paste(misura, "/giorni^2"))
    lines(der2$dt, ma2, col="black", lty=2)
    abline(h=0, col="gray")
    
    
    
    plot(der1$dt, der1$misura,pch=12, col="red",type="l", cex=2, main="Velocità", 
         ylab=paste(misura, " - differenza giorno precedente"))
    lines(der1$dt, ma1, col="black", lty=2)
    
  })
  
})
