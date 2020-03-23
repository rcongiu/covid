#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

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
  
    proj <- regData[, c("dt",misura)]
    names(proj) <- c("dt", "misura")
    
    plot(proj$dt,proj$misura, type="l", col="purple", lwd=5, main=paste(misura, region, sep=" - "), ylab=misura, 
         xlab="Date")
    
  })
  
})
