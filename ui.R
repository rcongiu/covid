#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tags$head(includeHTML(("google-analytics.html"))),

  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      # Application title
      titlePanel("COVID-19 in Italia"),
      h4("Seleziona regione, o tutta Italia"),
      div("Le linee tratteggiate rappresentano una moving average calcolata con decadimento esponenziale")
      ),
    
    # Show a plot of the generated distribution
    mainPanel(
      selectInput("regione", label="Regione", choices = list("ITALIA",
        "Abruzzo", "Basilicata","P.A. Bolzano", "Calabria", "Campania", "Emilia Romagna", 
        "Friuli Venezia Giulia", "Lazio", "Liguria", "Lombardia",   "Marche",  "Molise",  
        "Piemonte",   "Puglia", "Sardegna", "Sicilia",  "Toscana", "P.A. Trento",
        "Umbria", "Valle d'Aosta",  "Veneto"), selected = "ITALIA"),
      selectInput("misura",label="Misura", selected="totale_casi", choices= list(
        "ricoverati_con_sintomi",
        "terapia_intensiva",
        "totale_ospedalizzati",
        "isolamento_domiciliare",
        "totale_attualmente_positivi",
        "nuovi_attualmente_positivi","dimessi_guariti",
        "deceduti","totale_casi","tamponi"
      )),
       plotOutput("linePlot"),
      hr(),
      a("Dati da https://github.com/pcm-dpc/COVID-19", href="https://github.com/pcm-dpc/COVID-19")
      
    )
  )
))
