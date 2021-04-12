#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(expm)
library(markovchain)
library(readxl)
source("metodos.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Análisis del Sistema SPA"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        fileInput(inputId='ArchivoEntrada', label='Cargar información del sistema SPA',accept=c(".xlsx"),placeholder = "Casos2019_Hist.xlsx"),
        selectInput(inputId='EstadoInicial', label='Elegir estado inicial', choices=c("Imputado","Acusado pena no privativa","Liberado reincidente","Acusado pena privativa","Liberado no reincidente","Fallecido"))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
          textOutput(outputId = 'answer1'),
          textOutput(outputId = 'answer2'),
          plotOutput(outputId = 'plot')
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
     output$answer1<-renderText({
       archivo<-input$ArchivoEntrada
       if(is.null(archivo))
       {
         archivo<-"Casos2019_Hist.xlsx"
       }
       CMTD<-cargarCadena(archivo)
       paste("El tiempo esperado que dura un caso en el SPA empezando en ",input$EstadoInicial," es de ",tiempoEsperadoCaso(input$EstadoInicial,CMTD),sep="")
     })
     output$answer2<-renderText({
       archivo<-input$ArchivoEntrada
       if(is.null(archivo))
       {
         archivo<-"Casos2019_Hist.xlsx"
       }
       CMTD<-cargarCadena(archivo)
       paste("La probabilidad de que un ciudadano que entre en ",input$EstadoInicial," y salga resocializado es de ",pResocializado(input$EstadoInicial,CMTD),sep="")
     })
    output$plot<-renderPlot({
      archivo<-input$ArchivoEntrada
      if(is.null(archivo))
      {
        archivo<-"Casos2019_Hist.xlsx"
      }
      CMTD<-cargarCadena(archivo)
      plot(CMTD)
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

