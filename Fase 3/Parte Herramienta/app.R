#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(expm)
library(markovchain)
library(readxl)
library(igraph)
library(reshape2)
library(ggplot2)
source("metodosFase1.R")
source("metodosFase2.R")
source("metodosFase3.R")
# Definir la interfaz 
ui <- dashboardPage(
  dashboardHeader(title="ModelandoAndo"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Servicio de CAIs",tabName = "CAI",icon = icon("th")),
      menuItem("útilidad de la alcaldía",tabName = "utilidad",icon = icon("th")),
      menuItem("Sistema Penal Acusatorio",tabName = "SPA",icon = icon("th")),
      menuItem("Mapa calor Kennedy",tabName = "KND",icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "CAI",
              sidebarLayout(
                sidebarPanel(
                  h2("CAI Kennedy"),
                  numericInput(inputId="PoliciasCAIK_F1",label = "Cantidad de Policías Promedio",value = 8,min=1),
                  h2("CAI Suba"),
                  numericInput(inputId="PoliciasCAIS_F1",label = "Cantidad de Policías Promedio", value = 18,min=1),
                  h2("CAI Bosa"),
                  numericInput(inputId="PoliciasCAIB_F1",label = "Cantidad de Policías Promedio", value = 13,min=1)
                ),
                mainPanel(
                  h2("Gráficas"),
                  plotlyOutput(outputId = 'plotE_F1'),
                  plotlyOutput(outputId = 'plotV_F1')
                )
              )
      ),
      tabItem(tabName = "utilidad",
              sidebarLayout(
                sidebarPanel(
                  h2("Costos"),
                  numericInput(inputId="Costos_F1",label = "Salario", 1,0),
                  numericInput(inputId="Combustible_F1",label = "Combustible", 1,0),
                  numericInput(inputId="Salario_F1",label = "Dotación", 1,0)
                ),
                mainPanel(
                  h2("Útilidad de la Alcaldía"),
                  textOutput(outputId = "utilidad_F1")
                )
              )
      ),
      tabItem(tabName = "SPA",
              sidebarLayout(
                sidebarPanel(
                  fileInput(inputId='ArchivoEntrada', label='Cargar información del sistema SPA',accept=c(".xlsx"),placeholder = "Casos2019_Hist.xlsx"),
                  selectInput(inputId='EstadoInicial', label='Elegir estado inicial', choices=c("Imputado","Acusado pena no privativa","Liberado reincidente","Acusado pena privativa"))
                ),
                mainPanel(
                  textOutput(outputId = 'answer1_F2'),
                  textOutput(outputId = 'answer2_F2'),
                  plotOutput(outputId = 'plot_F2')
                )
              )
      ),
      tabItem(tabName = "KND",
              sidebarLayout(
                sidebarPanel(
                  h2("Averiguar Puntaje"),
                  selectInput(inputId="zona","Zona de mayor criminalidad", choices=list("A","B","C","D","E","F")),
                  selectInput(inputId="mes","Mes", choices=c(1:24)),
                  h3("Puntaje:"),
                  textOutput(outputId = 'puntajeS_F3')
                ),
                mainPanel(
                  textOutput(outputId = 'answer1_F3'),
                  plotOutput(outputId = 'plot_F3'),
                  textOutput(outputId = 'answer2_F3')
                )
              )
      )
    )
  )
)
# Definir la lógica de la aplicación
server <- function(input, output) 
{
  #--------------Fase 1--------------------------
  output$plotE_F1<-renderPlotly(
    {matrizQKennedy<-matrixQFunc(maxC=40,lambdaIn=0.3801787,lambdaOut=1/24,policias = input$PoliciasCAIK_F1)
    matrizQBosa<-matrixQFunc(maxC=31,lambdaIn=0.4574637,lambdaOut=1/20,policias = input$PoliciasCAIB_F1)
    matrizQSuba<-matrixQFunc(maxC=26,lambdaIn=0.4203695,lambdaOut=1/36,policias = input$PoliciasCAIS_F1)
    EF<-valoresEsperados(matrizQKennedy,matrizQBosa,matrizQSuba)
    figF<-plot_ly(EF,x=EF[1:337,1],y=EF[1:337,2],type='scatter',mode='lines+markers',name = "Casos Kennedy")
    figF<-figF%>%add_trace(y=EF[1:337,3],mode='lines+markers',name = "Casos Suba")
    figF<-figF%>%add_trace(y=EF[1:337,4],mode='lines+markers',name = "Casos Bosa")
    figF<-figF%>%layout(title="Casos Esperados por Hora",xaxis=list(title="horas"),yaxis=list(title="Casos"))
    figF}
  )
  output$plotV_F1<-renderPlotly(
    {matrizQKennedy<-matrixQFunc(maxC=40,lambdaIn=0.3801787,lambdaOut=1/24,policias = input$PoliciasCAIK_F1)
    matrizQBosa<-matrixQFunc(maxC=31,lambdaIn=0.4574637,lambdaOut=1/20,policias = input$PoliciasCAIB_F1)
    matrizQSuba<-matrixQFunc(maxC=26,lambdaIn=0.4203695,lambdaOut=1/36,policias = input$PoliciasCAIS_F1)
    EF<-valorVarianzas(matrizQKennedy,matrizQBosa,matrizQSuba)
    figF<-plot_ly(EF,x=EF[1:337,1],y=EF[1:337,2],type='scatter',mode='lines+markers',name = "Casos Kennedy")
    figF<-figF%>%add_trace(y=EF[1:337,3],mode='lines+markers',name = "Casos Suba")
    figF<-figF%>%add_trace(y=EF[1:337,4],mode='lines+markers',name = "Casos Bosa")
    figF<-figF%>%layout(title="Varianza de Casos por Hora",xaxis=list(title="horas"),yaxis=list(title="Casos^2"))
    figF}
  )
  output$utilidad_F1<-renderText({
    utilidades(input$Combustible_F1,input$Costos_F1,input$Salario_F1)
  })
  #--------------Fase 2--------------------------
  #Arreglar font 
  output$answer1_F2<-renderText({
    archivo<-input$ArchivoEntrada
    if(is.null(archivo))
    {
      archivo<-"Casos2019_Hist.xlsx"
    }
    CMTD<-cargarCadena(as.character(archivo[1]))
    paste("El tiempo esperado que dura un caso en el SPA empezando en ",input$EstadoInicial," es de ",tiempoEsperadoCaso(input$EstadoInicial,CMTD)," semanas",sep="")
  })
  output$answer2_F2<-renderText({
    archivo<-input$ArchivoEntrada
    if(is.null(archivo))
    {
      archivo<-"Casos2019_Hist.xlsx"
    }
    CMTD<-cargarCadena(as.character(archivo[1]))
    paste("La probabilidad de que un ciudadano que entre en ",input$EstadoInicial," y salga resocializado es de ",pResocializado(input$EstadoInicial,CMTD),sep="")
  })
  output$plot_F2<-renderPlot({
    archivo<-input$ArchivoEntrada
    if(is.null(archivo))
    {
      archivo<-"Casos2019_Hist.xlsx"
    }
    CMTD<-cargarCadena(as.character(archivo[1]))
    plot(CMTD,layout=layout.circle,main="Cadena de Markov del SPA",rescale = TRUE,asp=1,edge.arrow.size=0.7,curved=T,vertex.label.cex=0.5,edge.label.cex=0.7)
  })
  #-----------Fase 3-----------------------------
  output$plot_F3<-renderPlot({
    politicas_opt<-heatmapInfo()
    decisionesOptimasLista <- melt(politicas_opt[1:6,])
    decisionesOptimasLista$value <- as.factor(decisionesOptimasLista$value)
    colnames(decisionesOptimasLista) <- c("Zona", "Mes", "Decision")
    ggplot(data = decisionesOptimasLista, aes(x = Mes, y = Zona, fill = Decision)) +   geom_tile()
  })
  output$puntajeS_F3<-renderText({
    f_i<-getF_i()
    as.character(f_i[input$zona,input$mes])
  })
}
# Correr la aplicación 
shinyApp(ui = ui, server = server)