# Aplicación con exploraciones de los datos: Consumo de electricidad - Clima,
# a publicar en shinyapps.io 

library(shiny)
library(shinydashboard)
library(shinyWidgets)

datos_clima_consumo_cargados <<- FALSE

source("carga_datos_clima_consumo.R", encoding = "UTF-8")

source("carga_textos.R", encoding = "UTF-8")


titulo <- dashboardHeader(
  title = "Consumo de Energía Electrica y Clima",
  titleWidth = 400
)


barralateral <- dashboardSidebar(
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Introducción",
               tabName = "intro",
               icon = icon("info-circle")),

      menuItem("Gráfico Producción - Clima",
               tabName = "grafico1",
               icon = icon("chart-area")),
      
      menuItem("Gráfico Demanda - Clima",
               tabName = "grafico2",
               icon = icon("chart-area")),
      
      menuItem("Gráfico Demanda - Estación",
               tabName = "grafico3",
               icon = icon("chart-area")),

      menuItem("Gráfico Demanda Dens. Temp.",
               tabName = "grafico4",
               icon = icon("chart-area")),
      
      menuItem("Gráfico Demanda - Producción",
               tabName = "grafico5",
               icon = icon("chart-area")),
      
      menuItem("Control",
               tabName = "control",
               icon = icon("gamepad"),
               sliderTextInput(
                 inputId = "agrupacion",
                 label = "Agrupación de datos:",
                 choices = agrupacion.textos,
                 selected = agrupacion.textos[3]
               ),
               sliderInput(
                 inputId = "rango",
                 label = "Rango de fechas:",
                 min = rango.min,
                 max = rango.max,
                 value = as.Date(c(rango.min, rango.max)),
                 timeFormat="%Y-%m-%d"
              ))
    )
  )
)

cuerpo <- dashboardBody(
  fluidRow(
    conditionalPanel(
      condition = "input.tabs == 'intro'",
      infoBox(
        title = h2("Introducción"),
        icon = icon("info-circle"),
        width = 12,
        value = HTML(intro_texto),
        img(src = "diagrama.png", width = "100%")
      )      
    ),
    conditionalPanel(
      condition = "input.tabs != 'intro' && input.tabs != 'control'",
      box(
        title = textOutput("grafica.titulo"),
        plotOutput("grafica.plot", height = 400),
        textOutput("grafica.desc"),
        textOutput("grafica.obs"),
        width = "100%",
        height="100%"
      )
    )
  )
)
  

ui <- dashboardPage(
  titulo,
  barralateral,
  cuerpo
)

server <- function(input, output) {
    output$grafica.plot <- renderPlot({

      if(input$tabs != "intro" && input$tabs != "control")
      {
        ajustes.update(input)
      }

      if(input$tabs == "grafico1")
      {
        source("grafico_produccion_clima.R", encoding = "UTF-8")
      }

      if(input$tabs == "grafico2")
      {
        source("grafico_demanda_clima.R", encoding = "UTF-8")
      }

      if(input$tabs == "grafico3")
      {
        source("grafico_demanda_estacion.R", encoding = "UTF-8")
      }

      if(input$tabs == "grafico4")
      {
        source("grafico_demanda_densidad_temp.R", encoding = "UTF-8")
      }
      
      if(input$tabs == "grafico5")
      {
        source("grafico_demanda_produccion.R", encoding = "UTF-8")
      }

      if(input$tabs != "intro" && input$tabs != "control")
      {
        print(grafico)
        output$grafica.titulo <-  renderText({ grafico.titulo })
        output$grafica.desc <-  renderText({ grafico.descripcion })
        output$grafica.obs <- renderText({ grafico.observacion })
      }
      
    })
    
}
    

shinyApp(ui, server)
