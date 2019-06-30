# Aplicación con exploraciones de los datos: Consumo de electricidad - Clima,
# a publicar en shinyapps.io 

library(shiny)
library(shinydashboard)
library(shinyWidgets)

datos_clima_consumo_cargados <<- FALSE

textos_cargados <<- FALSE

source("carga_datos_clima_consumo.R", encoding = "UTF-8")

source("carga_textos.R", encoding = "UTF-8")


titulo <- dashboardHeader(
  title = "Consumo de Energía Eléctrica y Clima",
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
      
      menuItem("Gráfico Demanda - Temp",
               tabName = "grafico2",
               icon = icon("chart-area")),
      
      menuItem("Gráfico Demanda - Estación",
               tabName = "grafico3",
               icon = icon("chart-area")),

      menuItem("Gráfico Demanda - Producción",
               tabName = "grafico6",
               icon = icon("chart-area")),
      
      menuItem("Gráfico Temperatura - Tiempo",
               tabName = "grafico7",
               icon = icon("chart-area")),
      
      menuItem("Gráfico Temperatura - Tiempo2",
               tabName = "grafico8",
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
               radioButtons(
                 inputId = "tipofiltro",
                 label = "Filtrados por:",
                 choices = c("Rango", "Año"),
                 inline = TRUE
               ),
               conditionalPanel(
                 condition = "input.tipofiltro == 'Rango'",
                 sliderInput(
                   inputId = "rango",
                   label = "Rango de fechas:",
                   min = rango.min,
                   max = rango.max,
                   value = as.Date(c(rango.min, rango.max)),
                   timeFormat="%Y-%m-%d"
                )
              ),
              conditionalPanel(
                condition = "input.tipofiltro == 'Año'",
                sliderInput(
                  inputId = "año",
                  label = "Selección de Año:",
                  min = year(rango.min),
                  max = year(rango.max),
                  value = year(rango.max),
                  sep = ""
                )
              )
             )
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
        value = HTML(refs.traduce(intro_texto)),
        img(src = "diagrama.png", width = "100%")
      )      
    ),
    conditionalPanel(
      condition = "RegExp('grafico[1-8]').test(input.tabs)",
      box(
        title = htmlOutput("grafica.titulo"),
        plotOutput("grafica.plot", height = 400),
        htmlOutput("grafica.desc"),
        htmlOutput("grafica.obs"),
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
  
    reactive({

    })
    
    output$grafica.plot <- renderPlot({

      if(grepl("grafico[1-8]", input$tabs))
      {
        ajustes.update(input)
      }
      
      if(input$tabs == "grafico1")
      {
        source("grafico_produccion_clima.R", encoding = "UTF-8")
      }

      if(input$tabs == "grafico2")
      {
        source("grafico_demanda_clima_temp.R", encoding = "UTF-8")
      }

      if(input$tabs == "grafico3")
      {
        source("grafico_demanda_estacion_mes.R", encoding = "UTF-8")
      }

      if(input$tabs == "grafico6")
      {
        source("grafico_demanda_produccion.R", encoding = "UTF-8")
      }
      
      if(input$tabs == "grafico7")
      {
        source("grafico_temperatura_tiempo.R", encoding = "UTF-8")
      }
      
      if(input$tabs == "grafico8")
      {
        source("grafico_temperatura_tiempo2.R", encoding = "UTF-8")
      }

      if(grepl("grafico[1-8]", input$tabs))
      {
        print(grafico)
        output$grafica.titulo <-  renderUI({ HTML(grafico.titulo) })
        output$grafica.desc <-  renderUI({ HTML(paste("<p style='margin-top:30px'>", grafico.descripcion, "</p>")) })
        output$grafica.obs <- renderUI({ p(HTML(grafico.observacion)) })
      }

      
    })
    

}
    

shinyApp(ui, server)
