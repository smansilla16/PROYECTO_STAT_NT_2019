# Aplicación con exploraciones de los datos: Consumo de electricidad - Clima,
# a publicar en shinyapps.io 

library(shiny)
library(shinydashboard)
library(shinyWidgets)

source("carga_datos.R", encoding = "UTF-8")

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
      
      menuItem("Gráfico Demanda - Producción",
               tabName = "grafico3",
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
                 value = c(rango.min, rango.max),
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
      condition = "input.tabs == 'grafico1'",
      box(
        title = "Grafico de Producción según el Clima",
        plotOutput("grafica1.plot", height = 400),
        width = "100%",
        height="100%"
      )
    ),    
    conditionalPanel(
      condition = "input.tabs == 'grafico2'",
      box(
        title = "Grafico de Demanda según la temperatura",
        plotOutput("grafica2.plot", height = 400),
        width = "100%",
        height="100%"
      )
    ),
    conditionalPanel(
      condition = "input.tabs == 'grafico3'",
      box(
        title = "Grafico de Demanda y Producción energética",
        plotOutput("grafica3.plot", height = 400),
        width = "100%",
        height="100%"
      )
  
  )
))
  


ui <- dashboardPage(
  titulo,
  barralateral,
  cuerpo
)

server <- function(input, output) {
    output$grafica1.plot <- renderPlot({

      if(input$tabs == "grafico1")
      {
        g1 <-consumoEE.datos2 %>% 
          group_by(month = floor_date(Fecha, agrupacion.texto2text(input$agrupacion)), Fuente) %>%
          filter(Producción!=0, Fecha >= input$rango[1] && Fecha <= input$rango[2]) %>%
          summarise(ProducciónTotal=sum(Producción),
                    TempMedia=mean(temp_c)) %>% 
          ggplot(aes(x = TempMedia,
                     y = ProducciónTotal,
                     color=Fuente)) +
          geom_point() + geom_smooth(method="lm",se=FALSE) +
          labs(x=paste("Temperatura media", input$agrupacion,"(ºC)"),
               y=paste("Producción energética total", input$agrupacion ,"(GWh)"),
               color="Fuente") +
          theme(aspect.ratio = 1)
        print(g1)
      }
    })

    output$grafica2.plot <- renderPlot({
      if(input$tabs == "grafico2")
      {
        g2 <- consumoEE.datos2 %>%
          filter(Producción!=0, Fecha >= input$rango[1] && Fecha <= input$rango[2]) %>% 
          group_by(month = floor_date(Fecha,  agrupacion.texto2text(input$agrupacion))) %>% 
          summarise(DemandaPromedio=mean(Demanda),TempMedia=mean(temp_c)) %>% 
          ggplot(aes(x = TempMedia, y = DemandaPromedio)) +
          geom_point() + geom_smooth(se=FALSE) +
          labs(x=paste("Temperatura media", input$agrupacion,"(ºC)"),
               y=paste("Demanda energética media", input$agrupacion ,"(GWh)"),
               color="Fuente") +
          theme(aspect.ratio = 1)
        print(g2)
        
      }
    })
    
    output$grafica3.plot <- renderPlot({
      if(input$tabs == "grafico3")
      {
        g3 <- consumoEE.datos2 %>% select(Fecha, Demanda, `Producción Total`, Fuente) %>%
          filter(Fecha >= input$rango[1] && Fecha <= input$rango[2]) %>% 
          gather(key="Clase",value="Energía", Demanda, `Producción Total`) %>%
          group_by(month=floor_date(Fecha, agrupacion.texto2text(input$agrupacion)), Clase) %>%
          summarize(EnergíaTotal=sum(Energía)) %>% 
          ggplot(aes(x=month,y=EnergíaTotal,color=Clase)) + 
          geom_point() + geom_smooth(method="lm",se = FALSE) +
          labs(x="Fecha",
               y=paste("Energía total", input$agrupacion ,"(GWh)")) + 
          theme(aspect.ratio = 1)
        print(g3)
      }
    })

    }

shinyApp(ui, server)
