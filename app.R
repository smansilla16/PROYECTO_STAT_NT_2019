# Aplicación con exploraciones de los datos: Consumo de electricidad - Clima,
# a publicar en shinyapps.io 

library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)

library(readxl)
library(forcats)
library(lubridate)

# clima por fecha
clima <- read_xlsx("clima.xlsx")

# consumo por fecha
consumoen <- read_xlsx('consumoenergiaelectrica.xlsx')

# para asociar las tablas por fecha convertimos a tipo fecha
# las variables correspondientes, en la tabla de clima el formato
# de la variable fecha es una cadena de caracteres en la forma YYYYmmdd,
# en la tabla de consumo energético el formato de la variable fecha
# es representado en formato POSIXct, utilizamos la función ymd()
# de lubridate para la conversión a un formato común

# simplificación de la tabla de clima según las variables que nos interesan
# y conversión de temperatura de °F a °C
clima.simple <- clima %>%
  mutate(Fecha = ymd(YEARMODA),
         TEMP = (as.numeric(TEMP)-32)*5/9) %>%
  rename(temp_c = TEMP) %>%
  select(c(Fecha, temp_c))

# primera simplificación de la tabla de consumo energético según
# las variables que nos interesan y asociación con la tabla de clima
consumoen.simple <- consumoen %>%
  mutate(Fecha = ymd(Fecha)) %>%
  left_join(clima.simple, by = "Fecha")

# segunda simplificación de la tabla de consumo energético según
# las variables que nos interesan
consumoen.simple2 <- consumoen.simple %>%
                     gather(Fuente, Producción,`Hidráulica`, `Térmica`, `Eólica`, `Biomasa`, `Fotovoltaica`)


titulo <- dashboardHeader(
  title = "Consumo de electricidad - Clima",
  titleWidth = 350
)

intro.txt <- "<p>(..Problema..) Nuestra pregunta principal sería ¿el consumo de electricidad nacional depende del clima?</p>
<p>Para responderla obtuvimos registros del clima y del consumo energético nacional y los asociamos por fecha.</p>
<p>Relaciones entre variables:</p>
<pre>
Producción = Hidráulica + Térmica + Eólica + Biomasa + Fotovoltaica + exportaciones + consumos

exportaciones = Exportación + Exp. Otros Agentes + Exp. Salto Grande

consumos = Cons. de Generación

Demanda = producción + Importación - exportaciones - consumos</pre><br>
"


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
               icon = icon("chart-area"))
      
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
        value = HTML(intro.txt),
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
        g1 <-consumoen.simple2 %>% 
          group_by(month=floor_date(Fecha, "month"),Fuente) %>% filter(Producción!=0) %>%
          summarise(ProducciónTotal=sum(Producción),TempMedia=mean(temp_c)) %>% 
          ggplot(aes(x = TempMedia, y = ProducciónTotal/1000,color=Fuente)) +
          geom_point() + geom_smooth(method="lm",se=FALSE) +
          labs(x="Temperatura media mensual (ºC)",y="Producción energética total mensual (GWh)",color="Fuente") +
          theme(aspect.ratio = 1)
        print(g1)
      }
    })
    output$grafica2.plot <- renderPlot({
      if(input$tabs == "grafico2")
      {
        g2 <- consumoen.simple2 %>% filter(Fecha<="2018-12-31") %>% 
          group_by(month=floor_date(Fecha, "month")) %>% 
          summarise(DemandaTotal=sum(Demanda)/1000,TempMedia=mean(temp_c)) %>% 
          ggplot(aes(x = TempMedia, y = DemandaTotal/1000)) +
          geom_point() + geom_smooth(se=FALSE) +
          labs(x="Temperatura media mensual (ºC)",y="Demanda energética total mensual (GWh)",color="Fuente") +
          theme(aspect.ratio = 1)
        print(g2)
        
      }
    })
    
    output$grafica3.plot <- renderPlot({
      if(input$tabs == "grafico3")
      {
        g3 <- consumoen.simple2 %>% select(Fecha,Demanda,Producción,Fuente) %>%
          mutate(Demanda=Demanda/5) %>%
          filter(Fecha<="2018-12-31") %>% 
          gather(key="Clase",value="Energía",Demanda,Producción) %>%
          group_by(month=floor_date(Fecha, "month"),Clase) %>%
          summarize(EnergíaTotal=sum(Energía)/1000) %>% 
          ggplot(aes(x=month,y=EnergíaTotal,color=Clase)) + 
          geom_point() + geom_smooth(method="lm",se = FALSE) +
          labs(x="Fecha", y="Energía total mensual(GWh)") + 
          theme(aspect.ratio = 1)
        print(g3)
        
      }
    })

    }

shinyApp(ui, server)
