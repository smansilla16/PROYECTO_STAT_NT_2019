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
                     gather(fuente, produccion,`Hidráulica`, `Térmica`, `Eólica`, Biomasa, Fotovoltaica)


titulo <- dashboardHeader(
  title = "Consumo de electricidad - Clima",
  titleWidth = 350
)

intro.txt <- "<p>(..Problema..) Nuestra pregunta principal sería ¿el consumo de electricidad nacional depende del clima?</p>
<p>Para responderla obtuvimos registros del clima y del consumo energético nacional y los asociamos por fecha.</p>
<p>Relaciones entre variables:</p>
<pre>
produccion = Hidráulica + Térmica + Eólica + Biomasa + Fotovoltaica + exportaciones + consumos

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
    )    
  )
)


ui <- dashboardPage(
  titulo,
  barralateral,
  cuerpo
)

server <- function(input, output) {
    output$grafica1.plot <- renderPlot({

      if(input$tabs == "grafico1")
      {
          gg <- consumoen.simple2 %>%
                ggplot(aes(x = temp_c, y = produccion, color = fuente)) +
                geom_point()
          print(gg)
      }
    })
}

shinyApp(ui, server)
