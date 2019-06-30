instala.librerias <- function(...)
{
  for(l in list(...))
  {
     if(! l %in% rownames(installed.packages()))
     {
       cat("Instalando paquete que faltaba:", l, "\n")
       install.packages(l)
     }
  }
}

instala.librerias(
  "tidyverse",
  "ggplot2",
  "ggpmisc",
  "readxl",
  "forcats",
  "lubridate",
  "here",
  "shiny",
  "shinydashboard",
  "shinyWidgets",
  "cowplot",
  "shinycssloaders"
  "plotly"
)


