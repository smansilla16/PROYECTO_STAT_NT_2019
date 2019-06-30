
library(tidyverse)
library(ggplot2)
library(ggpmisc)

library(readxl)
library(forcats)
library(lubridate)

library(here)

library(cowplot) # para acomodar múltiples plots en un mismo bloque

library(scales)
library(plotly)

library(shiny)
library(shinydashboard)
library(shinyWidgets)


# instala.librerias <- function(...)
# {
#   for(l in list(...))
#   {
#      if(! l %in% rownames(installed.packages()))
#      {
#        cat("Instalando paquete que faltaba:", l, "\n")
#        install.packages(l)
#      }
#   }
# }
# 
# instala.librerias(
#   "tidyverse",
#   "ggplot2",
#   "ggpmisc",
#   "readxl",
#   "forcats",
#   "lubridate",
#   "here",
#   "shiny",
#   "shinydashboard",
#   "shinyWidgets",
#   "cowplot",
#   "shinycssloaders"
#   "plotly"
# )


