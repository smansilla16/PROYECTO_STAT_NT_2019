if(!exists("datos_cargados"))
{
  
library(tidyverse)
library(ggplot2)
library(ggpmisc)

library(readxl)
library(forcats)
library(lubridate)

library(here)

# clima por fecha
clima <- read_xlsx(here("datos", "clima.xlsx"))

# consumo por fecha
consumoEE <- read_xlsx(here("datos", "consumoenergiaelectrica.xlsx"))

# para asociar las tablas por fecha convertimos a tipo fecha
# las variables correspondientes, en la tabla de clima el formato
# de la variable fecha es una cadena de caracteres en la forma YYYYmmdd,
# en la tabla de consumo energético el formato de la variable fecha
# es representado en formato POSIXct, utilizamos la función ymd()
# de lubridate para la conversión a un formato común

# simplificación de la tabla de clima según las variables que nos interesan
# y conversión de temperatura de °F a °C
clima.datos <- clima %>%
  mutate(Fecha = ymd(YEARMODA),
         TEMP = (as.numeric(TEMP)-32)*5/9,
         MAX = (as.numeric(MAX)-32)*5/9,
         MIN = (as.numeric(MIN)-32)*5/9) %>%
  rename(temp_c = TEMP) %>%
  select(c(Fecha, temp_c, MIN, MAX))

# primera simplificación de la tabla de consumo energético según
# las variables que nos interesan y asociación con la tabla de clima
consumoEE.datos <- consumoEE %>%
  mutate(Fecha = ymd(Fecha)) %>%
  left_join(clima.datos, by = "Fecha")

# segunda simplificación de la tabla de consumo energético según
# las variables que nos interesan, la producción de cada fuente
# es expresada en GWh
consumoEE.datos2 <- consumoEE.datos %>%
  mutate(`Producción Total` = (`Hidráulica` + `Térmica` + `Eólica` + `Biomasa` + `Fotovoltaica`)/1000,
         `Exportación Total` = (`Exportación` + `Exp. Otros Agentes` + `Exp. Salto Grande`)/1000) %>%
  select(-`Exportación`, -`Exp. Otros Agentes`, -`Exp. Salto Grande`) %>%
  gather(Fuente, `Producción`,
         `Hidráulica`, `Térmica`, `Eólica`, `Biomasa`, `Fotovoltaica`) %>%
  mutate(`Producción` = `Producción`/1000,
         `Demanda` = `Demanda`/1000,
         `Importación` = `Producción`/1000,
         `Cons. de Generación`)

colnames(consumoEE.datos2)

agrupacion.texts <- c("day", "week", "month", "bimonth", "quarter", "season", "halfyear", "year")
agrupacion.textos <- c("diario", "semanal", "mensual", "bimensual", "trimestral", "estacional", "semestral", "anual")
agrupacion.texto2text <- function(t) { return(agrupacion.texts[match(t, agrupacion.textos)]) }

rango.min = ymd(min(consumoEE.datos2$Fecha))
rango.max = ymd(max(consumoEE.datos2$Fecha))

datos_cargados <- TRUE

}