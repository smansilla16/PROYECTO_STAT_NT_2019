if(!exists("datos_clima_consumo_cargados") || !datos_clima_consumo_cargados)
{
  
library(tidyverse)
library(ggplot2)
library(ggpmisc)

library(readxl)
library(forcats)
library(lubridate)

library(here)

library(cowplot) # para acomodar múltiples plots en un mismo bloque

library(scales)

theme_set(
  theme_grey() # para devolver el estilo estándar de ggplot
               # porque que es cambiado por cowplot
)

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
  mutate(MIN = as.numeric(gsub("\\*", "", MIN)),
         MAX = as.numeric(gsub("\\*", "", MAX))) %>%
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
  mutate(`Demanda` = `Demanda`/1000,
         `Importación` = `Importación`/1000,
         `Cons. de Generación` = `Cons. de Generación`/1000) %>%
  drop_na(Fecha, temp_c, Demanda, `Producción Total`)

colnames(consumoEE.datos2)

head(consumoEE.datos2)

# tercera simplificación de la tabla
consumoEE.datos3 <- consumoEE.datos2 %>%
  gather(Fuente, `Producción`,
         `Hidráulica`, `Térmica`, `Eólica`, `Biomasa`, `Fotovoltaica`) %>%
  mutate(`Producción` = `Producción`/1000) %>%
  drop_na(Fecha, temp_c, MAX, MIN, Demanda, `Producción Total`)

colnames(consumoEE.datos3)

head(consumoEE.datos3)

agrupacion.texts <- c("day", "week", "month", "bimonth", "quarter", "season", "halfyear", "year")
agrupacion.textos <- c("diario", "semanal", "mensual", "bimensual", "trimestral", "estacional", "semestral", "anual")
agrupacion.texto2text <- function(t) { return(agrupacion.texts[match(t, agrupacion.textos)]) }

rango.min <- ymd(min(consumoEE.datos2$Fecha))
rango.max <- ymd(max(consumoEE.datos2$Fecha))
rango.color.año <- hue_pal()(year(rango.max)-year(rango.min) + 1)
names(rango.color.año) <- year(rango.min):year(rango.max)

ajustes <- list(rango = as.Date(c(rango.min, rango.max)),
                agrupacion = "mensual")

ajustes.update <- function(i) {
  if(i$tipofiltro == "Rango")
  {
    ajustes$rango <<- i$rango
  }
  else
  {
    ajustes$rango <<- c(ymd(i$año*10000 + 101), ymd(i$año*10000 + 1231))
  }    
  ajustes$agrupacion <<- i$agrupacion
}

datos_clima_consumo_cargados <- TRUE

}