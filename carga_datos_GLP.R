if(!exists("datos_GLP_cargados") || !datos_GLP_cargados)
{

library(here)
library(readxl)
library(tidyverse)
glp.2008 <- read_xls(here("GLP/Ventas+GLP+2008.xls"))
glp.2009 <- read_xls(here("GLP/Ventas+GLP+2009.xls"))
glp.2010 <- read_xls(here("GLP/Ventas+GLP+2010.xls"))
glp.2011 <- read_xls(here("GLP/Ventas+GLP+2011.xls"))
glp.2012 <- read_xls(here("GLP/Ventas+GLP+2012.xls"))
glp.2013 <- read_xls(here("GLP/Ventas+GLP+2013.xls"))
glp.2014 <- read_xls(here("GLP/Ventas+GLP+2014+para+publicar+en+la+web.xls"))
glp.2014 <- rename(glp.2014,"...5"="...4","...6"="...5","...7"="...6","...8"="...7")
glp.2015 <- read_xls(here("GLP/Ventas+GLP+2015+para+publicar+en+la+web.xls"))
glp.2015 <- rename(glp.2015,"...5"="...4","...6"="...5","...7"="...6","...8"="...7")
glp.2016 <- read_xls(here("GLP/Ventas+GLP+2016+para+publicar+en+la+web.xls"))
glp.2017 <- read_xls(here("GLP/Ventas_GLP_2017.xls"))
glp <- full_join(glp.2008,glp.2009)
glp <- full_join(glp,glp.2010)
glp <- full_join(glp,glp.2011)
glp <- full_join(glp,glp.2012)
glp <- full_join(glp,glp.2013)
glp <- full_join(glp,glp.2014)
glp <- full_join(glp,glp.2015)
glp <- full_join(glp,glp.2016)
glp <- full_join(glp,glp.2017)

glp <- glp%>%
  select("2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","Cantidad GLP suministrada (kg)","...2","...5","...6","...7")%>%
  cbind(1:212)%>%
  gather(anio,"1:212","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017")%>%
  rename("distribuidor" = "1:212")%>%
  filter(distribuidor=="Acodike" | distribuidor == "DUCSA" | distribuidor == "Megal" | distribuidor == "Riogas")%>%
  rename("3k" = "Cantidad GLP suministrada (kg)","13k" = "...5","45k" = "...6","granel" = "...7","periodo" = "...2")%>%
  gather(tamanio, glp.suministrada, "3k","13k","45k","granel")

rm(glp.2008,glp.2009,glp.2010,glp.2017,glp.2016,glp.2015,glp.2014,glp.2013,glp.2011,glp.2012)

datos_GLP_cargados <- TRUE
}