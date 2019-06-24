source("carga_datos_clima_consumo.R")

grafico.titulo <- "Grafico de Demanda y Producción energética"

grafico.descripcion <- paste("Grafico de puntos de la demanda y la producción totales",
                             "para el período comprendido entre el", ajustes$rango[1],
                             "y el", ajustes$rango[2], ". Los datos se encuentran agrupados",
                             "por fechas de forma", ajustes$agrupacion, ". La demanda y producción",
                             "son expresadas en cientos de GWh.")

# En la descripción anterior de repente se podría ajustar las unidades
# según el período seleccionado

escala = 100

grafico.observacion <- paste("En 2004, al inicio del intervalo para el cuál se tiene registro, la 
                             producción total estaba por debajo de la demanda, mientras que en la 
                             actualidad, esto se ha invertido. El punto de corte, o sea el momento
                             en que Uruguay empezó a producir el 100% de lo que consume fue en 2012
                             y de allí en adelante contamos con sobrante de producción, pudiendo
                             éste quedar destinado para la exportación.")

grafico <- consumoEE.datos3 %>% select(Fecha, Demanda, `Producción Total`) %>%

  filter(Fecha >= ajustes$rango[1] & Fecha <= ajustes$rango[2]) %>% 

  gather(key="Clase", value="Energía", Demanda, `Producción Total`) %>%

  group_by(month=floor_date(Fecha, agrupacion.texto2text(ajustes$agrupacion)), Clase) %>%

  summarize(EnergíaTotal=sum(Energía)/escala) %>% 

  ggplot(aes(x=month,y=EnergíaTotal,color=Clase)) + 

  geom_point() + geom_smooth(method="lm",se = FALSE) +

  labs(x="Fecha",
       y=paste("Energía total", ajustes$agrupacion ,"(CGWh)")) + 
  
  theme(aspect.ratio = 1)
