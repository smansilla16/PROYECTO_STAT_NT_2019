source("carga_datos_clima_consumo.R")

grafico.titulo <- "Grafico de Demanda según la temperatura"

grafico.descripcion <- paste("Gráfico de la demanda de energía eléctrica según la temperatura,",
                             "para el período comprendido entre el", ajustes$rango[1],
                             "y el", ajustes$rango[2],
                             ". Los datos se encuentran agrupados por fechas de forma",
                             ajustes$agrupacion, ". La demanda y la temperatura son promediadas",
                             "en el período.")

grafico.observacion <- paste("Se observa que...")

grafico <- consumoEE.datos2 %>%

  filter(Fecha >= ajustes$rango[1] & Fecha <= ajustes$rango[2]) %>% 

  group_by(month = floor_date(Fecha,  agrupacion.texto2text(ajustes$agrupacion))) %>% 

  summarise(DemandaPromedio=mean(Demanda),TempMedia=mean(temp_c)) %>% 

  ggplot(aes(x = TempMedia, y = DemandaPromedio)) +

  geom_point() + geom_smooth(se=FALSE) +

  labs(x=paste("Temperatura media", ajustes$agrupacion,"(ºC)"),
       y=paste("Demanda energética media", ajustes$agrupacion ,"(GWh)"),
       color="Fuente") +

  theme(aspect.ratio = 1)