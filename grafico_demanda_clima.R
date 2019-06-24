source("carga_datos_clima_consumo.R", encoding = "UTF-8")

grafico.titulo <- "Grafico de Demanda según la temperatura"

grafico.descripcion <- paste("Gráfico de la demanda de energía eléctrica según la temperatura,",
                             "para el período comprendido entre el", ajustes$rango[1],
                             "y el", ajustes$rango[2],
                             ". Los datos se encuentran agrupados por fechas de forma",
                             ajustes$agrupacion, ". La demanda y la temperatura son promediadas",
                             "en el rango comprendido por la agrupación.")

grafico.observacion <- paste("Se puede apreciar una tendencia muy interesante: un comportamiento pseudo-parabólico,
                              con concavidad positiva y un mínimo en el entorno de los 18-19 ºC. Esto indicaría,
                             que el consumo de energía eléctrica aumenta a medida que la temperatura media es o 
                             muy baja o muy alta y esto estaría relacionado con el uso de equipamiento para control 
                             de temperatura. Cuando la temperatura es agradable, no existe la necesidad del uso de 
                             estos equipos, por lo que el consumo eléctrico disminuye.")

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