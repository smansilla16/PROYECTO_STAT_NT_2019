source("carga_datos_clima_consumo.R")

grafico.titulo <- "Grafico de Producción según el Clima"

grafico.descripcion <- paste("Grafico de puntos de producción promedio según la temperatura,",
                             "para el período comprendido entre el", ajustes$rango[1],
                             "y el", ajustes$rango[2])

grafico.observacion <- paste("Se observa que...")

grafico <- consumoEE.datos3 %>% 

  group_by(month = floor_date(Fecha, agrupacion.texto2text(ajustes$agrupacion)), Fuente) %>%

  filter(Producción!=0, Fecha >= ajustes$rango[1] & Fecha <= ajustes$rango[2]) %>%

  summarise(ProducciónPromedio = mean(Producción),
            TempMedia=mean(temp_c)) %>% 

  ggplot(aes(x = TempMedia,
             y = ProducciónPromedio,
             color = Fuente)) +

  geom_point() + geom_smooth(method="lm",se=FALSE) +

  labs(x=paste("Temperatura media", ajustes$agrupacion,"(ºC)"),
       y=paste("Producción energética media", ajustes$agrupacion ,"(GWh)"),
       color="Fuente") +

  theme(aspect.ratio = 1)
