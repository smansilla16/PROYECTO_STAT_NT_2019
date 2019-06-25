source("carga_datos_clima_consumo.R", encoding = "UTF-8")

grafico.titulo <- "Gráfico de Temperatura vs. Tiempo"

grafico.descripcion <- paste("Grafico de barras para la temperatura respecto al tiempo",
                             "para el período comprendido entre el", ajustes$rango[1],
                             "y el", ajustes$rango[2], ". Los datos se encuentran agrupados",
                             "por fechas de forma", ajustes$agrupacion,".")

grafico.observacion <- paste("OBSERVACIONES")

grafico <-   consumoEE.datos3 %>% select(Fecha, temp_c) %>%

  filter(Fecha >= ajustes$rango[1] & Fecha <= ajustes$rango[2]) %>%

  group_by(month=floor_date(Fecha, agrupacion.texto2text(ajustes$agrupacion))) %>%

  summarize(TempMedia=mean(temp_c)) %>% 

  ggplot(aes(x=month,y=TempMedia)) + 

  geom_col() + geom_smooth() +

  labs(x="Año",
       y=paste("Temperatura media", ajustes$agrupacion ,"(ºC)"))
