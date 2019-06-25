source("carga_datos_clima_consumo.R", encoding = "UTF-8")

grafico.titulo <- "Gráfico de Demanda - Meses"

grafico.descripcion <- paste("Gráfico de línea de la demanda según el mes del año,",
                             "para el período comprendido entre el", ajustes$rango[1],
                             "y el", ajustes$rango[2],
                             ". La demanda en el gráfico se resume como la media",
                             "de la energía eléctrica diaria que se destina al país",
                             "respecto de cada mes, y es expresada en GWh.")

grafico.observacion <- paste("Notamos que en promedio existe mayor consumo diaro primero entre junio y agosto,",
                             "y luego entre diciembre y febrero, lo cuál se corresponde con los meses más frios 
                             y más cálidos. Esto puede corresponder a la necesidad en el control de temperatura
                             de los hogares.")

dmconsumo <- consumoEE.datos3 %>%
  
  filter(Fecha >= ajustes$rango[1] & Fecha <= ajustes$rango[2]) %>%

  group_by(Mes = month(Fecha, label = TRUE, abbr = FALSE)) %>%
  
  summarise(Demanda = mean(Demanda))

splconsumo <- as.data.frame(spline(dmconsumo$Mes, dmconsumo$Demanda))

grafico <- dmconsumo %>%

  ggplot(aes(x = Mes,
             y = Demanda, group = 1)) +
  
  geom_point() +

  geom_line(data = splconsumo, aes(x = x, y = y), color="blue") +

  theme(aspect.ratio = 1,
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  
  labs(x = "Meses del año",
       y = "Demanda de energia promedio / 100 (GWh)",
       title = "¿Como se comporta la demanda de energia según los meses del año?")

rm(dmconsumo, splconsumo)
