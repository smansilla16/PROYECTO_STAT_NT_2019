source("carga_datos_clima_consumo.R", encoding = "UTF-8")

grafico.pre <- ""

grafico.titulo <- "Gráfico de Demanda - Estación"

grafico.descripcion <- paste("Gráfico de líneas de la demanda según la estación climática en cada año,",
                             "para el período comprendido entre el", ajustes$rango[1],
                             "y el", ajustes$rango[2],
                             ". La demanda en el gráfico se resume como la media",
                             "de la energía eléctrica diaria que se destina al país",
                             "para cada estación del año, y es expresada en GWh.")

grafico.observacion <- paste("Se observa que...")

# la demanda en el gráfico se resume como la media diaria, aunque antes
# estaba como el total de la estación, si fuera el total de la estación
# se podría expresar en cientos de GWh (CGWh) al realizar la suma en
# el período y dividirla entre 100

grafico <- consumoEE.datos3 %>%

  group_by(Estacion = as.POSIXct(floor_date(Fecha, "season") + months(1) + days(15))) %>%

  summarise(Demanda = mean(Demanda)) %>%

  filter(Estacion >= ajustes$rango[1] & Estacion <= ajustes$rango[2]) %>%

  ggplot(aes(x = Estacion,
             y = Demanda,
             ymin = min(Demanda) - 2,
             ymax = max(Demanda) + 2)) +

  geom_line() +

  scale_x_datetime(breaks = scales::date_breaks("1 year"),
                   labels = scales::date_format("%Y")) +
  
  stat_peaks(colour = "blue", geom = "point") +
  stat_peaks(colour = "blue", geom = "text", vjust = -0.3, x.label.fmt = "%m") +
  stat_peaks(geom = "rug",colour = "blue") +
  
  theme(aspect.ratio = 1/3,
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust=1)) +

  labs(x = "Fechas agrupadas según la estación (trimestres)",
       y = "Demanda de energia (GWh)",
       title = "¿Como se comporta la demanda de energia al dividirla por estacion?")
