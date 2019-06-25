source("carga_datos_clima_consumo.R", encoding = "UTF-8")

grafico.titulo <- "Gráfico de densidad de Demanda - Temperatura"

grafico.descripcion <- paste("Grafico de densidad de la demanda según la temperatura,",
                             "para el período comprendido entre el", ajustes$rango[1],
                             "y el", ajustes$rango[2])

grafico.observacion <- paste("Se observa que...")

grafico <- consumoEE.datos2 %>%
  
  gather("Temperatura", "valor", temp_c, MAX, MIN) %>%
  
  ggplot(aes(x = valor, y = Demanda, colour = Temperatura)) +

  geom_smooth() +
  
  scale_color_manual(values = c(temp_c = "#00BA38",
                                MAX = "#F8766D",
                                MIN = "#619CFF"),

                     labels = c(temp_c = "promedio",
                                MAX = "maxima",
                                MIN = "minima")) +

  labs(x = "Temperatura (ºC)",
       y = "Demanda de energia (GWh)")


# los primeros 3 colores de ggplot predeterminados
# serían los siguientes
#
# library(scales)
#
# hue_pal()(3)
#

