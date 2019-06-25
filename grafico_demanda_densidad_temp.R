source("carga_datos_clima_consumo.R", encoding = "UTF-8")

grafico.titulo <- "Gráfico de densidad de Demanda - Temperatura"

grafico.descripcion <- paste("Gráfico de densidad de la demanda según la temperatura,",
                             "para el período comprendido entre el", ajustes$rango[1],
                             "y el", ajustes$rango[2])

grafico.observacion <- paste("Se observa que las tres temperaturas: máxima, media y mínima presentan un comportamiento
                             parabólico, las tres con distinto mínimo, pero promediando los 18-19 ºc")

grafico <- consumoEE.datos2 %>%
  
  gather("Temperatura", "valor", temp_c, MAX, MIN) %>%
  
  ggplot(aes(x = valor, y = Demanda, colour = Temperatura)) +

  geom_smooth() +
  
  scale_color_manual(values = c(temp_c = "#00BA38",
                                MAX = "#F8766D",
                                MIN = "#619CFF"),

                     labels = c(temp_c = "Media",
                                MAX = "Máxima",
                                MIN = "Mínima")) +

  labs(x = "Temperatura (ºC)",
       y = "Demanda de energia / 100 (GWh)")


# los primeros 3 colores de ggplot predeterminados
# serían los siguientes
#
# library(scales)
#
# hue_pal()(3)
#

