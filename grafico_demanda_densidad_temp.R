source("carga_datos_clima_consumo.R")

grafico.titulo <- "Grafico de densidad de Demanda - Tepmeratura"

grafico.descripcion <- paste("Grafico de densidad de la demanda según la temperatura,",
                             "para el período comprendido entre el", ajustes$rango[1],
                             "y el", ajustes$rango[2])

grafico.observacion <- paste("Se observa que...")

grafico <- consumoEE.datos2 %>%

  ggplot() +

  geom_smooth(aes(x = temp_c, y = Demanda)) +

  geom_smooth(aes(x = MAX, y = Demanda)) +

  geom_smooth(aes(x = MIN, y = Demanda)) +

  labs(x = "Temperatura en celsius", y = "Demanda de energia")
