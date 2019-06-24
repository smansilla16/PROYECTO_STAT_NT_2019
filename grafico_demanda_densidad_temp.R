source("carga_datos_clima_consumo.R", encoding = "UTF-8")

grafico.titulo <- "Gráfico de densidad de Demanda - Temperatura"

grafico.descripcion <- paste("Grafico de densidad de la demanda según la temperatura,",
                             "para el período comprendido entre el", ajustes$rango[1],
                             "y el", ajustes$rango[2])

grafico.observacion <- paste("Se observa que...")

grafico <- consumoEE.datos2 %>%

  ggplot() +

  geom_smooth(aes(x = temp_c, y = Demanda,color="black")) +

  geom_smooth(aes(x = MAX, y = Demanda,color="red")) +

  geom_smooth(aes(x = MIN, y = Demanda,color="blue")) +

  labs(x = "Temperatura (ºC)",
       y = "Demanda de energia (GWh)")
