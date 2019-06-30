source("carga_datos_clima_consumo.R", encoding = "UTF-8")

grafico.titulo <- "Gráfico de Temperatura vs. Tiempo"

grafico.descripcion <- paste("Grafico de caja para la temperatura respecto al tiempo",
                             "para el período comprendido entre el", ajustes$rango[1],
                             "y el", ajustes$rango[2], ". Se muestran, respecto a la temperatura
                             media histórica diaria, la máxima (rojo), mediana (negro) y mínima (azul) temperatura.")

grafico.observacion <- paste("No podemos distinguir una relación entre la temperatura y el año. si tomamos los 
                            últimos 10 años completos del registro (2007-2017), 6/10 años, han tenido una mediana 
                             de temperatura mayor a la mediana de la temperatura histórica. De todas maneras esto no
                             permite sacar ningún tipo de conclusión acerca de cambio climático en Uruguay.")

grafico <-   
  clima.datos %>% select(Fecha, temp_c) %>%
  filter(Fecha <= ajustes$rango[2]) %>%
  ggplot(aes(x=format(Fecha,'%Y'),y=temp_c)) +           
  geom_boxplot() +
  geom_hline(yintercept=median(consumoEE.datos3$temp_c), linetype="dashed", color = "black") +
  geom_hline(yintercept=min(consumoEE.datos3$temp_c), linetype="dashed", color = "blue") +
  geom_hline(yintercept=max(consumoEE.datos3$temp_c), linetype="dashed", color = "red") +
  labs(x="Año",y="Temperatura media anual (ºC)") + 
  theme(axis.text.x = element_text(angle = 90,vjust = 0.5))


