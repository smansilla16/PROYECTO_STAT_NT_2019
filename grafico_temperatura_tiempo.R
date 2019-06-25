source("carga_datos_clima_consumo.R", encoding = "UTF-8")

grafico.titulo <- "Gráfico de Temperatura vs. Tiempo"

grafico.descripcion <- paste("Grafico de barras para la temperatura respecto al tiempo",
                             "para el período comprendido entre el", ajustes$rango[1],
                             "y el", ajustes$rango[2], ". Se muestran, respecto a la temperatura
                             media histórica diaria, la máxima (rojo), media (negro) y mínima (azul) temperatura.")

grafico.observacion <- paste("Podemos observar la relación clara entre el mes del año y la temperatura
                             media mensual. Por otra parte, no se es capaz de distinguir diferencias acerca
                             de una relación entre la temperatura y el año. No todos los años más recientes
                             tienen asociada una temperatura mayor según la escala de colores. Tampoco podemos
                             apreciar esto para los primeros años del registro. En fuentes online, donde los
                             registros poseen contienen un lapso temporal mayor, es posible ver estas diferencias
                             si uno además tiene la temperatura media global. Ver: https://www.bbc.com/mundo/noticias-46426822")

grafico <-   
  consumoEE.datos3 %>% select(Fecha, temp_c) %>%
  filter(Fecha >= ajustes$rango[1] & Fecha <= ajustes$rango[2]) %>%
  group_by(month=floor_date(Fecha, "month"))  %>% 
  summarize(TempMedia=mean(temp_c)) %>%
  ggplot(aes(x=month(month),y=TempMedia,color=factor(year(month)))) +           
  geom_line() + 
  scale_x_continuous(breaks= 1:12) +
  scale_y_continuous(limits = c(0,30)) +
  geom_hline(yintercept=mean(consumoEE.datos3$temp_c), linetype="dashed", color = "black") +
  geom_hline(yintercept=min(consumoEE.datos3$temp_c), linetype="dashed", color = "blue") +
  geom_hline(yintercept=max(consumoEE.datos3$temp_c), linetype="dashed", color = "red") +
  labs(x="Mes",y=paste("Temperatura media mensual (ºC)"),color="Año")
