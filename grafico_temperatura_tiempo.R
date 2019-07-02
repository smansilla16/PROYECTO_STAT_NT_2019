source("carga_datos_clima_consumo.R", encoding = "UTF-8")

grafico.titulo <- "¿Hay alguna tendencia en la temperatura con el paso de los años?"

grafico.descripcion <- paste("Gráfico de líneas de la temperatura media mensual en cada año,",
                             "para el período comprendido entre el", ajustes$rango[1],
                             "y el", ajustes$rango[2], ". Los años quedan representados por colores",
                             "del rojo al verde (", year(rango.min), " - ",year(rango.max), "). Se muestran entre las
                              líneas horizontales de máxima (rojo), media (negro) y mínima (azul)
                              temperatura media histórica diaria.")

grafico.observacion <- paste("Podemos observar la relación clara entre el mes del año y la temperatura
                             media mensual. Por otra parte, es posible distinguir diferencias muy leves acerca
                             de una relación entre la temperatura y el año. Algunos de los años más recientes (verde) 
                             tienen asociada una temperatura mayor y están por encima de los primeros años (rojo).
                             En fuentes online, donde los registros contienen un lapso temporal mayor, es posible ver estas diferencias
                             si uno además tiene la temperatura media global, por ejemplo en BBC News artículo sobre Calentamieto global
                             al 2018 y proyecciones. @ref.noticia.cambiocli")

grafico <-   
  clima.datos %>% select(Fecha, temp_c) %>%

  filter(Fecha <= ajustes$rango[2] & Fecha >= ajustes$rango[1]) %>%

  group_by(MesAño=floor_date(Fecha, "month"))  %>% 

  summarize(TempMedia=mean(temp_c)) %>%

  group_by(Mes=(month(MesAño, label = TRUE, abbr= FALSE)),
           Año=(year(MesAño))) %>%

  ggplot(aes(x=Mes,
             y=TempMedia,
             color=Año,
             group=Año)) +

  geom_line(size=1) + 
  
  #scale_color_manual(values = rango.color.año) +

  scale_color_gradient(low="red", high="green",
                       breaks= year(rango.min):year(rango.max),
                       limits = c(year(rango.min), year(rango.max))) +
  scale_y_continuous(limits = c(0,30)) +

  geom_hline(yintercept=mean(consumoEE.datos3$temp_c), linetype="dashed", color = "black") +
  geom_hline(yintercept=min(consumoEE.datos3$temp_c), linetype="dashed", color = "blue") +
  geom_hline(yintercept=max(consumoEE.datos3$temp_c), linetype="dashed", color = "red") +

  labs(x="Mes",
       y=paste("Temperatura media mensual (ºC)"),
       color="Año",
       title = ifelse(year(ajustes$rango[1]) < year(ajustes$rango[2]),
                      paste(year(ajustes$rango[1]), "-", year(ajustes$rango[2])),
                      year(ajustes$rango[1]))) +
  
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        legend.key.width = unit(1, "lines"),
        legend.key.height = unit(2.5, "lines"),
        plot.title = element_text(hjust = 0.5))
