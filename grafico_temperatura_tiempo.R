source("carga_datos_clima_consumo.R", encoding = "UTF-8")

grafico.titulo <- "¿Hay alguna tendencia de la temperatura con el paso de los años?"

grafico.descripcion <- paste("Grafico de barras para la temperatura respecto al tiempo",
                             "para el período comprendido entre el", ajustes$rango[1],
                             "y el", ajustes$rango[2], ". Se muestran, respecto a la temperatura
                             media histórica diaria, la máxima (rojo), media (negro) y mínima (azul) temperatura.")

grafico.observacion <- paste("Podemos observar la relación clara entre el mes del año y la temperatura
                             media mensual. Por otra parte, es posible distinguir diferencias muy leves acerca
                             de una relación entre la temperatura y el año. Algunos de los años más recientes (verde) 
                             tienen asociada una temperatura mayor y están por encima de los primeros años (rojo).
                             En fuentes online, donde los registros contienen un lapso temporal mayor, es posible ver estas diferencias
                             si uno además tiene la temperatura media global. Ver: https://www.bbc.com/mundo/noticias-46426822")

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
  #scale_color_brewer(palette = "Reds")+
  #scale_color_manual(values = scales::brewer_pal(palette="Reds")(year(rango.max)-year(rango.min) + 1))+
  scale_color_gradient(low="red", high="green",breaks=as.numeric(names(rango.color.año)),labels=names(rango.color.año),limits=c(year(rango.min),year(rango.max))) +
  scale_y_continuous(limits = c(0,30)) +

  geom_hline(yintercept=mean(consumoEE.datos3$temp_c), linetype="dashed", color = "black") +
  geom_hline(yintercept=min(consumoEE.datos3$temp_c), linetype="dashed", color = "blue") +
  geom_hline(yintercept=max(consumoEE.datos3$temp_c), linetype="dashed", color = "red") +

  labs(x="Mes",y=paste("Temperatura media mensual (ºC)"),color="Año") +
  
  theme(axis.text.x = element_text(angle = 45, hjust=1), legend.key.size = unit(1.5, "cm"))
