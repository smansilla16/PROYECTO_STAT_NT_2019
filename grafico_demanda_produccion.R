source("carga_datos_clima_consumo.R", encoding = "UTF-8")

grafico.titulo <- "¿Cómo se compara la demanda con la producción, y la importación/exportación? 
¿Uruguay ha mejorado su capacidad de autosuficiencia energética?"

grafico.descripcion <- paste("Gráfico de puntos de la demanda y la producción totales",
                             "para el período comprendido entre el", ajustes$rango[1],
                             "y el", ajustes$rango[2], ". Los datos se encuentran agrupados",
                             "por fechas de forma", ajustes$agrupacion, ". La demanda y producción",
                             "son expresadas en GWh.")

# En la descripción anterior de repente se podría ajustar las unidades
# según el período seleccionado

grafico.observacion <- paste("En 2004, al inicio del intervalo para el cuál se tiene registro, la 
                             producción total estaba por debajo de la demanda, mientras que en la 
                             actualidad, esto se ha invertido. El punto de corte, o sea el momento
                             en que Uruguay empezó a producir el 100% de lo que consume fue en el entorno del
                             2012 y de allí en adelante contamos con sobrante de producción, pudiendo
                             éste quedar destinado para la exportación. Al principio del intervalo, cuando 
                             la demanda era mayor a la producción, había mayor importación para compensar. 
                             Aproximadamente, la importación y la exportación se cruzan en el mismo momento de tiempo que 
                             la demanda y la producción.")

grafico <- consumoEE.datos3 %>% select(Fecha, Demanda, `Producción Total`,Importación,`Exportación Total`) %>%

  filter(Fecha >= ajustes$rango[1] & Fecha <= ajustes$rango[2]) %>% 

  gather(key="Clase", value="Energía", Demanda, `Producción Total`,Importación,`Exportación Total`) %>%

  group_by(month=floor_date(Fecha, agrupacion.texto2text(ajustes$agrupacion)), Clase) %>%

  summarize(EnergíaTotal=mean(Energía)) %>% 

  ggplot(aes(x=month,y=EnergíaTotal,color=Clase)) + 

  geom_point(size=2) + geom_line(size=0.7) +

  labs(x="Fecha",
       y=paste("Energía total", ajustes$agrupacion ,"(GWh)")) + 
  
  theme(aspect.ratio = 1/2,axis.text.x = element_text(angle = 45, hjust=1)) + scale_color_brewer(palette="Spectral") + 
  scale_x_date(breaks = scales::date_breaks("1 year"), labels = scales::date_format("%Y"))

#grafico  <- ggplotly(grafico1)
