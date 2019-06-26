source("carga_datos_clima_consumo.R", encoding = "UTF-8")

grafico.titulo <- "Gráfico de Demanda según la temperatura"

grafico.descripcion <- paste("A) Gráfico de la demanda de energía eléctrica según la temperatura,",
                             "para el período comprendido entre el", ajustes$rango[1],
                             "y el", ajustes$rango[2],
                             ". Los datos se encuentran agrupados por fechas de forma",
                             ajustes$agrupacion, ". La demanda y la temperatura son promediadas",
                             "en el rango comprendido por la agrupación.","B) Gráfico de densidad 
                             de la demanda según la temperatura, para el período comprendido entre el", ajustes$rango[1],
                             "y el", ajustes$rango[2])

grafico.observacion <- paste("<p>A. Se puede apreciar una tendencia muy interesante: un comportamiento pseudo-parabólico,
                              con concavidad positiva y un mínimo en el entorno de los 18-19 ºC. Esto indicaría,
                             que el consumo de energía eléctrica aumenta a medida que la temperatura media es o 
                             muy baja o muy alta y esto estaría relacionado con el uso de equipamiento para control 
                             de temperatura. Cuando la temperatura es agradable, no existe la necesidad del uso de 
                             estos equipos, por lo que el consumo eléctrico disminuye.</p>","\n<p>B. Se observa que las 
                             tres temperaturas: máxima, media y mínima presentan un comportamiento
                             parabólico, las tres con distinto mínimo, pero promediando los 18-19 ºc </p>")


grafico1 <- consumoEE.datos2 %>%

  filter(Fecha >= ajustes$rango[1] & Fecha <= ajustes$rango[2]) %>% 

  group_by(month = floor_date(Fecha,  agrupacion.texto2text(ajustes$agrupacion))) %>% 

  summarise(DemandaPromedio=mean(Demanda),TempMedia=mean(temp_c)) %>% 

  ggplot(aes(x = TempMedia, y = DemandaPromedio)) +

  geom_point() + geom_smooth(se=FALSE) +

  labs(x=paste("Temperatura media", ajustes$agrupacion,"(ºC)"),
       y=paste("Demanda energética media", ajustes$agrupacion ,"(GWh)"),
       color="Fuente",title="A") +

  theme(aspect.ratio = 1)

grafico2 <- consumoEE.datos2 %>%
  
  gather("Temperatura", "valor", temp_c, MAX, MIN) %>%
  
  ggplot(aes(x = valor, y = Demanda, colour = Temperatura)) +
  
  geom_smooth() +
  
  scale_color_manual(values = c(temp_c = "#00BA38",
                                MAX = "#F8766D",
                                MIN = "#619CFF"),
                     
                     labels = c(temp_c = "Media",
                                MAX = "Máxima",
                                MIN = "Mínima")) +
  
  labs(x = "Temperatura media (ºC)",
       y = "Demanda de energia (GWh)",title="B") +
  theme(aspect.ratio = 1)

#grafico <- grid.arrange(grafico1,grafico2,nrow=1,widths=c(9,10))
grafico <- plot_grid(grafico1+ theme_grey(),grafico2+ theme_grey(),rel_widths=c(1.5,2))


