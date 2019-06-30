source("carga_datos_clima_consumo.R", encoding = "UTF-8")

grafico.titulo <- "¿Cómo se comporta la producción respecto al mes del año y por fuente?"

grafico.descripcion <- paste("Gráfico de barras de producción energética promedio según el mes, separado por fuente,",
                             "para el período comprendido entre el", ajustes$rango[1],
                             "y el", ajustes$rango[2],".")

grafico.observacion <- paste("Se observa un comportamiento cíclico en la producción energética de fuente hidráulica con máximo en primavera y mínimo en verano, 
posiblemente asociado con la frecuencia de lluvia en ambas estaciones, mientras que la térmica resulta coplementaria. La energía de fuente fotovoltaica, tiene un 
mínimo en julio. En esa época, los días son más cortos y la intensidad lumínica promedio es menor. Por su parte la
producción por las demás fuentes parece permanecer constante, e independiente del mes del año.")

grafico <- 
  consumoEE.datos3 %>% 
  
  group_by(month = floor_date(Fecha, agrupacion.texto2text(ajustes$agrupacion)), Fuente) %>%
  
  filter(Producción!=0, Fecha >= ajustes$rango[1] & Fecha <= ajustes$rango[2]) %>%
  
  summarise(Producción = mean(Producción),
            temp_c=mean(temp_c)) %>% 
  group_by(Mes = month(month, label = TRUE, abbr = FALSE), Fuente)%>%
  summarise(Producción = mean(Producción),
            temp_c=mean(temp_c))%>%
  ggplot(aes(x = Mes,
             y = Producción,
             fill = Fuente)) +
  geom_bar(stat = "identity",show.legend = FALSE)+theme_gray()+
  facet_wrap(~Fuente,scales = "free")+
  theme(axis.text.x = element_text(angle = 90,hjust=0.95,vjust=0.2))+
  
  labs(x=paste("Temperatura media", ajustes$agrupacion,"(ºC)"),
       y=paste("Producción energética media", ajustes$agrupacion ,"(GWh)"),
       color="Fuente") 
