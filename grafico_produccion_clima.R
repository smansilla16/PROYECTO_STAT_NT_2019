source("carga_datos_clima_consumo.R", encoding = "UTF-8")

grafico.titulo <- "Gráfico de Producción según el Clima"

grafico.descripcion <- paste("Gráfico de puntos de producción promedio según la temperatura,",
                             "para el período comprendido entre el", ajustes$rango[1],
                             "y el", ajustes$rango[2],".")

grafico.observacion <- paste("Se observa una caída en la producción eléctrica de fuente hidráulica a medida que la 
                             temperatura aumenta. Las temperaturas promedio más altas, principalmente se ubican en 
                             las estaciones cálidas y también pueden estar acompañadas por períodos de sequía, por 
                             lo que resulta razonable la disminución de la producción proveniente de esta fuente. 
                             Por otra parte, la producción de otras fuentes parecería mantenerse constante y ser 
                             independiente de la temperatura atmosférica")

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
  geom_bar(stat = "identity")+
  facet_grid(~Fuente)+
  theme(axis.text.x = element_text(angle = 90))+
  
  labs(x=paste("Temperatura media", ajustes$agrupacion,"(ºC)"),
       y=paste("Producción energética media", ajustes$agrupacion ,"(GWh)"),
       color="Fuente") 
