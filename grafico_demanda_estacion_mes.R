source("grafico_demanda_estacion.R", encoding = "UTF-8")

grafico1 <- grafico + labs(title = "A")
grafico1.titulo <- grafico.titulo
grafico1.descripcion <- grafico.descripcion
grafico1.observacion <- grafico.observacion

source("grafico_demanda_mes.R", encoding = "UTF-8")

grafico2 <- grafico + labs(title = "B")
grafico2.titulo <- grafico.titulo
grafico2.descripcion <- grafico.descripcion
grafico2.observacion <- grafico.observacion

grafico.titulo <- "Grafico de Demanda según estaciones y meses"

grafico.descripcion <- paste("<p>A.", grafico1.descripcion, "</p>",
                             "<p>B.", grafico2.descripcion, "</p>")

grafico.observacion <- paste("<p>A.", grafico1.observacion, "</p>",
                             "<p>B.", grafico2.observacion, "</p>",
                             "<br>",
                             "<p>...</p>")


grafico <- plot_grid(grafico1, grafico2, nrow=1, rel_heights = c(1,1))