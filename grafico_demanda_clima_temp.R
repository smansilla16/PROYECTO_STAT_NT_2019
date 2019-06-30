source("grafico_demanda_clima.R", encoding = "UTF-8")

grafico1 <- grafico + labs(title = "A")
grafico1.titulo <- grafico.titulo
grafico1.descripcion <- grafico.descripcion
grafico1.observacion <- grafico.observacion

source("grafico_demanda_temp.R", encoding = "UTF-8")

grafico2 <- grafico + labs(title = "B")
grafico2.titulo <- grafico.titulo
grafico2.descripcion <- grafico.descripcion
grafico2.observacion <- grafico.observacion

grafico.titulo <- "¿Existe una relación entre la demanda y la temperatura media?"

grafico.descripcion <- paste("A.", grafico1.descripcion, "",
                             "B.", grafico2.descripcion, "")

grafico.observacion <- paste("<p>A.", grafico1.observacion, "</p>",
                             "<p>B.", grafico2.observacion, "</p>",
                             "<br>",
                             "<p>...</p>")


grafico <- plot_grid(grafico1, grafico2, rel_widths=c(1,1.4))
