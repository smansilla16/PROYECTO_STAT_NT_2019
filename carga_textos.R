if(!exists("textos_cargados") || !textos_cargados)
{
  
  library(readr)

  intro_texto <- read_file("Introduccion.txt")


  textos_cargados <- TRUE

}