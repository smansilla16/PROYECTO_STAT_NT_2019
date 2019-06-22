if(!exists("textos_cargados"))
{
  
  library(readr)

  intro_texto <- read_file("Introduccion.txt")

  #referencias <- read_file("Referencias.txt")

  textos_cargados <- TRUE

}