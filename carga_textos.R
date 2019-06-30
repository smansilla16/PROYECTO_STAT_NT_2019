if(!exists("textos_cargados") || !textos_cargados)
{
  
  library(readr)

  intro_texto <- read_file("Introduccion.txt")
  
  # las siguientes referencias son para la app,
  # estaría mejor si fuese leído desde Referencias.bib
  # como ocurre en el .rmd, pero, por ahora queda así:

  refs.ute <- list(ref = "@ref.ute",
                          url = "http://apps.ute.com.uy/SgePublico/ConsHistCompEnergetica.aspx")

  refs.clima <- list(ref = "@ref.clima",
                          url = "https://www7.ncdc.noaa.gov/CDO/cdoselect.cmd?datasetabbv=GSOD&countryabbv=&georegionabbv=")
  
  refs.2anchor <- function(x, t){
    return(paste0("[<a href='", x, "'>", t, "</a>]"))
  }
  
  refs.traduce <- function(x){
      refs = list(refs.clima, refs.ute)
      i = 1
      for(r in refs)
      {
        x <- gsub(r$ref, refs.2anchor(r$url, i), x)
        i = i+1
      }
      return(x)
  }
  
  textos_cargados <- TRUE

}