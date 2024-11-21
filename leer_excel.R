#Este es el paquete que permite leer archivos de tipo Excel
library(readxl)


#Esta funcion permite la lectura del excel de una ruta especifica
leer_excel <- function(ruta_archivo){
  datos <- read_excel(ruta_archivo)
  return(datos)
}
