# Función para realizar el contraste de Mann-Whitney sin librerías

mann_whitney_test <- function(datos, variable, grupo) {
  
  # Separar los datos de cada grupo
  valores <- datos[[variable]]
  grupos <- datos[[grupo]]
  
  # Combina valores y grupos en un solo data frame
  datos_unidos <- data.frame(valores, grupos)
  
  # Calcular los rangos de los datos combinados
  datos_unidos$rangos <- rank(datos_unidos$valores)
  
  # Obtener los nombres únicos de los grupos
  grupos_unicos <- unique(grupos)
  
  # Separar los rangos por grupo usando los nombres únicos
  rangos_grupo1 <- datos_unidos$rangos[datos_unidos$grupos == grupos_unicos[1]]
  rangos_grupo2 <- datos_unidos$rangos[datos_unidos$grupos == grupos_unicos[2]]
  
  # Calcular las sumas de los rangos por grupo
  R1 <- sum(rangos_grupo1)
  R2 <- sum(rangos_grupo2)
  
  # Tamaños muestrales
  n1 <- length(rangos_grupo1)
  n2 <- length(rangos_grupo2)
  
  # Calcular U para el grupo 1
  U1 <- (n1 * n2) + ((n1 * (n1 + 1)) / 2) - R1
  
  # Calcular U para el grupo 2
  U2 <- n1 * n2 + (n2 * (n2 + 1)) / 2 - R2
  
  # Escoger el menor U
  U <- min(U1, U2)
  
  # Aproximación a la distribución normal para tamaños muestrales grandes
  if (n1 >= 10 & n2 >= 10) {
    # Media y varianza de U
    mu_U <- (n1 * n2) / 2
    sigma_U <- sqrt((n1 * n2 * (n1 + n2 + 1)) / 12)
    
    # Estadístico Z
    Z <- (U - mu_U) / sigma_U
    p_valor <- 2 * pnorm(-abs(Z)) # Prueba a dos colas
  } else {
    Z <- NA
    p_valor <- NA
  }
  
  # Devolver resultados
  return(list(
    "U" = U,
    "Rango_Grupo1" = R1,
    "Rango_Grupo2" = R2,
    "Z" = Z,
    "p_valor" = p_valor
  ))
}

# Función para realizar el contraste de Mann-Whitney usando librerías
# A pesar de que se usa wilcox.tex como los datos son de grupos independientes 
# se asume automáticamente que se trata de la prueba de Mann-Whitney.
# Al no especificar paired (datos pareados) se sabe que es una prueba M-W

mann_whitney_test_con_librerias <- function(datos, variable, grupo) {
  
  # Ejecutar la prueba usando wilcox.test
  resultado <- wilcox.test(as.formula(paste(variable, "~", grupo)), data = datos, exact = FALSE, correct = FALSE)
  
  # Extraer el estadístico U y el p-valor
  U <- resultado$statistic
  p_valor <- resultado$p.value
  
  # Devolver los resultados
  return(list("U" = U, "p_valor" = p_valor))
}


