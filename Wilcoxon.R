wilcoxon_test <- function(datos, variable_x, variable_y) {
  
  # Obtener las dos muestras X e Y del data frame
  x <- datos[[variable_x]]
  y <- datos[[variable_y]]
  
  # Verificar que las muestras tengan el mismo tamaño
  if (length(x) != length(y)) {
    stop("Las muestras X e Y deben tener el mismo tamaño para un test pareado.")
  }
  
  # Calcular las diferencias
  diferencias <- x - y
  
  # Excluir diferencias iguales a 0 (no se incluyen en el test)
  diferencias <- diferencias[diferencias != 0]
  
  # Calcular los rangos absolutos de las diferencias
  rangos <- rank(abs(diferencias))
  
  # Asignar los signos de las diferencias a los rangos
  rangos_signed <- rangos * sign(diferencias)
  
  # Calcular la suma de rangos positivos y negativos
  T_plus <- sum(rangos_signed[rangos_signed > 0])
  T_minus <- -sum(rangos_signed[rangos_signed < 0])
  
  # Estadístico de Wilcoxon es el menor entre las dos sumas absolutas
  T <- min(abs(T_plus), abs(T_minus))
  source("~/Documents/GitHub/Proyecto_E2/leer_excel.R")

  # Tamaño muestral (número de diferencias válidas)
  n <- length(diferencias)
  
  # Calcular la media y desviación estándar de T para la aproximación normal
  mu_T <- n * (n + 1) / 4
  sigma_T <- sqrt(n * (n + 1) * (2 * n + 1) / 24)
  
  # Calcular el estadístico Z
  Z <- (T - mu_T) / sigma_T
  
  # Calcular el p-valor para una prueba a dos colas
  p_valor <- 2 * pnorm(-abs(Z))
  
  # Devolver resultados
  return(list(
    "T" = T,
    "T_plus" = T_plus,
    "T_minus" = T_minus,
    "Z" = Z,
    "p_valor" = p_valor
  ))
}
