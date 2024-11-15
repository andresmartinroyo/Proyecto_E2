#Funcion K-W sin el uso de librerias
#datos: Es el dataframe completo
#variable: Es el nombre de la variable independiente
#grupo: Es la variable que quiero explicar
kruskal_wallis_test <- function(datos, variable, grupo) {
  
  #Extraigo los de datos ambas columnas. La columna de varibales y la de grupos
  valores <- datos[[variable]]
  grupos <- datos[[grupo]]
  
  # Combina valores y grupos en un solo data frame
  datos_unidos <- data.frame(valores, grupos)
  
  # Creo una columna nueva llamada rangos. Esta columna se armara en funcion de ranks.
  #la cual segun la documentación, me deberia de devolver el ranking de cada uno de los datos.
  #asignandole el mismo rango a variables con el mismo valor.
  datos_unidos$rangos <- rank(datos_unidos$valores)
  
  #tapply es una manera deejecutar funciones a subgrupos. Toma a los rangos y os agrupa en funcion de los 
  #difrentes subgrupos dentro de la columna definida. A todos esos les aplica una funcion
  
  #En este caso aplico tapply para obtener la suma de los rangos por grupos
  suma_rangos <- tapply(datos_unidos$rangos, datos_unidos$grupos, sum)
  
  #En este caso aplico tapply para obtener la longitud de valores por grupos
  n_por_grupo <- tapply(datos_unidos$valores, datos_unidos$grupos, length)
  
  # Número total de observaciones
  N <- length(valores)
  
  #Aqui calculo K, para que se puedan calular los gl 
  k <- length(unique(grupos))
  gl <- k - 1
  
  
  # Cálculo del estadístico H
  H <- (12 / (N * (N + 1))) * sum((suma_rangos^2) / n_por_grupo) - 3 * (N + 1)
  
  #Me da la la cola derecha
  p_valor <- pchisq(H,gl,lower.tail = FALSE)
  
  # Las listas en r son los diccionarios de pyhon,
  return(list("H" = H, "p_valor" = p_valor))
}

#Funcion K-W con el uso de librerias
#datos: Es el dataframe completo
#variable: Es el nombre de la variable independiente
#grupo: Es la variable que quiero explicar
kruskal_wallis_test_con_librerias <- function(datos, variable, grupo){
  
  # paste: concatena el nombre de la variable y el grupo con la virgulilla.
  # variable ~ grupo: Esto en R define que variable es dependiente de grupo
  #as.formula, castea de string a formula
  #kruskal.test ejecuta la prueba
  resultado <- kruskal.test(as.formula(paste(variable, "~", grupo)), data = datos)
  
  # Extrae el estadístico H y el p-valor del resultado
  H <- resultado$statistic
  p_valor <- resultado$p.value
  
  # Devuelve el valor del estadístico H, los grados de libertad y el p-valor
  return(list("H" = H, "p_valor" = p_valor))
}