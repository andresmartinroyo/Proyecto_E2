# SCRIPT PRINCIPAL

#Leer los scripts
source("Wilcoxon.R")
source("mann-whitney.R")
source("kruskal-wallis.R")
source("leer_excel.R")

#Leer las rutas
ruta_wilcox <- "datos_prueba_wilcoxon.xlsx"
ruta_mw <- "datos_prueba_mann_whitney.xlsx"
ruta_kw <- "datos_prueba_kruskal_wallis.xlsx"

#Leer excel
datosW <- leer_excel(ruta_wilcox)
datosMW <- leer_excel(ruta_mw)
datosKW <- leer_excel(ruta_kw)

#Procesar funciones

#Wilcoxon
wilcox <- wilcoxon_test(datosW, "X", "Y")
wilcox_libr <- wilcox_lib(datosW)

#Mann-Whitney
mann_whitney <- mann_whitney_test(datosMW, "variable", "grupo")
mann_whitney_libr <- mann_whitney_test_con_librerias(datosMW, "variable", "grupo")

#Kruskal-Wallis
kruskal_wallis <- kruskal_wallis_test(datosKW, "peso", "tratamiento")
kruskal_wallis_libr <- kruskal_wallis_test_con_librerias(datosKW, "peso", "tratamiento")


#Mostrar resultados
cat("Resultados de las pruebas:\n")

cat("\nWilcoxon Test (propio):\n")
print(wilcox)

cat("\nWilcoxon Test (librerías):\n")
print(wilcox_libr)

cat("\nMann-Whitney Test (propio):\n")
print(mann_whitney)

cat("\nMann-Whitney Test (librerías):\n")
print(mann_whitney_libr)

cat("\nKruskal-Wallis Test (propio):\n")
print(kruskal_wallis)

cat("\nKruskal-Wallis Test (librerías):\n")
print(kruskal_wallis_libr)

