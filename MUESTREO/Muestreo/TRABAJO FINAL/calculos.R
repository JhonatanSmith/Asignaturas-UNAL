std_2019 = read.csv2(file.choose())

std_2020 = read.csv2(file.choose())

attach(std_2019)

attach(std_2020)

datos_2019 = data.frame(std_2019)

datos_2019$PADRE <- NULL
datos_2019$MADRE <- NULL
datos_2019$NOMBRE.COMPLETO <- NULL
datos_2019$ACUDIENTE <- NULL

datos_2020 = data.frame(std_2020)

datos_2020$PADRE <- NULL
datos_2020$MADRE <- NULL
datos_2020$NOMBRE.COMPLETO <- NULL
datos_2020$ACUDIENTE <- NULL



# Media muestral año 2019: 

# Se transforma punto a coma para que lo detecte como numero

prom_2019 = mean(datos_2019$PROMEDIO.ACUMULADO)

varianza_2019_prom= sd(datos_2019$PROMEDIO.ACUMULADO)


# Media muestral año 2020: 

# Se transforma punto a coma para que lo detecte como numero

prom_2020 = mean(datos_2020$PROMEDIO.ACUMULADO)

varianza_2020_prom= var(datos_2020$PROMEDIO.ACUMULADO)





