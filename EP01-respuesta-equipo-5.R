
# INTEGRANTES
# Gustavo Alcántara
# Aracely Castro
# Paolo Demarchi

# GRUPO: 5

# PREGUNTA: ¿Son similares los ingresos registrados en las diferentes provincias de la RM?

# Las variables relevantes para responder la pregunta son: 
# provincia: provincia (categórica y nominal)
# ytot: ingreso total (numérica y discreta)

# Se procedió a cargar el archivo con extensión .csv con la información, almacenándola en un tipo de dato data.frame "datos".
# Luego, se creó una tabla de frecuencias para la variable provincia con el objetivo de observar la cantidad de
# encuestados por cada provincia ("Chacabuco", "Cordillera", "Maipo", "Melipilla", "Santiago", "Talagante"), después se calculó
# el porcentaje de encuestados de Santiago. Finalmente, se graficó un diagrama de cajón con cada una de las provincias para 
# comparar sus ingresos totales (ytot).

# Librerías a utilizar
library(ggpubr)
library(dplyr)

# Carga del archivo
datos <- read.csv2('EP01 Datos Casen 2017.csv')

# Descripción de los datos
print(class(datos))
print(str(datos))

# Tabla de frecuencias
frecuencias <- table(datos$provincia)
print(frecuencias)

# Total de observaciones
ntotal <- sum(frecuencias)

# Porcentaje Santiago
nsantiago <- frecuencias[5]
porcentaje_santiago <- nsantiago / ntotal
porcentaje_santiago

# Gráfico
grafico <- ggboxplot(datos, x = 'provincia', 
                    y = 'ytot', 
                    palette = c('blue','pink', 'yellow','green', 'red', 'light blue'), 
                    fill = 'provincia',
                    ylab = "Ingresos Totales",
                    xlab = "Provincias")

# Mejora de visualización del rango de cada provincia
grafico <- grafico + coord_cartesian(ylim = c(0, 2000000)) 
print(grafico)

# Filtrado para los estadísticos
santiago <- datos %>% filter(provincia == 'Santiago')
chacabuco <- datos %>% filter(provincia == 'Chacabuco')
cordillera <- datos %>% filter(provincia == 'Cordillera')
maipo <- datos %>% filter(provincia == 'Maipo')
melipilla <- datos %>% filter(provincia == 'Melipilla')
talagante <- datos %>% filter(provincia == 'Talagante')

# Resumen general de cada provincia (calculando estadísticos)
summary(santiago$ytot)
summary(chacabuco$ytot)
summary(cordillera$ytot)
summary(maipo$ytot)
summary(melipilla$ytot)
summary(talagante$ytot)

# Se apreció que los ingresos son diferentes entre las provincias y Santiago, esto podría deberse a que el 75.47% de los datos 
# están concentrados en Santiago, también podría deberse a que en Santiago los sueldos resultarian ser más dispares respecto 
# al resto de provincias.

# En el caso de los estadísticos se consideró en un momento utilizar la media para comparar si los ingresos
# de las provincias son similares, pero se descartó, puesto que la cantidad de encuestados de cada provincia
# es muy dispareja, por lo que se prefirió analizar el rango y los cuartiles de los diagramas de cajón.
# A partir de ello se puede observar que los ingresos en el tercer cuartil son mucho mayores en Santiago en comparación
# con otras provincias como también es distinto en el segundo cuartil.

# En conclusión, en una primera instancia los ingresos de las provincias no son similares.