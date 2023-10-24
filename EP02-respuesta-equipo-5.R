# Integrantes
# Aracely Castro
# Paolo Demarchi
# Gustavo Alcántara

# Equipo: 5

# Librerías utilizadas
library(dplyr)
library(ggpubr)

# Cargando CSV
datos <- read.csv2("EP02 Datos.csv")

#### Pregunta 1 ####

# El Comité Olímpico cree que el mejor tiempo medio de los atletas de oriental
# después de ingresar al programa de entrenamiento es superior a 14,9 segundos.
# ¿Soportan los datos esta afirmación?

# Como se debe de analizar el tiempo medio posterior, se pueden utilizar las
# pruebas z y t para resolver el problema, no obstante, debido a que no se
# cumple que las observaciones sean superiores o iguales a 30 (≥ 30) y no se
# conoce la varianza de la población, se descarta utilizar la prueba z y
# se opta por la prueba t.

# Entonces, para poder responder a la pregunta, primero se filtran los datos
# dejando solamente los orientales.

orientales <- datos %>% filter(Raza == "Oriental")

# por contexto, son independientes las observaciones, luego se pasa a estudiar
# si la variable Posterior de la muestra con solo orientales sigue una
# distribución normal.

# Normalidad
# Usando prueba de contraste
normalidada <- shapiro.test(orientales$Posterior)
print(normalidada)

# Usando gráfico QQ-plot
g <- ggqqplot(orientales, x = "Posterior", color = "red",
              title = "Normalidad Orientales Posterior al Programa")
print(g)

# con los requisitos para realizar t-test cumplidos, se procede a plantear las
# hipótesis:

# H0: El tiempo medio de los atletas después de ingresar al programa es igual
#     a 14.9 segundos.
# Ha: El tiempo medio de los atletas después de ingresar al programa es superior
#     a 14.9 segundos.

# De forma matemática
# H0: u = 14.9
# Ha: u > 14.9

# Finalmente, se realiza la prueba t con un nivel de confianza del 95%.
ttest <- t.test(orientales$Posterior, alternative = "greater", mu = 14.9,
                conf.level = 0.95)
ttest

# Con un intervalo de confianza del 14.37084 al infinito positivo, un
# p-value = 0.8762 (superior a 0.05) y con un 95% de confianza se falla
# en rechazar la hipótesis nula de que el tiempo medio de los atletas
# orientales después del entrenamiento es igual a 14.9.

# En este caso los datos parecen respaldar la afirmación de que el
# mejor tiempo medio de los atletas de oriental después de ingresar
# al programa de entrenamiento es superior a 14,9 segundos.


#### Pregunta 2 ####
# ¿Sugieren los datos que la mejor marca de los atletas de raza negra se
# reduce en promedio menos de 1,3 segundos tras el entrenamiento.

# Como se debe de analizar la mejor marca promedio de los atletas de raza
# negra, entendiendo como mejor marca la diferencia entre el tiempo previo
# y posterior de cada atleta, se opta por utilizar la prueba t para dos
# muestras pareadas.

# Entonces, para responder a la pregunta, primero se filtran los datos,
# dejando solo a los de raza negra.
negros <- datos %>% filter(Raza == "Negra")

# Luego se calcula las diferencias entre el previo y posterior de los
# datos filtrados.
diferencia <- negros$Previo - negros$Posterior

# Por contexto, los datos están pareados. Luego se va a estudiar si la
# diferencia cumple que sigue una distribución normal.

# Normalidad
# Usando prueba de contraste
normalidadb <- shapiro.test(diferencia)
print(normalidadb)

# Usando gráfico QQ-plot
g <- ggqqplot(data.frame(diferencia), x = "diferencia", color = "red",
              title = "Normalidad mejora Marcas Negros")
print(g)

# Con los requisitos para realizar t-test cumplidos, se procede a plantear
# las hipótesis:

# H0: El tiempo medio en que se reduce las marcas de los atletas negros
#     es igual a 1.3 segundos después del programa.
# Ha: El tiempo medio en que se reduce las marcas de los atletas negros
#     es menor a 1.3 segundos después del programa.

# De forma matemática
# H0: a-b = 1.3
# Ha: a-b < 1.3

# Finalmente, se aplica la prueba t con un nivel de confianza del 95%.
ttest2 <- t.test(diferencia, alternative = "less", mu = 1.3,
                 conf.level = 0.95)
ttest2

# Con un intervalo de confianza del infinito negativo a 1.7, un
# p-value= 0.9989 (superior a 0.05) y con un 95% de confianza se falla
# en rechazar la hipótesis nula donde el tiempo medio en el que se
# reduce las marcas de los atletas negros es igual a 1.3 segundos
# después del programa.

# En este caso los datos no parecen sugerir que la mejor marca de
# los atletas de raza negra se reduce en promedio menos de 1.3 segundos
# tras el entrenamiento.


#### Pregunta 3 ####
# ¿Es posible afirmar que, en promedio, los atletas de raza negra superaban
# a los de raza oriental por más de 5,8 segundos antes del entrenamiento?

# Como se necesita analizar el promedio, los datos de los de raza oriental
# y negra son dos muestras independientes, por lo tanto, se utilizará la
# prueba t para dos muestras independientes.

# Como ya se filtraron a los orientales y negros anteriormente, lo siguiente
# es estudiar si la variable Previo de ambos sigue una distribución normal.

# Normalidad
# Usando prueba de contraste
normalidador <- shapiro.test(orientales$Previo)
normalidador

normalidadne <- shapiro.test(negros$Previo)
normalidadne

# Usando gráfico QQ-plot
g1 <- ggqqplot(orientales, x = "Previo", color = "red")
g2 <- ggqqplot(negros, x = "Previo", color = "blue")

ggarrange(g1, g2, nrow = 2,
          labels = c("Normalidad Orientales previo al Programa",
                     "Normalidad Negros previo al Programa"))

# con los requisitos para hacer el t-test para dos muestras independientes
# cumplidos, se procede a plantear las hipótesis:

# H0: los atletas de raza negra superaban a los orientales con una
#     diferencia igual a 5.8 segundos.
# Ha: los atletas de raza negra superaban a los orientales con una
#     diferencia mayor a 5.8 segundos.

# De forma matemática
# H0: u = 5.8
# Ha: u > 5.8

# Finalmente, se aplica la prueba t para dos muestras independientes
# con un nivel de confianza del 95%.
ttest3 <- t.test(x = orientales$Previo, y = negros$Previo,
                 alternative = "greater",
                 mu = 5.8, conf.level = 0.95, paired = FALSE, var.equal = TRUE)
ttest3

# Con un intervalo de confianza desde el infinito negativo hasta 5.088811,
# un p-value = 0.8351 (mayor a 0.05) y con un nivel de confianza del 95%,
# se falla en rechazar la hipótesis nula de que los atletas de raza negra
# superaban a los orientales con una diferencia igual a 5.8 segundos.


# Los datos parecer afirmar que, en promedio, los atletas de raza negra
# superaban a los de raza oriental por más de 5,8 segundos antes del
# entrenamiento.


#### Pregunta 4 ####
# ¿Será cierto que hay más atletas de raza oriental que, en promedio, redujeron
# sus marcas en al menos 4,8 segundos que atletas de raza blanca que lo hicieron
# en al menos 3,2 segundos?

# Para dar respuesta a esta pregunta, primero se realizará una prueba t en cada
# una de las muestras con el fin de analizar las medias. Para esto se deberán
# corroborar que las mejoras de los tiempos siguen una distribución cercana a
# la normal.

# Primer conjunto: Orientales

# Como los datos ya están filtrados de una pregunta anterior, se procede a
# calcular la diferencia antes de comprobar la normalidad.

diferenciaor <- orientales$Previo - orientales$Posterior

# Normalidad
# Usando prueba de contraste
normalidador2 <- shapiro.test(diferenciaor)
normalidador2

# Usando gráfico QQ-plot
g <- ggqqplot(data.frame(diferenciaor), x = "diferenciaor", color = "red",
              title = "Normalidad mejora Marca Orientales")
print(g)

# Como se cumplen las condiciones para t-test, se proceden a plantear las
# hipótesis:

# H0: La media de reducción de tiempo es igual 4.8 segundos en raza oriental.
# Ha: La media de reducción de tiempo es al menos de 4.8 segundos en raza
#     oriental.

# De forma matemática
# H0: u = 4.8
# Ha: u > 4.8

# Finalmente, se aplica la prueba t con un nivel de confianza del 95%.
ttest4a <- t.test(diferenciaor, mu = 4.8, alternative = "greater",
                  conf.level = 0.95)
ttest4a

# Con un intervalo de confianza del 4.856744 hasta el infinito positivo,
# un p-value = 0.007804 (menor a 0.05) y con un nivel de confianza del 95%
# se rechaza la hipótesis nula en favor de la alternativa, donde la media de
# reducción de tiempo es al menos de 4.8 segundos en raza oriental.

# Segundo conjunto: Blancos

# Se filtran los datos y se procede a calcular la diferencia antes de comprobar
# la normalidad.
blancos <- datos %>% filter(Raza == "Blanca")

diferenciabl <- blancos$Previo - blancos$Posterior

# Normalidad
# Usando prueba de contraste
normalidadbl <- shapiro.test(diferenciabl)
normalidadbl

# Usando gráfico QQ-plot
g <- ggqqplot(data.frame(diferenciabl), x = "diferenciabl", color = "red",
              title = "Normalidad mejora Marca Blancos")
print(g)

# Como se cumplen las condiciones para el t.test, se procede a plantear las
# hipótesis:

# H0: la media de reducción de tiempo es igual 3.2 segundos en raza blanca.
# Ha: la media de reducción de tiempo es al menos de 3.2 segundos en raza blanca.

# De forma matemática
# H0: u = 3.2
# Ha: u > 3.2

# Finalmente, se aplica la prueba t con un nivel de confianza del 95%.
ttest4b <- t.test(diferenciabl, mu = 3.2, alternative = "greater",
                  conf.level = 0.95)
ttest4b

# Con un intervalo de confianza del 2.929334 al infinito positivo,
# un p-value = 0.8852 (mayor a 0.05) y con un nivel de confianza del 95%,
# se falla en rechazar la hipótesis nula de que la media de reducción de tiempo
# es igual a 3.2 segundos en raza blanca.

# El resultado de los test de medias nos indican que la media de mejora de
# tiempo en los atletas de raza oriental es mayor que 4.8, mientras que la
# media de mejora en los atletas blancos es igual a 3.2.

# Si bien se podría concluir con esa información que debiesen ser mayor la
# cantidad de orientales en mejorar sus marcas que de blancos con sus
# respectivos puntos de referencia, se realizará una prueba de proporciones
# para darle mayor peso a la conclusión.

# Para esta prueba, sus condiciones ya han sido demostradas anteriormente
# (normalidad e independencia), por lo que planteamos las hipótesis:

# H0: La cantidad de orientales en mejorar más de 4.8 segundos es igual que la
#     cantidad de blancos en mejorar 3.2 segundos.
# Ha: La cantidad de orientales en mejorar más de 4.8 segundos es mayor que la
#     cantidad de blancos en mejorar 3.2 segundos.

# De forma matemática
# H0: p1 - p2 = 0
# Ha: p1 - p2 > 0

# Preparación de datos para prop.test
orientalesnuevo <- cbind(orientales, diferenciaor)
blancosnuevo <- cbind(blancos, diferenciabl)
exitosor <- orientalesnuevo %>% filter(diferenciaor >= 4.8)
exitosbl <- blancosnuevo %>% filter(diferenciabl >= 3.2)
nexitosor <- nrow(exitosor)
nexitosbl <- nrow(exitosbl)
norientales <- nrow(orientales)
nblancos <- nrow(blancos)

# éxitos
exitos <- c(nexitosor, nexitosbl)

# cantidad de orientales y blancos
cant_or_bl <- c(norientales, nblancos)

# Finalmente, se aplica la prueba de Wilson
proporciones <- prop.test(exitos, n = cant_or_bl, conf.level = 0.95,
                          alternative = "greater")
proporciones

# Con un intervalo de confianza del 0.1507536 hasta 1, un p-value = 0.004551
# (menor a 0.05) y con un nivel de confianza del 95%, se rechaza la hipótesis
# nula en favor de la alternativa, donde la cantidad de atletas orientales que
# mejoraron su marca en más de 4.8 segundos es mayor que la cantidad de atletas
# blancos que mejoraron su marca en 3.2 segundos.

# En este caso, parece cierto que hay más atletas de raza oriental que, en
# promedio, redujeron sus marcas en al menos 4,8 segundos que atletas de raza
# blanca que lo hicieron en al menos 3,2 segundos.
