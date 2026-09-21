############################################################
# TDBD - CLASE 4
# MODELOS DE ELECCIÓN DISCRETA:
# LOGIT BINARIO Y LOGIT MULTINOMIAL
#
# PROPÓSITO DE ESTE SCRIPT
# ----------------------------------------------------------



############################################################
# 0. PAQUETES
############################################################

# Si algún paquete no está instalado, ejecutar UNA sola vez:
# install.packages(c("ggplot2", "foreign", "nnet",
#                    "reshape2", "marginaleffects"))

library(ggplot2)
library(foreign)
library(nnet)
library(reshape2)
library(marginaleffects)


############################################################
# PARTE I. LOGIT BINARIO
############################################################

# ==========================================================
# 1. EL PROBLEMA
# ==========================================================

#
# "Vamos a empezar con el caso más sencillo.
# Una persona puede terminar en una de DOS situaciones:
# ser admitida o no ser admitida.
#
# Cuando la variable dependiente solo tiene dos resultados,
# un modelo lineal tradicional no es la herramienta más
# natural. Aquí utilizaremos un modelo Logit."
#
# En esta base queremos estudiar si:
#   - GRE
#   - GPA
#   - prestigio/ranking de la institución de pregrado
#
# ayudan a explicar la probabilidad de admisión a un
# programa de posgrado.


# ==========================================================
# 2. CARGAR Y CONOCER LOS DATOS
# ==========================================================

mydata <- read.csv("Datos/binary.csv")

head(mydata)
str(mydata)
summary(mydata)


#
# "Antes de estimar cualquier modelo:
# ¿cuál es nuestra variable dependiente?"
#
# Respuesta esperada:
# admit
#
# "¿Qué valores puede tomar?"
#
# 0 = no admitido
# 1 = admitido
#
# Este punto es importante:
# todavía estamos ante una decisión BINARIA.


# Revisemos la frecuencia de admisión.
table(mydata$admit)

# Revisemos la relación entre admisión y rank.
xtabs(~ admit + rank, data = mydata)


#
# "Todavía no estamos modelando nada.
# Solo estamos entendiendo la información.
#
# Siempre quiero que antes de correr un modelo ustedes sepan
# qué significa cada variable y cómo está construida la
# variable que queremos explicar."


# ==========================================================
# 3. PREPARAR LA VARIABLE CATEGÓRICA
# ==========================================================

# rank no debe interpretarse como una variable continua.
# Los valores 1, 2, 3 y 4 representan categorías.

mydata$rank <- factor(mydata$rank)

levels(mydata$rank)


#
# "Cuando convierto rank en factor, R entiende que no quiero
# imponer que pasar de 1 a 2 tenga exactamente el mismo
# efecto que pasar de 3 a 4.
#
# R va a construir comparaciones contra una categoría base.
# En este caso, rank = 1 será la referencia."


# ==========================================================
# 4. ESTIMAR EL MODELO LOGIT
# ==========================================================

mylogit <- glm(
  admit ~ gre + gpa + rank,
  data = mydata,
  family = binomial(link = "logit")
)

summary(mylogit)


#
# "Aquí aparece una primera dificultad de los modelos Logit:
# los coeficientes NO están directamente expresados como
# cambios en probabilidad.
#
# Están expresados en LOG-ODDS.
#
# Por eso, aunque debemos saber leer el signo y la
# significancia, no conviene detenernos demasiado en la
# interpretación directa del coeficiente."


# ==========================================================
# 5. ¿QUÉ NOS DICE EL SIGNO?
# ==========================================================


#
# "Si un coeficiente es positivo:
# manteniendo lo demás constante, valores mayores de esa
# variable están asociados con mayores odds de admisión.
#
# Si el coeficiente es negativo:
# están asociados con menores odds de admisión."
#
# En esta base normalmente observaremos:
#
# - GRE: coeficiente positivo.
# - GPA: coeficiente positivo.
# - rank 2, 3 y 4: coeficientes negativos respecto a rank 1.
#

# No decir:
# "la probabilidad aumenta en 0.80"
# cuando vemos un coeficiente 0.80.
#
# Eso sería incorrecto.
# El coeficiente está en escala log-odds.


# ==========================================================
# 6. ODDS RATIOS
# ==========================================================

exp(coef(mylogit))

# Intervalos de confianza para los odds ratios.
exp(cbind(
  OR = coef(mylogit),
  confint(mylogit)
))


#
# "Una forma más amigable de interpretar el Logit consiste
# en exponenciar los coeficientes.
#
# Así obtenemos ODDS RATIOS."
#
# Para GPA, en esta base suele obtenerse un OR cercano a 2.2.
#
# Interpretación:
#
# "Manteniendo GRE y rank constantes, aumentar una unidad
# el GPA multiplica aproximadamente por 2.2 los odds de
# admisión."
#
# IMPORTANTE:
# Odds NO es exactamente lo mismo que probabilidad.
#
# Por eso, para comunicar resultados a alguien que no sea
# especialista, normalmente será más natural trabajar con
# probabilidades predichas.


# ==========================================================
# 7. PROBABILIDADES PREDICHAS
# ==========================================================

# Construimos cuatro estudiantes "comparables":
# mismo GRE,
# mismo GPA,
# distinto rank.

newdata1 <- with(
  mydata,
  data.frame(
    gre = mean(gre),
    gpa = mean(gpa),
    rank = factor(1:4, levels = levels(rank))
  )
)

newdata1$prob_admision <- predict(
  mylogit,
  newdata = newdata1,
  type = "response"
)

newdata1


#
# "¿Qué estamos haciendo aquí?"
#
# Respuesta:
# estamos manteniendo constantes GRE y GPA y solamente
# cambiamos el rank.
#

#
# "Ahora sí estamos hablando directamente de probabilidades.
#
# Si dos estudiantes tuvieran GRE y GPA iguales, el modelo
# estima probabilidades distintas de admisión dependiendo
# del rank de su institución de origen.
#
# Esta forma de presentar el resultado es mucho más fácil
# de comunicar que un coeficiente en log-odds."


# ==========================================================
# 8. EFECTOS MARGINALES PROMEDIO DEL LOGIT
# ==========================================================

avg_slopes(mylogit)


#
# "Los efectos marginales nos permiten regresar a una escala
# que nos interesa mucho más: la PROBABILIDAD.
#
# Para una variable continua, el efecto marginal promedio
# nos indica aproximadamente cuánto cambia, en promedio,
# la probabilidad estimada cuando esa variable aumenta una
# unidad.
#
# Para una variable categórica, compara la probabilidad
# estimada al pasar de una categoría a otra."



# ==========================================================
# 9. TRANSICIÓN HACIA MULTINOMIAL
# ==========================================================


#
# "Hasta ahora teníamos una variable dependiente con dos
# posibilidades:
#
#     admitir = 0
#     admitir = 1
#
# Pero pensemos ahora en decisiones reales:
#
# ¿Qué programa académico elige un estudiante?
# ¿Qué medio de transporte utiliza?
# ¿Qué producto financiero escoge?
# ¿Qué marca compra?
#
# Aquí ya no tenemos solamente 0 y 1.
# Podemos tener tres, cuatro o más alternativas."
#
# PREGUNTA PARA EL GRUPO:
#
# "¿Cuál sería entonces el 0 y cuál sería el 1?"
#
# RESPUESTA:
# Ya no tiene sentido pensar la variable de esa manera.
#
# Aquí aparece el LOGIT MULTINOMIAL.


############################################################
# PARTE II. LOGIT MULTINOMIAL
############################################################


# ==========================================================
# 10. CARGAR LA BASE HSB
# ==========================================================

ml <- read.dta("Datos/hsb.dta")

head(ml)
str(ml)
summary(ml)


#
# "Tenemos información de 200 estudiantes.
#
# Nuestra variable dependiente es prog:
# el tipo de programa académico elegido.
#
# Tenemos tres alternativas:
#   - academic
#   - general
#   - vocation
#
# Como explicativas utilizaremos:
#   - ses   : nivel socioeconómico
#   - write : puntuación de escritura"


# ==========================================================
# 11. ENTENDER LA VARIABLE DEPENDIENTE
# ==========================================================

table(ml$prog)
prop.table(table(ml$prog))

# Revisemos SES.
table(ml$ses)

# Resumen de write.
summary(ml$write)

# QUÉ PREGUNTAR:
#
# "¿Cuántas alternativas tiene ahora nuestra variable Y?"
#
# Respuesta: 3.
#
# "¿Son categorías con un orden natural?"
#
# En este ejemplo NO.
#
# Academic, general y vocation son alternativas distintas,
# pero no estamos afirmando que una sea "mayor" que otra.
#
# Por eso hablamos de un modelo MULTINOMIAL y no de un
# modelo ordinal.


# ==========================================================
# 12. LA CATEGORÍA DE REFERENCIA
# ==========================================================

# Vamos a usar "academic" como categoría base.

ml$prog2 <- relevel(ml$prog, ref = "academic")

levels(ml$prog2)


#
# "Esta es una de las ideas MÁS IMPORTANTES de la clase.
#
# El modelo necesita una categoría de referencia.
#
# Elegimos 'academic'.
#
# Por lo tanto, R no nos va a entregar una ecuación
# independiente para academic.
#
# Nos va a entregar dos comparaciones:
#
#   general   VS academic
#   vocation  VS academic
#
# Todos los coeficientes deben leerse teniendo presente esa
# comparación."


# ==========================================================
# 13. ESTIMAR EL MODELO MULTINOMIAL
# ==========================================================

test <- multinom(
  prog2 ~ ses + write,
  data = ml,
  trace = FALSE
)

summary(test)


#
# "Miren la estructura de la salida.
#
# Tenemos una fila para GENERAL y otra para VOCATION.
#
# ¿Por qué no aparece ACADEMIC?
#
# Porque academic es la referencia.
#
# Cada coeficiente responde una pregunta relativa:
#
# ¿qué ocurre con general frente a academic?
# ¿qué ocurre con vocation frente a academic?"


# ==========================================================
# 14. VALORES z Y p
# ==========================================================

# multinom() no imprime directamente los p-valores.
# Podemos obtener una aproximación de Wald.

coef_m <- summary(test)$coefficients
se_m   <- summary(test)$standard.errors

z <- coef_m / se_m
z

p <- 2 * (1 - pnorm(abs(z)))
p


#
# "La significancia nos ayuda a distinguir qué asociaciones
# están respaldadas con mayor claridad por los datos.
#
# Pero no debemos quedarnos únicamente con el p-valor.
#
# También necesitamos:
#   - dirección del efecto,
#   - magnitud,
#   - y sobre todo su traducción a probabilidades."


# ==========================================================
# 15. INTERPRETACIÓN DE LOS COEFICIENTES
# ==========================================================

summary(test)

# ----------------------------------------------------------
# WRITE: GENERAL VS ACADEMIC
# ----------------------------------------------------------
#
# En esta base el coeficiente suele ser aproximadamente:
#
#     -0.058
#

#
# "Manteniendo constante el nivel socioeconómico,
# un punto adicional en write se asocia con una disminución
# de aproximadamente 0.058 en el log-odds de elegir el
# programa GENERAL frente al programa ACADEMIC."
#
# OJO:
# NO decir que la probabilidad baja 5.8%.
# Todavía estamos en log-odds.


# ----------------------------------------------------------
# WRITE: VOCATION VS ACADEMIC
# ----------------------------------------------------------
#
# El coeficiente suele ser aproximadamente:
#
#     -0.114
#

#
# "Manteniendo SES constante, aumentar write reduce el
# log-odds de elegir VOCATION frente a ACADEMIC."
#
# Intuición:
# estudiantes con mayor puntuación en escritura aparecen
# relativamente más orientados hacia academic que hacia
# vocation.


# ----------------------------------------------------------
# SES: HIGH VS LOW
# GENERAL VS ACADEMIC
# ----------------------------------------------------------
#
# El coeficiente suele ser aproximadamente:
#
#     -1.16
#

#
# "Comparado con SES bajo, pertenecer a SES alto reduce el
# log-odds de elegir GENERAL frente a ACADEMIC."
#
# De nuevo:
# esto NO significa que la probabilidad disminuya 1.16.
# La escala todavía es log-odds.


# ==========================================================
# 16. ODDS RATIOS DEL MULTINOMIAL
# ==========================================================

exp(coef(test))


#
# "Podemos exponenciar los coeficientes igual que hicimos
# con el Logit binario.
#
# Ahora, sin embargo, cada odds ratio pertenece a una
# comparación específica:
#
# GENERAL vs ACADEMIC
# VOCATION vs ACADEMIC."
#
# EJEMPLO CON WRITE:
#
# Si exp(-0.058) es aproximadamente 0.94:
#
# "Un punto adicional en write multiplica por cerca de 0.94
# los odds de elegir GENERAL frente a ACADEMIC,
# manteniendo SES constante."
#
# Como 0.94 < 1, esos odds disminuyen.
#
# Aun así, para explicar resultados a un público general,
# las probabilidades predichas serán mucho más intuitivas.


# ==========================================================
# 17. PROBABILIDADES PREDICHAS PARA CADA ESTUDIANTE
# ==========================================================

pp <- fitted(test)

head(pp)

# Verifique que las probabilidades suman 1.
head(rowSums(pp))


#
# "Ahora cada estudiante tiene TRES probabilidades:
#
# P(academic)
# P(general)
# P(vocation)
#
# Y necesariamente:
#
# P(academic) + P(general) + P(vocation) = 1.
#
# Esa restricción es fundamental.
#
# Si aumenta la probabilidad de una alternativa,
# ese aumento debe salir, de alguna manera, de las otras."


# ==========================================================
# 18. UN EXPERIMENTO MUY FÁCIL DE INTERPRETAR
# ==========================================================

# Dejamos write fijo en su media y cambiamos solamente SES.

dses <- data.frame(
  ses = factor(
    c("low", "middle", "high"),
    levels = levels(ml$ses)
  ),
  write = mean(ml$write)
)

dses

prob_ses <- predict(
  test,
  newdata = dses,
  type = "probs"
)

prob_ses


#
# "¿Qué hemos mantenido constante?"
#
# write.
#
# "¿Qué es lo único que estamos cambiando?"
#
# SES.
#

#
# "Esto es casi un pequeño experimento mental.
#
# Imaginamos estudiantes con exactamente la misma puntuación
# en escritura y cambiamos solamente su nivel
# socioeconómico.
#
# El modelo nos muestra cómo se redistribuyen las
# probabilidades entre academic, general y vocation."
#



# ==========================================================
# 19. ¿QUÉ PASA CUANDO CAMBIA WRITE?
# ==========================================================

# Construimos estudiantes hipotéticos con valores de write
# entre 30 y 70 para cada nivel de SES.

dwrite <- data.frame(
  ses = factor(
    rep(c("low", "middle", "high"), each = 41),
    levels = levels(ml$ses)
  ),
  write = rep(30:70, 3)
)

head(dwrite)

# Calculamos probabilidades predichas.
pp.write <- cbind(
  dwrite,
  predict(
    test,
    newdata = dwrite,
    type = "probs"
  )
)

head(pp.write)


#
# "Ahora ya no estamos mirando solamente tres personas
# hipotéticas.
#
# Vamos a recorrer distintos puntajes de escritura y vamos
# a observar cómo cambia la probabilidad de cada programa,
# separando además por nivel socioeconómico."


# ==========================================================
# 20. GRÁFICO DE PROBABILIDADES PREDICHAS
# ==========================================================

lpp <- melt(
  pp.write,
  id.vars = c("ses", "write"),
  variable.name = "programa",
  value.name = "probabilidad"
)

ggplot(
  lpp,
  aes(
    x = write,
    y = probabilidad,
    colour = ses
  )
) +
  geom_line(linewidth = 1) +
  facet_wrap(
    ~ programa,
    ncol = 1
  ) +
  labs(
    x = "Puntaje de escritura",
    y = "Probabilidad predicha",
    colour = "SES",
    title = "Probabilidad predicha de elegir cada programa"
  ) +
  theme_minimal()


#
# "Este gráfico es mucho más importante que memorizar los
# coeficientes."
#
# PREGUNTA 1:
# "¿Qué ocurre con la probabilidad de ACADEMIC cuando
# aumenta write?"
#
# Deberíamos observar una tendencia creciente.
#
# PREGUNTA 2:
# "¿Qué ocurre con VOCATION?"
#
# Deberíamos observar una tendencia decreciente.
#
# PREGUNTA 3:
# "¿GENERAL cambia con la misma intensidad?"
#
# No necesariamente.
#
# IDEA CENTRAL:
#
# "Una misma variable explicativa puede aumentar la
# probabilidad de una alternativa y disminuir la de otra.
#
# En multinomial no existe un único efecto sobre Y.
# El efecto depende de LA ALTERNATIVA que estamos mirando."


# ==========================================================
# 21. EFECTOS MARGINALES PROMEDIO
# ==========================================================

ame_multi <- avg_slopes(test)

ame_multi


#
# "Esta salida es probablemente la más útil para comunicar
# el modelo.
#
# Ahora los efectos están expresados directamente sobre
# PROBABILIDADES."


# ----------------------------------------------------------
# INTERPRETACIÓN: GROUP = ACADEMIC
# ----------------------------------------------------------
#
# En esta base se obtiene aproximadamente:
#
# write ≈ +0.017
#

#
# "Un punto adicional en write aumenta, en promedio,
# la probabilidad estimada de pertenecer al programa
# ACADEMIC en aproximadamente 0.017.
#
# Es decir, cerca de 1.7 PUNTOS PORCENTUALES."
#
# MUY IMPORTANTE:
# aquí sí estamos interpretando un cambio en probabilidad.
#
#
# Para SES high - low se obtiene aproximadamente:
#
# +0.232
#

#
# "Pasar de SES bajo a SES alto está asociado, en promedio,
# con un aumento cercano a 23 puntos porcentuales en la
# probabilidad estimada de elegir ACADEMIC."


# ----------------------------------------------------------
# INTERPRETACIÓN: GROUP = GENERAL
# ----------------------------------------------------------
#
# Para write el efecto suele ser pequeño y no significativo.
#

#
# "El puntaje de escritura no parece modificar con la misma
# claridad la probabilidad de elegir GENERAL."
#
# Esto es importante:
# una variable puede ser muy relevante para unas
# alternativas y poco relevante para otras.


# ----------------------------------------------------------
# INTERPRETACIÓN: GROUP = VOCATION
# ----------------------------------------------------------
#
# Para write se obtiene aproximadamente:
#
# -0.014
#

#
# "Un punto adicional en write reduce, en promedio,
# la probabilidad estimada de elegir VOCATION en alrededor
# de 1.4 puntos porcentuales."


# ==========================================================
# 22. UNA PROPIEDAD MUY IMPORTANTE
# ==========================================================


#
# "Recuerden que las probabilidades deben sumar 1.
#
# Por tanto, si write aumenta la probabilidad de ACADEMIC,
# necesariamente debe reducir, en conjunto, la probabilidad
# de las otras alternativas.
#
# Eso explica por qué los efectos marginales de una misma
# variable, sumados a través de todas las alternativas,
# tienden a compensarse."





# ==========================================================
# OPCIONAL A. CLASE PREDICHA
# ==========================================================

# Podemos pedir directamente la alternativa con mayor
# probabilidad estimada.

pred_clase <- predict(
  test,
  newdata = ml,
  type = "class"
)

head(pred_clase)

# Comparación sencilla entre observado y predicho.
table(
  Observado = ml$prog2,
  Predicho = pred_clase
)

# Accuracy simple.
mean(pred_clase == ml$prog2)


#
# "Esto convierte las probabilidades en una decisión:
# seleccionamos la alternativa con mayor probabilidad.
#
# Sin embargo, no debemos confundir dos objetivos:
#
# - explicar cómo cambian las probabilidades;
# - clasificar correctamente.
#
# En esta clase nuestro énfasis principal es la
# INTERPRETACIÓN del modelo."


# ==========================================================
# OPCIONAL B. HOSMER-LEMESHOW PARA EL LOGIT BINARIO
# ==========================================================

#
# install.packages("ResourceSelection")
# library(ResourceSelection)
#
# hoslem.test(
#   mydata$admit,
#   fitted(mylogit)
# )
#
# INTERPRETACIÓN:
#
# H0: no existe evidencia de una diferencia importante
# entre frecuencias observadas y esperadas por el modelo.
#
# Si p > 0.05:
# no rechazamos H0.
#
# IMPORTANTE:
# no decir que esto "demuestra que el modelo está bien".
# Solamente indica que esta prueba no encuentra evidencia
# suficiente de falta de ajuste.


