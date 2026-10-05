# =========================================================
# Clase 6 - ACM con FactoClass
# Orlando Joaqui-Barandica
# Universidad del Valle
# =========================================================

# Objetivo general:
# 1. Entender la logica del Analisis de Correspondencias Multiples.
# 2. Leer modalidades, ejes, contribuciones y calidad de representacion.
# 3. Construir perfiles de empresarios usando FactoClass.
# 4. Pasar del mapa factorial a una decision concreta.

# ---------------------------------------------------------
# 0. Paquetes
# ---------------------------------------------------------
# install.packages(c("tidyverse", "ade4", "FactoClass", "scales"))

library(tidyverse)
library(ade4)
library(FactoClass)
library(scales)

# ---------------------------------------------------------
# 1. Cargar base
# ---------------------------------------------------------
empresarios_raw <- read_delim(
  "Datos/empresarios_acm_demo.csv",
  delim = ";",
  show_col_types = FALSE
)

glimpse(empresarios_raw)

# ---------------------------------------------------------
# 2. Preparar base para ACM
# ---------------------------------------------------------
# El ACM trabaja con variables categoricas.
# Quitamos el id y convertimos todas las variables a factor.

empresarios <- empresarios_raw %>%
  select(-id) %>%
  mutate(across(everything(), as.factor)) %>%
  as.data.frame()

str(empresarios)

# ---------------------------------------------------------
# 3. Descriptivos iniciales
# ---------------------------------------------------------
empresarios %>% count(sector, sort = TRUE)
empresarios %>% count(tamano_empresa, sort = TRUE)
empresarios %>% count(uso_digital, sort = TRUE)
empresarios %>% count(principal_obstaculo, sort = TRUE)

# Grafico sencillo antes del ACM
empresarios %>%
  count(sector, uso_digital) %>%
  ggplot(aes(x = sector, y = n, fill = uso_digital)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Uso digital segun sector",
    x = NULL,
    y = "Participacion",
    fill = "Uso digital"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# ---------------------------------------------------------
# 4. ACM con FactoClass
# ---------------------------------------------------------
# Version reproducible, sin preguntas por consola.

clusters <- FactoClass(
  empresarios,
  dudi.acm,
  nf = 3,          # numero de ejes factoriales conservados
  nfcl = 3,        # ejes usados para clasificar
  k.clust = 4,     # numero de clusters o perfiles
  scanFC = FALSE   # evita pedir parametros en consola
)

# Version interactiva, similar al script original de consultoria:
# clusters <- FactoClass(empresarios, dudi.acm)

# ---------------------------------------------------------
# 5. Revisar objetos principales
# ---------------------------------------------------------
names(clusters)

# Cluster asignado a cada individuo
clusters$cluster

table(clusters$cluster)

# Unir cluster a la base original de categorias
Clus <- cbind(
  cluster = as.factor(clusters$cluster),
  empresarios
)

head(Clus)

# ---------------------------------------------------------
# 6. Graficos principales
# ---------------------------------------------------------
# Grafico general del analisis
plot(clusters$dudi)

# Mapa de individuos
s.label(
  clusters$dudi$li,
  label = row.names(empresarios),
  sub = "Empresarios en el plano factorial",
  possub = "bottomright"
)

# Mapa de modalidades, ejes 1 y 2
s.label(
  clusters$dudi$co,
  xax = 1,
  yax = 2,
  sub = "Modalidades: ejes 1 y 2",
  possub = "bottomright"
)

# Mapa de modalidades, ejes 1 y 3
s.label(
  clusters$dudi$co,
  xax = 1,
  yax = 3,
  sub = "Modalidades: ejes 1 y 3",
  possub = "bottomright"
)

# Graficar individuos por grupo encontrado
Grupo <- as.factor(as.vector(Clus[, 1]))

s.class(
  clusters$dudi$li,
  Grupo,
  sub = "Empresarios: clases en ejes 1 y 2",
  possub = "bottomright",
  xax = 1,
  yax = 2,
  col = c(1, 2, 3, 4)
)

s.class(
  clusters$dudi$li,
  Grupo,
  sub = "Empresarios: clases en ejes 1 y 3",
  possub = "bottomright",
  xax = 1,
  yax = 3,
  col = c(1, 2, 3, 4)
)

# ---------------------------------------------------------
# 7. Interpretacion de ejes
# ---------------------------------------------------------
# Valores propios / inercia
# Valores propios / inercia
Inercia <- inertia.dudi(
  clusters$dudi,
  row.inertia = TRUE,
  col.inertia = TRUE
)

Inercia$tot.inertia

# Coordenadas de las modalidades
clusters$dudi$co

# Contribuciones y calidad de representacion de modalidades
Contrib <- inertia.dudi(
  clusters$dudi,
  row.inertia = TRUE,
  col.inertia = TRUE
)$col.abs / 100

Calidad <- inertia.dudi(
  clusters$dudi,
  row.inertia = TRUE,
  col.inertia = TRUE
)$col.rel / 100

# Modalidades que mas contribuyen a cada eje
# Se ordena por posicion de columna y no por el nombre Axis1(%),
# porque ade4 puede cambiar esos nombres entre versiones.
Contrib_df <- as.data.frame(Contrib)

head(Contrib_df[order(Contrib_df[[1]], decreasing = TRUE), , drop = FALSE], 15)
head(Contrib_df[order(Contrib_df[[2]], decreasing = TRUE), , drop = FALSE], 15)
head(Contrib_df[order(Contrib_df[[3]], decreasing = TRUE), , drop = FALSE], 15)

# Calidad de representacion
Calidad_df <- as.data.frame(Calidad)

head(Calidad_df[order(Calidad_df[[1]], decreasing = TRUE), , drop = FALSE], 15)
head(Calidad_df[order(Calidad_df[[2]], decreasing = TRUE), , drop = FALSE], 15)

# ---------------------------------------------------------
# 8. Caracterizacion de perfiles
# ---------------------------------------------------------
# Esta es una de las salidas mas importantes.
# Muestra las modalidades que caracterizan cada cluster.

clusters$carac.cate

# Descriptivos adicionales por cluster
Clus %>%
  count(cluster, uso_digital) %>%
  group_by(cluster) %>%
  mutate(p = n / sum(n)) %>%
  arrange(cluster, desc(p))

Clus %>%
  count(cluster, sector) %>%
  group_by(cluster) %>%
  mutate(p = n / sum(n)) %>%
  arrange(cluster, desc(p))

Clus %>%
  count(cluster, principal_obstaculo) %>%
  group_by(cluster) %>%
  mutate(p = n / sum(n)) %>%
  arrange(cluster, desc(p))

# Graficos de perfil
Clus %>%
  count(cluster, uso_digital) %>%
  group_by(cluster) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(x = cluster, y = p, fill = uso_digital)) +
  geom_col() +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Uso digital por cluster",
    x = "Cluster",
    y = "Participacion",
    fill = "Uso digital"
  ) +
  theme_minimal(base_size = 13)

Clus %>%
  count(cluster, principal_obstaculo) %>%
  group_by(cluster) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(x = cluster, y = p, fill = principal_obstaculo)) +
  geom_col() +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Principal obstaculo por cluster",
    x = "Cluster",
    y = "Participacion",
    fill = "Obstaculo"
  ) +
  theme_minimal(base_size = 13)

# ---------------------------------------------------------
# 9. Cierre operativo
# ---------------------------------------------------------
# ACM: encuentra patrones de asociacion entre modalidades.
# FactoClass: combina ACM y clustering para construir perfiles.
# La interpretacion debe terminar en una decision: segmentar, priorizar, comunicar o disenar una oferta diferenciada.
