# =========================================================
# Clase 5 - ACP y ACP Dual
# Orlando Joaqui-Barandica
# Universidad del Valle
# =========================================================

# Objetivo general:
# 1. Recordar la lógica del ACP.
# 2. Entender qué agrega el ACP Dual.
# 3. Interpretar ejes, variables, individuos, grupos y variables parciales.
# 4. Cerrar con una aplicación corta de clusterización sobre factores.

# ---------------------------------------------------------
# 0. Paquetes
# ---------------------------------------------------------
# install.packages(c("tidyverse", "FactoMineR", "factoextra", "knitr", "scales"))

library(tidyverse)
library(FactoMineR)
library(factoextra)
library(knitr)
library(scales)

# ---------------------------------------------------------
# 1. Cargar base
# ---------------------------------------------------------
clientes <- read_csv("Datos/clientes_mayoristas_demo.csv", show_col_types = FALSE) %>%
  mutate(
    Region = factor(Region),
    Canal = factor(Canal)
  )

glimpse(clientes)

# Variables cuantitativas activas
vars_gasto <- c(
  "Fresh", "Milk", "Grocery", "Frozen",
  "Detergents_Paper", "Delicassen"
)

# ---------------------------------------------------------
# 2. Descriptivos iniciales
# ---------------------------------------------------------
clientes %>%
  group_by(Canal) %>%
  summarise(
    n = n(),
    across(all_of(vars_gasto), mean),
    .groups = "drop"
  )

clientes %>%
  select(all_of(vars_gasto)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "valor") %>%
  ggplot(aes(x = variable, y = valor)) +
  geom_boxplot(fill = "#D55E00", alpha = 0.35) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Distribución de gasto anual por categoría",
    x = NULL,
    y = "Gasto anual"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# ---------------------------------------------------------
# 3. ACP global
# ---------------------------------------------------------
# El ACP trabaja solo con variables cuantitativas activas.
# Canal y Region NO entran como variables activas en este primer ACP.

res.pca <- PCA(
  clientes %>% select(all_of(vars_gasto)),
  scale.unit = TRUE,
  graph = FALSE
)

# 3.1 Varianza explicada por dimensión
res.pca$eig
get_eigenvalue(res.pca)

fviz_eig(res.pca, addlabels = TRUE)

# 3.2 Variables: coordenadas, contribuciones y cos2
res.var <- get_pca_var(res.pca)
res.var$coord
res.var$contrib
res.var$cos2

fviz_pca_var(
  res.pca,
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#D55E00"),
  repel = TRUE
)

fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

# 3.3 Individuos
res.ind <- get_pca_ind(res.pca)
res.ind$coord %>% head()
res.ind$contrib %>% head()
res.ind$cos2 %>% head()

fviz_pca_ind(
  res.pca,
  geom.ind = "point",
  col.ind = clientes$Canal,
  palette = c("#0072B2", "#D55E00"),
  addEllipses = TRUE,
  legend.title = "Canal"
)

fviz_pca_biplot(
  res.pca,
  geom.ind = "point",
  col.ind = clientes$Canal,
  palette = c("#0072B2", "#D55E00"),
  col.var = "black",
  addEllipses = TRUE,
  repel = TRUE,
  legend.title = "Canal"
)

# ---------------------------------------------------------
# 4. ACP Dual
# ---------------------------------------------------------
# Queremos estudiar si la estructura de las variables cambia según el canal.
# Para DMFA, la base debe tener:
# - una variable categórica que define los grupos: Canal
# - variables cuantitativas activas: categorías de gasto
# - opcionalmente, una cualitativa suplementaria: Region

base_dual <- clientes %>%
  select(
    Region,      # cualitativa suplementaria
    Canal,       # factor activo que define los grupos
    all_of(vars_gasto)
  )

res.dual <- DMFA(
  base_dual,
  num.fact = 2,      # la columna 2 es Canal, el factor que define los grupos
  quali.sup = 1,     # la columna 1 es Region, solo acompaña la interpretación
  scale.unit = TRUE,
  graph = FALSE
)

# 4.1 Varianza explicada
res.dual$eig

# 4.2 Variables globales
res.dual$var$coord
res.dual$var$contrib
res.dual$var$cos2

# 4.3 Coordenadas parciales de las variables por grupo
# Este objeto es la parte más importante para entender el DUAL.
res.dual$var.partiel

# 4.4 Resultados para los grupos
res.dual$group

# 4.5 Graficos principales
plot(res.dual, choix = "ind", invisible = "quali", label = "none", title = "ACP Dual: individuos")
plot(res.dual, choix = "var", label = "all", title = "ACP Dual: variables")
plot(res.dual, choix = "group", label = "all", title = "ACP Dual: grupos")
plot(res.dual, choix = "quali", label = "all", title = "ACP Dual: variable suplementaria")

# ---------------------------------------------------------
# 5. Lectura guiada de una variable clave
# ---------------------------------------------------------
# En clase revisamos si una misma variable ocupa posiciones parecidas
# o diferentes cuando se mira por cada grupo.
# Si las coordenadas parciales se separan mucho, la variable no juega
# el mismo papel en todos los grupos.

res.dual$var.partiel

# ---------------------------------------------------------
# 6. Bonus: clusterización sobre factores del ACP
# ---------------------------------------------------------
# Ya no agrupamos usando todas las variables originales.
# Agrupamos usando las coordenadas factoriales de los clientes.

coord_factores <- res.pca$ind$coord[, 1:3]

set.seed(123)
modelo_cluster <- kmeans(
  coord_factores,
  centers = 3,
  nstart = 25
)

clientes_cluster <- clientes %>%
  mutate(cluster = factor(modelo_cluster$cluster))

# Ver los clusters sobre el plano factorial
p_ind <- fviz_pca_ind(
  res.pca,
  geom.ind = "point",
  col.ind = clientes_cluster$cluster,
  palette = c("#0072B2", "#D55E00", "#009E73"),
  addEllipses = TRUE,
  legend.title = "Cluster"
)
p_ind

# Perfil promedio de cada cluster
perfil_cluster <- clientes_cluster %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    across(all_of(vars_gasto), mean),
    .groups = "drop"
  )

perfil_cluster

perfil_cluster %>%
  pivot_longer(all_of(vars_gasto), names_to = "variable", values_to = "media") %>%
  ggplot(aes(x = variable, y = media, group = cluster, color = cluster)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Perfil promedio de gasto por cluster",
    x = NULL,
    y = "Gasto promedio",
    color = "Cluster"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# ---------------------------------------------------------
# 7. Cierre operativo
# ---------------------------------------------------------
# ACP: resume variables cuantitativas correlacionadas en pocas dimensiones.
# DUAL: pregunta si esa estructura cambia según un grupo conocido.
# Clusters sobre factores: segmenta individuos usando una representación reducida.
