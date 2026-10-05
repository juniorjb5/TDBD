# Taller Clase 6 — ACM

**Objetivo.** Usar Analisis de Correspondencias Multiples para identificar perfiles a partir de variables categoricas y convertir esos perfiles en una decision de negocio o de gestion.

**Entrega.** Informe maximo de 5 paginas, en grupos de 3–4 estudiantes.

## Pasos minimos

1. Presenten el contexto, la base de datos y la pregunta de decision.
2. Seleccionen variables categoricas utiles para construir perfiles.
3. Limpien categorias repetidas, mal escritas o con muy pocos casos.
4. Conviertan las variables seleccionadas a `factor`.
5. Realicen un ACM usando `FactoClass` y `dudi.acm`.
6. Reporten la inercia de las primeras dimensiones.
7. Interpreten Dim.1 y Dim.2 usando modalidades con mayor contribucion.
8. Caractericen los clusters usando `clusters$carac.cate` y tablas por grupo.
9. Den nombre a cada perfil encontrado.
10. Cierren con una recomendacion concreta.

## Pregunta guia

¿Que perfiles aparecen en la base y que decision cambia al reconocer esos perfiles?

## Recomendacion

No llenen el informe de salidas de R. El valor del taller esta en explicar con claridad:

- que representan las dimensiones;
- que modalidades construyen cada eje;
- que caracteriza a cada cluster;
- que decision se puede tomar con cada perfil.
