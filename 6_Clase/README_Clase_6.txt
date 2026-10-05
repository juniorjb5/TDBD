Clase 6 - ACM con FactoClass

Archivos principales:
- Class_6.Rmd: diapositivas xaringan de la clase.
- Codigo_ACM_FactoClass.R: codigo limpio y continuo para ejecutar en R/RStudio.
- Taller_Clase_6_ACM.md: taller corto de la clase.
- Datos/empresarios_acm_demo.csv: base didactica para ACM y perfiles de empresarios.

Para renderizar las diapositivas:
1. Abrir Class_6.Rmd en RStudio.
2. Instalar los paquetes necesarios si no estan instalados:
   install.packages(c("xaringan", "xaringanExtra", "tidyverse", "ade4", "FactoClass", "fontawesome", "scales"))
3. Knit.

Nota:
- La clase usa FactoClass con scanFC = FALSE para que no pida parametros por consola.
- Si se desea usar la version interactiva, puede ejecutarse: FactoClass(empresarios, dudi.acm)
- La base de datos es didactica y fue creada para la clase.

CORRECCION DE COMPATIBILIDAD (02-oct-2026)
-----------------------------------------
FactoClass es un paquete antiguo y algunas de sus funciones internas esperan un data.frame base de R.
read_delim() crea un tibble, por lo que antes de ejecutar FactoClass la base se convierte con as.data.frame().
Esto evita el error: "the condition has length > 1" en FactoClass::Fac.Num().
