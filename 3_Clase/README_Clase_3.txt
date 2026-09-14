Clase 3 - Regresión cuantílica

Contenido del paquete:
- Class_3.Rmd: diapositivas en formato xaringan, coherentes con la 2_Clase.
- Class_3.html: versión navegable en navegador. Usa remark.js desde internet.
- Datos/Salaries.xlsx y Datos/Salaries.csv: base de trabajo.
- Script_Clase_3.R: código de apoyo para clase/taller.
- Taller_Clase_3_Regresion_Cuantilica.md: enunciado corto del taller.
- Class_3_files/figures: gráficos de apoyo incluidos en la clase.

Para renderizar el Rmd en R/RStudio:
install.packages(c("xaringan", "tidyverse", "readxl", "quantreg", "broom", "purrr", "scales", "fontawesome"))
rmarkdown::render("Class_3.Rmd")
