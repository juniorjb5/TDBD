CLASE 7 - SERIES DE TIEMPO Y PRONOSTICO
========================================

Archivo principal:
- Class_7.Rmd

Codigo limpio para ejecutar en clase:
- Codigo_Series_Modeltime.R

Estructura pedagogica:
1. Estructura temporal: tendencia, estacionalidad, ciclo y ruido.
2. Transformacion y estacionariedad.
3. Diferenciacion ordinaria y estacional.
4. ADF, KPSS, ndiffs() y nsdiffs().
5. ACF y PACF.
6. AR, MA y ARIMA / ARIMA estacional.
7. auto.arima(), diagnostico de residuos y validacion temporal.
8. Benchmark Seasonal Naive.
9. Modeltime: SNAIVE vs Auto ARIMA vs ETS.
10. Calibracion, metricas fuera de muestra, refit y pronostico final.
11. Traduccion del pronostico a una decision.

Paquetes requeridos:
install.packages(c(
  "xaringan", "xaringanExtra", "tidyverse", "lubridate",
  "forecast", "tseries", "tidymodels", "timetk",
  "modeltime", "fontawesome"
))

Notas:
- La clase utiliza AirPassengers, incluido en R, para evitar depender de
  descargas externas de datos.
- La parte de Modeltime usa la interfaz documentada de arima_reg(),
  exp_smoothing(), naive_reg(), modeltime_table(), modeltime_calibrate(),
  modeltime_accuracy(), modeltime_refit() y modeltime_forecast().
- Se mantiene el formato visual de la Clase 5 (xaringan + estilos y assets).
- El HTML no se incluye porque en este entorno no esta instalado R; el Rmd
  esta preparado para compilarse en R/RStudio con los paquetes anteriores.
