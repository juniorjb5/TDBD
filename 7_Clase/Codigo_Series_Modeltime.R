
# install.packages(c(
#   "tidyverse", "lubridate", "forecast", "tseries",
#   "tidymodels", "timetk", "modeltime"
# ))

library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)
library(tidymodels)
library(timetk)
library(modeltime)

# --------------------------------------------------------------
# 1. DATOS
# --------------------------------------------------------------

serie_tbl <- tibble(
  date = seq.Date(
    as.Date("1949-01-01"),
    by = "month",
    length.out = length(AirPassengers)
  ),
  pasajeros = as.numeric(AirPassengers)
)

serie_ts <- AirPassengers

serie_tbl %>%
  plot_time_series(date, pasajeros, .interactive = FALSE)

# --------------------------------------------------------------
# 2. TRANSFORMACION, ESTACIONARIEDAD Y DIFERENCIACION
# --------------------------------------------------------------

forecast::ggAcf(log(serie_ts))

tseries::adf.test(log(serie_ts))
tseries::kpss.test(log(serie_ts), null = "Trend")

forecast::ndiffs(log(serie_ts))
forecast::nsdiffs(log(serie_ts))

serie_estacionaria <- diff(
  diff(log(serie_ts), lag = 12),
  differences = 1
)

forecast::ggtsdisplay(serie_estacionaria)

tseries::adf.test(serie_estacionaria)
tseries::kpss.test(serie_estacionaria, null = "Level")

# --------------------------------------------------------------
# 3. ARIMA CLASICO
# --------------------------------------------------------------

modelo_auto <- forecast::auto.arima(
  serie_ts,
  seasonal = TRUE,
  lambda = 0,
  biasadj = TRUE
)

modelo_auto
forecast::checkresiduals(modelo_auto)

# Train/test: 1959-1960 quedan para evaluacion
train_ts <- window(serie_ts, end = c(1958, 12))
test_ts  <- window(serie_ts, start = c(1959, 1))

fit_arima <- forecast::auto.arima(
  train_ts,
  seasonal = TRUE,
  lambda = 0,
  biasadj = TRUE
)

fc_arima <- forecast::forecast(
  fit_arima,
  h = length(test_ts)
)

forecast::accuracy(fc_arima, test_ts)

autoplot(fc_arima) +
  autolayer(test_ts, series = "Real")

# Benchmark seasonal naive
fc_snaive <- forecast::snaive(
  train_ts,
  h = length(test_ts)
)

forecast::accuracy(fc_snaive, test_ts)

# --------------------------------------------------------------
# 4. MODELTIME
# --------------------------------------------------------------

splits <- serie_tbl %>%
  time_series_split(
    assess = "2 years",
    cumulative = TRUE
  )

train_tbl <- training(splits)
test_tbl  <- testing(splits)

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(
    date,
    pasajeros,
    .interactive = FALSE
  )

# Benchmark SNAIVE
fit_snaive_mt <- naive_reg(
  seasonal_period = "1 year"
) %>%
  set_engine("snaive") %>%
  fit(
    pasajeros ~ date,
    data = train_tbl
  )

# Auto ARIMA
fit_arima_mt <- arima_reg(
  seasonal_period = "1 year"
) %>%
  set_engine(
    "auto_arima",
    lambda = 0,
    biasadj = TRUE
  ) %>%
  fit(
    pasajeros ~ date,
    data = train_tbl
  )

# ETS
fit_ets_mt <- exp_smoothing(
  seasonal_period = "1 year"
) %>%
  set_engine("ets") %>%
  fit(
    pasajeros ~ date,
    data = train_tbl
  )

# Tabla de modelos
models_tbl <- modeltime_table(
  fit_snaive_mt,
  fit_arima_mt,
  fit_ets_mt
)

models_tbl

# Calibracion sobre test
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(
    new_data = test_tbl
  )

# Ranking fuera de muestra
ranking_modelos <- calibration_tbl %>%
  modeltime_accuracy() %>%
  arrange(rmse)

ranking_modelos

# Pronosticos del test
calibration_tbl %>%
  modeltime_forecast(
    new_data = test_tbl,
    actual_data = serie_tbl
  ) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .legend_max_width = 28
  )

# Elegir ganador por RMSE y reentrenar con toda la historia
id_ganador <- ranking_modelos %>%
  slice_min(rmse, n = 1, with_ties = FALSE) %>%
  pull(.model_id)

modelo_final_tbl <- calibration_tbl %>%
  filter(.model_id == id_ganador) %>%
  modeltime_refit(
    data = serie_tbl
  )

# Pronosticar 1 ano hacia adelante
modelo_final_tbl %>%
  modeltime_forecast(
    h = "1 year",
    actual_data = serie_tbl
  ) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .title = "Pronostico final: proximos 12 meses"
  )
