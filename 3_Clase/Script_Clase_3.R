# Clase 3 - Regresión cuantílica
# Orlando Joaqui-Barandica

library(tidyverse)
library(readxl)
library(quantreg)
library(broom)
library(purrr)

salarios <- read_excel("Datos/Salaries.xlsx") %>%
  rename(
    yrs_since_phd = `yrs.since.phd`,
    yrs_service   = `yrs.service`
  ) %>%
  mutate(
    rank = factor(rank),
    discipline = factor(discipline),
    sex = factor(sex),
    log_salary = log(salary)
  )

# Descriptivos
salarios %>%
  summarise(
    n = n(),
    media = mean(salary),
    mediana = median(salary),
    p10 = quantile(salary, .10),
    p90 = quantile(salary, .90)
  )

# OLS
m_lm <- lm(log_salary ~ yrs_since_phd + yrs_service + rank + discipline + sex,
           data = salarios)
summary(m_lm)

# Regresión cuantílica
taus <- c(.10, .25, .50, .75, .90)

m_qr <- rq(log_salary ~ yrs_since_phd + yrs_service + rank + discipline + sex,
           tau = taus, data = salarios)
summary(m_qr, se = "nid")

# Tabla tidy
coef_qr <- map_dfr(taus, function(t) {
  rq(log_salary ~ yrs_since_phd + yrs_service + rank + discipline + sex,
     tau = t, data = salarios) %>%
    tidy(se.type = "nid") %>%
    mutate(tau = t)
})

coef_qr %>%
  select(tau, term, estimate, std.error, statistic, p.value) %>%
  arrange(term, tau)

# Perfil de coeficientes
coef_qr %>%
  filter(term %in% c("yrs_since_phd", "yrs_service")) %>%
  ggplot(aes(x = tau, y = estimate, group = term, color = term)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Cuantil",
    y = "Coeficiente",
    color = "Variable",
    title = "Perfil de coeficientes cuantílicos"
  ) +
  theme_minimal()
