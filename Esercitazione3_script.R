#Esercitazione3_script

library(tidyverse)
library(knitr)
library(kableExtra)
library(modelsummary)
library(fixest)

library(wooldridge)

data("wage1", package = "wooldridge")

reg_wage <- feols(wage ~ educ, data = wage1, vcov = "hetero")

reg_logwage <- feols(log(wage) ~ educ, data = wage1, vcov = "hetero")

# Previsione valori stimati dal modello log-lineare
wage1 <- wage1 %>%
  mutate(
    logwage_pred = predict(reg_logwage),    #valori predetti in log
    wage_pred = exp(logwage_pred)           #salario predetto in scala originale
  )

# Grafico con curva log-lineare + retta OLS
ggplot(wage1, aes(x = educ)) +
  geom_line(aes(y = wage_pred), color = "blue", size = 1.2) +          # curva log-lineare
  geom_smooth(aes(y = wage), method = "lm", se = FALSE, color = "black", size = 1) + # retta OLS
  labs(
    title = "Confronto: regressione lineare vs log-lineare",
    x = "Anni di istruzione",
    y = "Salario orario $"
  ) +
  theme_minimal()

  modelsummary(list("Wage" = reg_wage, "Log Wage" = reg_logwage), output = "markdown", gof_omit = "AIC|BIC|RMSE|R2 Adj.")


  reg_wagelog <- feols(wage ~ log(educ), data = wage1, vcov = "hetero")
reg_logwagelog <- feols(lwage ~ log(educ), data = wage1, vcov = "hetero")
modelsummary(list("Log Wage" = reg_logwage, "Wage" = reg_wagelog, "Log Wage" =reg_logwagelog), output = "markdown", gof_omit = "AIC|BIC|RMSE|R2 Adj.")


wage1 <- wage1 %>%
  mutate(
    marrmale = ifelse(female == 0 & married == 1, 1, 0),
    marrfemale = ifelse(female == 1 & married == 1, 1, 0),
    singfem = ifelse(female == 1 & married == 0, 1, 0)
  )


reg_logwage_dummy <- feols(log(wage) ~ marrmale + marrfemale + singfem + educ + exper + tenure, data = wage1, vcov = "hetero")
modelsummary(list("Log Wage" = reg_logwage_dummy), output = "markdown", gof_omit = "AIC|BIC|RMSE")


data("campus", package = "wooldridge")


reg_log_crime <- feols(log(crime) ~ log(enroll), data = campus, vcov = "hetero")
reg_log_crime2 <- feols(log(crime) ~ log(enroll) + log(police), data = campus, vcov = "hetero")
modelsummary(list("Log Crime" = reg_log_crime, "Log Crime" = reg_log_crime2 ), output = "markdown", gof_omit = "AIC|BIC|RMSE|R2 Adj.")

reg_wage_exper <- feols(wage ~ exper + I(exper^2), data = wage1, vcov = "hetero")
reg_wage_exper


# 1. Previsione dei valori stimati
wage1 <- wage1 %>%
  mutate(wageexp_pred = predict(reg_wage_exper))

# 2. Filtro per esper ≤ 40
wage_cut <- wage1 %>% filter(exper <= 40)

# 3. Trova il punto massimo della curva wageexp_pred
punto_max <- wage_cut %>%
  filter(wageexp_pred == max(wageexp_pred, na.rm = TRUE))

# 4. Grafico
ggplot(wage_cut, aes(x = exper)) +  
  geom_line(aes(y = wageexp_pred), color = "blue", size = 1.2) +              # curva stimata
  geom_smooth(aes(y = wage), method = "lm", se = FALSE, color = "black", size = 1) + # retta OLS
  geom_point(data = punto_max, aes(y = wageexp_pred), color = "red", size = 3) +      
  labs(
    title = "Confronto: lineare vs polinomio quadratico",
    x = "Anni di Esperienza",
    y = "Salario orario in $"
  ) +
  theme_minimal()


  reg_wage_exper2 <- feols(wage ~ exper + I(exper^2) + educ + tenure, data = wage1, vcov = "hetero")
modelsummary(list("Wage" = reg_wage_exper, "Wage" = reg_wage_exper2), output = "markdown", gof_omit = "AIC|BIC|RMSE|R2 Adj.")