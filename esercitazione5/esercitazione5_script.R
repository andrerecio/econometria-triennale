# Esercitazione 5

library(tidyverse)
library(knitr)
library(kableExtra)
library(modelsummary)
library(fixest)
library(wooldridge)

# Load the dataset
data("wage2", package = "wooldridge")


reg_wage1 <- feols(wage ~ educ + exper + tenure, data = wage2, vcov = "hetero")
reg_logwage1 <- feols(log(wage) ~ educ + exper + tenure, data = wage2, vcov = "hetero")

modelsummary(list("Wage" = reg_wage1, "Log Wage" = reg_logwage1), output = "kableExtra", gof_omit = "AIC|BIC|RMSE|R2 Adj.")


reg_wagepol <- feols(wage ~ exper + I(exper^2), data = wage1, vcov = "hetero")
reg_logwagepol <- feols(log(wage) ~ + exper + I(exper^2), data = wage1, vcov = "hetero")

modelsummary(list("Wage" = reg_wagepol, "Log Wage" = reg_logwagepol), output = "kableExtra", gof_omit = "AIC|BIC|RMSE|R2 Adj.")

data("wage1", package = "wooldridge")

wage1 <- wage1 %>%
  mutate(exper_center = exper - mean(exper, na.rm = TRUE))

reg_experc <- feols(wage ~ exper_center, data = wage1, vcov = "hetero")

reg_exper <- feols(wage ~ exper, data = wage1, vcov = "hetero")
modelsummary(list("Wage" = reg_experc, "Wage" = reg_exper), output = "kableExtra", gof_omit = "AIC|BIC|RMSE|R2 Adj.")

reg_wagepolcenter <- feols(wage ~ educ + exper_center + I(exper_center^2) + tenure, data = wage1, vcov = "hetero")
reg_wagepol <- feols(wage ~ educ + exper + I(exper^2) + tenure, data = wage1, vcov = "hetero")

modelsummary(list("Wage" = reg_wagepolcenter, "Wage" = reg_wagepol), output = "kableExtra", gof_omit = "AIC|BIC|RMSE|R2 Adj.")


wage2 <- wage2 %>%
  mutate(pareduc = meduc + feduc)



regeduc_int <- feols(wage ~ educ*pareduc + exper + tenure, data = wage2, vcov = "hetero")
regeduc_intlog <- feols(log(wage) ~ educ*pareduc + exper + tenure, data = wage2, vcov = "hetero")
modelsummary(list("Wage " = regeduc_int, "Log Wage " = regeduc_intlog), output = "markdown", gof_omit = "AIC|BIC|RMSE|R2 Adj.")



wage2 <- wage2 %>%
  mutate(
    pareduc_center = pareduc - mean(pareduc, na.rm = TRUE),
    educ_center = educ - mean(educ, na.rm = TRUE)
  )


regeduc_int <- feols(wage ~ educ*pareduc + exper + tenure, data = wage2, vcov = "hetero")
regeduc_int_mean <- feols(wage ~ educ_center * pareduc_center + exper + tenure, data = wage2, vcov = "hetero")
modelsummary(list("Wage " = regeduc_int, "Wage " = regeduc_int_mean), output = "markdown", gof_omit = "AIC|BIC|RMSE|R2 Adj.")



reg_wage_educexper <- feols(wage ~ educ * exper, data = wage1, vcov = "hetero")

#Risultati
modelsummary(list("Wage" = reg_wage_educexper), output = "kableExtra", gof_omit = "AIC|BIC|RMSE|R2 Adj.")


summary(wage1$exper)


wage1 <- wage1 %>%
  mutate(
    educ_center= educ - mean(educ, na.rm = TRUE),
    exper_center = exper - mean(exper, na.rm = TRUE)
  )


reg_wage_educexper_center <- feols(wage ~ educ_center * exper_center, data = wage1, vcov = "hetero")
#Risultati
modelsummary(list("Wage" = reg_wage_educexper_center), output = "kableExtra", gof_omit = "AIC|BIC|RMSE|R2 Adj.")


##Primo modello regressione di wage su educ, female e la loro interazione
reg_wage_educfe <- feols(wage ~ educ * female, data = wage1, vcov = "hetero")
reg_wage_educfe_center <- feols(wage ~ educ_center * female, data = wage1, vcov = "hetero")

#Risultati
modelsummary(list("Wage" = reg_wage_educfe, "Wage" = reg_wage_educfe_center), output = "kableExtra", gof_omit = "AIC|BIC|RMSE|R2 Adj.")



