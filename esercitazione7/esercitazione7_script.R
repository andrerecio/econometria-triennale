#Esercitazione 7 script
library(tidyverse)
library(knitr)
library(kableExtra)
library(modelsummary)
library(fixest)
library(wooldridge)

data("card", package = "wooldridge")
head(card)

wage_educ <- feols(wage ~ educ + exper, data = card, vcov = "hetero")
logwage_educ <- feols(log(wage) ~ educ + exper, data = card, vcov = "hetero")


modelsummary(
  list("Wage" = wage_educ, "Log Wage" = logwage_educ),
  output = "markdown", gof_omit = "AIC|BIC|RMSE|R2 Adj."
)


iv_card <- feols(log(wage) ~ 1 | educ ~ nearc4, data = card, vcov = "hetero")
iv_card

fs_card <- feols(educ ~ nearc4, data = card, vcov = "hetero")
fs_card

card$educ_hat <- predict(fs_card)


sstage_card <- feols(log(wage) ~ educ_hat, data = card, vcov = "hetero")
sstage_card

cov(card$nearc4, card$lwage)/cov(card$nearc4, card$educ)

rf_card <- feols(log(wage) ~ nearc4, data = card, vcov = "hetero")
rf_card


# Estrai i coefficienti
beta_fs_1 <- coef(fs_card)["nearc4"]  # coefficiente del primo stadio
beta_rf_1 <- coef(rf_card)["nearc4"]  # coefficiente della forma ridotta

beta_rf_1 / beta_fs_1 


iv_card_controlli <- feols(log(wage) ~ exper + black + smsa + south + married | educ ~ nearc4, data = card, vcov = "hetero")

summary(iv_card_controlli)


ols_card_controlli <- feols(log(wage) ~ exper + black +smsa + south + educ + married, data = card, vcov = "hetero")

modelsummary(
  list("Log Wage (OLS)" = ols_card_controlli, "Log Wage (IV)" = iv_card_controlli),
  output = "markdown", gof_omit = "AIC|BIC|RMSE|R2 Adj."
)


iv_card_overid <- feols(log(wage) ~ exper + black +smsa + south + married | educ ~ nearc4 + nearc2, data = card, vcov = "hetero")
iv_card_overid


library(car)
card <- card |> mutate(uhat = log(wage) - iv_card_overid$fitted.values)
Jlm <- feols(uhat~ nearc4 + nearc2 + exper + black +smsa + south + married, data = card, vcov = "iid")
J <- linearHypothesis(Jlm, c("nearc4=0", "nearc2=0"))
J


linearHypothesis(fs_card , "nearc4=0")


linearHypothesis(fs_card , "nearc4=0", test="F")


fs_card_overid <- feols(educ ~ nearc4 + nearc2 + exper + black +smsa + south + married, data = card, vcov = "hetero")

linearHypothesis(fs_card_overid, c("nearc4=0", "nearc2=0"))


data("labsup", package = "wooldridge")
head(labsup)



# Modello IV (samesex come strumento per kids)
iv_labsup <- feols(hours ~ educ + age + black + hispan | kids ~ samesex, data = labsup, vcov = "hetero")


summary(iv_labsup, stage =1)


summary(iv_labsup, stage =2)


#OLS ore e numero figli
ols_labsup <- feols(hours ~ kids + educ + age + black + hispan, data = labsup, vcov = "hetero")

modelsummary(
  list("OLS" = ols_labsup,
       "IV" = iv_labsup),
  stars = TRUE,
  gof_omit = "IC|Adj|RMSE"
)


iv_labsup_twins <- feols(hours ~ educ + age + black + hispan | kids ~ multi2nd, data = labsup, vcov = "hetero")


summary(iv_labsup_twins, stage =1)


summary(iv_labsup_twins, stage =2)


modelsummary(
  list("Hours (IV samesex)" = iv_labsup ,
       "Hours (IV multi2nd)" = iv_labsup_twins),
  stars = TRUE,
  gof_omit = "IC|Adj|RMSE"
)


iv_labsup_twins_over <- feols(hours ~ educ + age + black + hispan | kids ~ multi2nd + samesex, data = labsup, vcov = "hetero")
iv_labsup_twins_over