#Esercitazione4 script


library(tidyverse)
library(knitr)
library(kableExtra)
library(modelsummary)
library(fixest)
library(wooldridge)
data("wage1", package = "wooldridge")

reg_wage_dummy <- feols(wage ~ female, data = wage1, vcov = "hetero")
reg_wagemonthly_dummy <- feols((wage*140) ~ female, data = wage1, vcov = "hetero")
reg_logwage_dummy <- feols(log(wage) ~  female, data = wage1, vcov = "hetero")
reg_logwagemonthly_dummy <- feols(log(wage * 140) ~  female, data = wage1, vcov = "hetero")
modelsummary(list("Wage" = reg_wage_dummy, "Wage Monthly" = reg_wagemonthly_dummy, "Log Wage"= reg_logwage_dummy, "Log (Wage Monthly)" = reg_logwagemonthly_dummy ), output = "kableExtra", gof_omit = "AIC|BIC|RMSE|R2 Adj.")

wage1 <- wage1 %>%
  mutate(
    marrmale = ifelse(female == 0 & married == 1, 1, 0),
    marrfemale = ifelse(female == 1 & married == 1, 1, 0),
    singfem = ifelse(female == 1 & married == 0, 1, 0)
  )


reg_wage_sm1 <- feols(wage ~ marrmale + marrfemale + singfem, data = wage1, vcov = "hetero")
modelsummary(list("Wage" = reg_wage_sm1), output = "markdown", gof_omit = "AIC|BIC|RMSE|R2 Adj.")


reg_wage_marrfe <- feols(wage ~ female * married, data = wage1, vcov = "hetero")
modelsummary(list("Wage" = reg_wage_marrfe), output = "markdown", gof_omit = "AIC|BIC|RMSE|R2 Adj.")


wage1 <- wage1 %>% 
  mutate(male = 1 - female)


reg_wage_marrmal <- feols(wage ~ male * married, data = wage1, vcov = "hetero")
modelsummary(list("Wage" = reg_wage_marrfe, "Wage" = reg_wage_marrmal), output = "kableExtra", gof_omit = "AIC|BIC|RMSE|R2 Adj.")


##Prima modello regressione di wage su educ, female e la loro interazione
reg_wage_educfe <- feols(wage ~ educ * female, data = wage1, vcov = "hetero")


#Risultati
modelsummary(list("Wage" = reg_wage_educfe), output = "kableExtra", gof_omit = "AIC|BIC|RMSE|R2 Adj.")

##install.packages("marginaleffects")
library(marginaleffects)
plot_predictions(reg_wage_educfe, 
                condition = c("educ", "female"),
                ) + 
  labs(title = "Salario predetto per livelli di istruzione e genere",
       x = "Anni di istruzione",
       y = "Salario predetto")


##Prima modello regressione di wage su edud, female e la loro interazione
reg_wage_educexper <- feols(wage ~ educ * exper, data = wage1, vcov = "hetero")

reg_wage_educexper2 <- feols(wage ~ educ * exper + female + tenure, data = wage1, vcov = "hetero")
#Risultati
modelsummary(list("Wage" = reg_wage_educexper, "Wage" = reg_wage_educexper2), output = "kableExtra", gof_omit = "AIC|BIC|RMSE|R2 Adj.")



summary(wage1$exper)
