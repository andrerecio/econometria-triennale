#Esercitazione N.2 script
# Il seguente script riproduce l'esercitazione 2 del corso di Econometria
# ad eccezione delle tabelle di regressione che sono state create con il pacchetto modelsummary
# e le tabelle con le statistiche descrittive che sono state create con il pacchetto kableExtra

# Pacchetti necessari
library(tidyverse)
library(modelsummary)
library(wooldridge)

# Caricamento del dataset
data("wage1", package = "wooldridge")
head(wage1)

#Regressione
# Regressione wage su educ
# Regressione wage su educ + exper
# Regressione wage su educ + exper + tenure
library(fixest)
reg_wage1 <- feols(wage ~ educ, data = wage1, vcov = "hetero")
reg_wage2 <- feols(wage ~ educ + exper, data = wage1, vcov = "hetero")
reg_wage3 <- feols(wage ~ educ + exper + tenure, data = wage1, vcov = "hetero")

modelsummary(list("Wage" = reg_wage1, "Wage" = reg_wage2, "Wage" = reg_wage3), output = "markdown", gof_omit = "AIC|BIC|RMSE")


# Regressione con dummy aggiunta
reg_wage4 <- feols(wage ~ educ + exper + tenure + female, data = wage1, vcov = "hetero")
modelsummary(list("Wage" = reg_wage3, "Wage" = reg_wage4), output = "markdown", gof_omit = "AIC|BIC|RMSE")


# Cambio unità di misura

wage1 <- wage1 %>%
  mutate(wage_monthly = wage*140)


reg_wage_dummy3 <- feols(wage_monthly ~ educ + exper + tenure + female, data = wage1, vcov = "hetero")
modelsummary(list("Wage" = reg_wage4, "Wage Monthly" = reg_wage_dummy3), output = "markdown", gof_omit = "AIC|BIC|RMSE") 

# Wage e genere:

reg_wage_dummy <- feols(wage ~ female, data = wage1, vcov = "hetero")
reg_wage_dummy2 <- feols(wage ~ female + educ + exper + tenure, data = wage1, vcov = "hetero")
modelsummary(list("Wage (Esercitazione 1)" = reg_wage_dummy, "Wage" = reg_wage_dummy2), output = "markdown", gof_omit = "AIC|BIC|RMSE")


###########################################################################
##########################################################################
#College GPA

data("gpa1", package = "wooldridge")

head(gpa1)

# Statistiche descrittive
statgpa <- gpa1 %>%
       summarise(
       colgpa_mean = mean(colGPA, na.rm = TRUE),
       hsgpa_mean = mean(hsGPA, na.rm = TRUE),
       act_mean = mean(ACT, na.rm = TRUE),
  )

statgpa

summary(gpa1$colGPA)
summary(gpa1$hsGPA)

##Regressione
reg_gpa1 <- feols(colGPA ~ hsGPA + ACT, data = gpa1, vcov = "hetero")
modelsummary(list("colGPA" = reg_gpa1), output = "markdown", gof_omit = "AIC|BIC|RMSE")

#Cambio scala

gpa1 <- gpa1 %>%
  mutate(hsGPA100 = hsGPA *25)

reg_gpa2 <- feols(colGPA ~ hsGPA100 + ACT, data = gpa1, vcov = "hetero")
modelsummary(list("colGPA100" = reg_gpa2), output = "markdown", gof_omit = "AIC|BIC|RMSE")


reg_gpa3 <- feols(colGPA ~ hsGPA + ACT + skipped, data = gpa1, vcov = "hetero")
modelsummary(list("colGPA" = reg_gpa1, "colGPA" = reg_gpa3), output ="markdown", gof_omit = "AIC|BIC|RMSE")


###Effetti PC

reg_gpa_pc1 <- feols(colGPA ~ PC, data = gpa1, vcov = "hetero")
reg_gpa_pc2 <- feols(colGPA ~ PC + hsGPA + ACT , data = gpa1, vcov = "hetero")
reg_gpa_pc3 <- feols(colGPA ~ PC + hsGPA + ACT + mothcoll + fathcoll , data = gpa1, vcov = "hetero")
modelsummary(list("colGPA" = reg_gpa_pc1, "colGPA" = reg_gpa_pc2, "colGPA" = reg_gpa_pc3), output = "markdown", gof_omit = "AIC|BIC|RMSE")

## Dummy con categorie:


wage1 <- wage1 %>%
  mutate(
    marrmale = ifelse(female == 0 & married == 1, 1, 0),
    marrfemale = ifelse(female == 1 & married == 1, 1, 0),
    singfem = ifelse(female == 1 & married == 0, 1, 0))

#Regressione
reg_wage_sm1 <- feols(wage ~ marrmale + marrfemale + singfem, data = wage1, vcov = "hetero")
reg_wage_sm2 <- feols(wage ~ marrmale + marrfemale + singfem + educ + exper + tenure, data = wage1, vcov = "hetero")
modelsummary(list("Wage" = reg_wage_sm1, "Wage" = reg_wage_sm2), output = "markdown", gof_omit = "AIC|BIC|RMSE")


# Collinearità
wage1 <- wage1 %>% 
  mutate(male = 1 - female)


reg_wage_sm3 <- feols(wage ~ marrmale + marrfemale + singfem + female + male, data = wage1, vcov = "hetero")
reg_wage_sm3
