#Esercitazione8_script

#Esercitazione 7
library(tidyverse)
library(knitr)
library(kableExtra)
library(modelsummary)
library(fixest)
library(wooldridge)

data("alcohol", package = "wooldridge")
head(alcohol)


datasummary_skim(alcohol)


reg_lpm_alcohol <- feols(employ ~ abuse + educ + age + married, data = alcohol, vcov = "hetero")
modelsummary(list("Employ" = reg_lpm_alcohol), output = "markdown", gof_omit = "AIC|BIC|RMSE")


probit_alcohol <- feglm(employ ~ abuse + educ + age + married, data = alcohol, family = 'probit')

logit_alcohol <- feglm(employ ~ abuse + educ + age + married, data = alcohol, family = 'logit')

modelsummary(list("Employ (LPM)" = reg_lpm_alcohol, "Employ (Probit)" = probit_alcohol, "Employ (Logit)"=logit_alcohol), output = "markdown", gof_omit = "AIC|BIC|RMSE")




# Creazione di due dataset: uno con abuse=1 per tutti e uno con abuse=0 per tutti
alcohol_tmp1 <- alcohol
alcohol_tmp0 <- alcohol

# Impostiamo abuse=1 per tutti nel primo dataset
alcohol_tmp1$abuse <- 1
# Impostiamo abuse=0 per tutti nel secondo dataset
alcohol_tmp0$abuse <- 0

# Calcolo delle probabilità predette per il modello logit
proba_employ_probit <- predict(probit_alcohol, newdata = alcohol_tmp1)
proba_nonemploy_probit <- predict(probit_alcohol, newdata = alcohol_tmp0)

# Calcolo dell'effetto marginale medio (differenza media nelle probabilità predette)
ame_manual_probit <- mean(proba_employ_probit - proba_nonemploy_probit)
print(paste("Effetto marginale medio manuale (Probit):", round(ame_manual_probit, 4)))


# Calcolo delle probabilità predette per il modello logit
proba_employ_logit <- predict(logit_alcohol, newdata = alcohol_tmp1)
proba_nonemploy_logit <- predict(logit_alcohol, newdata = alcohol_tmp0)

# Calcolo dell'effetto marginale medio (differenza media nelle probabilità predette)
ame_manual_logit <- mean(proba_employ_logit - proba_nonemploy_logit)
print(paste("Effetto marginale medio manuale (Logit):", round(ame_manual_logit, 4)))




library(marginaleffects)
all_effects_logit <- avg_slopes(logit_alcohol)
all_effects_probit <- avg_slopes(probit_alcohol)
all_effects_probit

# Tabella elegante con modelsummary
modelsummary(list("Logit (AME)" = all_effects_logit, "Probit (AME)" = all_effects_probit), output = "markdown", gof_omit = "AIC|BIC|RMSE|R2|Adj")             





library(AER)
data(HMDA, package = "AER")

# Rimozione osservazioni con NA
HMDA <- na.omit(HMDA)

# Creazione variabili 'concesso' e 'afroam'
HMDA <- HMDA |>
  mutate(
    concesso = ifelse(deny == "no", 1, 0), # 1 = Concesso, 0 = Negato
    afroam   = ifelse(afam == "yes", 1, 0)  # 1 = Afroamericano, 0 = Non afroamericano
  ) |>
  # Selezione per chiarezza (opzionale)
  select(concesso, afroam, everything()) |>
  select(-deny, -afam)

head(HMDA)

HMDA <- HMDA %>%
  mutate(pirat = pirat * 100)


# Specifica della formula (utilizza le variabili AER::HMDA)
formula_aer <- concesso ~ afroam + pirat + hirat + lvrat + chist + mhist + phist +
  unemp + selfemp + insurance + condomin + single + hschool

# Modello Lineare di Probabilità (LPM) con errori standard robusti
# Usiamo feols con vcov = "hetero" per gestire l'eteroschedasticità intrinseca nei modelli LPM
lpm_HMDA <- feols(formula_aer, data = HMDA, vcov = "hetero")

# Modello Logit
# Usiamo feglm con family = binomial("logit") per stimare un modello logit
logit_HMDA <- feglm(formula_aer, data = HMDA, family = binomial("logit"))

# Modello Probit
# Usiamo feglm con family = binomial("probit") per stimare un modello probit
probit_HMDA <- feglm(formula_aer, data = HMDA, family = binomial("probit"))
