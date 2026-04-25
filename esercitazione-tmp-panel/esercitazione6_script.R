# Esercitazione 6

library(tidyverse)
library(knitr)
library(kableExtra)
library(modelsummary)
library(fixest)
library(wooldridge)


data("crime2", package = "wooldridge")
head(crime2)

regcrimcs <- feols(crmrte ~ unem, data = crime2, vcov = "hetero")
regcrimcs

regcrimcs2 <- feols(crmrte ~ unem + d87, data = crime2, vcov = "hetero")
regcrimcs2


regcrimcs_fe <- feols(crmrte ~ unem | year, data = crime2, vcov = "hetero")
regcrimcs_fe



# Calcola media per gruppo (year)
crime2_dev <- crime2 %>% 
  group_by(year) %>%
  mutate(crmrte_bar = mean(crmrte),  # media crmrte per anno
         unem_bar = mean(unem)) %>%   # media unem per anno
  ungroup() %>%
  mutate(Wcrmrte = crmrte - crmrte_bar,  # deviazione crmrte
         Wunem = unem - unem_bar)        # deviazione unem

# Stima modello sulle deviazioni
mod_crime_within <- feols(Wcrmrte ~ Wunem -1, data = crime2_dev, vcov = "hetero")
mod_crime_within


regcrimcs_fd <- feols(ccrmrte ~ cunem, data = crime2, vcov = "hetero")
regcrimcs_fd



data("wagepan", package = "wooldridge")
datasummary_skim(wagepan)


reg_logwageunion <- feols(lwage ~ union + exper + expersq + educ + black + hisp + married, data = wagepan, vcov = "hetero")
reg_logwageunion


feols(lwage ~ union + exper + expersq + educ + black + hisp + married + d81 + d82 + d83 + d84 + d85 + d86 + d87, data = wagepan, vcov = "hetero")

feols(lwage ~ union + exper + expersq + educ + black + hisp + married | year , data = wagepan, vcov = "hetero")

feols(lwage ~ union + exper + expersq + educ + black + hisp + married | nr , data = wagepan, vcov = "hetero")


wagepan_demeaned <- wagepan %>%
  group_by(nr) %>%
  mutate(
    lwage_dm = lwage - mean(lwage, na.rm = TRUE),
    union_dm = union - mean(union, na.rm = TRUE),
    exper_dm = exper - mean(exper, na.rm = TRUE),
    expersq_dm = expersq - mean(expersq, na.rm = TRUE),
    educ_dm = educ - mean(educ, na.rm = TRUE),
    black_dm = black - mean(black, na.rm = TRUE),
    hisp_dm = hisp - mean(hisp, na.rm = TRUE),
    married_dm = married - mean(married, na.rm = TRUE)
  ) %>%
  ungroup()


  #L'intercetta sarà zero e la rimuovo con -1
feols(lwage_dm ~ union_dm + exper_dm + expersq_dm + educ_dm + black_dm + hisp_dm + married_dm  -1, data = wagepan_demeaned, vcov = "hetero")

reg_logwageunion_fe <- feols(lwage ~ union + exper + expersq + educ + black + hisp + married | nr + year , data = wagepan, vcov = "hetero")
reg_logwageunion_fe


modelsummary(list("Log Wage" = reg_logwageunion, "Log Wage" = reg_logwageunion_fe), output = "markdown", gof_omit = "AIC|BIC|RMSE|R2 Adj.")


wagepan <- wagepan %>%
  mutate(wage = exp(lwage))
summary(wagepan$wage)


reg_wageunion_fe <- feols(wage ~ union + exper + expersq + educ + black + hisp + married | nr + year , data = wagepan, vcov = "hetero")
reg_wageunion_fe

feols(wage ~ union + exper + expersq + educ + black + hisp + married | nr + year , data = wagepan, vcov = "cluster")