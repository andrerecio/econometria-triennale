#Esercitazione N.1 script

library(AER)
library(wooldridge)
library(tidyverse)
library(knitr)
library(kableExtra)
library(modelsummary)


data("wage1", package = "wooldridge")
head(wage1)

# Statistiche descrittive
stat <- wage1 %>%
  summarise(
    wage_mean = mean(wage, na.rm = TRUE),
    wage_median = median(wage, na.rm = TRUE),
    wage_sd = sd(wage, na.rm = TRUE),
    education_years_mean = mean(educ, na.rm = TRUE),
    education_years_median = median(educ, na.rm = TRUE)
  )

# Visualizzazione dei risultati
stat %>%
  kable(caption = "Statistiche descrittive",
        digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# Istogramma con ggplot
ggplot(wage1, aes(x = wage)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 15) +
  labs(title = "Distribuzione di wage",
       x = "Wage",
       y = "Frequenza") +
  theme_minimal()


ggplot(wage1, aes(x = educ)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 15) +
  labs(title = "Distribuzione dell'istruzione",
       x = "Anni di Istruzione",
       y = "Frequenza") +
  theme_minimal()



ggplot(wage1, aes(y = wage, x = educ)) + 
  geom_point(color = "black") + 
  theme_minimal()



ggplot(wage1, aes(y = wage, x = educ)) + 
  geom_point(color = "black") + 
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  theme_minimal()



library(fixest)
reg1 <- feols(wage ~ educ, data = wage1, vcov = "hetero")
reg1



reg1_ho <- feols(wage ~ educ, data = wage1)
reg1_ho



# Seleziona valori specifici di educ
educ_target <- c(4, 8, 12, 16)

# Calcola varianza di wage per quei valori specifici
varianza_fissa <- wage1 %>%
  filter(educ %in% educ_target) %>%
  group_by(educ) %>%
  summarise(varianza = var(wage, na.rm = TRUE)) %>%
  ungroup()

# Grafico
ggplot(varianza_fissa, aes(x = educ, y = varianza)) +
  geom_point(size = 3, color = "darkblue") +
  labs(title = "Varianza del salario per valori specifici di istruzione",
       x = "Anni di istruzione",
       y = "Varianza di wage") +
  theme_minimal()


modelsummary(list("Wage" = reg1, "Wage" = reg1_ho), output = "markdown", gof_omit = "AIC|BIC|RMSE|R2 Adj.")


wage1 <- wage1 %>%
  mutate(wage_100 = wage/100)


reg2 <- feols(wage_100 ~ educ, data = wage1, vcov = "hetero")

modelsummary(list("Wage" = reg1, "Wage centinaia di $" = reg2), output = "markdown", gof_omit = "AIC|BIC|RMSE|R2 Adj.")


wage1 <- wage1 %>%
  mutate(wage_monthly = wage*140)


reg3 <- feols(wage_monthly ~ educ, data = wage1, vcov = "hetero")
reg3


modelsummary(
  list("Wage Hourly" = reg1, "Wage Monthly" = reg3),
  output = "markdown", gof_omit = "AIC|BIC|RMSE|R2 Adj."
)


wage1 <- wage1 %>%
  mutate(educ_mesi = educ*12)


reg4 <- feols(wage ~ educ_mesi, data = wage1, vcov = "hetero")
modelsummary(list("Wage" = reg1, "Wage" = reg4), output = "markdown", gof_omit = "AIC|BIC|RMSE|R2 Adj.")



modelsummary(list("Wage Hourly" = reg1), output = "markdown", gof_omit = "AIC|BIC|RMSE|R2 Adj.")



tabledummy <- table(wage1$female)

kable(tabledummy, 
      caption = "Numero di Maschi (0) e Femmine (1)") %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"))



stat_genere <- wage1 %>%
  group_by(female) %>%
  summarize(
    wage_mean = mean(wage, na.rm = TRUE),
    wage_median = median(wage, na.rm = TRUE),
    wage_sd = sd(wage, na.rm = TRUE),
    education_years_mean = mean(educ, na.rm = TRUE),
    education_years_median = median(educ, na.rm = TRUE)
  )

# Visualizzazione dei risultati
stat_genere %>%
  kable(caption = "Statistiche descrittive",
        digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))



reg_female <- feols(wage ~ female, data = wage1, vcov = "hetero")



modelsummary(reg_female, output = "markdown", gof_omit = "AIC|BIC|RMSE|R2 Adj.")


stat_genere %>%
  kable(caption = "Statistiche descrittive",
        digits = 2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

wage1 <- wage1 %>%
  mutate(male = ifelse(female == 1, 0, 1))

reg_male <- feols(wage ~ male, data = wage1, vcov = "hetero")
modelsummary(list("Female = 1" = reg_female, "Male = 1" = reg_male),output = "markdown", gof_omit = "AIC|BIC|RMSE|R2 Adj.", title = "Nota: La variabile dipendente è `wage`")


reg2dummy <- feols(wage ~ male + female, data = wage1, vcov = "hetero")
reg2dummy