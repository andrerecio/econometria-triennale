#Esercitazione N.1 script
# Il seguente script riproduce l'esercitazione 1 del corso di Econometria
# ad eccezione delle tabelle di regressione che sono state create con il pacchetto modelsummary
# e le tabelle con le statistiche descrittive che sono state create con il pacchetto kableExtra

#Pacchetti necessari
library(tidyverse)
library(wooldridge)

# Caricamento del dataset
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

stat

# Istogramma con ggplot di wage
ggplot(wage1, aes(x = wage)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 15) +
  labs(title = "Distribuzione di wage",
       x = "Wage",
       y = "Frequenza") +
  theme_minimal()

#Istogramma di educ
ggplot(wage1, aes(x = educ)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 15) +
  labs(title = "Distribuzione dell'istruzione",
       x = "Anni di Istruzione",
       y = "Frequenza") +
  theme_minimal()


#Scatterpplot
ggplot(wage1, aes(y = wage, x = educ)) +
  geom_point(color = "black") +
  theme_minimal()


#Scatterplot con retta di regressione
ggplot(wage1, aes(y = wage, x = educ)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  theme_minimal()


#Pacchetto per le regressioni
# vcov = "hetero" per calcolare gli errori standard robusti
library(fixest)
reg1 <- feols(wage ~ educ, data = wage1, vcov = "hetero")
reg1


# Regressione senza opzione di errore standard 
reg1_ho <- feols(wage ~ educ, data = wage1)
reg1_ho



#Calcolo della varianza di wage per valori specifici di educ
# Seleziona valori specifici di educ
educ_valori <- c(4, 8, 12, 16)

# Calcola varianza di wage per quei valori specifici di educ
varianza_valori <- wage1 %>%
  filter(educ %in% educ_valori) %>%
  group_by(educ) %>%
  summarise(varianza = var(wage, na.rm = TRUE)) %>%
  ungroup()

# Grafico
ggplot(varianza_valori, aes(x = educ, y = varianza)) +
  geom_point(size = 3, color = "darkblue") +
  labs(title = "Varianza del salario per valori specifici di istruzione",
       x = "Anni di istruzione",
       y = "Varianza di wage") +
  theme_minimal()

#Cambiamento unità di misura di wage
wage1 <- wage1 %>%
  mutate(wage_100 = wage/100)

reg2 <- feols(wage_100 ~ educ, data = wage1, vcov = "hetero")
reg2

# Cambiamento unità di misura di wage in mensile (in base a 140 ore lavorative al mese)
wage1 <- wage1 %>%
  mutate(wage_monthly = wage*140)

reg3 <- feols(wage_monthly ~ educ, data = wage1, vcov = "hetero")
reg3



# Cambiamento unità di misura di educ in mensile (12 mesi)
# educ è in anni, quindi lo moltiplichiamo per 12 per ottenere i mesi
wage1 <- wage1 %>%
  mutate(educ_mesi = educ*12)


reg4 <- feols(wage ~ educ_mesi, data = wage1, vcov = "hetero")
reg4

# ---- Parte 2 ----

#Tabella di contigenza per il genere
tabledummy <- table(wage1$female)

# Statistiche descrittive per il genere
stat_genere <- wage1 %>%
  group_by(female) %>%
  summarize(
    wage_mean = mean(wage, na.rm = TRUE),
    wage_median = median(wage, na.rm = TRUE),
    wage_sd = sd(wage, na.rm = TRUE),
    education_years_mean = mean(educ, na.rm = TRUE),
    education_years_median = median(educ, na.rm = TRUE)
  )


stat_genere


#Regressione con varibaile dummy (female =1)
reg_female <- feols(wage ~ female, data = wage1, vcov = "hetero")
reg_female

#Creazione di una variabile dummy per il genere (male =1)
wage1 <- wage1 %>%
  mutate(male = ifelse(female == 1, 0, 1))


reg_male <- feols(wage ~ male, data = wage1, vcov = "hetero")
reg_male

#Regressione con male e female
reg2dummy <- feols(wage ~ male + female, data = wage1, vcov = "hetero")
reg2dummy