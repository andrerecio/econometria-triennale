# ==============================================================================
# Esercitazioni di Econometria — Introduzione a R
# ==============================================================================

# --- Installazione pacchetti (solo la prima volta) ----------------------------

install.packages("tidyverse")
install.packages("wooldridge")

# --- Caricare i pacchetti -----------------------------------------------------

library(tidyverse)
library(wooldridge)

# ==============================================================================
# 1. Primi passi
# ==============================================================================

x <- 1:10
y <- x^2
plot(x, y)

# ==============================================================================
# 2. Dataset: wage1
# ==============================================================================

data("wage1", package = "wooldridge")
str(wage1)

# ==============================================================================
# 3. Manipolare i dati con dplyr
# ==============================================================================

# Filtriamo le osservazioni con più di 12 anni di istruzione
wage_educ_higher <- wage1 |>
  filter(educ > 12)

head(wage_educ_higher)

# ==============================================================================
# 4. Statistiche descrittive
# ==============================================================================

wage1 |>
  summarise(
    wage_mean   = mean(wage),
    wage_median = median(wage),
    wage_sd     = sd(wage),
    educ_mean   = mean(educ)
  )

# --- Per gruppi ---------------------------------------------------------------

# female = 0 → uomini, female = 1 → donne
wage1 |>
  group_by(female) |>
  summarise(
    wage_mean = mean(wage),
    wage_sd   = sd(wage),
    n         = n()
  )

# ==============================================================================
# 5. Grafici con ggplot2
# ==============================================================================

# Distribuzione del salario orario
ggplot(wage1, aes(x = wage)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 20) +
  labs(
    title = "Distribuzione del salario orario",
    x = "Salario orario (dollari)",
    y = "Frequenza"
  ) +
  theme_minimal()

# ==============================================================================
# 6. Polizia e crimine
# ==============================================================================

# Carichiamo il dataset da GitHub
# In alternativa, scaricatelo e importatelo con Import Dataset
url_data <- "https://raw.githubusercontent.com/andrerecio/econometria-triennale/main/intro/crime2_clean.csv"

crime <- read_csv(url_data, show_col_types = FALSE, na = ".")
glimpse(crime)

# --- Creiamo variabili per 100.000 abitanti -----------------------------------

crime_sub <- crime |>
  filter(year %in% c(1985, 1987, 1989, 1991)) |>
  mutate(
    violent = (murder + rape + robbery + assault) / citypop * 100000,
    police  = sworn / citypop * 100000
  )

# --- Scatter plot: polizia vs crimine violento --------------------------------

ggplot(crime_sub, aes(x = police, y = violent)) +
  geom_point(alpha = 0.75) +
  labs(
    x = "Poliziotti per 100.000 abitanti",
    y = "Crimini violenti per 100.000 abitanti"
  ) +
  theme_minimal(base_size = 20)