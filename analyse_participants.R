library(readr)
library(dplyr)

# Charger les données
participants <- read.csv2("participants_saint_benoit.csv", encoding = "UTF-8-BOM")

# Analyser le sexe
cat("SEXE:\n")
sexe_count <- participants %>%
  filter(!is.na(Sexe), Sexe != "") %>%
  count(Sexe) %>%
  mutate(pourcentage = round(n / sum(n) * 100, 1))
print(sexe_count)

# Analyser l'âge
cat("\nAGE:\n")
age_count <- participants %>%
  filter(!is.na(Age), Age != 0) %>%
  mutate(
    age_groupe = case_when(
      Age < 26 ~ "moins de 26 ans",
      Age < 41 ~ "26 - 40 ans",
      Age < 51 ~ "41 - 50 ans",
      TRUE ~ "Plus de 51 ans"
    )
  ) %>%
  count(age_groupe) %>%
  mutate(pourcentage = round(n / sum(n) * 100, 1))
print(age_count)

# Statistiques globales
cat("\nTOTAL:\n")
cat("Nombre total de participants:", nrow(participants), "\n")
cat("Nombre de bénéficiaires (sexe connu):", sexe_count$n %>% sum(), "\n")
