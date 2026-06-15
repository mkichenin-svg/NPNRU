#!/usr/bin/env Rscript
# Script d'installation des dépendances pour Posit Connect Cloud
# Ce script s'exécute avant le démarrage de l'app

# Installer remotes si nécessaire
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes", repos = "https://cran.r-project.org")
}

# Installer shinymanager depuis GitHub
cat("Installation de shinymanager depuis GitHub...\n")
remotes::install_github(
  "datastorm-open/shinymanager",
  upgrade = "never",
  quiet = TRUE
)

cat("✓ shinymanager installé avec succès\n")

# Vérifier que le package est disponible
if (requireNamespace("shinymanager", quietly = TRUE)) {
  cat("✓ Vérification réussie : shinymanager est chargeable\n")
} else {
  stop("✗ Erreur : shinymanager n'a pas pu être installé")
}
