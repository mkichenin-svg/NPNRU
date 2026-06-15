# Installation des packages avant le déploiement
# Installer depuis GitHub car shinymanager n'est pas sur CRAN

if (!require("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Installer shinymanager depuis GitHub
remotes::install_github("datastorm-open/shinymanager")

# Installer les autres packages
packages <- c(
  "shiny",
  "bslib",
  "ggplot2",
  "sf",
  "tibble",
  "bsicons"
)

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
  }
}
