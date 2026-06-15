# Script pour précharger et compiler les données
# Exécutez ce script UNE FOIS localement pour générer le fichier data.Rdata

# Configurer le miroir CRAN
options(repos = c(CRAN = "https://cran.rstudio.com/"))

# Charger les packages nécessaires
library(sf)
library(tibble)

# Fonction pour lire CSV avec encodage UTF-8-BOM correctement
read_csv2_utf8 <- function(file) {
  con <- file(file, "r", encoding = "UTF-8")
  on.exit(close(con))
  df <- read.csv2(con, stringsAsFactors = FALSE)
  lines <- readLines(file, n = 1, encoding = "UTF-8")
  lines <- gsub("^\xef\xbb\xbf", "", lines)
  col_names <- strsplit(lines, ";")[[1]]
  colnames(df) <- col_names
  df
}

cat("Chargement des shapefiles...\n")
shapefile  <- read_sf("QPV/quartiers-prioritaires-de-la-politique-de-la-ville-qpv.shp")
shapefile1 <- read_sf("communes/communesPolygon.shp")
shapefile2 <- read_sf("QPV2/quartiers-prioritaires-de-la-politique-de-la-ville-qpv.shp")

cat("Chargement des IRIS...\n")
iris  <- read_sf("iris/georef-france-iris-millesime.shp")
iris2 <- read_sf("iris2/georef-france-iris-millesime.shp")

cat("Chargement des données CSV...\n")
genre <- read.csv2("genre.csv")
age <- read.csv2("age.csv")
quali_le_port <- read.csv2("quali_le_port.csv", fileEncoding = "UTF-8-BOM")

heures_saint_denis                  <- read_csv2_utf8("heures_saint_denis.csv")
participants_saint_benoit           <- read_csv2_utf8("participants_saint_benoit.csv")
heure_echelle_saint_benoit          <- read_csv2_utf8("heure_echelle_saint_benoit.csv")
heure_ANRU_saint_benoit             <- read_csv2_utf8("heure_ANRU_saint_benoit.csv")
heure_echelle_saint_andré           <- read_csv2_utf8("heure_echelle_saint_andré.csv")
heure_anru_saint_andré              <- read_csv2_utf8("heure_anru_saint_andré.csv")
heure_conventionné_saint_pierre     <- read_csv2_utf8("heure_conventionné_saint_pierre.csv")
heure_non_conventionné_saint_pierre <- read_csv2_utf8("heure_non_conventionné_saint_pierre.csv")
heures_le_port                      <- read_csv2_utf8("heures_le_port.csv")
heure_saint_louis1                  <- read_csv2_utf8("heure_saint_louis1.csv")

cat("Création des dataframes supplémentaires...\n")
genre_le_port <- data.frame(
  genre       = c("Masculin", "Féminin"),
  nombre      = c(29, 1),
  pourcentage = c("96,7%", "3,3%")
)

age_le_port <- data.frame(
  age         = factor(c("moins de 26 ans", "26 - 40 ans", "41 - 50 ans", "Plus de 51 ans"),
                       levels = c("moins de 26 ans", "26 - 40 ans", "41 - 50 ans", "Plus de 51 ans")),
  nombre      = c(2, 4, 14, 10),
  pourcentage = c("7%", "13%", "47%", "33%")
)

contrat_le_port <- data.frame(
  contrat     = factor(c("Intérim", "Contrat de chantier", "CDD 6 mois"),
                       levels = c("Intérim", "Contrat de chantier", "CDD 6 mois")),
  nombre      = c(14, 15, 1),
  pourcentage = c("47%", "50%", "3%")
)

genre_saint_andre <- data.frame(
  genre       = c("Masculin"),
  nombre      = c(41),
  pourcentage = c("100%")
)

age_saint_andre <- data.frame(
  age         = factor(c("moins de 26 ans", "26 - 40 ans", "41 - 50 ans", "Plus de 51 ans"),
                       levels = c("moins de 26 ans", "26 - 40 ans", "41 - 50 ans", "Plus de 51 ans")),
  nombre      = c(5, 20, 10, 6),
  pourcentage = c("12,2%", "48,8%", "24,4%", "14,6%")
)

modalite_saint_andre <- data.frame(
  modalite    = factor(c("ETTI", "Embauche directe"),
                       levels = c("ETTI", "Embauche directe")),
  nombre      = c(33, 8),
  pourcentage = c("80,5%", "19,5%")
)

metier_saint_andre <- data.frame(
  metier      = factor(c("Autre / Métier non défini", "Entretien des espaces verts", "Manoeuvre et conduite d'engins lourds de manutention"),
                       levels = c("Autre / Métier non défini", "Entretien des espaces verts", "Manoeuvre et conduite d'engins lourds de manutention")),
  nombre      = c(7, 10, 24),
  pourcentage = c("17,1%", "24,4%", "58,5%")
)

genre_saint_benoit <- data.frame(
  genre       = c("Masculin", "Féminin"),
  nombre      = c(35, 1),
  pourcentage = c("97,2%", "2,8%")
)

age_saint_benoit <- data.frame(
  age         = factor(c("moins de 26 ans", "26 - 40 ans", "41 - 50 ans", "Plus de 51 ans"),
                       levels = c("moins de 26 ans", "26 - 40 ans", "41 - 50 ans", "Plus de 51 ans")),
  nombre      = c(8, 18, 5, 5),
  pourcentage = c("22,2%", "50,0%", "13,9%", "13,9%")
)

modalite_saint_benoit <- data.frame(
  modalite    = factor(c("ETTI", "Embauche directe", "AI", "EI"),
                       levels = c("ETTI", "Embauche directe", "AI", "EI")),
  nombre      = c(34, 6, 1, 1),
  pourcentage = c("81,0%", "14,3%", "2,4%", "2,4%")
)

metier_saint_benoit <- data.frame(
  metier      = factor(c("Préparation du gros oeuvre et travaux publics", "Manoeuvre et conduite d'engins lourds", "Montage d'agencements", "Maçonnerie et béton", "Électricité et équipements", "Menuiseries et fermetures", "Engins de terrassement", "Études et autres"),
                       levels = c("Préparation du gros oeuvre et travaux publics", "Manoeuvre et conduite d'engins lourds", "Montage d'agencements", "Maçonnerie et béton", "Électricité et équipements", "Menuiseries et fermetures", "Engins de terrassement", "Études et autres")),
  nombre      = c(17, 6, 4, 4, 3, 4, 2, 2),
  pourcentage = c("40,5%", "14,3%", "9,5%", "9,5%", "7,1%", "9,5%", "4,8%", "4,8%")
)

cat("Sauvegarde des données dans app_data.Rdata...\n")
save(
  shapefile, shapefile1, shapefile2,
  iris, iris2,
  genre, age, quali_le_port,
  heures_saint_denis, participants_saint_benoit,
  heure_echelle_saint_benoit, heure_ANRU_saint_benoit,
  heure_echelle_saint_andré, heure_anru_saint_andré,
  heure_conventionné_saint_pierre, heure_non_conventionné_saint_pierre,
  heures_le_port, heure_saint_louis1,
  genre_le_port, age_le_port, contrat_le_port,
  genre_saint_andre, age_saint_andre, modalite_saint_andre, metier_saint_andre,
  genre_saint_benoit, age_saint_benoit, modalite_saint_benoit, metier_saint_benoit,
  file = "app_data.Rdata",
  compress = "gzip"
)

cat("✓ Données compilées et sauvegardées dans app_data.Rdata\n")
cat("Vous pouvez maintenant charger rapidement avec: load('app_data.Rdata')\n")
