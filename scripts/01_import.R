
# Chargement des packages
library(readxl)
library(here)

# Chemin source : fichier sur le Bureau

source <- "C:/Users/administrateur local/Desktop/Données-Phytopathologie.xlsx"

# Chemin destination : sous-dossier data du projet

destination <- here("data", "Données-Phytopathologie.xlsx")

# Copier le fichier dans data/ (écrase si déjà présent)

file.copy(source, destination, overwrite = TRUE)

# Vérifier que le fichier est bien présent

list.files(here("data"))

# Importer les feuilles depuis le fichier stocké dans data/

fichier <- destination

# Afficher les noms des feuilles

excel_sheets(fichier)

# Importer chaque feuille

cercosporiose   <- read_excel(fichier, sheet = "Cercosporiose")
prevalence      <- read_excel(fichier, sheet = "Prévalence")
indice_severite <- read_excel(fichier, sheet = "Indice-Sévérité")

# Vérification rapide

head(cercosporiose)
head(prevalence)
head(indice_severite)
