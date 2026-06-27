
# 1. Chargement des packages requis
library(here)
library(readxl)
library(tidyverse)
library(janitor)  # Idéal pour standardiser les en-têtes (ex: Traitment -> traitement)
library(writexl) # Pour l'exportation propre en Excel

# --- ÉTAPE 1 : Déplacement et centralisation du fichier ---

source_file <- "C:/Users/administrateur local/Desktop/Données-Phytopathologie.xlsx"
dest_file <- here("data", "Données-Phytopathologie.xlsx")

if (!dir.exists(here("data"))) {
  dir.create(here("data"), recursive = TRUE)
}

if (file.exists(source_file)) {
  file.copy(from = source_file, to = dest_file, overwrite = TRUE)
  cat("✨ Fichier copié avec succès dans le projet :", dest_file, "\n")
} else {
  stop("⚠️ Le fichier source est introuvable sur le Bureau. Vérifiez le chemin !")
}

# --- ÉTAPE 2 : Importation et nettoyage de la feuille 'Cercosporiose' ---

cat("Nettoyage de la feuille: Cercosporiose...\n")

# Nettoyage de la feuille: Cercosporiose

cercosporiose_clean <- read_excel(dest_file, sheet = "Cercosporiose") %>%
  
  # Supprime les colonnes totalement vides à droite
  
  select(where(~ !all(is.na(.)))) %>%
  
  # Uniformise les en-têtes
  
  janitor::clean_names() %>%
  
  # Convertit les types de variables
  
  mutate(
    dates = as.Date(dates),
    localite = as.factor(localite),
    bloc = as.factor(bloc),
    traitement = as.factor(traitement),
    nfe = as.integer(nfe),
    hauteur = as.numeric(hauteur),
    diametre = as.numeric(diametre)
  ) %>%
  drop_na(dates)

# --- ÉTAPE 3 : Importation et nettoyage de la feuille 'Prévalence' ---

cat("Nettoyage de la feuille: Prévalence...\n")

prevalence_clean <- read_excel(dest_file, sheet = "Prévalence") %>%
  select(where(~ !all(is.na(.)))) %>%
  janitor::clean_names() %>%
  mutate(
    dates = as.Date(dates),
    blocs = as.factor(blocs),
    pieds = as.factor(pieds),
    traitement = as.factor(traitement),
    pjft = as.integer(pjft),
    pjfn = as.integer(pjfn)
  ) %>%
  drop_na(dates)


# --- ÉTAPE 4 : Importation et nettoyage de la feuille 'Indice-Sévérité' ---

cat("Nettoyage de la feuille: Indice-Sévérité...\n")

indice_severite_clean <- read_excel(dest_file, sheet = "Indice-Sévérité") %>%
  select(where(~ !all(is.na(.)))) %>%
  janitor::clean_names() %>%
  mutate(
    dates = as.Date(dates),
    bloc = as.factor(bloc),
    traitement = as.factor(traitement),
    is = as.numeric(is)
  ) %>%
  drop_na(dates)


# --- ÉTAPE 5 : Sauvegarde dans un nouveau fichier Excel multi-feuilles ---

cat("\nExportation des onglets nettoyés vers Excel...\n")

# Création d'une liste nommée : chaque nom sera le titre de la feuille Excel

liste_export <- list(
  "Cercosporiose"   = cercosporiose_clean,
  "Prévalence"      = prevalence_clean,
  "Indice-Sévérité" = indice_severite_clean
)

# Définition du chemin du fichier nettoyé

fichier_sortie <- here("data", "Données-Phytopathologie_Nettoyees.xlsx")

# Écriture finale

write_xlsx(liste_export, path = fichier_sortie)

cat("💾 Succès ! Les données nettoyées sont enregistrées ici :\n", fichier_sortie, "\n")