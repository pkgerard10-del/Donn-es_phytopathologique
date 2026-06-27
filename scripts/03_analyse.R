
# ==============================================================================
# SCRIPT DE TRAITEMENT STATISTIQUE COMPLET : PHYTOPATHOLOGIE
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. PRÉPARATION ET CHARGEMENT DES PACKAGES
# ------------------------------------------------------------------------------
library(here)
library(readxl)
library(tidyverse)
library(janitor)
library(writexl)
library(FactoMineR)
library(factoextra)

# Déplacement et centralisation du fichier brut

source_file <- "C:/Users/administrateur local/Desktop/Données-Phytopathologie.xlsx"
dest_file <- here("data", "Données-Phytopathologie.xlsx")

if (!dir.exists(here("data"))) { dir.create(here("data"), recursive = TRUE) }
if (file.exists(source_file)) {
  file.copy(from = source_file, to = dest_file, overwrite = TRUE)
  cat("✨ Fichier centralisé avec succès.\n")
} else {
  stop("⚠️ Fichier source introuvable sur le Bureau !")
}

# ------------------------------------------------------------------------------
# 2. IMPORTATION ET NETTOYAGE DES DONNÉES
# ------------------------------------------------------------------------------
cat("\n--- Nettoyage des données ---\n")

# A. Feuille Cercosporiose

cercosporiose_clean <- read_excel(dest_file, sheet = "Cercosporiose") %>%
  select(where(~ !all(is.na(.)))) %>% janitor::clean_names() %>%
  mutate(dates = as.Date(dates), across(c(localite, bloc, traitement), as.factor),
         nfe = as.integer(nfe), across(c(hauteur, diametre), as.numeric)) %>%
  drop_na(dates)

# B. Feuille Prévalence (Calcul direct du taux de prévalence en %)

prevalence_clean <- read_excel(dest_file, sheet = "Prévalence") %>%
  select(where(~ !all(is.na(.)))) %>% janitor::clean_names() %>%
  rename(bloc = blocs) %>% # Harmonisation du nom de la colonne bloc
  mutate(dates = as.Date(dates), across(c(bloc, pieds, traitement), as.factor),
         across(c(pjft, pjfn), as.integer),
         taux_prevalence = (pjft / pjfn) * 100) %>%
  drop_na(dates)

# C. Feuille Indice-Sévérité

indice_severite_clean <- read_excel(dest_file, sheet = "Indice-Sévérité") %>%
  select(where(~ !all(is.na(.)))) %>% janitor::clean_names() %>%
  mutate(dates = as.Date(dates), across(c(bloc, traitement), as.factor), is = as.numeric(is)) %>%
  drop_na(dates)

# Sauvegarde des données propres dans un nouveau fichier Excel

write_xlsx(list("Cercosporiose" = cercosporiose_clean, 
                "Prévalence" = prevalence_clean, 
                "Indice-Sévérité" = indice_severite_clean), 
           path = here("data", "Données-Phytopathologie_Nettoyees.xlsx"))

# ------------------------------------------------------------------------------
# 3. ANALYSE DESCRIPTIVE
# ------------------------------------------------------------------------------
cat("\n--- 3. Exécution de l'Analyse Descriptive ---\n")

# Synthèse de l'Indice de Sévérité (IS) par traitement

desc_severite <- indice_severite_clean %>%
  group_by(traitement) %>%
  summarise(IS_Moyen = mean(is, na.rm = TRUE), IS_Max = max(is, na.rm = TRUE), SD_IS = sd(is, na.rm = TRUE))
print(desc_severite)

# Graphique : Évolution de la sévérité (IS) dans le temps selon les traitements

g1 <- ggplot(indice_severite_clean, aes(x = dates, y = is, color = traitement, group = traitement)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  theme_minimal() +
  labs(title = "Progression de l'Indice de Sévérité au cours du temps", x = "Date", y = "IS Moyen")
print(g1)

# ------------------------------------------------------------------------------
# 4. ANALYSE COMPARATIVE (ANOVA & TUKEY)
# ------------------------------------------------------------------------------
cat("\n--- 4. Exécution de l'Analyse Comparative ---\n")

# ANOVA sur l'Indice de Sévérité (Effets Traitement + Bloc)

anova_is <- aov(is ~ traitement + bloc, data = indice_severite_clean)
print(summary(anova_is))

# Test de comparaison multiple de Tukey (si l'effet traitement est significatif)

tukey_is <- TukeyHSD(anova_is, "traitement")

 print(tukey_is) 

# Boxplot de comparaison de la sévérité
 
g2 <- ggplot(indice_severite_clean, aes(x = traitement, y = is, fill = traitement)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Comparaison de l'Indice de Sévérité (IS) entre Traitements", x = "Traitements", y = "IS")
print(g2)

# ------------------------------------------------------------------------------
# 5. PREPARATION DES DONNÉES AGREGÉES & ANALYSE MULTIVARIÉE (ACP)
# ------------------------------------------------------------------------------
cat("\n--- 5. Exécution de l'Analyse Multivariée ---\n")

# Agréger la croissance (Cercosporiose) et la prévalence à l'échelle Bloc/Traitement/Date

cerc_agg <- cercosporiose_clean %>%
  group_by(dates, bloc, traitement) %>%
  summarise(NFE = mean(nfe), Hauteur = mean(hauteur), Diametre = mean(diametre), .groups = 'drop')

prev_agg <- prevalence_clean %>%
  group_by(dates, bloc, traitement) %>%
  summarise(Prevalence = mean(taux_prevalence), .groups = 'drop')

# Fusion globale de toutes les métriques (Croissance + Prévalence + Sévérité)

data_analyse <- cerc_agg %>%
  inner_join(prev_agg, by = c("dates", "bloc", "traitement")) %>%
  inner_join(indice_severite_clean, by = c("dates", "bloc", "traitement"))

# Exécution de l'ACP sur les indicateurs numériques (NFE, Hauteur, Diamètre, Prévalence, IS)

res_pca <- PCA(data_analyse %>% select(NFE, Hauteur, Diametre, Prevalence, is), graph = FALSE)

# Cercle des corrélations de l'ACP

g3 <- fviz_pca_var(res_pca, col.var = "contrib", 
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) + 
  labs(title = "ACP - Corrélations entre Croissance et Variables Pathologiques")
print(g3)

# ------------------------------------------------------------------------------
# 6. ANALYSE PRÉDICTIVE (MODÉLISATION)
# ------------------------------------------------------------------------------
cat("\n--- 6. Exécution de l'Analyse Prédictive ---\n")

# Modèle linéaire : Prédire l'Indice de Sévérité selon l'état de la plante et le traitement

modele_patho <- lm(is ~ NFE + Hauteur + Diametre + Prevalence + traitement, 
                   data = data_analyse)
print(summary(modele_patho))

# Graphique de diagnostic : Valeurs observées vs Valeurs Prédites par le modèle
# ⚠️ Important : préciser newdata pour éviter les problèmes de dimensions

data_analyse$is_predit <- predict(modele_patho, newdata = data_analyse)

g4 <- ggplot(data_analyse, aes(x = is_predit, y = is)) +
  geom_point(aes(color = traitement), alpha = 0.7, size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", 
              color = "red", size = 1) +
  theme_minimal() +
  labs(title = "Ajustement du Modèle Prédictif : Valeurs Réelles vs Prédites",
       x = "Indice de Sévérité Prédit (Modèle)", 
       y = "Indice de Sévérité Réel")

print(g4)

cat("\n🏁 Script exécuté en intégralité. Les graphiques g1, g2, g3 et g4 sont prêts à être analysés !\n")



# ------------------------------------------------------------------------------
# 7. SAUVEGARDE DES RÉSULTATS DANS OUTPUTS
# ------------------------------------------------------------------------------

cat("\n--- 7. Sauvegarde des Résultats ---\n")

# Créer le dossier outputs s'il n'existe pas

if (!dir.exists(here("outputs"))) {
  dir.create(here("outputs"), recursive = TRUE)
}

# Sauvegarde des tableaux (descriptif, ANOVA, Tukey)

write_xlsx(list(
  "Desc_Severite" = desc_severite,
  "ANOVA_IS" = as.data.frame(summary(anova_is)[[1]]),
  "Tukey_IS" = as.data.frame(tukey_is$traitement)
), path = here("outputs", "Resultats_Analyses.xlsx"))

# Sauvegarde des graphiques en PNG

ggsave(here("outputs", "Graphique_IS_Temps.png"), g1, width = 8, height = 6)
ggsave(here("outputs", "Graphique_Boxplot_Traitements.png"), g2, width = 8, height = 6)
ggsave(here("outputs", "Graphique_ACP.png"), g3, width = 8, height = 6)
ggsave(here("outputs", "Graphique_Modele_Predit.png"), g4, width = 8, height = 6)

cat("\n🏁 Script exécuté en intégralité. Les résultats sont enregistrés dans le dossier 'outputs'.\n")

