# ==============================================================================
# main.R - Rapport Final : Analyse Phytopathologique
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. Chargement des packages
# ------------------------------------------------------------------------------
library(here)
library(readxl)
library(tidyverse)
library(janitor)
library(writexl)
library(FactoMineR)
library(factoextra)

# ------------------------------------------------------------------------------
# 2. Importation et Nettoyage des données
# ------------------------------------------------------------------------------
source_file <- "C:/Users/administrateur local/Desktop/Données-Phytopathologie.xlsx"
dest_file <- here("data", "Données-Phytopathologie.xlsx")

if (!dir.exists(here("data"))) dir.create(here("data"), recursive = TRUE)
if (file.exists(source_file)) file.copy(source_file, dest_file, overwrite = TRUE)

cercosporiose_clean <- read_excel(dest_file, sheet = "Cercosporiose") %>%
  clean_names() %>% drop_na()

prevalence_clean <- read_excel(dest_file, sheet = "Prévalence") %>%
  clean_names() %>% rename(bloc = blocs) %>%
  mutate(taux_prevalence = (pjft / pjfn) * 100) %>% drop_na()

indice_severite_clean <- read_excel(dest_file, sheet = "Indice-Sévérité") %>%
  clean_names() %>% drop_na()

write_xlsx(list("Cercosporiose" = cercosporiose_clean,
                "Prévalence" = prevalence_clean,
                "Indice-Sévérité" = indice_severite_clean),
           path = here("data", "Données-Phytopathologie_Nettoyees.xlsx"))

# ------------------------------------------------------------------------------
# 3. Analyses Descriptives
# ------------------------------------------------------------------------------
desc_severite <- indice_severite_clean %>%
  group_by(traitement) %>%
  summarise(IS_Moyen = mean(is, na.rm = TRUE),
            IS_Max = max(is, na.rm = TRUE),
            SD_IS = sd(is, na.rm = TRUE))

g1 <- ggplot(indice_severite_clean, aes(x = dates, y = is, color = traitement)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  theme_minimal() +
  labs(title = "Progression de l'Indice de Sévérité", x = "Date", y = "IS Moyen")

# ------------------------------------------------------------------------------
# 4. Analyse Comparative (ANOVA & Tukey)
# ------------------------------------------------------------------------------
anova_is <- aov(is ~ traitement + bloc, data = indice_severite_clean)
tukey_is <- TukeyHSD(anova_is, "traitement")

g2 <- ggplot(indice_severite_clean, aes(x = traitement, y = is, fill = traitement)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Comparaison de l'Indice de Sévérité", x = "Traitements", y = "IS")

# ------------------------------------------------------------------------------
# 5. Analyse Multivariée (ACP) avec vérification
# ------------------------------------------------------------------------------
cerc_agg <- cercosporiose_clean %>%
  group_by(dates, bloc, traitement) %>%
  summarise(NFE = mean(nfe), Hauteur = mean(hauteur), Diametre = mean(diametre), .groups = 'drop')

prev_agg <- prevalence_clean %>%
  group_by(dates, bloc, traitement) %>%
  summarise(Prevalence = mean(taux_prevalence), .groups = 'drop')

data_analyse <- cerc_agg %>%
  inner_join(prev_agg, by = c("dates", "bloc", "traitement")) %>%
  inner_join(indice_severite_clean, by = c("dates", "bloc", "traitement"))

data_pca <- data_analyse %>%
  select(NFE, Hauteur, Diametre, Prevalence, is) %>%
  drop_na() %>%
  mutate(across(everything(), as.numeric))

if (nrow(data_pca) > 0) {
  res_pca <- PCA(data_pca, graph = FALSE)
  g3 <- fviz_pca_var(res_pca, col.var = "contrib",
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) +
    labs(title = "ACP - Corrélations entre Variables")
} else {
  g3 <- ggplot() + theme_void() + labs(title = "⚠️ Pas assez de données pour l'ACP")
}

# ------------------------------------------------------------------------------
# 6. Analyse Prédictive (Modèle Linéaire) robuste
# ------------------------------------------------------------------------------

# Vérifier que la variable traitement a au moins 2 niveaux
if (length(unique(data_analyse$traitement)) > 1) {
  modele_patho <- lm(is ~ NFE + Hauteur + Diametre + Prevalence + traitement, data = data_analyse)
  data_analyse$is_predit <- predict(modele_patho, newdata = data_analyse)
  
  g4 <- ggplot(data_analyse, aes(x = is_predit, y = is)) +
    geom_point(aes(color = traitement), alpha = 0.7, size = 2) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    theme_minimal() +
    labs(title = "Valeurs Réelles vs Prédites", x = "IS Prédit", y = "IS Réel")
} else {
  cat("⚠️ Impossible de lancer le modèle : la variable 'traitement' n'a qu'un seul niveau.\n")
  g4 <- ggplot() + theme_void() + labs(title = "⚠️ Modèle non estimé (1 seul traitement)")
}

# ------------------------------------------------------------------------------
# 7. Sauvegarde des Résultats dans outputs
# ------------------------------------------------------------------------------
if (!dir.exists(here("outputs"))) dir.create(here("outputs"), recursive = TRUE)

write_xlsx(list(
  "Desc_Severite" = desc_severite,
  "ANOVA_IS" = as.data.frame(summary(anova_is)[[1]]),
  "Tukey_IS" = as.data.frame(tukey_is$traitement)
), path = here("outputs", "Resultats_Analyses.xlsx"))

ggsave(here("outputs", "Graphique_IS_Temps.png"), g1, width = 8, height = 6)
ggsave(here("outputs", "Graphique_Boxplot_Traitements.png"), g2, width = 8, height = 6)
ggsave(here("outputs", "Graphique_ACP.png"), g3, width = 8, height = 6)
ggsave(here("outputs", "Graphique_Modele_Predit.png"), g4, width = 8, height = 6)

cat("\n🏁 Rapport final exécuté. Résultats et visuels disponibles dans 'outputs'.\n")
