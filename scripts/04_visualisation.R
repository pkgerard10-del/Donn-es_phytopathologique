
# ------------------------------------------------------------------------------
# 7. VISUALISATION ET SAUVEGARDE DES ANALYSES
# ------------------------------------------------------------------------------

cat("\n--- 7. Visualisation et Sauvegarde des Résultats ---\n")

if (!dir.exists(here("outputs"))) {
  dir.create(here("outputs"), recursive = TRUE)
}

# Graphique 1 : Progression de l'Indice de Sévérité

g1 <- ggplot(indice_severite_clean, aes(x = dates, y = is, color = traitement, group = traitement)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  theme_minimal() +
  labs(title = "Progression de l'Indice de Sévérité au cours du temps", x = "Date", y = "IS Moyen")
ggsave(here("outputs", "Graphique_IS_Temps.png"), g1, width = 8, height = 6)

# Graphique 2 : Comparaison des traitements (Boxplot)

g2 <- ggplot(indice_severite_clean, aes(x = traitement, y = is, fill = traitement)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Comparaison de l'Indice de Sévérité entre Traitements", x = "Traitements", y = "IS")
ggsave(here("outputs", "Graphique_Boxplot_Traitements.png"), g2, width = 8, height = 6)

# Graphique 3 : ACP - Corrélations

g3 <- fviz_pca_var(res_pca, col.var = "contrib", 
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) +
  labs(title = "ACP - Corrélations entre Croissance et Variables Pathologiques")
ggsave(here("outputs", "Graphique_ACP.png"), g3, width = 8, height = 6)

# Graphique 4 : Modèle prédictif

g4 <- ggplot(data_analyse, aes(x = is_predit, y = is)) +
  geom_point(aes(color = traitement), alpha = 0.7, size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red", size = 1) +
  theme_minimal() +
  labs(title = "Ajustement du Modèle Prédictif : Valeurs Réelles vs Prédites",
       x = "Indice de Sévérité Prédit", y = "Indice de Sévérité Réel")
ggsave(here("outputs", "Graphique_Modele_Predit.png"), g4, width = 8, height = 6)

cat("\n✅ Les graphiques ont été générés et sauvegardés dans le dossier 'outputs'.\n")
