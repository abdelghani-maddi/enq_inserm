# Regression


# Chargement des packages ----
library(tidyverse)
library(readxl)
library(openxlsx)
library(questionr)
library(stats)
library(gtsummary)
library(ggstats)
library(forcats)


# Données ----
datainserm <- read_excel("~/Documents/enquete inserm/datainserm.xlsx")

############################
# Liste des variables à expliquer
variables_a_expliquer <- c("Q1_r1", "Q1_r2", "Q1_r3", "Q1_r4", "Q1_r5", "Q1_r6", "Q1_r7")

# Créer une liste pour stocker les graphiques
plots_list <- list()

# Boucle sur chaque variable à expliquer
for (variable in variables_a_expliquer) {
  
  # Filtrer les données
  df <- datainserm %>%
    filter(!(SEXE == "autre") & !(Statut == "Autres") & !(CORPS == "Autre")) # auu fait, il y a que 6 "autre".
  
  # Recodage de la variable
  recoded_variable <- paste0(variable, "_rec")
  df[[recoded_variable]] <- df[[variable]] %>%
    fct_recode(
      "0" = "Pas importante du tout",
      "0" = "Peu importante",
      "1" = "Assez importante",
      "1" = "Très importante"
    )
  
  # Régression logistique
  model <- glm(as.formula(paste(recoded_variable, "~ SEXE + RAGE3 + Statut + Domainescientifique1 + CORPS + Direquipe")), 
               data = df, family = binomial)
  
  # Graphique des coefficients
  plot <- ggcoef_model(model, exponentiate = TRUE) +
    ggtitle(paste("Régression logistique pour", variable))
  
  # Stocker le graphique dans la liste
  plots_list[[variable]] <- plot
}

# Imprimer tous les graphiques
for (plot in plots_list) {
  print(plot)
}







