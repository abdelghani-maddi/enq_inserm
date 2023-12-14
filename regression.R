# Regression

#####################################################################
###          Analyse de données           ###
#####################################################################
rm(list = ls()) #supprimer tous les objets 



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
# Liste des variables à expliquer de type :

# Très importante
# Assez importante
# Peu importante
# Pas importante du tout

variables_a_expliquer <- c("Q1_r1", "Q1_r2", "Q1_r3", "Q1_r4", "Q1_r5", "Q1_r6", "Q1_r7")

# Créer une liste pour stocker les graphiques
plots_list <- list()

# Boucle sur chaque variable à expliquer
for (variable in variables_a_expliquer) {
  
  # Filtrer les données
  df <- datainserm %>%
    filter(!(SEXE == "autre") & !(Statut == "Autres") & !(CORPS == "Autre") & !(CORPS == "Adjoint technique de la recherche") )
  
  # Recodage de la variable
  recoded_variable <- paste0(variable, "_rec")
  df[[recoded_variable]] <- df[[variable]] %>%
    fct_recode(
      "0" = "Pas importante du tout",
      "0" = "Peu importante",
      "1" = "Assez importante",
      "1" = "Très importante",
    )
  
  # Régression logistique
  model <- glm(as.formula(paste(recoded_variable, "~ SEXE + RAGE3 + Statut + CORPS + Direquipe")), 
               data = df, 
               family = binomial,
               weights = POIDS)
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
  
  
  
#######################

# Liste des valeurs de recodage communes à tous les ensembles de variables
common_recode_values <- list(
  "Toutafaitdaccord" = 4, 
  "Plutotdaccord" = 3, 
  "Plutotpasdaccord" = 2, 
  "Pasdaccorddutout" = 1
)

# Supprimer les caractères spéciaux d'une colonne
remove_special_chars <- function(x) {
  iconv(as.character(x), to = "ASCII//TRANSLIT") %>%
    str_replace_all("[^a-zA-Z0-9]", "")
}

# Appliquer la suppression des caractères spéciaux à toutes les colonnes
df <- datainserm %>%
  select(starts_with(c("Q7", "Q50_", "Q51"))) %>%
  mutate(across(everything(), remove_special_chars))

# Récodage pour chaque ensemble de variables
df <- df %>%
  mutate(across(everything(), 
                ~ {
                  recoded <- recode_factor(., !!!common_recode_values)
                  recoded
                },
                .names = "{.col}_Recode"
  ))

############

# Modifier le type des colonnes _Recode en numérique
df <- df %>%
  mutate_at(vars(ends_with("_Recode")), as.numeric)

# Fonction pour calculer les moyennes par groupe de variables
calculate_group_means <- function(data, prefixes, suffix) {
  recoded_columns <- names(data) %>% 
    grep(paste0("^", prefixes), value = TRUE) %>% 
    grep(paste0(suffix, "$"), value = TRUE)
  
  data %>%
    rowwise() %>%
    mutate(Mean = mean(c_across(all_of(recoded_columns)), na.rm = TRUE))
}

# Liste des préfixes pour tous les ensembles de variables
prefixes <- c("Q7", "Q50", "Q51")


# Calculer les moyennes par groupe de variables
df_means <- calculate_group_means(df, prefixes, suffix = "_Recode")

