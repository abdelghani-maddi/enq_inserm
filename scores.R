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

# Fonction pour calculer la moyenne, la somme et le nombre de colonnes par groupe de colonnes
calculate_stats_by_prefix_group <- function(df, prefix) {
  # Sélectionner les colonnes qui commencent par le préfixe et se terminent par "Recode"
  selected_cols <- grep(paste0("^", prefix, ".*Recode$"), names(df), value = TRUE)
  
  # Calculer la somme par groupe de colonnes partageant le même préfixe
  df[[paste0(prefix, "_sum")]] <- rowSums(df[selected_cols], na.rm = TRUE)
  
  # Ajouter une colonne pour le nombre de colonnes par groupe
  df[[paste0(prefix, "_count")]] <- length(selected_cols)
  
  # Calculer la moyenne par groupe de colonnes partageant le même préfixe
  df[[paste0(prefix, "_score")]] <- df[[paste0(prefix, "_sum")]] / df[[paste0(prefix, "_count")]]
  
  # Retourner le dataframe mis à jour
  return(df)
}

# Utiliser la fonction pour calculer la moyenne, la somme et le nombre de colonnes par groupe de colonnes Q7, Q50, Q51, etc.
df <- calculate_stats_by_prefix_group(df, "Q7")
df <- calculate_stats_by_prefix_group(df, "Q50")
df <- calculate_stats_by_prefix_group(df, "Q51")




# Fonction pour calculer la moyenne et la somme par groupe de colonnes
calculate_stats_by_prefix_group <- function(df, prefix) {
  # Sélectionner les colonnes qui commencent par le préfixe et se terminent par "Recode"
  selected_cols <- grep(paste0("^", prefix, ".*Recode$"), names(df), value = TRUE)
  
  # Calculer la moyenne par groupe de colonnes partageant le même préfixe
  df[[paste0(prefix, "_mean")]] <- rowSums(df[selected_cols], na.rm = TRUE) / length(selected_cols)
  
  # Retourner le dataframe mis à jour
  return(df)
}

# Utiliser la fonction pour calculer la moyenne et la somme par groupe de colonnes Q7, Q50, Q51, etc.
df <- calculate_stats_by_prefix_group(df, "Q7")
df <- calculate_stats_by_prefix_group(df, "Q50")
df <- calculate_stats_by_prefix_group(df, "Q51")

# Afficher le résultat
print(df)


