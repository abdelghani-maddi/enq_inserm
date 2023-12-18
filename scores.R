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
############################
############################

# Calculer des scores pour par groupe de variable et selon le type de modalités

# Il suffit de rajouter le préfixe du groupe de varialbes pour calculer le score (pour toutes les déclinaisons d'une même question) 👇

############################
############################
############################
# Liste des valeurs de recodage communes à tous les ensembles de variables (c'est possible d'en rajouter d'autres ou de modifier les valeurs)
common_recode_values <- list(
  "Toutafaitdaccord"      =   4, 
  "Plutotdaccord"         =   3, 
  "Plutotpasdaccord"      =   2, 
  "Pasdaccorddutout"      =   1,
  
  "Tresimportante"        =   4, 
  "Assezimportante"       =   3,
  "Peuimportante"         =   2,
  "Pasimportantedutout"   =   1,   
  
  "Ouisurement"           =   4,
  "Ouipeutetre"           =   3,
  "Nonsansdoutepas"       =   2,
  "Nonsurementpas"        =   1,

  "Souvent"               =   5,
  "Parfois"               =   4,
  "Rarement"              =   3,
  "Jamais"                =   2,
  "Neseprononcepas"       =   1,
  "Nonconcerne"           =   0    
    
    
)


# Supprimer les caractères spéciaux car parfois les apostrophe ne sont pas homogènes et cela créé des problèmes
remove_special_chars <- function(x) {
  iconv(as.character(x), to = "ASCII//TRANSLIT") %>%
    str_replace_all("[^a-zA-Z0-9]", "")
}

# Appliquer la suppression des caractères spéciaux à toutes les colonnes pour faciliter le matching
df <- datainserm %>%
  select(starts_with(c("Q1_", "Q4_", "Q5", "Q7", "Q50_", "Q51"))) %>% # 👈 Rajouter d'autres colonnes qui ont les mêmes modalités si besoin de calculser des scores
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

# Fonction de recodage pour les variables de type factor : la sortie est de type factor (i.e ensemble de modalités), alors qu'on a besoin de valeurs numériques pour les scores
recoding_function <- function(x) {
  recode(x, `5` = 5,`4` = 4, `3` = 3, `2` = 2, `1` = 1, `0` = 0)
}

# Récodage pour chaque ensemble de variables
df <- df %>%
  mutate(across(where(is.factor), recoding_function))


# Fonction pour calculer la moyenne et la somme par groupe de colonnes
calculate_stats_by_prefix_group <- function(df, prefix) {
  # Sélectionner les colonnes qui commencent par le préfixe et se terminent par "Recode" (car si on a que le préfixes il prendrea les valeurs de brutes : caractères)
  selected_cols <- grep(paste0("^", prefix, ".*Recode$"), names(df), value = TRUE)
  
  # Calculer la moyenne par groupe (le score) de colonnes partageant le même préfixe
  df[[paste0(prefix, "_score")]] <- rowSums(df[selected_cols], na.rm = TRUE) / length(selected_cols)
  
  # Retourner le dataframe mis à jour
  return(df)
}

# Utiliser la fonction pour calculer le score par groupe de colonnes Q7, Q50, Q51, etc.
df <- calculate_stats_by_prefix_group(df, "Q1")
df <- calculate_stats_by_prefix_group(df, "Q4")
df <- calculate_stats_by_prefix_group(df, "Q7")
df <- calculate_stats_by_prefix_group(df, "Q50")
df <- calculate_stats_by_prefix_group(df, "Q51")
# df <- calculate_stats_by_prefix_group(df, "...") # 👈 Rajouter d'autres colonnes qui ont les mêmes modalités

## Ne garder que les scores pour matcher ensuite avec les données de base
df <- df %>%
  select(ends_with("score"))

## Merger avec les données de base
datainserm <- merge(datainserm, df, by = "row.names")

###################################################



