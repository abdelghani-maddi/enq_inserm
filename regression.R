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
library(labelled)


# Données ----
datainserm <- read_excel("~/Documents/enquete inserm/inserm/datainserm_scores.xlsx")

############################
# Liste des variables à expliquer de type :

# Très importante
# Assez importante
# Peu importante
# Pas importante du tout
############################
############################
############################
# Quelques mises en forme
## Recodage de datainserm$RAGE3
datainserm$RAGE3 <- datainserm$RAGE3 %>%
  fct_recode(
    "De 18 à 40 ans" = "De18à40ans",
    "De 40 à 49 ans" = "De40a49ans",
    "De 50 à 59 ans" = "De50a59ans",
    "60 ans et plus" = "De60ansetplus"
  )

## Recodage de datainserm$Delegation
datainserm$Delegation <- datainserm$Delegation %>%
  fct_recode(
    "Auvergne Rhône Alpes" = "DR Auvergne Rhône Alpes",
    "Grand-Ouest" = "DR Grand-Ouest",
    "Nord-Ouest" = "DR Nord-Ouest",
    "Nouvelle-Aquitaine" = "DR Nouvelle-Aquitaine",
    "Occitanie Méditerranée" = "DR Occitanie Méditerranée",
    "Occitanie Pyrénées" = "DR Occitanie Pyrénées",
    "PACA Corse" = "DR PACA Corse",
    "Paris Centre-Est" = "DR Paris Ile-de-France Centre-Est",
    "Paris Centre-Nord" = "DR Paris Ile-de-France Centre-Nord",
    "Paris Sud" = "DR Paris Ile-de-France Sud",
    "Est" = "DR régionale Est"
  )

## Recoding datainserm$Domainescientifique1
datainserm$Domainescientifique1 <- datainserm$Domainescientifique1 %>%
  fct_recode(
    "Biologie cel., dév. et évolution" = "Biologie cellulaire, développement et évolution",
    "Génét., génom. et bioinfo." = "Génétique, génomique et bioinformatique",
    "Immuno., inflammation, infectio. et microbio." = "Immunologie, inflammation, infectiologie et microbiologie",
    "Neurosc., sc. cognitives, neurol., psychiatrie" = "Neurosciences, sciences cognitives, neurologie, psychiatrie"
  )

## Recoding datainserm$SEXE
datainserm$SEXE <- datainserm$SEXE %>%
  fct_recode(
    "Femme" = "femme",
    "Homme" = "homme"
  )


# Labelliser les variables pour un meilleur rendu dans les graphiques des coefficients

# Définir les étiquettes pour chaque variable
var_label(datainserm$SEXE) <- "Sexe"
var_label(datainserm$RAGE3) <- "Groupes d'âges"
var_label(datainserm$Statut) <- "Statut"
var_label(datainserm$CORPS) <- "Corps"
var_label(datainserm$Domainescientifique1) <- "Domaine Scientifique"
var_label(datainserm$Dirunite) <- "Direction d'unité"
var_label(datainserm$Direquipe) <- "Direction d'équipe"
var_label(datainserm$Delegation) <- "Délégation"
var_label(datainserm$Rechfond_recod) <- "Degré recherche fondamentale"
var_label(datainserm$Partrechclin_recod) <- "Degré recherche clinique"
var_label(datainserm$Partadmin_recod) <- "Degré administration"


############################
############################
############################

variables_a_expliquer <- c("Q1_r1", "Q1_r2", "Q1_r3", "Q1_r4", "Q1_r5", "Q1_r6", "Q1_r7")

# Définir les modalités de référence pour chaque variable de contrôle
modalites_reference <- c(
  SEXE = "femme",  
  RAGE3 = "60 ans et plus",
  Statut = "Contractuel",
  CORPS = "Technicien",
  Domainescientifique1 = "Cancer",
  Dirunite = "non",
  Direquipe = "non",
  Delegation = "Paris Centre-Est",
  Rechfond_recod = "Moyen",
  Partrechclin_recod = "Moyen",
  Partadmin_recod = "Moyen"
)

# # Transformer toutes les variables explicatives en facteur
look <- datainserm %>%
   select(starts_with("Q1_"),POIDS, SEXE, RAGE3, Statut, CORPS, Domainescientifique1, Dirunite, Direquipe, Delegation, Rechfond_recod, Partrechclin_recod, Partadmin_recod)
look_for(look)

# Créer une liste pour stocker les graphiques
plots_list <- list()

# Boucle sur chaque variable à expliquer
for (variable in variables_a_expliquer) {
  
  # Filtrer les données
  df <- datainserm %>%
    filter(!(SEXE == "autre") & 
             !(Statut == "Autres") & 
             !(CORPS == "Autre") & 
             !(CORPS == "Adjoint technique de la recherche") &
             !(Delegation == "En attente d’affectation") &
             !(Domainescientifique1 == "Ne sait pas") &
             !(Domainescientifique1 == "Ne se prononce pas"))
  
 
  # Recodage de la variable
  recoded_variable <- paste0(variable, "_rec")
  df[[recoded_variable]] <- df[[variable]] %>%
    fct_recode(
      "0" = "Pas importante du tout",
      "0" = "Peu importante",
      "1" = "Assez importante",
      "1" = "Très importante",
    )
  
  # Spécifier la modalité de référence pour chaque variable explicative
  for (var_name in names(modalites_reference)) {
    df[[var_name]] <- fct_relevel(df[[var_name]], modalites_reference[var_name])
  }
  
  # Régression logistique
  model <- glm(as.formula(paste(recoded_variable, "~ SEXE + RAGE3 + Statut + CORPS +  
                                                     Domainescientifique1 + Dirunite + Direquipe + Delegation +
                                                     Rechfond_recod + Partrechclin_recod + Partadmin_recod")), 
               data = df, 
               family = binomial,
               weights = POIDS)
  # Graphique des coefficients
  plot <- ggcoef_model(model, exponentiate = TRUE) +
    ggtitle(paste("Régression logistique pour", variable)) +
    coord_cartesian(xlim = c(1e-1, 1e1))  # Définir les limites de l'échelle
  
  # Stocker le graphique dans la liste
  plots_list[[variable]] <- plot
}

# Imprimer tous les graphiques
for (plot in plots_list) {
  print(plot)
}


# Tableau des coefficients
model |> 
  tbl_regression(intercept = TRUE) |> 
  bold_labels()
