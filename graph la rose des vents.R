install.packages("fmsb")
# Installer et charger la bibliothèque
library(fmsb)
library(ggplot2)

# Données
data <- data.frame(
  row.names = c("désir de savoir", "désir de rendre service", "désir de progresser", 
                "changer le monde", "être le meilleur", "bénéficier financièrement", "être connu"),
  posrdoc = c(78, 87, 65, 90, 90, 49, 60),
  chercheur = c(80, 65, 78, 85, 70, 87, 70),
  ingénieur = c(65, 58, 70, 90, 50, 84, 80),
  technicien = c(57, 77, 69, 50, 77, 54, 90)
)

# Ajout des directions
data$Direction <- rownames(data)

# Mise en forme des données pour ggplot2
data_long <- reshape2::melt(data, id.vars = "Modalités")

# Création du graphique La rose des vents amélioré
ggplot(data_long, aes(x = Direction, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8, color = "white") +
  coord_polar(start = 0) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 1, size = 10),
    axis.ticks = element_blank(),
    panel.grid.major = element_line(color = "gray", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.direction = "horizontal"
  ) +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")) +
  labs(title = "Graphique de type la rose des vents", y = "Valeurs")

