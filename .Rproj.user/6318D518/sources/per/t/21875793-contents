#tri croisés de PROTEGO avec variables d'état (PCS et sexe)

table (bnum22$PROTEGO, useNA="ifany")

## Recodage de bnum22$PROTEGO
bnum22$PROTEGO <- as.character(bnum22$PROTEGO)
bnum22$PROTEGO[bnum22$PROTEGO == "N'a pas internet fixe à domicile"] <- "N'a pas internet"
bnum22$PROTEGO[bnum22$PROTEGO == "Non"] <- "Pas vigilant"
bnum22$PROTEGO[bnum22$PROTEGO == "Oui, j'y pense de temps en temps"] <- "Peu vigilant"
bnum22$PROTEGO[bnum22$PROTEGO == "Oui, et je suis très vigilant"] <- "Très vigilant"

## Réordonnancement de bnum22$PROTEGO
bnum22$PROTEGO <- factor(bnum22$PROTEGO,
  levels = c("N'a pas internet", "Pas vigilant", "Peu vigilant", "Très vigilant")
)

# PCS
table(bnum22$PCS8EGO)

# Nom des moda
moda_PCS <- unique(bnum22$PCS8EGO)
moda_PCSc <- c("Employé","Au foyer","Inactif", "PI",
               "CPIS", "Retraités", "AE ACCE", "Ouvrier")

bnum22$PCSc <- NA
for (i in 1:length(moda_PCS)){ 
  bnum22$PCSc[bnum22$PCS8EGO==moda_PCS[i]] <- moda_PCSc[i]
}
table(bnum22$PCSc,bnum22$PCS8EGO, useNA = 'ifany')

#tri croisé PROTEGO PCS
table(bnum22$PROTEGO, bnum22$PCSc, useNA="ifany")
lprop(table(bnum22$PROTEGO, bnum22$PCSc, useNA="ifany"))
chisq.test(table(bnum22$PROTEGO, bnum22$PCSc, useNA="ifany")) #on peut 

#tableaux PROTEGO et PCSc
write.csv2(table(bnum22$PCSc, bnum22$PROTEGO, useNA="ifany"), 
           file="Sorties/PROTE_PCS_eff.csv", fileEncoding = "UTF-8")
write.csv2(lprop(table(bnum22$PCSc, bnum22$PROTEGO, useNA="ifany")), 
           file="Sorties/PROTE_PCS_lprop.csv", fileEncoding = "UTF-8")

lprop_pcs_protego <- as.data.frame(lprop(table(bnum22$PCSc, bnum22$PROTEGO, useNA="ifany")))

#diagramme à barres PCS PROTEGO

graph_export <- ggplot(data = as.data.frame(lprop_pcs_protego), aes(x = Var1, y = Freq, fill = Var2)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Groupes socioprofessionnels", y = "Pourcentage", fill = "Vigilance envers les données") +
  ggtitle("Vigilance envers les données par groupe socioprofessionnel") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave("Sorties/Graphique_PCS.png", plot = graph_export, width = 6, height = 3, bg = "white")
