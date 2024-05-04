#tri croisés de PROTEGO avec variables d'état (PCS et sexe)
library(questionr)
library(foreign)
library(ggplot2)

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

#tri croisé PROTEGO SEXE
table(bnum22$SEXE, useNA="ifany")
table (bnum22$PROTEGO, bnum22$SEXE, useNA="ifany")
lprop(table(bnum22$PROTEGO, bnum22$SEXE, useNA="ifany"))
chisq.test(table (bnum22$PROTEGO, bnum22$SEXE, useNA="ifany"))

#tableaux PROTEGO et PCSc
write.csv2(table(bnum22$PCSc, bnum22$PROTEGO, useNA="ifany"), 
           file="Sorties/PROTE_PCS_eff.csv", fileEncoding = "UTF-8")
write.csv2(lprop(table(bnum22$PCSc, bnum22$PROTEGO, useNA="ifany")), 
           file="Sorties/PROTE_PCS_lprop.csv", fileEncoding = "UTF-8")

#diagrammes à barre PROTEGO et SEXE
PROTSEXE <- ggplot(bnum22) + aes(x=PROTEGO, weight=POND, fill=SEXE) +
  geom_bar(position="fill") + 
  labs(title="Diagramme à barre de l'attention à la protection des données personnelles selon le sexe",
       x="Attention à la protection des données personnelles", y="Effectif (en %)",
       caption="Source : Baromètre du numérique 2022") + theme_bw()
ggsave("Sorties/yipee.png", plot=PROTSEXE, width=9, height=4)

#diagramme à barres PCS PROTEGO

graph_export <- ggplot(bnum22, aes(x = PCSc, fill = PROTEGO)) +
  geom_bar(position = "dodge") +
  labs(x = "Groupe socioprofessionnel", y = "Nombre d'observations", fill = "Vigilance envers les données") +
  ggtitle("Vigilance envers les données par groupe socioprofessionnel") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#bon en fait il sert pas à grand chose

ggsave("sorties/Graphique_PCS.png", plot = graph_export, width = 6, height = 3, bg = "white")

#Cookies ?
table(bnum22$COOKIE, useNA="ifany")
## Recodage de bnum22$COOKIE
bnum22$COOKIE_rec <- bnum22$COOKIE
bnum22$COOKIE_rec[bnum22$COOKIE == "Le moins souvent possible"] <- "Peu"
bnum22$COOKIE_rec[bnum22$COOKIE == "N'a pas internet fixe à domicile"] <- "N'a pas internet"
bnum22$COOKIE_rec[bnum22$COOKIE == "Régulièrement"] <- "Souvent"
bnum22$COOKIE_rec[bnum22$COOKIE == "Systématiquement"] <- "Toujours"

## Réordonnancement de bnum22$COOKIE_rec
bnum22$COOKIE_rec <- factor(bnum22$COOKIE_rec,
  levels = c("N'a pas internet", "Jamais", "Peu", "Souvent", "Toujours")
)

#tri croisé
table(bnum22$PROTEGO, bnum22$COOKIE_rec, useNA="ifany")
lprop(table(bnum22$PROTEGO, bnum22$COOKIE_rec, useNA="ifany"))
chisq.test(table(bnum22$PROTEGO, bnum22$COOKIE_rec, useNA="ifany"))
cookieprot <- table(bnum22$PROTEGO, bnum22$COOKIE_rec, useNA="ifany")
cramer.v(cookieprot)
ggplot(bnum22) + aes(x=COOKIE_rec, weights=POND, fill=PROTEGO) +
  geom_bar(position="dodge") + labs(x="Acceptation des cookies", y="Effectifs",
                                    fill="Attention à la protection des données")
