##########################################
#   CPES - L2 - Sociologie quantitative  #
#   Script O - Groupe 1                  #
#   Recodage                             #
#   PA BILBAUT                           #
##########################################


library("questionr")
library("foreign")


# Définir le dossier de travail
setwd("~/Bureau/socio quanti")
## ATTENTION ! Cette ligne doit être modifiée en fonction de l'organisation
## des fichiers sur votre ordinateur


# Charger la base entière
bnum <- read.csv2("Data/barometre-du-numerique-2007-2022.csv", sep = ",",
                  quote = "\"", dec = ".", fileEncoding = "CP1252")
## ATTENTION ! Cette ligne doit être modifiée en fonction de l'organisation
## des fichiers sur votre ordinateur


## Recodage : changer le type de variables
bnum$IDENTc <- bnum$IDENT
bnum$IDENTc <- as.character(bnum$IDENTc)


# Variable qui distingue les +, et les - de 50 ans
bnum$plus50 <- NA
bnum$plus50[bnum$AGE<50] <- "Moins de 50 ans"
bnum$plus50[bnum$AGE>=50] <- "50 ans et plus"
bnum$plus50 <- factor(bnum$plus50,
                      levels = c("Moins de 50 ans", "50 ans et plus"))


## Créer une variable ayant 4 modalités
# Femmes / Hommes et Moins de 50 ans et 50 ans et plus
bnum$HF50 <- NA

bnum$HF50[bnum$SEXE=="Femme" & bnum$AGE<50] <- "Femme de moins de 50 ans"
bnum$HF50[bnum$SEXE=="Femme" & bnum$AGE>=50] <- "Femme de 50 ans et plus"
bnum$HF50[bnum$SEXE=="Homme" & bnum$AGE<50] <- "Homme de moins de 50 ans"
bnum$HF50[bnum$SEXE=="Homme" & bnum$AGE>=50] <- "Homme de 50 ans et plus"

bnum$HF50 <- factor(bnum$HF50,
                    levels = c("Femme de moins de 50 ans","Femme de 50 ans et plus",
                               "Homme de moins de 50 ans","Homme de 50 ans et plus"))


# Restreindre à 2022
bnum22 <- bnum[bnum$ANNEE==2022, ]
