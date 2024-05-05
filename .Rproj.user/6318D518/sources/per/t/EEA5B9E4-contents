library("questionr")
library("foreign")
library("ggplot2")

# Définir le dossier de travail (à modifier selon l'emplacement)
setwd("Documents/GitHub/Socio-quanti")


# Charger la base entière (à modifier selon l'emplacement aussi)
bnum <- read.csv2("Data/barometre-du-numerique-2007-2022.csv", sep = ",",
                  quote = "\"", dec = ".", fileEncoding = "CP1252")

# Restreindre à 2022
bnum22 <- bnum[bnum$ANNEE==2022, ]
