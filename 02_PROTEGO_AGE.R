##TRI CROISE AGE ET PROTEGO


# Calcul des quintiles de l'âge
quintiles_age <- quantile(bnum22$AGE, probs = seq(0, 1, by = 0.2), na.rm = TRUE)

# Définition des étiquettes basées sur les intervalles d'âge
labels <- c()
for (i in 1:(length(quintiles_age) - 1)) {
  labels[i] <- paste(quintiles_age[i], "à", quintiles_age[i + 1], "ans", sep = " ")
}

# Création d'une nouvelle variable pour les quintiles de l'âge avec les étiquettes personnalisées
bnum22$AGE_QUINTILE <- cut(bnum22$AGE, breaks = quintiles_age, labels = labels, include.lowest = TRUE)

# Tri croisé 
table_age_protego <- table(bnum22$AGE_QUINTILE, bnum22$PROTEGO, useNA = 'ifany')
table_age_protego

# Pourcentage en ligne
lprop_age_protego <- lprop(table_age_protego)
lprop_age_protego <- round(lprop_age_protego, digits = 2)# cela permet d'avoir des résultats arrondis à deux chiffres après la virgule dans le tableau extrait par la suite

lprop_age_protego

# Pourcentage en colonne
cprop_age_protego <- cprop(table_age_protego)

cprop_age_protego

# Test du khi2
chisq.test(table_age_protego)


# Extraction du tableau
write.csv2(lprop_age_protego, file = "Sorties/AGE_QUINTILE_PROTEGO_lprop.csv", fileEncoding = "UTF-8")
