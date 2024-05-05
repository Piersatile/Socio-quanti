##PRODUCTION DE LA STATISTIQUE DU TITRE 

# tri à plat 
tri_plat <- table(bnum22$PROTEGO, useNA="ifany")
print(tri_plat)

# Calculer le pourcentage d'individus vigilants
pourcentage_vigilants <- prop.table(tri_plat)["Très vigilant"] * 100
print(pourcentage_vigilants)
