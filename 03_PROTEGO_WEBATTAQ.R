library("questionr")
library("foreign")

bnum <- barometre_du_numerique_2007_2022
rm(barometre_du_numerique_2007_2022)

table(bnum$WEBATTAQ)
warnings(bnum$WEBATTAQ)
table(bnum$WEBATTAQ,bnum$ANNEE)

bnum22 <- barometre_du_numerique_2022
rm(barometre_du_numerique_2022)

table(bnum22$WEBATTAQ)

table(bnum22$WEBATTAQ, bnum22$PROTEGO, useNA = "ifany")

bnum22$ATTAQ <- NA
bnum22$ATTAQ[bnum22$WEBATTAQ=="Non, ce n\x92est pas possible"] <- "N'a jamais subi d'attaque sur ses données"
bnum22$ATTAQ[bnum22$WEBATTAQ=="Non, c\x92est peu probable"]<- "Ne pense pas avoir subi d'attaque sur ses données"
bnum22$ATTAQ[bnum22$WEBATTAQ=="Oui, c\x92est probable"]<- "Pense avoir subi une attaque sur ses données"
bnum22$ATTAQ[bnum22$WEBATTAQ=="Oui, c\x92est s\xfbr"]<- "A déjà subi une attaque sur ses données"  

table(bnum22$ATTAQ)

bnum22$VIGI <- NA
bnum22$VIGI[bnum22$PROTEGO=="N'a pas internet fixe \xe0 domicile"] <- "N'a pas internet"
bnum22$VIGI[bnum22$PROTEGO=="Non"]<- "Pas vigilant"
bnum22$VIGI[bnum22$PROTEGO=="Oui, et je suis tr\xe8s vigilant"]<- "Très vigilant"
bnum22$VIGI[bnum22$PROTEGO=="Oui, j'y pense de temps en temps"]<- "Peu vigilant"

table(bnum22$VIGI)

table(bnum22$ATTAQ, bnum22$VIGI, useNA = "ifany")

lprop(table(bnum22$ATTAQ, bnum22$VIGI, useNA = "ifany"))

chisq.test(table(bnum22$ATTAQ, bnum22$VIGI, useNA = "ifany"))

write.csv2(lprop(table(bnum22$ATTAQ, bnum22$VIGI, useNA = "ifany"))
)

write.csv2(lprop(table(bnum22$ATTAQ, bnum22$VIGI, useNA = "ifany")), 
           file = "ATTAQxVIGI.csv")



