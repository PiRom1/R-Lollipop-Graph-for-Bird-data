setwd("~/Documents/R&Stats/Pour Bichette/Tableaux_abondances")
library(openxlsx)
library(dplyr)

ajout_annee_enclos = function(annee,enclos,don) {
  nom_tab = paste(annee,enclos,sep = "_")
  nom_tab = paste(nom_tab,"xlsx",sep = ".")
  don2 = read.xlsx(xlsxFile = nom_tab,sep = ",")
  
  don2 = cbind(don2,annee = rep(annee,nrow(don2)))
  don2 = cbind(don2,enclos = rep(enclos,nrow(don2)))
  
  don = rbind(don,don2)
  return(don)
}

don = data.frame()

years = c(2004:2012)
modalite_enclos = c("in","out")

for (annee in years) {
  for (enclos in modalite_enclos) {
    don = ajout_annee_enclos(annee,enclos,don)
    
  }
}

# Uniformisation noms


nom_a_changer = c("Buse","Corneille","Coucou","Etourneau","Geai","Gobe mouche gris","Grimpereau","Gros-bec","Huppe",
                  "Hypolaïs","Merle","Pie","Pinson","Sitelle","Troglodyte","Verdier","Loriot")

nouveau_nom = c("Buse variable","Corneille noire","Coucou gris","Etourneau sansonnet","Geai des chênes",
                "Gobemouche gris","Grimpereau des jardins","Gros-bec casse noyaux","Huppe fasciée",
                "Hypolaïs polyglotte","Merle noir","Pie bavarde","Pinson des arbres","Sitelle torchepot",
                "Troglodyte mignon","Verdier d'Europe","Loriot d'Europe")



for (nom in c(1:length(nom_a_changer))) {
  
  don = don %>% mutate(Espèces = ifelse(Espèces==nom_a_changer[nom],nouveau_nom[nom],Espèces))
  
}


# Ajout guilde déchaînée

guilde_chenes = c("Fauvette à tête noire","Pinson des arbres","Pouillot véloce",
                  "Grimpereau des jardins","Loriot d'Europe","Mésange bleue",
                  "Sitelle torchepot","Pic épeiche","Pic épeichette",
                  "Troglodyte mignon","Mésange à longue queue","Mésange charbonnière",
                  "Coucou gris","Roitelet à triple bandeau","Pic vert","Pic mar",
                  "Fauvette des jardins","Mésange nonette")

don = don %>% mutate(Guilde_chenes = Espèces %in% guilde_chenes)


don$enclos = as.factor(don$enclos)
don$Espèces = as.factor(don$Espèces)

oiseau = sample(don$Espèces,1)
ggplot(data = filter(don,Espèces==oiseau),aes(x = annee,y=Abondance,col = enclos)) + geom_line() + geom_point() + ylab(oiseau)
ggplot(data = don,aes(x = annee,y=Abondance,col = Espèces)) + geom_line() + geom_point()

ggplot(data = don,aes(x = enclos,y = Abondance,fill = Guilde_chenes)) + geom_boxplot() + scale_fill_viridis_c()


# Exemple corrélation sur la sittelle
out = filter(don,Espèces == "Sittelle") %>% filter(enclos == "out")
inn = filter(don,Espèces == "Sittelle") %>% filter(enclos == "in")
cor.test(out$Abondance,inn$Abondance)

test_cor = function(bird_name) {
  out = filter(don,Espèces == bird_name) %>% filter(enclos == "out")
  inn = filter(don,Espèces == bird_name) %>% filter(enclos == "in")
  if (nrow(out)!=nrow(inn)) {
    return(" /!\ Pas autant d'observations pour enclos in et out")
  }
  print(cor.test(out$Abondance,inn$Abondance))
  ggplot(data = filter(don,Espèces==bird_name),aes(x = annee,y=Abondance,col = enclos)) + geom_line() + geom_point() + ylab(bird_name)
}

