
library(openxlsx)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggalt)

setwd("~/Documents/R&Stats/Pour Bichette/Tableaux_abondances")

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

years = c(2004:2016)
modalite_enclos = c("in","out")

for (annee in years) {
  for (enclos in modalite_enclos) {
    don = ajout_annee_enclos(annee,enclos,don)
    
  }
}

# Uniformisation noms


nom_a_changer = c("Buse","Corneille","Coucou","Etourneau","Geai","Gobe mouche gris","Grimpereau","Gros-bec","Huppe",
                  "Hypolaïs","Merle","Pie","Pinson","Sitelle ","Troglodyte","Verdier","Loriot",
                  "proyer","Rougegorge","Tarier patre","Hypolaïs","Hypolais polyglotte","Huppe fascié","Alouette lulu ",
                  "Roitelet à trible bandeau","Pic vert ","Grand Corbeau","Sittelle ","Tarier patre ")

nouveau_nom = c("Buse variable","Corneille noire","Coucou gris","Etourneau sansonnet","Geai des chênes",
                "Gobemouche gris","Grimpereau des jardins","Gros-bec casse noyaux","Huppe fasciée",
                "Hypolaïs polyglotte","Merle noir","Pie bavarde","Pinson des arbres","Sitelle torchepot",
                "Troglodyte mignon","Verdier d'Europe","Loriot d'Europe","Bruant proyer","Rouge-gorge",
                "Tarier pâtre",rep("Hypolaïs polyglotte",2),"Huppe fasciée","Roitelet à triple bandeau",
                "Alouette lulu","Pic vert","Grand corbeau","Sittelle torchepot","Tarier pâtre")



for (nom in c(1:length(nom_a_changer))) {
  
  don = don %>% mutate(Espèces = ifelse(Espèces==nom_a_changer[nom],nouveau_nom[nom],Espèces))
  
}


# Ajout guilde des chênaies

guilde_chenes = c("Fauvette à tête noire","Pinson des arbres","Pouillot véloce",
                  "Grimpereau des jardins","Loriot d'Europe","Mésange bleue",
                  "Sittelle torchepot","Pic épeiche","Pic épeichette",
                  "Troglodyte mignon","Mésange à longue queue","Mésange charbonnière",
                  "Coucou gris","Roitelet à triple bandeau","Pic vert")




don = don %>% mutate(Guilde_chenes = Espèces %in% guilde_chenes)


don$enclos = as.factor(don$enclos)
don$Espèces = as.factor(don$Espèces)

cor_guilde = function(guilde) {
  p.val = c()
  cor = c()
  bird = c()
  for (bird_name in guilde) {
    
    out = filter(don,Espèces == bird_name) %>% filter(enclos == "out")
    inn = filter(don,Espèces == bird_name) %>% filter(enclos == "in")
    print(out)
    print(inn)
    
    if ( (length(out$Abondance) == 0 ) || (length(inn$Abondance)==0) ) {
      next
    }
    join = inner_join(out,inn,by = c("annee"))
    print(join)
    
    if (nrow(join) == 1) {
      next
    }
    bird = c(bird,bird_name)
    test = cor.test(join$Abondance.x,join$Abondance.y)
    cor = c(cor,test$estimate)
    p.val = c(p.val,test$p.value)
    
    
    
  }
  print(bird)
  don_cor = data.frame(Espèces = bird,cor = cor,p.val = p.val) %>% mutate(significatif = p.val<0.05)
  
  
  return(don_cor)
}


don_cor = cor_guilde(guilde_chenes)

don_cor = mutate(don_cor,cor_negative = cor<0)

#Non colored graph
ggplot(data = don_cor,aes(x = Espèces,y = cor,col = cor_negative,shape = significatif)) + geom_lollipop(size = 2) +
  labs(col = c(""),shape = " ") + scale_color_discrete(labels = c("Corrélation positive","Corrélation négative")) + 
  scale_shape_discrete(labels = c("Corrélation non significative","Corrélation significative")) + ylab("Corrélation") + 
  geom_hline(yintercept = 0,col = 'black',linetype = "dashed") + theme(legend.text = element_text(size = 15),axis.text.x = element_text(angle = 45,size = 15),title = element_text(size = 15),axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15)) +
  ggtitle("Corrélation entre l'évolution des abondances dans ou hors enclos, \n pour la guilde des insectivores des chênaies") 

#Colored graph
ggplot(data = don_cor,aes(x = Espèces,y = cor,col = cor_negative,shape = significatif)) +
  geom_rect(xmin = 0,xmax = 500,ymin = 0,ymax = 500,alpha = 0.3,fill = '#f5b7b1') +
  geom_rect(xmin = 0,xmax = 500,ymin = -500, ymax = 0,alpha = 0.3,fill = "#76d7c4") + geom_lollipop(size = 2) +
  labs(col = c(""),shape = " ") + scale_color_discrete(labels = c("Corrélation positive","Corrélation négative")) + 
  scale_shape_discrete(labels = c("Corrélation non significative","Corrélation significative")) + ylab("Corrélation") + 
  geom_hline(yintercept = 0,col = 'black',linetype = "dashed") + theme(legend.text = element_text(size = 15),axis.text.x = element_text(angle = 45,size = 15),title = element_text(size = 15),axis.title.x = element_text(size = 15),axis.title.y = element_text(size = 15)) +
  ggtitle("Corrélation entre l'évolution des abondances dans ou hors enclos, \n pour la guilde des insectivores des chênaies") 
  



