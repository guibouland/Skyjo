#tech col evo

library(ggplot2)
library(tidyr)

cartes_dispo= c(rep(-2,5),rep(-1,10),rep(0,15), rep(1:12,10))

initial <- function(){
  tabnum <- matrix(NA, nrow=3,ncol=4)
  tabbool <- matrix(0, nrow=3,ncol=4)
  cartes_dispo= c(rep(-2,5),rep(-1,10),rep(0,15), rep(1:12,10))
  count_col=0
  for (i in 1:3) {
    for (j in 1:4) {
      u <- sample(cartes_dispo,1)
      tabnum[i,j] <- u
      num <- which(cartes_dispo==u) # quelle iteration?
      cartes_dispo <- cartes_dispo[-num[1]]#Retirer la carte utilisée
    }
    #print(length(cartes_dispo))
  }
  defausse <- sample(cartes_dispo,1)
  num <- which(cartes_dispo==defausse) # quelle iteration?
  cartes_dispo <- cartes_dispo[-num[1]]#Retirer la carte utilisée
  tabbool[1,1]<-1
  tabbool[1,2]<-1
  
  return(list(tabnum,tabbool,cartes_dispo,defausse))
}

maxpos <- function(tabnum, tabbool) {
  # Trouver les indices où tabbool est égal à 1
  indices_avec_1 <- which(tabbool == 1, arr.ind = TRUE)
  
  # Filtrer les valeurs de tabnum correspondantes
  valeurs_associées <- tabnum[indices_avec_1]
  
  # Trouver l'indice du maximum parmi les valeurs associées à 1 dans tabbool
  indice_max <- which.max(valeurs_associées)
  
  # Récupérer l'indice (ligne, colonne) du maximum dans la matrice
  ligne <- indices_avec_1[indice_max, 1]
  colonne <- indices_avec_1[indice_max, 2]
  
  # Récupérer la valeur maximale
  valeur_max <- tabnum[ligne, colonne]
  
  # Retourner le maximum et sa position
  return(c(valeur_max, ligne, colonne))
}

sumspeciale <- function(tabnum, tabbool) {
  # Filtrer les éléments de tabnum associés à des 1 dans tabbool
  valeurs_associees <- tabnum[(tabbool == 1) || (tabbool == 2)]
  
  # Calculer la somme de ces valeurs
  somme <- sum(valeurs_associees)
  
  # Retourner la somme
  return(somme)
}



interet <- function(tabnum, tabbool) {
  # Initialiser la matrice d'intérêt
  matrice_interet <- matrix(NA, nrow = 2, ncol = 4)
  # Parcourir chaque colonne de tabnum
  for (j in 1:4) {
    petiteliste<-tabbool[,j]
    indices_avec_1 <- which(petiteliste == 1)
    #print(indices_avec_1)
    valeurs_assoc <- tabnum[indices_avec_1,j]
    if (length(valeurs_assoc)!=0){
      #print("valeurs_assoc:")
      #print(valeurs_assoc)
      comptage <- table(valeurs_assoc)
      #print("comptage:")
      #print(comptage)
      nb_occurrences <- max(comptage)
      valeur_plusprez <- min(as.numeric(names(comptage)[which(comptage == nb_occurrences)]))
      #print("valeur plus preze")
      #print(as.numeric(names(comptage)[which(comptage == nb_occurrences)]))
      #print(valeur_plusprez)
      matrice_interet[1, j] <- valeur_plusprez
      matrice_interet[2, j] <- nb_occurrences
    }
  }
  # Retourner la matrice d'intérêt
  return(matrice_interet)
}



condition <- function(tabnum, tabbool, carte, seuil) {
  # Si on pioche une carte qui ne vaut aucune de celles retournées, on la place à la suite (nouvelle col pour les deux premières)
  indices_avec_1 <- which(tabbool == 1, arr.ind = TRUE)
  valeurs_assoc <- tabnum[indices_avec_1]
  for (j in 1: 4) {
    pos<-0
    non_carte<-0
    if (carte %in% valeurs_assoc) {
      pos <- which(tabnum[,j] == carte & tabbool[,j]==1, arr.ind = TRUE) # pos qui sont retournées dans la colonne j
      non_carte <- suppressWarnings(which(tabnum[,j] != tabnum[pos, j] | tabbool[,j]==0, arr.ind = TRUE)) # pos qui ne sont pas la carte mais qui sont retournées (sinon c inutile)
      if (length(pos) == 2 && carte>0)  {
        return(c(0, non_carte, j))
      }
      else if (carte <= seuil) {
        place <- place(tabbool)
        return(c(2, place[1], place[2]))
      }
    }
    else if (carte <= seuil && sum(tabbool)<11){
      place <- place(tabbool)
      return(c(2, place[1], place[2]))
    }
  }
  if(sum(tabbool)==11){
    place <- maxpos(tabnum,tabbool)
    return(c(2, place[2], place[3]))
  }
  else{return(c(-1))}
}

place<- function(tabbool){
  for(i in 1:nrow(tabbool)){
    for(j in 1:ncol(tabbool)){
      if(tabbool[i,j]==0)
      {
        return(c(i,j))
      }
    }
  }
}

sum_sans_col <- function(tabnum){
  sum<-0
  count <-0
  for(j in 1:ncol(tabnum)){
    occurrences <- table(tabnum[, j])
    if(any(occurrences != 3)==TRUE){
      sum <-sum+ sum(tabnum[,j])
    }
    else{ count <- count+1}
  }
  return(c(sum,count))
}

score_alpha_solodoub <- function(a=3,b=4, seuil){
  init <-initial()
  tabnum<-do.call(rbind,init[1])
  tabbool<-do.call(rbind,init[2])
  cartes_dispo<-do.call(rbind,init[3])
  defausse<-as.numeric(init[4])
  score_tem <- list()
  score_tem <- c(score_tem, sum(tabnum)[1])
  cpteur<-0
  stop <- round(runif(1, 15,30),0)
  while(cpteur<stop && sum(tabbool)<12){
    indices_avec_1 <- which(tabbool == 1, arr.ind = TRUE)
    valeurs_assoc <- tabnum[indices_avec_1]
    if(defausse %in% valeurs_assoc || defausse <= seuil){
      u <- defausse
    }
    else{
      u<- sample(cartes_dispo, 1)  # Sélectionner un index aléatoire dans cartes_dispo
      num <- which(cartes_dispo==u) # quelle iteration?
      cartes_dispo <- cartes_dispo[-num[1]]
    }
    a<-condition(tabnum,tabbool,u, seuil)
    if(a[1]==0 || a[1]==1){
      tabnum[a[2],a[3]] <- u
      tabbool[a[2],a[3]] <- 1
    }
    else if (a[1]==-1){
      pos <- place(tabbool)
      tabbool[pos[1],pos[2]] <- 1
    }
    else if (a[1]==2) {
      tabnum[a[2],a[3]] <- u
      tabbool[a[2],a[3]] <- 1
    }
    score_tem <- c(score_tem, sum_sans_col(tabnum)[1])
    defausse <- as.numeric(sample(cartes_dispo,1))
    num <- which(cartes_dispo==defausse) # quelle iteration?
    cartes_dispo <- cartes_dispo[-num[1]]#Retirer la carte utilisée
    cpteur<-cpteur+1
  }
  countcol<-sum_sans_col(tabnum)[2]
  return(list(score_tem,countcol))
}

#score_alpha_solodoub(3,4, 12)



#resultats_alpha_doub <- replicate(100, score_alpha_solodoub(20, 3,4))
#a<- resultats_alpha_doub
#n<-100

normalisation <- function(listdeliste) {
  result <- lapply(listdeliste, function(inner_list) {
    if(length(inner_list) > 40) {
      inner_list <- inner_list[1:40]  # Raccourcir la liste à 30 éléments
    } else if(length(inner_list) < 40) {
      inner_list <- c(inner_list, rep(inner_list[length(inner_list)], 40 - length(inner_list)))  # Remplir avec la dernière valeur
    }
    return(inner_list)
  })
  return(result)
}

#b<-normalisation(a)

get_ith_element <- function(liste, i) {
  elements <- c()  # Vecteur pour stocker les éléments
  
  # Parcourir chaque ligne de la liste de listes
  for (ligne in liste) {
    # Extraire le i-ème élément de la ligne
    element <- ligne[[i]]
    # Ajouter l'élément au vecteur
    elements <- c(elements, element)
  }
  
  return(elements)
}

sc_moy_alpha <- function(b, n) {
  my_list <- list()
  for(i in 1:39) {
    sum <- 0
    for(j in 1:n) {
      sum <- sum + b[[j]][[i]]
    }
    coli<-get_ith_element(b,i)
    moyenne <- sum/n
    ecart_type <- sd(coli)
    IC_99 <- c(moyenne - 2.58 * (ecart_type/sqrt(n)),
               moyenne + 2.58 * (ecart_type/sqrt(n)))
    my_list <- c(my_list, sum/n, IC_99)
  }
  return(my_list)
}



sc_moy_tot <- function(n, seuil) {
  aa<-list()
  resultats<- replicate(n, score_alpha_solodoub(3, 4, seuil))
  b<-normalisation(resultats[1,])
  d <- mean(as.numeric(resultats[2,]))
  return(list(sc_moy_alpha(b, n),d))
}
score_alpha_solodoub(3, 4, 12)

#aaa <- sc_moy_tot(2, 12)
#aaa





#time <- Sys.time()
#vasystp<-sc_moy_tot(100, 5)
##time2 <- Sys.time()
#time2-time
vasystp120<-sc_moy_tot(10000, 12)
vasystp12<-vasystp120[[1]]
countcol12 <- vasystp120[[2]]
vasystp40<-sc_moy_tot(10000, 3)
vasystp4<-vasystp40[[1]]
countcol4 <- vasystp40[[2]]
#
#
vasystp_pas_de_3_12 <- vasystp12[seq(1, length(vasystp12), by = 3)]
vasystp_pas_de_3_4 <- vasystp4[seq(1, length(vasystp4), by = 3)]
vasymax12<-vasystp12[seq(0, length(vasystp12), by = 3)]
vasymin12<-vasystp12[seq(2, length(vasystp12), by = 3)]
vasymax4<-vasystp4[seq(0, length(vasystp4), by = 3)]
vasymin4<-vasystp4[seq(2, length(vasystp4), by = 3)]

vasystp4

#mat12

mat12<- (unlist(vasystp_pas_de_3_12))
mat4<- (unlist(vasystp_pas_de_3_4))
df <- data.frame(x=0:38, y12=mat12,lower12=unlist(vasymin12),upper12=unlist(vasymax12), y4=mat4,lower4=unlist(vasymin4),upper4=unlist(vasymax4))
#df
ggplot(df, aes(x = x)) +
  # Ajout des lignes pour y1 et y2 avec les noms de légende
  geom_line(aes(y = y12, color = "seuil 12"),linewidth=0.85) +
  geom_line(aes(y = y4, color = "seuil 3"),linewidth=0.85) +
  # Ajout des bandes avec les noms de légende
  #geom_ribbon(aes(ymin = lower12, ymax = upper12, fill = "seuil 12"), alpha = 0.3) +
  #geom_ribbon(aes(ymin = lower4, ymax = upper4, fill = "seuil 3"), alpha = 0.3) +
  # Autres paramètres esthétiques
  labs(x = "Tour", y = "Score moyen") +
  # Définition des couleurs et remplissages manuels
  scale_color_manual(name = "Seuil", values = c("seuil 12" = "#000004", "seuil 3" = "#eb5760")) +
  scale_fill_manual(name = "Seuil", values = c("seuil 12" = "#000004", "seuil 3" = "#eb5760")) +
  # Positionnement de la légende en haut à droite
  theme(legend.position = "topright") +
  # Utilisation d'un thème minimal
  coord_cartesian(xlim = c(0, 30))+
  scale_x_continuous(breaks = seq(0,60,5))+
  scale_y_continuous(breaks = seq(5,60,5))+
  theme_minimal()

#
#vasystp_pas_de_3 <- vasystp[seq(1, length(vasystp), by = 3)]
#vasymax<-vasystp[seq(0, length(vasystp), by = 3)]
#vasymin<-vasystp[seq(2, length(vasystp), by = 3)]
#
## enlever la premiere ligne de vasystp
#
#mat<- (unlist(vasystp_pas_de_3))
#df <- data.frame(x=1:39, y=mat,lower=unlist(vasymin),upper=unlist(vasymax))
#df
#ggplot(df, aes(x = x, y = mat)) +
#  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "orange", alpha = 0.3) +  # Zone de confiance
#  geom_line(linewidth=0.5) +
#  labs(x = "Tour",y ="Score moyen")+
#  theme_minimal()




ecart_types <- function(normalised){
  derniers_elements <- sapply(normalised, function(x) tail(x, 1))
  #print(derniers_elements)
  # Calculer l'écart-type des derniers éléments
  ecart_type <- sd(as.numeric(derniers_elements))
  
  # Retourner l'écart-type
  return(ecart_type)
}



sc_moy_tot_2 <- function(n) {
  aa <- list()
  bb <- list()
  for (i in -2:12) {
    resultats <- replicate(n, score_alpha_solodoub(3, 4, i))
    b <- normalisation(resultats)
    #print("1")
    aa <- c(aa, sc_moy_alpha(b, n))
    #print("2")
    bb <- c(bb, ecart_types(b))
    print(ecart_types(b))
    #print("3")
  }
  return(c(aa, bb))
}

N <- 100

time <- Sys.time()
colbase<-sc_moy_tot_2(N)
Sys.time() - time

colbase <- colbase

write.table(colbase, file = "colbase.txt", sep = "\t", row.names = FALSE)

#View(colbase)
vasystp<- colbase[-(length(colbase) - 14):-length(colbase)]
#vasystp
ectyp <- colbase[(length(colbase) - 14):length(colbase)]
moyprgrph <- vasystp[seq(from=115, to=length(vasystp), by =117)]

datavase <- matrix(NA,3,15)

for(i in 1:15){
  datavase[1,i] <- moyprgrph[[i]]
  datavase[2,i] <- moyprgrph[[i]]-2.58*(ectyp[[i]]/sqrt(N))
  datavase[3,i] <- moyprgrph[[i]]+2.58*(ectyp[[i]]/sqrt(N))
}

library(xtable)

# Créer un objet xtable à partir de la matrice datavase
datavase_tablecolintel <- xtable(datavase)
print(datavase_tablecolintel, include.rownames = TRUE, include.colnames = TRUE, floating = FALSE)


data <- data.frame(essai = 1:15, 
                   moyenne = datavase[1,], 
                   inf = datavase[2,], 
                   sup = datavase[3,])

# Définition des étiquettes pour l'axe horizontal
labels <- c("-2", "-1", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")

my_colors <- c("#fcfdbf", "#fddc9e", "#febb81", "#fd9a6a", "#f8765c", "#eb5760", "#d3436e", "#b73779", "#982d80", "#7b2382", "#5f187f", 
               "#400f74", "#221150", "#0c0926", "#000004")


ggplot(data, aes(x = essai, y = moyenne)) +
  geom_col(fill = my_colors) +
  geom_errorbar(aes(ymin = inf, ymax = sup), width = 0.4, color = "darkgrey", linewidth = 0.5) +
  labs(x = "Seuil", y = "Score moyen", fill = " ") +
  scale_x_continuous(breaks = 1:15, labels = labels) +
  scale_y_continuous(
    name = "Score moyen",
    breaks = seq(0, 35, 5)
  ) +
  theme_minimal()


