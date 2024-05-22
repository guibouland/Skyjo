library(ggplot2)
library(tidyr)

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

initial2 <- function(){
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
  tabbool[2,1]<-1
  
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

condition <- function(tabnum, tabbool, carte) {
  # Si on pioche une carte qui ne vaut aucune de celles retournées, on la place à la suite (nouvelle col pour les deux premières)
  indices_avec_1 <- which(tabbool == 1, arr.ind = TRUE)
  valeurs_assoc <- tabnum[indices_avec_1]
  for (j in 1: 4) {
    pos<-0
    non_carte<-0
    if (carte %in% valeurs_assoc) {
      pos <- which(tabnum[,j] == carte & tabbool[,j]==1, arr.ind = TRUE) # pos qui sont retournées dans la colonne j
      non_carte <- suppressWarnings(which(tabnum[,j] != tabnum[pos, j] | tabbool[,j]==0, arr.ind = TRUE)) # pos qui ne sont pas la carte mais qui sont retournées (sinon c inutile)
      if (length(pos) == 2) {
        return(c(0, non_carte, j))
      }
      else if (length(pos) == 1) {
        # on prend le premier des deux
        return(c(1, non_carte[1], j))
      }
    }
  }
  return(c(-1))
}

#matrice <- matrix(c(1, 2, 3, 4,
##                    5, 11, 7, 8,
##                    9, 11, 11, 10), nrow = 3)
##pos<-which(matrice[,4] == 11 & matbool[,4]==1, arr.ind = TRUE)
##pos
##non_carte <- suppressWarnings(which(matrice[,4] != matrice[pos, 4] | matbool[,4]==0, arr.ind = TRUE)) # pos qui ne sont pas la carte mais qui sont retournées (sinon c inutile)
##non_carte
##matbool <- matrix(c(0, 0, 0, 0,
##                    0, 0, 0, 0,
##                    0, 1, 0, 0), nrow = 3)
##
#condition(matrice,matbool,11)



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

interet <- function(tabnum, tabbool) {
  matrice_interet <- matrix(NA, nrow = 2, ncol = 4)
  for (j in 1:4) {
    petiteliste<-tabbool[,j]
    indices_avec_1 <- which(petiteliste == 1)
    valeurs_assoc <- tabnum[indices_avec_1,j]
    if (length(valeurs_assoc)!=0){
      comptage <- table(valeurs_assoc)
      nb_occurrences <- max(comptage)
      valeur_plusprez <- min(as.numeric(names(comptage)[which(comptage == nb_occurrences)]))
      matrice_interet[1, j] <- valeur_plusprez
      matrice_interet[2, j] <- nb_occurrences
    }
  }
  return(matrice_interet)
}

sum_sans_col <- function(tabnum){
  
  sum<-0
  for(j in 1:4){
    if(any(table(tabnum[,j]) != 3)==TRUE){
      sum <-sum+ sum(tabnum[,j])
    }
    else{
      sum<-sum+0
    }
  }
  return(sum)
}

score_alpha_solodoub <- function(a=3,b=4){
  init <-initial()
  tabnum<-do.call(rbind,init[1])
  tabbool<-do.call(rbind,init[2])
  cartes_dispo<-do.call(rbind,init[3])
  defausse<-as.numeric(init[4])
  score_tem <- list()
  score_tem <- c(score_tem, sum(tabnum))
  cpteur<-0
  stop <- round(runif(1, 15,30),0)
  while(cpteur<=stop && sum(tabbool)<12){
    indices_avec_1 <- which(tabbool == 1, arr.ind = TRUE)
    valeurs_assoc <- tabnum[indices_avec_1]
    if(defausse %in% valeurs_assoc){
      u <- defausse
    }
    else{
      u<- sample(cartes_dispo, 1)  # Sélectionner un index aléatoire dans cartes_dispo
      num <- which(cartes_dispo==u) # quelle iteration?
      cartes_dispo <- cartes_dispo[-num[1]]
    }
    a<-condition(tabnum,tabbool,u)
    if(a[1]==0||a[1]==1){
      tabnum[a[2],a[3]] <- u
      tabbool[a[2],a[3]] <- 1
    }
    else if(a==2){
      pos<-place(tabbool)
      tabnum[pos[1],pos[2]] <- u
      tabbool[pos[1], pos[2]] <- 1
    }
    else{
      pos<-place(tabbool)
      tabbool[pos[1], pos[2]] <- 1
    }
    
    score_tem <- c(score_tem, sum_sans_col(tabnum))
    defausse <- as.numeric(sample(cartes_dispo,1))
    num <- which(cartes_dispo==defausse) # quelle iteration?
    cartes_dispo <- cartes_dispo[-num[1]]#Retirer la carte utilisée
    cpteur<-cpteur+1
  }
  return(score_tem)
}

score_alpha_solodoub2 <- function(a=3,b=4){
  init <-initial2()
  tabnum<-do.call(rbind,init[1])
  tabbool<-do.call(rbind,init[2])
  cartes_dispo<-do.call(rbind,init[3])
  defausse<-as.numeric(init[4])
  score_tem <- list()
  score_tem <- c(score_tem, sum(tabnum))
  cpteur<-0
  stop <- round(runif(1, 15,30),0)
  while(cpteur<=stop && sum(tabbool)<12){
    indices_avec_1 <- which(tabbool == 1, arr.ind = TRUE)
    valeurs_assoc <- tabnum[indices_avec_1]
    if(defausse %in% valeurs_assoc){
      u <- defausse
    }
    else{
      u<- sample(cartes_dispo, 1)  # Sélectionner un index aléatoire dans cartes_dispo
      num <- which(cartes_dispo==u) # quelle iteration?
      cartes_dispo <- cartes_dispo[-num[1]]
    }
    a<-condition(tabnum,tabbool,u)
    if(a[1]==0||a[1]==1){
      tabnum[a[2],a[3]] <- u
      tabbool[a[2],a[3]] <- 1
    }
    else if(a==2){
      pos<-place(tabbool)
      tabnum[pos[1],pos[2]] <- u
      tabbool[pos[1], pos[2]] <- 1
    }
    else{
      pos<-place(tabbool)
      tabbool[pos[1], pos[2]] <- 1
    }
    
    score_tem <- c(score_tem, sum_sans_col(tabnum))
    defausse <- as.numeric(sample(cartes_dispo,1))
    num <- which(cartes_dispo==defausse) # quelle iteration?
    cartes_dispo <- cartes_dispo[-num[1]]#Retirer la carte utilisée
    cpteur<-cpteur+1
  }
  return(score_tem)
}
normalisation <- function(listdeliste) {
  result <- lapply(listdeliste, function(inner_list) {
    if(length(inner_list) > 30) {
      inner_list <- inner_list[1:30]  # Raccourcir la liste à 30 éléments
    } else if(length(inner_list) < 30) {
      inner_list <- c(inner_list, rep(inner_list[length(inner_list)], 30 - length(inner_list)))  # Remplir avec la dernière valeur
    }
    return(inner_list)
  })
  return(result)
}

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
  for(i in 1:29) {
    sum <- 0
    for(j in 1:n) {
      sum <- sum + b[[j]][[i]]
    }
    coli<-get_ith_element(b,i)
    moyenne <- sum/n
    ecart_type <- sd(coli)
    IC_99 <- c(moyenne - 1.64 * (ecart_type/sqrt(n)),
               moyenne + 1.64 * (ecart_type/sqrt(n)))
    my_list <- c(my_list, sum/n, IC_99)
  }
  return(my_list)
}



sc_moy_tot <- function(n) {
  aa<-list()
  resultats<- replicate(n, score_alpha_solodoub(3, 4))
  b<-normalisation(resultats)
  return(sc_moy_alpha(b, n))
}

sc_moy_tot2 <- function(n) {
  aa<-list()
  resultats<- replicate(n, score_alpha_solodoub2(3, 4))
  b<-normalisation(resultats)
  return(sc_moy_alpha(b, n))
}
vasystp<-sc_moy_tot(10000)
vasystp_pas_de_3 <- vasystp[seq(1, length(vasystp), by = 3)]
vasymax<-vasystp[seq(0, length(vasystp), by = 3)]
vasymin<-vasystp[seq(2, length(vasystp), by = 3)]

vasystp2<-sc_moy_tot2(10000)
vasystp_pas_de_3_2 <- vasystp2[seq(1, length(vasystp2), by = 3)]
vasymax2<-vasystp2[seq(0, length(vasystp2), by = 3)]
vasymin2<-vasystp2[seq(2, length(vasystp2), by = 3)]

# enlever la premiere ligne de vasystp

mat<- (unlist(vasystp_pas_de_3))
df <- data.frame(x=0:28, y1=mat,lower1=unlist(vasymin),upper1=unlist(vasymax), y2=unlist(vasystp_pas_de_3_2),lower2=unlist(vasymin2),upper2=unlist(vasymax2))
df
ggplot(df, aes(x = x)) +
  geom_line(aes(y = y1, color = "Cas 1")) +
  geom_line(aes(y = y2, color = "Cas 2")) +
  # Ajout des bandes avec les noms de légende
  geom_ribbon(aes(ymin = lower1, ymax = upper1, fill = "Cas 1"), alpha = 0.3) +
  geom_ribbon(aes(ymin = lower2, ymax = upper2, fill = "Cas 2"), alpha = 0.3) +
  # Autres paramètres esthétiques
  labs(x = "Nombre de tours", y = "Score moyen") +
  # Définition des couleurs et remplissages manuels
  scale_color_manual(name = "Initialisation", values = c("Cas 1" = "#4ac16d", "Cas 2" = "#46327e")) +
  scale_fill_manual(name = "Initialisation", values = c("Cas 1" = "#4ac16d", "Cas 2" = "#46327e")) +
  # Positionnement de la légende en haut à droite
  theme(legend.position = "topright") +
  scale_y_continuous(breaks = seq(0, 60, 5)) +
  scale_x_continuous(breaks = seq(0, 60, 5)) +
  theme_minimal()


#####################
score_alpha_solodoub2_count <- function(a=3,b=4){
  init <-initial2()
  tabnum<-do.call(rbind,init[1])
  tabbool<-do.call(rbind,init[2])
  cartes_dispo<-do.call(rbind,init[3])
  defausse<-as.numeric(init[4])
  score_tem <- list()
  score_tem <- c(score_tem, sum(tabnum))
  cpteur<-0
  countcol<-0
  stop <- round(runif(1, 15,30),0)
  while(cpteur<=stop && sum(tabbool)<12){
    indices_avec_1 <- which(tabbool == 1, arr.ind = TRUE)
    valeurs_assoc <- tabnum[indices_avec_1]
    if(defausse %in% valeurs_assoc){
      u <- defausse
    }
    else{
      u<- sample(cartes_dispo, 1)  # Sélectionner un index aléatoire dans cartes_dispo
      num <- which(cartes_dispo==u) # quelle iteration?
      cartes_dispo <- cartes_dispo[-num[1]]
    }
    a<-condition(tabnum,tabbool,u)
    if(a[1]==0||a[1]==1){
      tabnum[a[2],a[3]] <- u
      tabbool[a[2],a[3]] <- 1
    }
    else if(a==2){
      pos<-place(tabbool)
      tabnum[pos[1],pos[2]] <- u
      tabbool[pos[1], pos[2]] <- 1
    }
    else{
      pos<-place(tabbool)
      tabbool[pos[1], pos[2]] <- 1
    }
    
    score_tem <- c(score_tem, sum_sans_col(tabnum))
    defausse <- as.numeric(sample(cartes_dispo,1))
    num <- which(cartes_dispo==defausse) # quelle iteration?
    cartes_dispo <- cartes_dispo[-num[1]]#Retirer la carte utilisée
    cpteur<-cpteur+1
  }
  for(j in 1:4){
    if(any(table(tabnum[,j]) == 3)){
      countcol<-countcol+1
    }
  }
  return(countcol)
}


score_alpha_solodoub1_count <- function(a=3,b=4){
  init <-initial()
  tabnum<-do.call(rbind,init[1])
  tabbool<-do.call(rbind,init[2])
  cartes_dispo<-do.call(rbind,init[3])
  defausse<-as.numeric(init[4])
  score_tem <- list()
  score_tem <- c(score_tem, sum(tabnum))
  cpteur<-0
  countcol<-0
  stop <- round(runif(1, 15,30),0)
  while(cpteur<=stop && sum(tabbool)<12){
    indices_avec_1 <- which(tabbool == 1, arr.ind = TRUE)
    valeurs_assoc <- tabnum[indices_avec_1]
    if(defausse %in% valeurs_assoc){
      u <- defausse
    }
    else{
      u<- sample(cartes_dispo, 1)  # Sélectionner un index aléatoire dans cartes_dispo
      num <- which(cartes_dispo==u) # quelle iteration?
      cartes_dispo <- cartes_dispo[-num[1]]
    }
    a<-condition(tabnum,tabbool,u)
    if(a[1]==0||a[1]==1){
      tabnum[a[2],a[3]] <- u
      tabbool[a[2],a[3]] <- 1
    }
    else if(a==2){
      pos<-place(tabbool)
      tabnum[pos[1],pos[2]] <- u
      tabbool[pos[1], pos[2]] <- 1
    }
    else{
      pos<-place(tabbool)
      tabbool[pos[1], pos[2]] <- 1
    }
    
    score_tem <- c(score_tem, sum_sans_col(tabnum))
    defausse <- as.numeric(sample(cartes_dispo,1))
    num <- which(cartes_dispo==defausse) # quelle iteration?
    cartes_dispo <- cartes_dispo[-num[1]]#Retirer la carte utilisée
    cpteur<-cpteur+1
  }
  for(j in 1:4){
    if(any(table(tabnum[,j]) == 3)){
      countcol<-countcol+1
    }
  }
  return(countcol)
}

countcolonnes0 <- mean(replicate(10000, score_alpha_solodoub1_count(3, 4)))
countcolonnes <- mean(replicate(10000, score_alpha_solodoub2_count(3, 4)))
