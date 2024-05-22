#tech seuil1 pos max contre seuil2 pos max

library(ggplot2)
library(tidyr)

initial2 <- function(){
  cartes_dispo= c(rep(-2,5),rep(-1,10),rep(0,15), rep(1:12,10))
  count_col=0
  tabnum1 <- matrix(NA, nrow=3,ncol=4)
  tabbool1 <- matrix(0, nrow=3,ncol=4)
  tabnum2 <- matrix(NA, nrow=3,ncol=4)
  tabbool2 <- matrix(0, nrow=3,ncol=4)
  for (i in 1:3) {
    for (j in 1:4) {
      u <- sample(cartes_dispo,1)
      tabnum1[i,j] <- u
      num1 <- which(cartes_dispo==u) # quelle iteration?
      cartes_dispo <- cartes_dispo[-num1[1]]#Retirer la carte utilisée
      v <- sample(cartes_dispo,1)
      tabnum2[i,j] <- v
      num2 <- which(cartes_dispo==v) # quelle iteration?
      cartes_dispo <- cartes_dispo[-num2[1]]
    }
    #print(length(cartes_dispo))
  }
  defausse <- as.numeric(sample(cartes_dispo,1))
  num <- which(cartes_dispo==defausse) # quelle iteration?
  cartes_dispo <- cartes_dispo[-num[1]]#Retirer la carte utilisée
  tabbool1[1,1]<-1
  tabbool1[1,2]<-1
  tabbool2[1,1]<-1
  tabbool2[1,2]<-1
  return(list(tabnum1,tabnum2,tabbool1,tabbool2,cartes_dispo,defausse))
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
  new_matrix <- tabnumex
  new_matrix[tabboolex == 0] <- 3
  # Calculer la somme de ces valeurs
  return(sum_sans_col(new_matrix))
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



conditionseuilposmax <- function(tabnum,tabbool,carte, seuil){
  if(carte>seuil){
    return(c(-1))
  }
  for(i in 1:nrow(tabnum)){
    for(j in 1:ncol(tabnum)){
      if((tabnum[i,j]>seuil)&&(tabbool[i,j]==1)){
        pos<-maxpos(tabnum,tabbool)
        return(c(1,pos[2],pos[3]))
      }
    }
  }
  return(c(-1))
}

conditionhybride <- function(tabnum, tabbool, carte, seuil,som1,cas1) {
  som1=0
  interet <- interet(tabnum, tabbool)
  int <- interet[1,]
  j<-which(int==carte & interet[2,]==2)[1]
  if (!(is.na(j))&&carte>0) {
    pos<-0
    non_carte<-0
    pos <- which(tabnum[,j] == carte & tabbool[,j]==1, arr.ind = TRUE)# pos qui sont retournées dans la colonne j
    non_carte <- suppressWarnings(which(tabnum[,j] != tabnum[pos, j] | tabbool[,j]==0, arr.ind = TRUE)) # pos qui ne sont pas la carte mais qui sont retournées (sinon c inutile)
    return(c(0, non_carte, j))
  }
  else if (carte<=seuil) {
    for(i in 1:nrow(tabnum)){
      for(j in 1:ncol(tabnum)){
        if((tabnum[i,j]>seuil)&&(tabbool[i,j]==1)){
          posmax<-maxpos(tabnum, tabbool)
          return(c(1,posmax[2],posmax[3]))
        }
      }
    }
    posplace<-place(tabbool)
    return(c(3,posplace[1],posplace[2]))
  }
  else{
    return(c(-1))
  }
}

conditionhybrideRegscore <- function(tabnum, tabbool, carte, seuil,som1,cas1) {
  interet <- interet(tabnum, tabbool)
  int <- interet[1,]
  j<-which(int==carte & interet[2,]==2)[1]
  new_matrix <- tabnum
  new_matrix[tabboolex == 0] <- carte
  som2 <- sum_sans_col(new_matrix)
  som1<-som1
  if (!(is.na(j))&&carte>0) {
    pos<-0
    non_carte<-0
    pos <- which(tabnum[,j] == carte & tabbool[,j]==1, arr.ind = TRUE)# pos qui sont retournées dans la colonne j
    non_carte <- suppressWarnings(which(tabnum[,j] != tabnum[pos, j] | tabbool[,j]==0, arr.ind = TRUE)) # pos qui ne sont pas la carte mais qui sont retournées (sinon c inutile)
    return(c(0, non_carte, j))
  }
  else if (sum(tabbool)==11 && som1<=som2 && cas1!=1) {
    posmax<-maxpos(tabnum, tabbool)
    return(c(1,posmax[2],posmax[3]))
  }
  else if (carte<=seuil) {
    for(i in 1:nrow(tabnum)){
      for(j in 1:ncol(tabnum)){
        if((tabnum[i,j]>seuil)&&(tabbool[i,j]==1)){
          posmax<-maxpos(tabnum, tabbool)
          return(c(1,posmax[2],posmax[3]))
        }
      }
    }
    posplace<-place(tabbool)
    return(c(3,posplace[1],posplace[2]))
  }
  else{
    return(c(-1))
  }
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
  for(j in 1:ncol(tabnum)){
    occurrences <- table(tabnum[, j])
    if(any(occurrences != 3)==TRUE){
      sum <-sum+ sum(tabnum[,j])
    }
  }
  return(sum)
}


condseuilposmax <- function(tabnum,tabbool,carte, seuil) {
  return(carte <= seuil)
}

condhybride <- function(tabnum, tabbool,carte, seuil) {
  interet <- interet(tabnum, tabbool)
  return((!is.na(which(interet[1,]==carte & interet[2,]==2)[1])&&carte>0) || carte <= seuil)
}


score_doubleRegscore<- function(a=3,b=4, seuil1,seuil2,fun1,fun2,conditionj1,conditionj2){
  init<-initial2()
  tabnum1<-do.call(rbind,init[1])
  tabnum2<-do.call(rbind,init[2])
  tabbool1<-do.call(rbind,init[3])
  tabbool2<-do.call(rbind,init[4])
  cartes_dispo<-do.call(rbind,init[5])
  defausse<-as.numeric(init[6])
  score_tem1 <- 0
  score_tem2 <- 0
  bool1 <- TRUE
  bool2 <- TRUE
  vic1 <- 0
  vic2 <- 0
  counttour<-0
  cas1<-0
  while(bool1 && bool2){
    if(sum(tabbool1)!=12){
      if(sum(tabbool2)==12){
        bool2 <- FALSE
      }
      u <- sample(cartes_dispo,1)
      if (fun1(tabnum1,tabbool1,defausse, seuil1)){
        a<-conditionj1(tabnum1,tabbool1,defausse,seuil1,som1)
        if(a[1]==-1){
          #print(-1)
          #print(sum(tabbool1))
          posi<-place(tabbool1)
          tmp <- tabnum1[posi[1],posi[2]]
          tabbool1[posi[1],posi[2]] <- 1
          defausse <- tmp
        }
        else {
          tmp <- tabnum1[a[2], a[3]]
          tabnum1[a[2], a[3]] <- defausse
          tabbool1[a[2], a[3]] <- 1
          defausse <- tmp
        }
      }
      else{
        #print(sum(tabbool1))
        a<-conditionj1(tabnum1,tabbool1,u,seuil1,som1)
        if(a[1]==-1){
          posi<-place(tabbool1)
          tmp <- tabnum1[posi[1],posi[2]]
          tabbool1[posi[1],posi[2]] <- 1
        }
        else {
          tmp <- tabnum1[a[2], a[3]]
          tabnum1[a[2], a[3]] <- u
          tabbool1[a[2], a[3]] <- 1
        }
        defausse <- tmp
        num <- which(cartes_dispo==u) 
        cartes_dispo <- cartes_dispo[-num[1]]
      }
    }
    if(sum(tabbool2)!=12){
      if(sum(tabbool1)==12){
        bool1 <- FALSE
        cas1<-1
      }
      som1<-sumspeciale(tabnum1,tabbool1)
      v <- sample(cartes_dispo,1)
      if (fun2(tabnum2,tabbool2,defausse, seuil2)){
        a<-conditionj2(tabnum2,tabbool2,defausse,seuil2,som1,cas1)
        if(a[1]==-1){
          posi<-place(tabbool2)
          tmp <- tabnum2[posi[1],posi[2]]
          tabbool2[posi[1],posi[2]] <- 1
          defausse <- tmp
        }
        else {
          tmp <- tabnum2[a[2], a[3]]
          tabnum2[a[2], a[3]] <- defausse
          tabbool2[a[2], a[3]] <- 1
          defausse <- tmp
        }
      }
      else{
        #print(v)
        a<-conditionj2(tabnum2,tabbool2,v,seuil2,som1,cas1)
        #print(4)
        if(a[1]==-1){
          posi<-place(tabbool2)
          tmp <- tabnum2[posi[1],posi[2]]
          tabbool2[posi[1],posi[2]] <- 1
        }
        else {
          tmp <- tabnum2[a[2], a[3]]
          tabnum2[a[2], a[3]] <- v
          tabbool2[a[2], a[3]] <- 1
        }
        defausse <- tmp
        num <- which(cartes_dispo==v) 
        cartes_dispo <- cartes_dispo[-num[1]]
      }
    }
    counttour<-counttour+1
  }
  score_tem1 <- sum_sans_col(tabnum1)
  score_tem2 <- sum_sans_col(tabnum2)
  s1 <- sum_sans_col(tabnum1)
  s2 <- sum_sans_col(tabnum2)
  if (!bool1 && s1 < s2){
    vic1 <- 1
    s2<-2*s2
  }
  else if (!bool2 && s2 < s1){
    vic2 <- 1
    s1<-2*s1
  }
  else if (!bool1 && s1 >= s2){
    vic2 <- 1
  }
  else if (!bool2 && s2 >= s1){
    vic1 <- 1
  }
  return(list(s1,s2,vic1,vic2,counttour,cas1))
}

score_doubleRegscore(3,4,3,3,condhybride,condhybride,conditionhybride,conditionhybrideRegscore)



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
  my_list <- c()
  for(i in 1:40) {
    coli<-get_ith_element(b,i)
    #print(coli)
    moyenne <- mean(coli)
    ecart_type <- sd(coli)
    IC_99 <- c(moyenne - 2.58 * (ecart_type/sqrt(n)),
               moyenne + 2.58 * (ecart_type/sqrt(n)))
    my_list <- c(my_list, moyenne, IC_99)
  }
  return(my_list)
}



sc_moy_totRegscore <- function(n, seuil1,seuil2,fun1,fun2,conditionj1,conditionj2) {
  aa<-list()
  resultats<- replicate(n,{
    setTxtProgressBar(pb, getTxtProgressBar(pb) + 1) 
    return(score_doubleRegscore(3, 4, seuil1,seuil2,fun1,fun2,conditionj1,conditionj2))
  })
  #print(resultats)
  b<-mean(as.numeric(resultats[1,]))
  ecart_typeb <- sd(as.numeric(resultats[1,]))
  IC_99b <- c(b - 2.58 * (ecart_typeb/sqrt(n)),
              b + 2.58 * (ecart_typeb/sqrt(n)))
  c<-mean(as.numeric(resultats[2,]))
  ecart_typec <- sd(as.numeric(resultats[2,]))
  IC_99c <- c(c - 2.58 * (ecart_typec/sqrt(n)),
              c + 2.58 * (ecart_typec/sqrt(n)))
  d<-mean(as.numeric(resultats[3,]))
  e<-mean(as.numeric(resultats[4,]))
  f<-mean(as.numeric(resultats[5,]))
  g<-mean(as.numeric(resultats[6,]))
  return(list(b,IC_99b, c,IC_99c,d,e,f,g))
}


pb <- txtProgressBar(min = 0, max = n, style = 3)
hyb3_hyb3_Regscore<-sc_moy_totRegscore(10000, 3,3,condhybride,condhybride,conditionhybride,conditionhybrideRegscore)
close(pb)
hyb3_hyb3_Regscore

