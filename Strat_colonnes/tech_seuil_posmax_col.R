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



condition <- function(tabnum, tabbool, carte, seuil, interet) {
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
    posplace<-place(tabbool)
    return(c(2,posplace[1],posplace[2]))
  }
}


matrice <- matrix(c(0, 2, 4, 
                    3, 4, 4,
                    1, 0, 2, 
                    5, -2, 10), nrow = 3)

matbool <- matrix(c(1, 1, 0,
                    1, 0, 0, 
                    1, 0, 0, 
                    0, 0, 0), nrow = 3)

condition(matrice,matbool,1, 4,interet(matrice,matbool))






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

score_alpha_solodoub <- function(a=3,b=4, seuil){
  init <-initial()
  tabnum<-do.call(rbind,init[1])
  tabbool<-do.call(rbind,init[2])
  cartes_dispo<-do.call(rbind,init[3])
  defausse<-as.numeric(init[4])
  score_tem <- list()
  score_tem <- c(score_tem, sum(tabnum))
  cpteur<-0
  stop <- round(runif(1, 15,30),0)
  #print(stop)
  while(cpteur<stop && sum(tabbool)<12){
    int<- interet(tabnum, tabbool)
    if((!is.na(which(int[1,]==defausse & int[2,]==2)[1])&&defausse>0) || defausse <= seuil){
      u <- defausse
    }
    else{
      u<- sample(cartes_dispo, 1)  # Sélectionner un index aléatoire dans cartes_dispo
      num <- which(cartes_dispo==u) # quelle iteration?
      cartes_dispo <- cartes_dispo[-num[1]]
    }
    a<-condition(tabnum,tabbool,u, seuil, int)
    if(a[1]==2){
      tabbool[a[2],a[3]] <- 1
    }
    else{
      tabnum[a[2],a[3]] <- u
      tabbool[a[2],a[3]] <- 1
    }
    score_tem <- c(score_tem, sum_sans_col(tabnum))
    defausse <- as.numeric(sample(cartes_dispo,1))
    num <- which(cartes_dispo==defausse) # quelle iteration?
    cartes_dispo <- cartes_dispo[-num[1]]#Retirer la carte utilisée
    cpteur<-cpteur+1
    #if(cpteur==stop){
    #  print("stop")
    #}
  }
  return(score_tem)
}

score_alpha_solodoub(3,4, 2)


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
  b<-normalisation(resultats)
  return(sc_moy_alpha(b, n))
}


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

time <- Sys.time()
colbase<-sc_moy_tot_2(10000)
Sys.time() - time

colbase <- colbase

#write.table(colbase, file = "colbase.txt", sep = "\t", row.names = FALSE)

#View(colbase)
vasystp<- colbase[-(length(colbase) - 14):-length(colbase)]
#vasystp
ectyp <- colbase[(length(colbase) - 14):length(colbase)]
moyprgrph <- vasystp[seq(from=115, to=length(vasystp), by =117)]

colbase_pas_de_3 <- vasystp[seq(1, length(colbase), by = 3)]

matricolbase_3 <- matrix(unlist(colbase_pas_de_3), nrow = 15, byrow = TRUE)

write.table(matricolbase_3, file = "matricehybride.txt", sep = ",", row.names = FALSE)


my_colors <- c("#E6E7A7", "#fddc9e", "#febb81", "#fd9a6a", "#f8765c", "#eb5760", "#d3436e", "#b73779", "#982d80", "#7b2382", "#5f187f", 
               "#400f74", "#221150", "#0c0926", "#000004")

library(ggrepel)
df <- data.frame(x=1:28, t(matricolbase_3[,1:28]))
names(df) <- c("x", "-2", "-1", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
#df
df_long <- gather(df, key = "variable", value="value", -x)
names(df_long)=c("x","Seuil","value")

df_long$Seuil <- factor(df_long$Seuil, levels = c("x", "-2", "-1", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))

ggplot(df_long, aes(x = as.numeric(x)-1, y = as.numeric(value), color = Seuil)) +
  geom_line(linewidth=0.85) +
  labs(x = "Nombre de tours",
       y = "Score moyen") +
  scale_color_manual(values = my_colors) + 
  scale_x_continuous(breaks = seq(0,28,5)) +
  scale_y_continuous(breaks = seq(0, 60,10)) +
  geom_text_repel(data = subset(df_long, x == max(as.numeric(x))), aes(label = Seuil), direction = "x", segment.color = NA, box.padding = 0.1, force = 1, hjust = 0, nudge_x = 0.7, color = my_colors)+
  theme_minimal()



datavase <- matrix(NA,3,15)

for(i in 1:15){
  datavase[1,i] <- moyprgrph[[i]]
  datavase[2,i] <- moyprgrph[[i]]-2.58*(ectyp[[i]]/sqrt(10000))
  datavase[3,i] <- moyprgrph[[i]]+2.58*(ectyp[[i]]/sqrt(10000))
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
library(ggpattern)
library(magick)
my_colorsbis <- c("#fddc9e", "#febb81", "#fd9a6a", "#f8765c", "#eb5760", "#d3436e", "#b73779", "#982d80", "#7b2382", "#5f187f", 
               "#400f74", "#221150", "#0c0926", "#000004","gray10")
my_patterns <- c("crosshatch", "crosshatch","crosshatch","crosshatch","crosshatch","crosshatch","crosshatch","crosshatch","crosshatch","crosshatch","crosshatch","crosshatch","crosshatch","crosshatch","crosshatch")


ggplot(data, aes(x = essai, y = moyenne)) +
  geom_col_pattern(fill = my_colors, pattern = my_patterns, pattern_fill=my_colorsbis,pattern_density = 0.2, colour = my_colors,pattern_colour=my_colors) +  # Utilisation de geom_col_pattern au lieu de geom_col pour inclure les motifs
  scale_pattern_fill_manual(values = my_colors)+
  geom_errorbar(aes(ymin = inf, ymax = sup), width = 0.4, color = "darkgrey", linewidth = 0.5) +
  labs(x = "Seuil", y = "Score moyen", fill = " ") +
  scale_x_continuous(breaks = 1:15, labels = labels) +
  scale_y_continuous(
    name = "Score moyen",
    breaks = seq(0, 65, 5)
  ) +
  theme_minimal()


score_alpha_solodoub_count <- function(a=3,b=4, seuil){
  init <-initial()
  tabnum<-do.call(rbind,init[1])
  tabbool<-do.call(rbind,init[2])
  cartes_dispo<-do.call(rbind,init[3])
  defausse<-as.numeric(init[4])
  score_tem <- list()
  score_tem <- c(score_tem, sum(tabnum))
  cpteur<-0
  stop <- round(runif(1, 15,30),0)
  countcol<-0
  while(cpteur<stop && sum(tabbool)<12){
    int<- interet(tabnum, tabbool)
    if(defausse %in% int[1,] || defausse <= seuil){
      u <- defausse
    }
    else{
      u<- sample(cartes_dispo, 1)  # Sélectionner un index aléatoire dans cartes_dispo
      num <- which(cartes_dispo==u) # quelle iteration?
      cartes_dispo <- cartes_dispo[-num[1]]
    }
    a<-condition(tabnum,tabbool,u, seuil, int)
    #print(a)
    if(a[1]==2){
      tabbool[a[2],a[3]] <- 1
    }
    else{
      tabnum[a[2],a[3]] <- u
      tabbool[a[2],a[3]] <- 1
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

countcolseuil<-c()
for(i in -2:12){
  resultats<- replicate(5000, score_alpha_solodoub_count(3,4, i))
  countcolseuil<-c(countcolseuil, mean(resultats))
}
countcolseuil
