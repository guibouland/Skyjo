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
  valeurs_associees <- tabnum[tabbool == 1]
  
  # Calculer la somme de ces valeurs
  somme <- sum(valeurs_associees)
  
  # Retourner la somme
  return(somme)
}

condition <- function(tabnum, tabbool, maxi, carte,seuil){
  indices_avec_1 <- which(tabbool == 1, arr.ind = TRUE)
  valeurs_assoc <- tabnum[indices_avec_1]
  som <- sum(valeurs_assoc)
  if (((som + carte) > maxi) && (sum(tabbool) < 12)) {
    # Vérifie si les 11 premières cartes sont retournées et si leur somme avec la carte piochée est correcte
    return(1)
  } else if(((som + carte) <= maxi) && (sum(tabbool) == 11)){
    return(2)
  }
  else{
    return(0)
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

ecart_types <- function(normalised){
  derniers_elements <- sapply(normalised, function(x) tail(x, 1))
  #print(derniers_elements)
  # Calculer l'écart-type des derniers éléments
  ecart_type <- sd(as.numeric(derniers_elements))
  
  # Retourner l'écart-type
  return(ecart_type)
}

score_alpha_solodoub <- function(maxi, a=3,b=4){
  init <-initial()
  tabnum<-do.call(rbind,init[1])
  tabbool<-do.call(rbind,init[2])
  cartes_dispo<-do.call(rbind,init[3])
  defausse<-as.numeric(init[4])
  score_tem <- list()
  score_tem <- c(score_tem, sum(tabnum))
  cpteur<-0
  #stop <- round(runif(1, 15,30),0)
  while(sum(tabbool)<12 && cpteur<=80){
    u <- sample(cartes_dispo,1)
    if (defausse <= 2){
      a<-condition(tabnum,tabbool,maxi,defausse,2)
      if(a==1){
        top<- maxpos(tabnum,tabbool)
        tabnum[top[2],top[3]] <- defausse
      }
      else if(a==2){
        pos<-place(tabbool)
        tabnum[pos[1],pos[2]] <- defausse
        tabbool[pos[1], pos[2]] <- 1
      }
      else{
        pos<-place(tabbool)
        tabbool[pos[1], pos[2]] <- 1
      }
    }
    else{
      a<-condition(tabnum,tabbool,maxi,u,2)
      if (a==1){
        top<- maxpos(tabnum,tabbool)
        tabnum[top[2],top[3]] <- u
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
    }
    score_tem <- c(score_tem, sum_sans_col(tabnum))
    cpteur<- cpteur+1
    defausse <- sample(cartes_dispo,1)
    num <- which(cartes_dispo==defausse) # quelle iteration?
    cartes_dispo <- cartes_dispo[-num[1]]
  }
  return(score_tem)
}


normalisation <- function(listdeliste) {
  taille_99_percentile <- quantile(sapply(listdeliste, length), probs = 0.99)
  result <- lapply(listdeliste, function(inner_list) {
    if(length(inner_list) > 80) {
      inner_list <- inner_list[1:80]  # Raccourcir la liste à 30 éléments
    } else if(length(inner_list) < 80) {
      inner_list <- c(inner_list, rep(inner_list[length(inner_list)], 80 - length(inner_list)))  # Remplir avec la dernière valeur
    }
    return(inner_list)
  })
  return(result)
}



sc_moy_alpha <- function(b, n) {
  res<-list()
  for(i in 2:80){
    sum<-0
    for(j in 1:n){
      sum<-sum+b[[j]][[i]]
    }
    res <- c(res, sum/n)
  }
  return(res)
}

valeurs_i <- c(0,5, 10,15, 20,30)

sc_moy_tot <- function(n) {
  aa<-list()
  bb <- list()
  for (i in valeurs_i) {
    resultats<- replicate(n, score_alpha_solodoub(i, 3,4))
    b<-normalisation(resultats)
    #print("1")
    aa <- c(aa, sc_moy_alpha(b, n)[-1])
    #print("2")
    bb <- c(bb, ecart_types(b))
    print(ecart_types(b))
    #print("3")
  }
  return(c(aa, bb))
}
time<-Sys.time()
vasystp0<-sc_moy_tot(10000)
print(Sys.time()-time)
vasystp<- vasystp0[-(length(vasystp0) - (length(valeurs_i)-1)):-length(vasystp0)]
ectyp <- vasystp0[(length(vasystp0) - (length(valeurs_i)-1)):length(vasystp0)]
moyprgrph <- vasystp[seq(from=78, to=length(vasystp), by = 78)]


datavase <- matrix(NA,3,length(valeurs_i))

for(i in 1:length(valeurs_i)){
  datavase[1,i] <- moyprgrph[[i]]
  datavase[2,i] <- moyprgrph[[i]]-2.58*(ectyp[[i]]/sqrt(10000))
  datavase[3,i] <- moyprgrph[[i]]+2.58*(ectyp[[i]]/sqrt(10000))
}

library(xtable)

# Créer un objet xtable à partir de la matrice datavase
#datavase_table <- xtable(datavase)
#print(datavase_table, include.rownames = TRUE, include.colnames = TRUE, floating = FALSE)


data <- data.frame(essai = 1:6, 
                   moyenne = datavase[1,], 
                   inf = datavase[2,], 
                   sup = datavase[3,])

# Définition des étiquettes pour l'axe horizontal
labels <- c("0","05", "10","15", "20", "30")
taille99<- c(77,67,57,53,52,53)
my_colors <- c("#e2e418","#8bd646", "#24aa83", "#2a768e", 
               "#443b84", "#440154")
# Tracer le graphique avec ggplot2 et modification de l'axe horizontal
ggplot(data, aes(x = essai, y = moyenne)) +
  geom_col(fill = my_colors) +
  geom_errorbar(aes(ymin = inf, ymax = sup), width = 0.2, color = "darkgrey", linewidth = 0.6) +
  labs(x = "Palier", y = "Score moyen",fill=" ") +
  scale_x_continuous(breaks = 1:length(valeurs_i), labels = labels)+
  theme_minimal()+
  scale_y_continuous(breaks = seq(-5, 60,5) )

matrice <- t(matrix(unlist(vasystp), nrow = 6, byrow = TRUE))
matrice <- matrice[-1, ]
df <- data.frame(x=1:77, matrice)
names(df) <- c("x", "0","05", "10","15", "20", "30")
#df
df_long <- gather(df, key = "variable", value="value", -x)
names(df_long)=c("x","Palier","value")

df_long$Palier <- factor(df_long$Palier)

ggplot(df_long, aes(x = as.numeric(x)-1, y = as.numeric(value), color = Palier)) +
  geom_line(linewidth=0.75) +
  labs(x = "Nombre de tours",
       y = "Score moyen") +
  scale_color_manual(values = my_colors) + 
  scale_x_continuous(breaks = seq(0,77,10)) +
  scale_y_continuous(breaks = seq(-5, 60,10)) +
  theme_minimal()

