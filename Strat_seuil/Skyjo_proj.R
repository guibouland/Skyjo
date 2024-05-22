library(ggplot2)
library(tidyr)

cartes_dispo= c(rep(-2,5),rep(-1,10),rep(0,15), rep(1:12,10))

table <- matrix(data = NA, nrow = 3, ncol = 4)

score_ini <- function(a=3,b=4){
  cartes_dispo= c(rep(-2,5),rep(-1,10),rep(0,15), rep(1:12,10))
  count_col=0
  for (i in 1:a) {
    for (j in 1:b) {
      u <- sample(cartes_dispo,1)
      table[i, j] <- u
      num <- which(cartes_dispo==u) # quelle iteration?
      cartes_dispo <- cartes_dispo[-num[1]]#Retirer la carte utilisée
    }
    #print(length(cartes_dispo))
  }
  sum_sans_col <- 0
  for (j in 1:4) {
    if (length(unique(table[, j])) > 1) {  # Vérifier s'il y au moins 2 éléments uniques dans la colonne
      sum_sans_col <- sum_sans_col + sum(table[, j])
    }
    else { (count_col<- count_col + 1) }
  }
  return(c(sum_sans_col,count_col))
}

score_ini(3,4)

count_score <- function(b){
  count_col <- 0
  sum_sans_col <- 0
  for (j in 1:4) {
    if (length(unique(table[, j])) > 1) {  # Vérifier s'il y au moins 2 éléments uniques dans la colonne
      sum_sans_col <- sum_sans_col + sum(table[, j])
    }
    else { (count_col<- count_col + 1) }
  }
  return(c(sum_sans_col,count_col))
}

score_ini_moyen <- function(n){
  tot=0
  nbcol=0
  for(i in 1:n) {tot = tot + score_ini(3,4)[1];
                  nbcol = nbcol+ score_ini(3,4)[2]}
  return (c(tot/n,nbcol/n))
}




# score ini pour 1M itérations 
resultats <- replicate(100000, score_ini(3,4)[1])
resultatscol <- replicate(100000, score_ini(3,4)[2])

meancol <- mean(resultatscol)
#score_ini_moyen(1000000)
mean_score <- mean(resultats)
sd_score <- sd(resultats)

# graph
ggplot(data.frame(score = resultats), aes(x = score)) +
  geom_histogram(aes(y = ..density..),binwidth = 1, fill = "#fddc9e", color = "#fd9a6a") +
  geom_vline(xintercept = mean_score, color = "#400f74", linetype = 4,  linewidth = 0.5) +
  labs(x = "Score initial",
       y = "Fréquence")+
  theme(panel.background = element_rect(fill = "gray98"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray80", linetype = 1,  linewidth=0.1),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "gray20"),
        text = element_text(color = "gray20")) +
  annotate("text", x = mean_score, y = 0.035, label = paste("Score moyen:", round(mean_score, 2)), color = "#d3436e", size = 4, hjust = 0, vjust = 1)+
  stat_function(fun = function(x) dnorm(x, mean = mean_score, sd = sd_score),
                color = "#7b2382", lwd=0.75, linetype=1)  

IC_99 <- c(mean_score-2.58*(sd_score/sqrt(1000000)),
           mean_score+2.58*(sd_score/sqrt(1000000)))
IC_99
IC_95 <-c(mean_score-1.96*(sd_score/sqrt(1000000)),
          mean_score+1.96*(sd_score/sqrt(1000000)))
IC_95

############## aleatoire
#table1 <- matrix(data = NA, nrow = 3, ncol = 4)
#table2 <- matrix(data = NA, nrow = 3, ncol = 4)
#
#score_ini2 <- function(a=3,b=4){
#  cartes_dispo= c(rep(-2,5),rep(-1,10),rep(0,15), rep(1:12,10))
#  count_col1=0
#  count_col2=0
#  for (i in 1:a) {
#    for (j in 1:b) {
#      u <- sample(cartes_dispo,1)
#      table1[i, j] <- u
#      num <- which(cartes_dispo==u) # quelle iteration?
#      cartes_dispo <- cartes_dispo[-num[1]]#Retirer la carte utilisée
#      v <- sample(cartes_dispo,1)
#      table2[i, j] <- v
#      num <- which(cartes_dispo==u) # quelle iteration?
#      cartes_dispo <- cartes_dispo[-num[1]]#Retirer la carte utilisée
#    }
#    print(length(cartes_dispo))
#  }
#  sum_sans_col1 <- 0
#  sum_sans_col2 <- 0
#  for (j in 1:4) {
#    if (length(unique(table1[, j])) > 1) {  # Vérifier s'il y au moins 2 éléments uniques dans la colonne
#      sum_sans_col1 <- sum_sans_col1 + sum(table1[, j])
#    }
#    else { (count_col1<- count_col1 + 1) }
#    if (length(unique(table2[, j])) > 1) {  # Vérifier s'il y au moins 2 éléments uniques dans la colonne
#      sum_sans_col2 <- sum_sans_col2 + sum(table2[, j])
#    }
#    else { (count_col2<- count_col2 + 1) }
#  }
#  return(c(sum_sans_col1,count_col1,sum_sans_col2,count_col2))
#}
#
#score_ini2(3,4)
#
##### choisir sous alpha ou retourner prochaine carte cachée solo
#score_alpha_solo <- function(alpha, a=3,b=4){
#  cartes_dispo= c(rep(-2,5),rep(-1,10),rep(0,15), rep(1:12,10))
#  count_col=0
#  score_tem=list()
#  table <- matrix(data = NA, nrow = 3, ncol = 4)
#  for (i in 1:a) {
#    for (j in 1:b) {
#      u <- sample(cartes_dispo,1)
#      table[i, j] <- u
#      num <- which(cartes_dispo==u) # quelle iteration?
#      cartes_dispo <- cartes_dispo[-num[1]]#Retirer la carte utilisée
#    }
#    #print(length(cartes_dispo))
#  }
#  score_tem <- c(score_tem, sum(table))
#  for (k in 3:12){
#    u <- sample(cartes_dispo,1)
#    if (u <= alpha){
#      table[ceiling(k/4), k%%3] <- u
#      num <- which(cartes_dispo==u) # quelle iteration?
#      cartes_dispo <- cartes_dispo[-num[1]]
#    }
#    score_tem <- c(score_tem, sum(table))
#    #print(score_tem)
#  }
#  return(score_tem)
#}
#
#score_alpha_solo(alpha, 3,4)
#
##resultats_alpha <- replicate(10000, score_alpha_solo(alpha, 3,4))
##a<- resultats_alpha
#n<-10000
#
#sc_moy_alpha <- function(a, n) {
#  moy<-list()
#  for (i in 1:11) {
#    ligne <- as.numeric(a[i,])
#    moy <- c(moy, sum(ligne)/n)
#  }
#  return(moy)
#}
#sc_moy_alpha(a, n)
#
#sc_moy_tot <- function(n) {
#  aa<-list()
#  for (i in -2:12) {
#    resultats<- replicate(n, score_alpha_solo(i, 3,4))
#    aa<- c(aa, sc_moy_alpha(resultats, n))
#  }
#  return(aa)
#}
#time<- Sys.time()
#vasystp<-sc_moy_tot(n)
#Sys.time()-time
#
#my_colors <- c("#E6E7A7", "#fddc9e", "#febb81", "#fd9a6a", "#f8765c", "#eb5760", "#d3436e", "#b73779", "#982d80", "#7b2382", "#5f187f", 
#               "#400f74", "#221150", "#0c0926", "#000004")
#
#matrice <- t(matrix(unlist(vasystp), nrow = 15, byrow = TRUE))
#
#df <- data.frame(x=1:11, matrice)
#names(df) <- c("x", "-2", "-1", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
#df
#df_long <- gather(df, key = "variable", value="value", -x)
#names(df_long)=c("x","Seuil","value")
#df_long$Seuil <- factor(df_long$Seuil, levels = c("x","-2","-1","0","1","2","3","4","5","6","7","8","9","10","11","12"))
#ggplot(df_long, aes(x = as.numeric(x), y = as.numeric(value), color = Seuil)) +
#  geom_line(linewidth=0.75) +
#  labs(x = "Nombre de tours",
#       y = "Score moyen") +
#  scale_color_manual(values = my_colors) +  
#  theme_minimal()+scale_x_continuous(breaks = 1:11)+
#  scale_y_continuous(breaks = seq(49, 65, by = 2))
#

#####
cartes_dispo= c(rep(-2,5),rep(-1,10),rep(0,15), rep(1:12,10))

# Créer une matrice de listes contenant des paires (numero, booleen)

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

score_alpha_solodoub <- function(alpha, a=3,b=4){
  init <- initial()
  tabnum<-do.call(rbind,init[1])
  tabbool<-do.call(rbind,init[2])
  cartes_dispo<-do.call(rbind,init[3])
  defausse<-as.numeric(init[4])
  score_tem <- list()
  score_tem <- c(score_tem, sum(tabnum))
  for (k in 3:12){
    pos <- place(tabbool)
    if (defausse <= alpha){   
      tabnum[pos[1], pos[2]] <- defausse
      tabbool[pos[1], pos[2]] <- 1
    }
    else{
      u <- sample(cartes_dispo,1)
      if (u <= alpha){
        tabnum[pos[1], pos[2]] <- u
        tabbool[pos[1], pos[2]] <- 1
        num <- which(cartes_dispo==u) # quelle iteration?
        cartes_dispo <- cartes_dispo[-num[1]]
      }
    }
    defausse <- sample(cartes_dispo,1)
    num <- which(cartes_dispo==defausse) # quelle iteration?
    cartes_dispo <- cartes_dispo[-num[1]]#Retirer la carte utilisée
    score_tem <- c(score_tem, sum(tabnum))
    #print(score_tem)
  }
  return(score_tem)
}


#resultats_alpha_doub <- replicate(10000, score_alpha_solodoub(alpha, 3,4))
#a<- resultats_alpha_doub
#n<-10000

sc_moy_alpha <- function(a, n) {
  moy<-list()
  for (i in 1:11) {
    ligne <- as.numeric(a[i,])
    moy <- c(moy, sum(ligne)/n)
  }
  return(moy)
}

sc_moy_tot <- function(n) {
  aa<-list()
  for (i in -2:12) {
    resultats<- replicate(n, score_alpha_solodoub(i, 3,4))
    aa<- c(aa, sc_moy_alpha(resultats, n))
  }
  return(aa)
}

time <- Sys.time()
vasystp<-sc_moy_tot(10000)
print(Sys.time()-time)

my_colors <- c("#E6E7A7", "#fddc9e", "#febb81", "#fd9a6a", "#f8765c", "#eb5760", "#d3436e", "#b73779", "#982d80", "#7b2382", "#5f187f", 
               "#400f74", "#221150", "#0c0926", "#000004")

matrice <- t(matrix(unlist(vasystp), nrow = 15, byrow = TRUE))
library(dplyr)
library(ggrepel)
df <- df %>% replace_na(list(column_name = "replacement_value"))
df <- data.frame(x=0:10, matrice)
names(df) <- c("x", "-2", "-1", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
df

df_long <- gather(df, key = "variable", value="value", -x)
names(df_long)=c("x","Seuil","value")

last_values <- df_long %>%
  group_by(Seuil) %>%
  filter(x == max(x))


ggplot(df_long, aes(x = as.numeric(x), y = as.numeric(value), color = Seuil)) +
  geom_line(linewidth = 0.75) +
  labs(x = "Nombre de tours",
       y = "Score moyen") +
  scale_color_manual(values = my_colors) +  
  theme_minimal() +
  scale_x_continuous(breaks = 0:10) +
  scale_y_continuous(breaks = seq(20, 65, by = 4))+
  geom_text_repel(data = subset(df_long, x == max(as.numeric(x))), aes(label = Seuil), direction = "x", segment.color = NA, box.padding = 0.1, force = 1, hjust = 0, nudge_x = 0.7, color = my_colors)
  
print(tabnum)
last_values


