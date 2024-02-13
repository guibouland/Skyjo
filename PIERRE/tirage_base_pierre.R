library(ggplot2)
cartes_dispo= c(rep(-2,5),rep(-1,10),rep(0,15), rep(1:12,10))

table <- matrix(data = NA, nrow = 3, ncol = 4)

score_ini <- function(a=3,b=4){
  count_col=0
  for (i in 1:a) {
    for (j in 1:b) {
      table[i, j] <- sample(cartes_dispo,1)
      #cartes_dispo <- setdiff(cartes_dispo, table[i, j]) # Retirer la carte utilisée
    }
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

#score_ini_moyen(100000)
sim <- score_ini_moyen(10000)

# score ini pour 1M itérations

resultats <- replicate(1000000, score_ini(3,4))

# graph
ggplot(data.frame(score = resultats), aes(x = score)) +
  geom_histogram(binwidth = 1, fill = "#EFD1FF", color = "#BA6DE3") +
  geom_vline(xintercept = sim, color = "#EFA70C", linetype = 4, size = 0.75) +
  labs(title = "Distribution des scores initiaux",
       x = "Score initial",
       y = "Fréquence")+
  theme(panel.background = element_rect(fill = "gray98"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "#EFA70C", linetype = 1, size=0.1),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "gray20"),
        text = element_text(color = "gray20")) +
  coord_cartesian(xlim = c(sim - 40, sim +40))+
  annotate("text", x = sim, y = 30000, label = paste("Score moyen:", round(sim, 2)), color = "#EFA70C", size = 4, hjust = 0, vjust = 1)