a<-list(rep(-2, 5))
b<- list(rep(-1, 10))
c<- list(rep(0, 15))
d<- list(rep(1, 10))
e<- list(rep(2, 10))
f<- list(rep(3, 10))
g<- list(rep(4, 10))
h<- list(rep(5, 10))
i<- list(rep(6, 10))
j<- list(rep(7, 10))
k<- list(rep(8, 10))
l<- list(rep(9, 10))
m<- list(rep(10, 10))
n<- list(rep(11, 10))
o<- list(rep(12, 10))

fin<-c(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
print(fin)
z<-unlist(fin, recursive = FALSE)
jeu<-list(z)

tirage<-sample(z, 12)

# fonction qui renvoie le score moyen en début de partie
f<- function(n){
  l<-c(rep(0, n))
  m<-c(rep(0, n))
  for (i in 1:n) {
    tirage_i <- sample(z, 12)
    m[[i]] <- median(tirage_i)
    l[[i]] <- sum(tirage_i)
  }
  moy<- mean(l)
  med <- mean(m)
  return(c(Moyenne = moy, Médiane = med))
}
f(100000)

score_init_moy<- function(n){
  l<-c(rep(0, n))
  for (i in 1:n) {
    tirage_i <- sample(z, 12)
    l[[i]] <- sum(tirage_i)
  }
  moy<- mean(l)
  return(moy)
}
# ca a l'air de converger vers 61

median(z)


res <- replicate(1000000, score_init_moy(1))

ggplot(data.frame(score= res), aes(x=score))+
  geom_histogram(binwidth = 1, fill="lavender", color="purple")+
  labs(title = "Distribution des scores initiaux moyens", 
       x="Score initial moyen",
       y="Fréquence")+
  theme(panel.background = element_rect(fill="white"),
        plot.background = element_rect(fill="white"),
        panel.grid.major = element_line(color="gray40"),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color="white"),
        text=element_text(color="white"))

