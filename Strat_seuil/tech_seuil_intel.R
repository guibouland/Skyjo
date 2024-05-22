  library(ggplot2)
  library(tidyr)
  library(colorspace)
  
  
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
  
  
  condition <- function(tabnum,tabbool, seuil=12){
    for(i in 1:nrow(tabnum)){
      for(j in 1:ncol(tabnum)){
        if((tabnum[i,j]>seuil)&&(tabbool[i,j]==1)){
          return(c(TRUE,i,j))
        }
      }
    }
    return(c(FALSE))
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
  
  score_alpha_solodoub <- function(a=3,b=4,seuil=12){
    init <- initial()
    tabnum<-do.call(rbind,init[1])
    tabbool<-do.call(rbind,init[2])
    cartes_dispo<-do.call(rbind,init[3])
    defausse<-as.numeric(init[4])
    score_tem <- list()
    score_tem <- c(score_tem, sum(tabnum))
    stop <- round(runif(1, 15,30),0)
    compteur<-0
    while(sum(tabbool)<12 && compteur<=stop){
      u <- sample(cartes_dispo,1)
      if (defausse <= seuil){
        a<-condition(tabnum,tabbool,seuil)
        if(a[1]){
          tabnum[a[2],a[3]] <- u
          #tabbool[a[2],a[3]] <- 1
        }
        else {
          pos<-place(tabbool)
          tabnum[pos[1], pos[2]] <- u
          tabbool[pos[1], pos[2]] <- 1
        }
      }
      else if(u<=seuil){
        a<-condition(tabnum,tabbool,seuil)
        if(a[1]){
          tabnum[a[2],a[3]] <- u
          #tabbool[a[2],a[3]] <- 1
          num <- which(cartes_dispo==u) 
          cartes_dispo <- cartes_dispo[-num[1]]
        }
        else {
          pos<-place(tabbool)
          tabnum[pos[1], pos[2]] <- u
          tabbool[pos[1], pos[2]] <- 1
          num <- which(cartes_dispo==u) 
          cartes_dispo <- cartes_dispo[-num[1]]
        }
      }
      else{
        pos<-place(tabbool)
        tabbool[pos[1], pos[2]] <- 1
      }
      score_tem <- c(score_tem, sum_sans_col(tabnum))
      compteur<- compteur+1
      defausse <- sample(cartes_dispo,1)
      num <- which(cartes_dispo==defausse) # quelle iteration?
      cartes_dispo <- cartes_dispo[-num[1]]
      
      if(compteur==stop){
        countfin <- countfin+1
      }
    }
    return(score_tem)
  }
  
  countfin <- 0
  #resultats_alpha_doub <- replicate(1000, score_alpha_solodoub(-1, 3,4))
  #normalisation(resultats_alpha_doub)[1]
  #taille99<-c(10,13,15,18,18,19,19,18,17,16,16,15,14,13,10)
  #n<-100
  
  
  
  normalisation <- function(listdeliste) {
    taille_99_percentile <- quantile(sapply(listdeliste, length), probs = 0.99)
    result <- lapply(listdeliste, function(inner_list) {
      tempsdejeu<-length(inner_list)
      if(tempsdejeu > 30) {
        inner_list <- inner_list[1:30]  # Raccourcir la liste à 30 éléments
      } else if(tempsdejeu < 30) {
        inner_list <- c(inner_list, rep(inner_list[tempsdejeu], 30 - tempsdejeu))  # Remplir avec la dernière valeur
      }
      return(inner_list)
    })
    return(result)
  }
  
  
  
  sc_moy_alpha <- function(b, n) {
    res <- list()
    for (i in 2:30) {
      sum <- 0
      for (j in 1:n) {
        sum <- sum + b[[j]][[i]]
      }
      res <- c(res, sum/n)
    }
    return(res)
  }
  
  ecart_types <- function(normalised){
    derniers_elements <- sapply(normalised, function(x) tail(x, 1))
    #print(derniers_elements)
    # Calculer l'écart-type des derniers éléments
    ecart_type <- sd(as.numeric(derniers_elements))
    
    # Retourner l'écart-type
    return(ecart_type)
  }
  
  sc_moy_tot <- function(n) {
    aa <- list()
    bb <- list()
    for (seuil in -2:12) {
      resultats <- replicate(n, score_alpha_solodoub(3, 4,seuil))
      b <- normalisation(resultats)
      #print("1")
      aa <- c(aa, sc_moy_alpha(b, n)[-1])
      #print("2")
      bb <- c(bb, ecart_types(b))
      print(ecart_types(b))
      #print("3")
    }
    return(c(aa, bb))
  }
  
  
  vasystp0<-sc_moy_tot(100)
  
  vasystp<- vasystp0[-(length(vasystp0) - 14):-length(vasystp0)]
  ectyp <- vasystp0[(length(vasystp0) - 14):length(vasystp0)]
  moyprgrph <- vasystp[seq(from=28, to=length(vasystp), by = 28)]
  
  datavase <- matrix(NA,3,15)
  
  for(i in 1:15){
    datavase[1,i] <- moyprgrph[[i]]
    datavase[2,i] <- moyprgrph[[i]]-2.58*(ectyp[[i]]/sqrt(10000))
    datavase[3,i] <- moyprgrph[[i]]+2.58*(ectyp[[i]]/sqrt(10000))
  }
  
  library(xtable)
  
  # Créer un objet xtable à partir de la matrice datavase
  #datavase_table <- xtable(datavase)
  #print(datavase_table, include.rownames = TRUE, include.colnames = TRUE, floating = FALSE)
  
  
  data <- data.frame(essai = 1:15, 
                     moyenne = datavase[1,], 
                     inf = datavase[2,], 
                     sup = datavase[3,])
  
  # Définition des étiquettes pour l'axe horizontal
  labels <- c("-2", "-1", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
  
  my_colors <- c("#fcfdbf", "#fddc9e", "#febb81", "#fd9a6a", "#f8765c", "#eb5760", "#d3436e", "#b73779", "#982d80", "#7b2382", "#5f187f", 
                      "#400f74", "#221150", "#0c0926", "#000004")

  matrice <- t(matrix(unlist(vasystp), nrow = 15, byrow = TRUE))
  df <- data.frame(x=1:28, matrice)
  names(df) <- c("x", "-2", "-1", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
  df
  df_long <- gather(df, key = "variable", value="value", -x)
  names(df_long)=c("x","Seuil","value")
  
  df_long$Seuil <- factor(df_long$Seuil, levels = c("x", "-2", "-1", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))
  
  ggplot(df_long, aes(x = as.numeric(x)-1, y = as.numeric(value), color = Seuil)) +
    geom_line(linewidth=0.85) +
    labs(x = "Nombre de tours",
         y = "Score moyen") +
    scale_color_manual(values = my_colors) + 
    scale_x_continuous(breaks = seq(0,28,5))+
    geom_text_repel(data = subset(df_long, x == max(as.numeric(x))), aes(label = Seuil), direction = "x", segment.color = NA, box.padding = 0.1, force = 1, hjust = 0, nudge_x = 0.7, color = my_colors)+
    theme_minimal()
  
  ggplot(data, aes(x = essai, y = moyenne)) +
    geom_col(fill = my_colors) +
    geom_errorbar(aes(ymin = inf, ymax = sup), width = 0.2, color = "darkgrey", linewidth = 0.3) +
    labs(x = "Seuil", y = "Score moyen", color = "Légende") +
    scale_x_continuous(breaks = 1:15, labels = labels)+
    theme_minimal()
  
  
