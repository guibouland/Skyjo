#comparaison seuils

library(ggplot2)
library(ggpattern)

tabcompare <- data.frame(Stratégie=c("c","d","a","b"),
                         group = factor(c(1, 2, 1, 2)),
                         moyenne = c(19.76, 20.47, 9.57, 11.53), 
                         inf = c(19.45, 20.21, 9.31, 11.33),
                         sup = c(20.07, 20.72, 9.82, 11.74))

ggplot(tabcompare, aes(x = Stratégie, y = moyenne, group = group)) +
  geom_col_pattern(
    aes(pattern = Stratégie, fill = Stratégie, pattern_fill = Stratégie), 
    colour = 'white', 
    pattern_density = 0.3, 
    pattern_key_scale_factor = 1.3,
    pattern_colour="#fe9a8a") +
  theme_bw() +
  geom_errorbar(aes(ymin = inf, ymax = sup), width = 0.4, color = "darkgrey", linewidth = 0.6) +
  scale_fill_manual(values = c(c = '#eb5760', d = '#d3436e', a = '#f8765c', b = '#eb5760'),
                    labels = c(c = "\nPremière", d = "position \n", a = "\nPosition", b = "maximale \n")) +
  scale_pattern_fill_manual(values = c(c = '#eb5760', d = '#b73779', a = '#fd9a6a', b = '#f8765c'),
                            labels = c(c = "\nPremière", d = "position \n", a = "\nPosition", b = "maximale \n")) +
  scale_pattern_manual(values = c(c = 'none', d = 'none', a = 'stripe', b = 'stripe'),
                       labels = c(c = "\nPremière", d = "position \n", a = "\nPosition", b = "maximale \n")) +
  labs(x = "Seuils", y = "Score moyen", color = "Légende") +
  theme(legend.key.size = unit(1.5, 'cm')) +
  scale_x_discrete(labels = c("c" = "3", "d" = "4", "a" = "2", "b" = "3")) +
  coord_cartesian(ylim = c(0, 20)) +
  scale_y_continuous(breaks = seq(0, 23, 2)) +
  guides(fill = guide_legend(nrow = 2), position="center") +
  theme_minimal()

compare <- data.frame(Seuil1_3 = c(55.7766,53.4198,51.1066,48.7819,46.5719,44.2149,41.8996,39.5809,37.3381,34.9743,32.6989,30.4252,28.2415,26.233,24.4838,23.0249,21.8868,21.1151,20.5979,20.2607,20.0344,19.895,19.8205,19.7912,19.7738,19.7671,19.7609,19.761),
                      Seuil1_4 = c(55.3207,52.9031,50.4593,48.1029,45.6757,43.3104,41.001,38.6493,36.2108,33.8579,31.5385,29.267,27.2316,25.4772,23.9489,22.7266,21.8645,21.2674,20.9118,20.6999,20.5811,20.5142,20.4877,20.4725,20.4668,20.4666,20.4666,20.4666),
                      Seuil2_2 = c(52.6228,49.0496,45.5443,42.0430,38.5671,35.1059,31.6668,28.2978,24.8887,21.5065,18.2340,15.2804,13.0274,11.3925,10.4424,9.9721,9.8033,9.7656,9.7585,9.7585,9.7585,9.7585,9.7585,9.7585,9.7585,9.7585,9.7585,9.7585),
                      Seuil2_3 = c(51.9077,48.2414,44.6623,41.2357,37.6957,34.2804,30.8203,27.3465,23.8863,20.5373,17.4539,14.9751,13.2495,12.2788,11.7874,11.6230,11.5711,11.5566,11.5542,11.5535,11.5535,11.5535,11.5535,11.5535,11.5535,11.5535,11.5535,11.5535))
ggplot(compare, aes(x=0:27)) +
  geom_line(aes(y=Seuil1_3, color="Seuil2_2"), linetype="solid",linewidth=0.85) +
  geom_line(aes(y=Seuil1_4, color="Seuil2_3"), linetype="solid",linewidth=0.85) +
  geom_line(aes(y=Seuil2_2, color="Seuil1_3"), linetype="solid",linewidth=0.85) +
  geom_line(aes(y=Seuil2_3, color="Seuil1_4"), linetype="solid",linewidth=0.85) +
  labs(x = "Nombre de tours", y = "Score moyen", color="          Stratégie \n\n  Position      Première  \n maximale      position") +
  scale_color_manual(values=c( "#b73779", "#5f187f","#fddc9e", "#f8765c"),
                     labels=c( "Seuil 2", "Seuil 3","Seuil 3", "Seuil 4")) +
  guides(color = guide_legend(ncol = 2))+
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  theme_minimal()
