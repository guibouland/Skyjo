#comp_col

library(ggplot2)
library(ggpattern)

tabcompare <- data.frame(Stratégie=c("c","d","b","a"),
                         group = c(1, 1, 2,2),
                         moyenne = c(10.46, 11.53,10.90, 8.89), 
                         inf = c(10.22, 11.33,10.71, 8.65),
                         sup = c(10.70, 11.74,11.10, 9.14))

my_colors <- c("#fd9a6a", "#f8765c", "#eb5760", "#d3436e")
my_patterns <- c("crosshatch", "crosshatch", "none", "none")
my_colorsbis <- c( "#fd9a6a", "#eb5760", "#982d80","blue")
seuilslab <- c("Seuil 2", "Seuil 3", "Seuil 2", "Seuil 3")

ggplot(tabcompare, aes(x = Stratégie, y = moyenne, group = group)) +
  geom_col_pattern(
    aes(pattern = Stratégie, fill = Stratégie, pattern_fill = Stratégie), 
    colour = 'white', 
    pattern_density = 0.3, 
    pattern_key_scale_factor = 1.4,
    pattern_colour="#fdAa6a") +
  geom_errorbar(aes(ymin = inf, ymax = sup), width = 0.4, color = "darkgrey", linewidth = 0.6) +
  scale_fill_manual(values = c(c = '#eb5760', d = '#d3436e', a = '#f8765c', b = '#eb5760'),
                    labels = c(c = "\nColonne\n   sous ", d = "contrainte\n de seuil \n", a = "\n Seuil", b = "hybride \n")) +
  scale_pattern_fill_manual(values = c(c = '#eb5760', d = '#d3436e', a = '#fd9a6a', b = '#f8765c'),
                            labels = c(c = "\nColonne\n   sous ", d = "contrainte\n de seuil \n", a = "\n Seuil", b = "hybride \n")) +
  scale_pattern_manual(values = c(c = 'none', d = 'none', a = 'crosshatch', b = 'crosshatch'),
                       labels = c(c = "\nColonne\n   sous ", d = "contrainte\n de seuil \n", a = "\n Seuil", b = "hybride \n")) +
  labs(x = "Seuils", y = "Score moyen", color = "Légende") +
  theme(legend.key.size = unit(1.5, 'cm')) +
  scale_x_discrete(labels = c("c" = "3", "d" = "4", "a" = "2", "b" = "3")) +
  scale_y_continuous(breaks = seq(0, 12, 2)) +
  coord_cartesian(ylim = c(0, 12)) +
  guides(fill = guide_legend(nrow = 2), position="center") +
  theme_minimal()
  




compare <- data.frame(col1_3 = c(60.858,55.834,51.922,48.279,44.646,41.053,37.555,34.024,30.552,26.985,23.471,20.034,16.880,14.365,12.637,11.644,11.159,10.972,10.919,10.905,10.905,10.904,10.904,10.904,10.904,10.904,10.904,10.904,10.904,10.9048,10.9048,10.9048,10.9048,10.9048,10.9048,10.9048,10.9048,10.9048,10.9048),
                      col1_2 = c(60.7316,56.2182,52.4661,48.8992,45.3637,41.8611,38.3252,34.8167,31.3437,27.8323,24.3565,20.9106,17.6680,14.7412,12.3132,10.5878,9.5885,9.1303,8.9486,8.9013,8.8932,8.8926,8.8926,8.8926,8.8926,8.8926,8.8926,8.8926,8.8926,8.8926,8.8926,8.8926,8.8926,8.8926,8.8926,8.8926,8.8926,8.8926,8.8926 ),
                      col22_2 = c(60.6436,56.2112,52.5972,49.2908,46.1958,42.9154,39.6588,36.43,33.1668,29.8922,26.8052,23.67,20.5288,17.7658,15.323,13.2692,11.9292,11.1396,10.7302,10.5612,10.4836,10.4658,10.4588,10.4572,10.4572,10.4572,10.4572,10.4572,10.4572,10.4572,10.4572,10.4572,10.4572,10.4572,10.4572,10.4572,10.4572,10.4572,10.4572),
                      col22_3 = c(61.0608,56.3674,52.6646,49.3,45.9778,42.7262,39.4046,36.1112,32.7694,29.4444,26.2644,23.1116,20.1048,17.385,15.162,13.4854,12.456,11.927,11.6832,11.5858,11.5466,11.5374,11.5348,11.5348,11.5348,11.5348,11.5348,11.5348,11.5348,11.5348,11.5348,11.5348,11.5348,11.5348,11.5348,11.5348,11.5348,11.5348,11.5348))


ggplot(compare[1:31,], aes(x=0:30)) +
  geom_line(aes(y=col1_2, color="col1_2"), linetype="solid",linewidth=0.85) +
  geom_line(aes(y=col1_3, color="col1_3"), linetype="solid",linewidth=0.85) +
  geom_line(aes(y=col22_2, color="col22_2"), linetype="solid",linewidth=0.85) +
  geom_line(aes(y=col22_3, color="col22_3"), linetype="solid",linewidth=0.85) +
  labs(x = "Nombre de tours", y = "Score moyen", color="        Stratégies \n\n  Seuil     Colonne sous\n hybride      contrainte\n                    de seuil") +
  scale_color_manual(values=c("#febb81", "#eb5760", "#b73779", "#5f187f"),
                     labels=c("Seuil 2", "Seuil 3", "Seuil 3", "Seuil 4")) +
  guides(color = guide_legend(ncol = 2))+
  scale_y_continuous(breaks = seq(0,60,5))+
  scale_x_continuous(breaks = seq(0,30,2))+
  theme_minimal()
