##Histograma##

(p6 <- ggplot(model.lm1, aes(x = model.lm1$residuals)) +
  geom_histogram(bins = 11,
                 colour = "black",
                 fill = "#CC662f",
                 aes(y=..density..),
                 alpha = 0.75) +
  labs(title = "Histograma",
       x = "Residuals",
       y = "Frequency") + 
  geom_density(color = "#003350") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 7),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 8.5),
        plot.subtitle = element_text(size = 7.5)))

##Q-Q Plot

(qq_G <- ggplot(Splitplot_02, aes(sample = pheno1)) + 
  stat_qq(color = "#003350") + 
  stat_qq_line(color = "#CC662f") +
  theme_bw() +
  labs(title = "QQ-Plot") +
  theme(axis.title = element_blank(),
        plot.title = element_text(size = 12)))

gridExtra::grid.arrange(p6, p6, qq_G,
                        nrow = 2)

grid.arrange(p6, p6, qq_G, 
             layout_matrix = rbind(c(1, 3),
                                   c(2, 3)))

