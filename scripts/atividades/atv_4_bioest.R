#ANOVA FATORIAL
#Gabriela Neres
#05 Junho 2025

#Packages ####
library(ggplot2)
library(dplyr)

#ANOVA FATORIAL EM BLOCO (GRADIENTE) ####

bloco <- factor(rep(seq(1:6),each = 4))

agua <- factor(c(rep(c(rep("sem",2), rep("com",2)),6)))
nut <- factor(c(rep(c(rep("sem nutriente",1), rep("com nutriente",1)),12)))
biomassa <- c(9.2, 12.8, 21.8, 33.4, 12.6, 22.6, 16.3, 26.7, 8.6, 11.8, 20.1, 34.8, 14.0, 14.6, 11.2, 41.9, 14.5, 4.2, 17.5, 30.6, 6.7, 12.9, 8.9, 29.4)

d <- data.frame(bloco, agua, nut, biomassa)
d <- arrange(d, bloco, agua, nut)
View(d)

# gráfico ####
ggplot(d, aes(agua, biomassa, fill = agua)) +
  stat_summary(geom = "bar", fun = mean) +
  stat_summary(geom = "errorbar", width = .2, fun.data = mean_se)+
  facet_wrap(~nut) +
  labs(x = "Adição de água", y = "Biomassa (g)") +
  theme(legend.position = "none")

mod <- aov(biomassa ~ bloco + agua*nut, data = d)
summary(mod)
anova(mod)

summary(lm(biomassa ~ bloco + agua*nut, data = d))$r.squared




