#ANOVA FATORIAL
#Carlos Roberto Fonseca
#03 Junho 2025

#Packages
library(ggplot2)
library(dplyr)


#VARIAVEIS
# Y  - VARIAVEL CONTINUA
# X1 - VARIAVEL CATEGORICA
# X2 - VARIAVEL CATEGORICA

#HIPOTESES
# H0: X1 NAO INFLUENCIA Y
# H1: X1 INFLUENCIA Y

# H0: X2 NAO INFLUENCIA Y
# H1: X2 INFLUENCIA Y

# H0: NAO HA INTERACAO ENTRE X1 E X2 PARA DETERMINAR Y
# H1: HA INTERACAO ENTRE X1 E X2 PARA DETERMINAR Y


#EXEMPLO 1. ANOVA SIMPLES COM DOIS NIVEIS ####
#TRATAMENTO AGUA COM DOIS NIVEIS (-A e +A)
#N = 20, sendo 10 PLANTAS POR TRATAMENTO
#ALEATORIZACAO DECIDE QUAL PLANTAS RECEBE QUAL TRATAMENTO
agua <- factor(c(rep("-A",10), rep("+A",10)))
set.seed(245)
crescimento <- round(c(rnorm(10,8,4), rnorm(10,16,5)),2)
d <- data.frame(agua, crescimento)
View(d)

mod_agua <- aov(crescimento ~ agua, data = d)
anova(mod_agua)

ggplot(d, aes(agua, crescimento)) +
  stat_summary(geom = "bar", fun = mean, fill = "gray") +
  stat_summary(geom = "errorbar", width = .2, fun.data = mean_se) +
  labs(x = "Agua", y = "Crescimento (cm / ano)")


#EXEMPLO 2. ANOVA SIMPLES COM QUATRO NIVEIS ####
#TRATAMENTO NUTRIENTE COM QUATRO NIVEIS (0,10,20,30 g/m2)
#N= 20, sendo 5 PLANTAS POR NIVEL
#ALEATORIZACAO DECIDE QUAL PLANTAS RECEBE QUAL TRATAMENTO
nut <- factor(c(rep("0",5), rep("10",5), rep("20",5), rep("30",5)))
set.seed(123)
crescimento <- round(c(rnorm(5,8,3),rnorm(5,16,5),rnorm(5,24,3),rnorm(5,24,5)),2)
d <- data.frame(nut, crescimento)
View(d)

mod_nut4 <- aov(crescimento~nut, data = d)
anova(mod_nut4)

ggplot(d, aes(nut, crescimento)) +
  stat_summary(geom = "bar", fun = mean, fill = "gray") +
  stat_summary(geom = "errorbar", width = .2, fun.data = mean_se) +
  labs(x = expression ("Nutriente"~(g/m^2)), y = "Crescimento (cm / ano)")


#EXEMPLO 3. ANOVA SIMPLES EM BLOCO ####
#TRATAMENTO AGUA COM DOIS NIVEIS (-A e +A)
#10 BLOCOS COM DUAS PLANTAS EM CADA (N = 20)
#RANDOMIZACAO PARA DEFINIR TRATAMENTO FEITA DENTRO DO BLOCO (CARA,COROA)
agua <- factor(c(rep("-A",10), rep("+A",10)))
bloco <- factor(rep(seq(1:10),2))
crescimento <- c(1,5,8,5,10,8,12,15,13,17,3,7,11,6,15,10,16,21,12,19)
d <- data.frame(bloco, agua, crescimento)
d <- arrange(d, bloco, agua)
View(d)

mod_agua <- aov(crescimento ~ agua, data = d)
summary(mod_agua)

ggplot(d, aes(agua, crescimento)) +
  stat_summary(geom = "bar", fun = mean, fill = "gray") +
  stat_summary(geom = "errorbar", width = .2, fun.data = mean_se) +
  labs(x = "Agua", y = "Crescimento (cm / ano)") +
  geom_point(size = 2, alpha = 0.5)

mod_blocoagua <- aov(crescimento ~ bloco + agua, data = d)
summary(mod_blocoagua)

ggplot(d, aes(agua, crescimento, colour = bloco)) +
  geom_point (size = 4) +
  geom_segment(x = "-A", y = crescimento[1], xend = "+A", yend = crescimento[11], colour = "black") +
  geom_segment(x = "-A", y = crescimento[2], xend = "+A", yend = crescimento[12], colour = "black") +
  geom_segment(x = "-A", y = crescimento[3], xend = "+A", yend = crescimento[13], colour = "black") +
  geom_segment(x = "-A", y = crescimento[4], xend = "+A", yend = crescimento[14], colour = "black") +
  geom_segment(x = "-A", y = crescimento[5], xend = "+A", yend = crescimento[15], colour = "black") +
  geom_segment(x = "-A", y = crescimento[6], xend = "+A", yend = crescimento[16], colour = "black") +
  geom_segment(x = "-A", y = crescimento[7], xend = "+A", yend = crescimento[17], colour = "black") +
  geom_segment(x = "-A", y = crescimento[8], xend = "+A", yend = crescimento[18], colour = "black") +
  geom_segment(x = "-A", y = crescimento[9], xend = "+A", yend = crescimento[19], colour = "black") +
  geom_segment(x = "-A", y = crescimento[10], xend = "+A", yend = crescimento[20], colour = "black")
  

#EXEMPLO 4. ANOVA DE DOIS FATORES (SEM INTERACAO) ####
#TRATAMENTO AGUA E NUTRIENTE COM DOIS NIVEIS CADA
#20 PLANTAS NO TOTAL
#PRIMEIRA RANDOMIZACAO DECIDE AS DEZ PLANTAS QUE VAO RECEBER AGUA OU NAO
#SEGUNDA RANDOMIZACAO. PARA CADA TRATAMENTO DECIDE AS 5 QUE VAO RECEBER NUTRIENTE OU NAO
agua <- factor(c(rep("-A",10), rep("+A",10)))
nut <-  factor(rep(c(rep("-N",5), rep("+N",5)),2))
set.seed(123)
crescimento <- round(c(rnorm(5,10,3), rnorm(5,10,3), rnorm(5,30,3), rnorm(5,50,5)),2)
d <- data.frame(agua, nut, crescimento)
View(d)

mod_dois <- aov(crescimento ~ agua + nut, data = d)
anova(mod_dois)


#EXEMPLO 5. ANOVA FATORIAL DE DOIS FATORES (COM INTERACAO) ####
#TRATAMENTO AGUA E NUTRIENTE COM DOIS NIVEIS CADA
#20 PLANTAS NO TOTAL
#PRIMEIRA RANDOMIZACAO DECIDE AS DEZ PLANTAS QUE VAO RECEBER AGUA OU N?O
#SEGUNDA RANDOMIZACAO. PARA CADA TRATAMENTO DECIDE AS 5 QUE VAO RECEBER NUTRIENTE OU N?O
agua <- factor(c(rep("-Agua",10), rep("+Agua",10)))
nut <-  factor(rep(c(rep("-Nutriente",5), rep("+Nutriente",5)),2))
crescimento <- round(c(rnorm(5,10,4), rnorm(5,10,4), rnorm(5,30,4), rnorm(5,50,5)),2)
d <- data.frame(agua, nut, crescimento)
View(d)

mod_fatorial <- aov(crescimento~agua+nut+agua:nut)
mod_fatorial <- aov(crescimento~agua*nut)
anova(mod_fatorial)


ggplot(d, aes(agua, crescimento, fill = agua)) +
  stat_summary(geom = "bar", fun = mean) +
  stat_summary(geom = "errorbar", width = .2, fun.data = mean_se)+
  facet_wrap(~nut) +
  labs(x = "Agua", y = "Crescimento (cm / ano)") +
  theme(legend.position = "none")


#EXEMPLO 6. ANOVA FATORIAL EM BLOCO (GRADIENTE) ####
agua <- factor(c(rep("-A",10), rep("+A",10)))
nut <-  factor(rep(c(rep("-N",5), rep("+N",5)),2))
crescimento <- c(1,3,6,9,11,2,4,7,10,13,14,16,17,19,20,23,21,27,26,28)
bloco <- factor(rep(seq(1:5),4))
d <- data.frame(bloco, agua, nut, crescimento)
d <- arrange(d, bloco, agua, nut)
View(d)

#Figura 6a - Variacao entre blocos - gradiente ambiental?
ggplot(d, aes(bloco, crescimento)) +
  stat_summary(geom = "bar", fun = mean, fill = "gray") +
  stat_summary(geom = "errorbar", width = .2, fun.data = mean_se) +
  labs(x = "Bloco", y = "Crescimento (cm / ano)")

Mod_sembloco <- aov(crescimento ~ agua*nut)
summary(Mod_sembloco)

mod_combloco <- aov(crescimento ~ bloco + agua*nut)
summary(mod_combloco)

#Representacao normal - variacao entre blocos nao controlada
ggplot(d, aes(agua, crescimento, fill = agua)) +
  stat_summary(geom = "bar", fun = mean) +
  stat_summary(geom = "errorbar", width = .2, fun.data = mean_se)+
  facet_wrap(~nut) +
  labs(x = "Agua", y = "Crescimento (cm / ano)") +
  theme(legend.position = "none")

#Representacao alternativa - controlando pelo bloco
#Crescimento das plantas e dado pela diferença em relacao a media do bloco
mod_b <- lm(crescimento ~ bloco, data = d)
d$cresc_b <- mod_b$residuals

ggplot(d, aes(agua, cresc_b, fill = agua)) +
  stat_summary(geom = "bar", fun = mean) +
  stat_summary(geom = "errorbar", width = .2, fun.data = mean_se)+
  facet_wrap(~nut) +
  labs(x = "Agua", y = "Crescimento ǀ bloco (cm / ano)") +
  theme(legend.position = "none")


#EXEMPLO 7. ANOVA FATORIAL TRES FATORES EM BLOCO ####
agua <- factor(c(rep(c(rep("-Agua",4), rep("+Agua",4)),3)))
fos <-  factor(rep(c(rep(c(rep("-F",2), rep("+F",2)),6))))
nit<-  factor(rep(c(rep("-N",1), rep("+N",1)),12))
bloco <- factor(rep(seq(1:3),each = 8))
crescimento <- c(1,3,6,9,11,23,4,47,  10,13,14,23,17,19,20,44,23,30,26,28,24,67,76,73)
d <- data.frame(bloco,agua,fos,nit,crescimento)
d <- arrange(d, bloco, agua, fos, nit)

View(d)

mod_tres <- aov(crescimento ~ bloco + agua*fos*nit, data = d)
summary(mod_tres)

ggplot(d, aes(x = agua, y = crescimento, fill = agua)) +
  stat_summary(geom = "bar", fun = mean) +
  stat_summary(geom = "errorbar", width = .2, fun.data = mean_se)+
  facet_wrap(fos~nit, ncol = 4) +
  labs(x = "Tratamentos", y = "Crescimento (cm / ano)") +
  theme(legend.position = "none")

ggplot(d, aes(fos:nit, crescimento, fill = fos:nit)) +
  stat_summary(geom = "bar", fun = mean) +
  stat_summary(geom = "errorbar", width = .2, fun.data = mean_se)+
  facet_wrap(agua) +
  labs(x = "Tratamentos", y = "Crescimento (cm / ano)") +
  theme(legend.position = "none")

#Representacao de anova fatorial em bloco
mod_b <- lm(crescimento ~ bloco, data = d)
d$cresc_b <- mod_b$residuals

ggplot(d, aes(fos:nit, cresc_b, fill = fos:nit)) +
  stat_summary(geom = "bar", fun = mean) +
  stat_summary(geom = "errorbar", width = .2, fun.data = mean_se)+
  facet_wrap(agua) +
  labs(x = "Tratamentos", y = "Crescimento (cm / ano)") +
  theme(legend.position = "none")

