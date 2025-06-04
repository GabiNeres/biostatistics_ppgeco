#Script Anova
#Carlos Roberto Fonseca
#27 Novembro 2024

#Pacotes ####
#install.packages("ggplot2")
library(ggplot2)

#Dados ####
Height <- c(2,5,4,1,3,4,6,5,7,3,7,9,8,6,5)
Nutrient <- rep(c("Low", "Medium", "High"), each = 5)
dados <- data.frame(Nutrient, Height)
View(dados)
str(dados)
dados$Nutrient <- as.factor(dados$Nutrient)
str(dados)


#Representacao grafica ####
ggplot(dados, aes(x = Nutrient, y = Height)) +
  geom_point(size = 5)

ggplot(dados, aes(x = Nutrient, y = Height)) +
  geom_jitter(width = 0.15, height = 0, size = 5)

ggplot(dados, aes(x = Nutrient, y = Height)) +
  geom_boxplot()

ggplot(dados, aes(x = Nutrient, y = Height)) +
  stat_summary(geom = "bar", fun = mean, fill = "gray") +
  stat_summary(geom = "errorbar", width = .2, fun.data = mean_se)

ggplot(dados, aes(x = factor(Nutrient, levels = c("Low", "Medium", "High")), y = Height)) +
  stat_summary(geom = "bar", fun = mean, fill = c("#deebf7", "#9ecae1","#3182bd")) +
  stat_summary(geom = "errorbar", width = .2, fun.data = mean_se) +
  theme_bw() +
  labs(y = "Plant height (cm)", x = "Nutrient addition") +
  theme(axis.title = element_text(size = 16)) + 
  theme(axis.text = element_text(size = 14))
  
ggplot(dados, aes(x = factor(Nutrient, levels = c("Low", "Medium", "High")), y = Height)) +
  stat_summary(geom = "bar", fun = mean, fill = c("#deebf7", "#9ecae1","#3182bd")) +
  stat_summary(geom = "errorbar", width = .2, fun.data = mean_se) +
  scale_y_continuous(limits=c(0,10), breaks = c(0,2,4,6,8,10)) +
  labs(y = "Plant height (cm)", x = "Nutrient addition") +
  theme(axis.title = element_text(size = 16)) + 
  theme(axis.text = element_text(size = 14)) +
  geom_jitter(width = 0.10, height = 0, size = 3, colour = "black", alpha = 0.3) +
  theme_bw() 
  

#Rodando uma ANOVA usando aov
mod_anova <- aov(Height ~ Nutrient)
summary(mod_anova)

#Verificando os pressupostos dos modelos
#Homocedasticidade & Normalidade
#Homocedasticidade = a variância é parecida entre os grupos
plot(mod_anova)


#Pos-doc test - Comparando niveis a posteriori
tukey<-TukeyHSD(mod_anova)
tukey

#Representacao grafica do Tukey
plot(tukey, las = 1)

#Contrastes a priori
c1 = c(-2,1,1) # Contraste entre o controle (low) e os outros grupos
c2 = c(0,-1,1) # Contraste entre medium e high, o 0 desconsidera o low
contrasts(dados$Nutrient) = cbind(c1,c2)
mod_anova = aov(Height ~ Nutrient, data = dados)
summary(mod_anova, split = list(Nutrient = list("Low vs Others" = 1, "Medium vs High" = 2)))

#Outras funcoes para ANOVA
#funcao lm
mod2 <- lm(Height ~ Nutrient)
anova(mod2)
summary(mod2)

SQ_erro <- sum((mod2$residuals)^2)
SQ_modelo <- sum((mod2$fitted.values-mean(Height))^2)

#glm
mod3 <- glm(Height ~ Nutrient, family = gaussian)
summary(mod3)



            