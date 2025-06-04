#Script Anova
#Gabriela Neres 
#03 Junho 2025

#Pacotes ####
#install.packages("ggplot2")
library(ggplot2)
library(dplyr)

#Dados ####
frutos <- c(1,2,3,4,5,7,8,9,10,10,8,10,11,11,13)
tratamento <- rep(c("Controle", "A", "B"), each = 5)
dados <- data.frame(tratamento, frutos)
View(dados)
str(dados)
dados$tratamento <- as.factor(dados$tratamento)
str(dados)

sum(dados$frutos)
media <- mean(dados$frutos)

val_cont <- dados$frutos[dados$tratamento == "Controle"]
media_cont <- mean(val_cont)
erro_cont <- sd(val_cont) / sqrt(length(val_cont))

val_a <- dados$frutos[dados$tratamento == "A"]
media_a <- mean(val_a)
erro_a <- sd(val_a) / sqrt(length(val_a))

val_b <- dados$frutos[dados$tratamento == "B"]
media_b <- mean(val_b)
erro_b <- sd(val_b) / sqrt(length(val_b))

grafico <- ggplot(dados, aes(x = factor(tratamento, levels = c("Controle", "A", "B")), y = frutos)) +
  stat_summary(geom = "bar", fun = mean, fill = "gray") +
  stat_summary(geom = "errorbar", width = 0.2, fun.data = mean_se) +
  scale_x_discrete(labels = c("Controle" = "Controle", "A" = "Inseticida A", "B" = "Inseticida B")) +
  scale_y_continuous(limits = c(0, 15), breaks = c(0, 5, 10, 15)) +
  labs(y = "Número de frutos", x = "Tratamento") +
  theme_classic() +
  theme(
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 12)
  )

# Exibe a figura
print(grafico)

# Salva a figura como .jpg com largura = 4 e altura = 3
ggsave("output/figures/grafico_tratamentos.jpg", plot = grafico, width = 4, height = 3, units = "in", dpi = 300)

# Soma dos quadrados
# Total
DIFt <- c()
for (i in 1:15){
  DIFt <- c(DIFt, (dados$frutos[i] - media)^2)
}
SQt <- sum (DIFt)

# Erro

categorias <- unique(dados$tratamento)
DIFe <- c()
for (c in categorias){
  for (i in 1:5){
    val <- dados$frutos[dados$tratamento == c]
    DIFe <- c(DIFe, (val[i] - mean(val))^2)
  }
}

SQe <- sum (DIFe)

#Tratamentos

categorias <- unique(dados$tratamento)
DIFtrt <- c()
for (c in categorias){
    val <- dados$frutos[dados$tratamento == c]
    for (i in 1:5){
      DIFtrt <- c(DIFtrt, (mean(val) - media)^2)
    }
}

SQtrt <- sum (DIFtrt)

# Média dos quadrados

Mtrt <- SQtrt / 2
Merro <- SQe / 12

# Valor de F

valor_f <- Mtrt / Merro

# Valor de P

valor_p <- 1-pf(valor_f, 2, 12)

# Coeficiente de determinação

coef_det <- SQtrt/SQt

# Teste de Tukey

mod_anova <- aov(frutos ~ tratamento, data = dados)
#Pos-doc test - Comparando niveis a posteriori
tukey<-TukeyHSD(mod_anova)
tukey

#Representacao grafica do Tukey
plot(tukey, las = 1)
