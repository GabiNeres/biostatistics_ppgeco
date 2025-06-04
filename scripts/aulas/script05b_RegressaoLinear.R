#Regressao Linear ####
#Carlos Roberto Fonseca
#01 Junho 2025

# pacotes ####
library(ggplot2)

#ACHANDO O MODELO POR TENTATIVA E ERRO ####

# Minhas hipoteses ####
#Testando a relacao entre concentracao de nutrientes (x) e produtividade (y)
#H0: A concentracao de nutrientes nao afeta a produtividade
#H1: A concentracao de nutrientes afeta a produtividade

#Criando meus dados ####
#CRIANDO A RELACAO PURA ENTRE X e Y (SO DEUS SABE)
set.seed(123)
x <- rnorm(50,100,20)
y <- 5*x + 10
plot(y~x)

#ADICIONANDO ERRO NO Y (COISA DO DIABO)
set.seed(456)
erro <- rnorm(50)*20
y <- 5*x + 10 + erro
plot(y~x)


#Criando um dataframe ####
meusdados <- data.frame(x,y)
names(meusdados) <- c("Nutriente", "Produtividade")

#Tentando adivinhar os parametros ####
#Chute valores de a e b
a <- 100
b <- 0

meusdados$estimado <- b*meusdados$Nutriente + a   # Save the predicted values
meusdados$residuos <- meusdados$Produtividade - meusdados$estimado # Save the residual values

# Contrastando os dados com o modelo provisorio
ggplot(meusdados, aes(x = Nutriente, y = Produtividade)) +
  geom_segment(aes(xend = Nutriente, yend = estimado), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuos), size = abs(residuos))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = estimado), shape = 1) +
  geom_abline(intercept = a, slope = b, color="blue", size=1) +
  theme_bw()

##Soma dos quadrados do modelo provisorio ####
SQerro <- sum((meusdados$Produtividade-meusdados$estimado)^2)
SQerro

#Altere o "a" e "b" da regressao para obter um melhor ajuste


#REGRESSAO LINEAR ####

#Modelo de regressao ####
mod_reg <- lm(Produtividade ~ Nutriente, data = meusdados)
summary(mod_reg)
anova(mod_reg)

# Scatterplot ####
ggplot(data = meusdados, aes(x = Nutriente,y = Produtividade)) +
  geom_point(size=3) +
  geom_smooth(method = "lm", se = TRUE, color="blue", size=1) +
  xlab("Concentracao de nutrientes (g/L)") +
  ylab("Produtividade (g/ha/ano)") +
  theme_classic()


#RODANDO A REGRESSAO DA AULA TEORICA ####
#Preparando os dados
Altura <- c(3,2,1,4,3,2,5,4,3,6,5,4,7,6,5)
Frutos <- c(30,40,50,40,50,60,50,60,70,60,70,80,70,80,90)
d <- data.frame(Altura,Frutos)

#Verificando as medias ####
mean(Altura)
mean(Frutos)

#Regressao linear ####
mod <- lm(Frutos ~ Altura, data = d)
summary(mod)
anova(mod)

#Figura de regressao ####
ggplot(d, aes(x = Altura, y = Frutos)) +
  geom_point(size = 5, colour = "black") +
  lims(x = c(0,8), y = c(0,100)) +
  labs (x = "Altura (m)", y = "Número de frutos") +
  theme(axis.text = element_text(size = 14, colour = "blue")) +
  theme(axis.title = element_text(size = 20, colour = "blue")) +
  geom_smooth(method = "lm", se = FALSE, size = 2) +
  annotate("text", y=mean(Frutos), x=mean(Altura), label="+", colour = "red", size = 10)

#Salvando a figura ####
ggsave("output/figures/Fig_Regressao.pdf", width = 6, height = 4)

#Conferindo o beta ####
#beta = covarianciaXY / varianciaX
beta <- cov(d$Frutos,d$Altura)/var(d$Altura)
beta

#Conferindo o intercepto####
#intercepto = mediaY - b*mediaX
intercepto <- mean(d$Frutos) - mean(d$Altura)*beta
intercepto

#ANALISE DOS RESIDUOS (HETEROCEDASTICIDADE, NORMALIDADE) ####
par(mfrow=c(1,1))						
plot(mod)					



