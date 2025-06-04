#Regressao Linear ####
#Gabriela Neres
#04 Junho 2025

# pacotes ####
library(ggplot2)

#RODANDO A REGRESSAO ####
#Preparando os dados
pluviosidade <- c(970.8, 1290.3, 420.9, 30.7, 550.8, 1060.4, 500.5, 250.9, 630.0, 450.9, 760.6, 530.8, 590.6, 820.3, 200.5)
produtividade <- c(508.4, 686.8, 260.4, 98.0, 331.6, 595.9, 264.9, 139.9, 447.4, 262.3, 429.7, 312.8, 322.6, 395.2, 161.3)
d <- data.frame(pluviosidade,produtividade)

sum(d$pluviosidade)
sum(d$produtividade)

mean(d$pluviosidade)
min(d$pluviosidade)
max(d$pluviosidade)

mean(d$produtividade)
min(d$produtividade)
max(d$produtividade)

#Gráfico ####
p <- ggplot(data = d, aes(x = pluviosidade, y = produtividade)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 1) +
  xlab("Pluviosidade anual (mm)") +
  ylab(bquote("Produtividade primária líquida (gC/"*m^2*")")) +
  xlim(0, 1500) +      # Exemplo: redefinindo os limites do eixo X
  ylim(0, 800) +      # Exemplo: redefinindo os limites do eixo Y
  theme_classic(base_size = 10) +   # Define fonte padrão como 10
  theme(
    axis.title = element_text(size = 12)  # Fonte dos títulos dos eixos
  )

ggsave("output/figures/grafico_regressao.jpg", plot = p, width = 4, height = 4, units = "in", dpi = 300)


#Conferindo o beta ####
#beta = covarianciaXY / varianciaX
beta <- cov(d$produtividade,d$pluviosidade)/var(d$pluviosidade)
beta

#Conferindo o intercepto####
#intercepto = mediaY - b*mediaX
intercepto <- mean(d$produtividade) - mean(d$pluviosidade)*beta
intercepto

y <- 0.484*1000 + 55.511

mod <- lm(produtividade ~ pluviosidade, data = d)
summary(mod)
