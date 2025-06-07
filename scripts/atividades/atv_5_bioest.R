#Regressao Multipla
#Gabriela Neres
#07 Junho 2025

#INSTALAR PACOTES
#install.packages("car")
library(car)
#install.packages("rgl")
library(rgl)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("cowplot")
library(cowplot)
#install.packages("scatterplot3d")
library(scatterplot3d)
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)


x1 <- c(25, 22, 21, 14, 15, 11, 8, 18, 23, 16, 23, 26, 24, 27, 9, 30, 23, 20, 22, 13, 29, 24, 26, 27, 12, 14, 23, 19, 19, 19)
x2 <- c(28.2, 133.0, 80.2, 87.6, 95.1, 111.2, 114.4, 112.0, 107.5, 69.6, 58.4, 57.5, 103.9, 57.3, 64.2, 75.9, 76.7, 43.3, 43.0, 149.1, 56.1, 59.3, 77.1, 86.0, 40.6, 76.6, 127.7, 96.7, 115.0, 69.8)
y <- c(177, 687, 429, 469, 513, 576, 572, 584, 592, 388, 362, 336, 544, 317, 348, 423, 420, 251, 246, 760, 334, 326, 459, 463, 219, 429, 694, 490, 589, 368)

d <- data.frame(x1, x2, y)
View(d)

cor(x1,x2)

mod<-lm(y ~ x1 + x2)
summary(mod)

1.430*20 + 4.912*100 + 11.825
zx1 <- scale(x1)
d$x1 <- zx1[,1]
zx2 <- scale(x2)
d$x2 <- zx2[,1]
zy <- scale(y)
d$y <- zy[,1]

modz<-lm(y ~ x1 + x2, data = d)
summary(modz)

#Figuras Regressoes Parciais
#Regressao parcial x1
Py_x1 <- resid(lm(y~x2, data = d))
Px1 <- resid(lm(x1~x2, data = d))
d_x1 <- data.frame(Py_x1, Px1)
ggplot(d_x1, aes(Px1, Py_x1)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm") +
  labs(x = "Altura | Parcial", y = "Carbono | Parcial")

#Regressao parcial x2
Py_x2 <- resid(lm(y~x1, data = d))
Px2 <- resid(lm(x2~x1, data = d))
d_x2 <- data.frame(Py_x2, Px2)
ggplot(d_x2, aes(Px2, Py_x2)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm") +
  labs(x = "Área foliar | Parcial", y = "Carbono | Parcial")

# Regressões simples

reg_x1 <- lm (y ~ x1, data = d)
summary(reg_x1)
reg_x2 <- lm (y ~ x2, data = d)
summary(reg_x2)
