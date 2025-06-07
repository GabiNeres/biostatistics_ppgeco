#Regressao Multipla
#Carlos Roberto Fonseca
#05 Junho 2025

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


#VARIAVEIS
# Y - UMA VARIAVEL Y CONTINUA
# X1,X2,...Xn - VARIOS X CONTINUOS

#HIPOTESES
# H0: Y NAO E INFLUENCIADO PELOS VARIOS Xs
# H1: Y E INFLUENCIADO PELOS X (PELO MENOS UM DELES)

#EXEMPLO
#X1 - Nitrogenio
#X2 - Fosforo
#Y - Crescimento


#COMPREENDENDO O ERRO EM REGRESSOES MULTIPLAS ####

#CENARIO DE DEUS (SEM ERRO ALEATORIO)
set.seed (123)
fos <- rnorm(30,90,20)
set.seed (234)
nit <- rnorm(30,130,30)
cre <- 3*fos + 10*nit #Verdade de Deus: crescimento afetado por N e P
                      #Atencao: Este modelo nao tem erro!!!
d <- data.frame(fos, nit, cre)
View(d)

#VISUALISANDO AS DUAS REGRESSOES
#Visualizando o efeito do Nitrogenio
ggplot(d, aes(nit, cre)) +
        geom_point(size = 5) +
        geom_smooth(method = "lm", se = FALSE) +
        labs(x = "Nitrogenio (kg/ha)", y = "Crescimento (mm)")

#Visualizando o efeito do Fosforo
ggplot(d, aes(fos, cre)) +
        geom_point(size = 5) +
        geom_smooth(method = "lm", se = FALSE) +
        labs(x = "Fosforo (kg/ha)", y = "Crescimento (mm)")

#Fazendo a regressao do nitrogenio
mod_nit<-lm(cre~nit)
summary(mod_nit)

#Fazendo a regressao do fosforo
mod_fos<-lm(cre~fos)
summary(mod_fos)

#Porque fosforo nao e significativo? O que esta acontecendo???
#O que é o erro da regressao do nitrogenio?
#O que é o erro da regressão do fosforo?


#ACRESCENTANDO UM ERRO AO MODELO ####
set.seed(789)
cre <- 3*fos + 10*nit + rnorm(30,0,10)
d <- data.frame(fos, nit, cre)

#Checando o pressuposto da ortogonalidade
plot(fos~nit)
cor(fos,nit)

# REGRESSOES SIMPLES
Fig_nit <-
        ggplot(d, aes(nit, cre)) +
        geom_point(size = 5) +
        geom_smooth(method = "lm", size = 2) +
        labs(x = "Nitrogenio (kg/ha)", y = "Crescimento (mm)", title = "Regressao Simples")

Fig_fos <-
        ggplot(d, aes(fos, cre)) +
        geom_point(size = 5) +
        geom_smooth(method = "lm", size = 2) +
        labs(x = "Fosforo (kg/ha)", y = "Crescimento (mm)", title = "Regressao Simples")


#### REGRESSAO MULTIPLA NO R ####

#MODELO DE REGRESSAO MULTIPLA TIPO III COM DUAS VARIAVEIS EXPLANATORIAS
op<-options(contrasts=c("contr.sum", "contr.poly"))  	# Para mudar os contrastes do calculo dos coeficientes para "contr.sum", o default do R ? o "contr.treatment"
mod_RegMultipla <- lm(cre ~ nit+fos)
summary(mod_RegMultipla)
Anova(mod_RegMultipla, type="III")[-1,] #ANOVA COM MAIUSCULA, tipo 3 mostra a soma dos quadrados exclusiva de cada variável


#MANEIRA FACIL DE FAZER OS PLOTES PARCIAIS
par(mfrow=c(1,1))
car::avPlot(mod_RegMultipla,variable="nit")
car::avPlot(mod_RegMultipla,variable="fos")
dev.off()


#### UMA REGRESSAO MULTIPLA E MELHOR QUE DUAS SIMPLES! ####
#Comparando os betas
#REGRESSAO SIMPLES - NITROGENIO
mod1 <- lm(cre~nit)
summary (mod1)
#REGRESSAO SIMPLES - FOSFORO
mod2 <- lm(cre~fos)
summary(mod2)
#REGRESSAO MULTIPLA
summary(mod_RegMultipla)

#Comparando as somas dos quadrados
anova (mod1)
anova(mod2)
Anova(mod_RegMultipla, type="III")[-1,] #ANOVA COM MAIUSCULA


#### REGRESSOES PARCIAIS A MAO ####

#REGRESSAO PARCIAL - NITROGENIO (CONTROLANDO PELO FOSFORO)
C_F <- resid(lm(cre~fos))
N_F <- resid(lm(nit~fos))
d1 <- data.frame(C_F, N_F)

Fig_P_nit <-
        ggplot(d1, aes(C_F, N_F)) +
        geom_point(size = 5) +
        geom_smooth(method = "lm", size = 2) +
        labs(x = "Nitrogenio (kg/ha)| Parcial ", y = "Crescimento (mm) | Parcial", title = "Regressao Parcial")
        
RegParcial_nit <- lm(C_F~N_F)
summary(RegParcial_nit)
anova  (RegParcial_nit)


#REGRESSAO PARCIAL - FOSFORO (CONTROLANDO PELO NITROGENIO)
C_N <- resid(lm(cre~nit))
F_N <- resid(lm(fos~nit))
d2 <- data.frame(C_N, F_N)

Fig_P_fos <-
        ggplot(d2, aes(C_N, F_N)) +
        geom_point(size = 5) +
        geom_smooth(method = "lm", size = 2) +
        labs(x = "Fosforo (kg/ha)| Parcial ", y = "Crescimento (mm) | Parcial", title = "Regressao Parcial")

RegParcial_fos <- lm(C_N~F_N)
summary(RegParcial_fos)
anova(RegParcial_fos)


#Comparando as figuras brutas e parciais
all <- plot_grid(Fig_nit, Fig_fos, Fig_P_nit, Fig_P_fos,
        nrow = 2, ncol = 2, 
        labels = c("A","B","C","D"))

save_plot("output/figures/Reg_simples&parciais.pdf", all, 
          base_width = 8, base_height = 8)


#GRAFICO 3D DA REGRESSAO MULTIPLA ####
#3D plot, y~x,z

#Redefinindo os eixos
y <- cre/10 #Tansformando cm em metros
x <- nit
z <- fos

# Compute the linear regression (y = ax + bz + d)
fit <- lm(y ~ x + z)

## scatterplot
s3d <- scatterplot3d(x, z, y, pch = 19, type = "p", color = "darkgrey",
                     xlab = "Nitrogênio (kg/ha)",
                     ylab = "Fósforo (kg/ha)",
                     zlab = "Crescimento (cm)",
                     grid = TRUE, box = T,  
                     mar = c(3, 3, 2.5, 2), angle = 45)

# regression plane
s3d$plane3d(fit, draw_polygon = TRUE, draw_lines = TRUE, 
            polygon_args = list(col = rgb(.1, .2, .7, .3)))
# overlay positive residuals
wh <- resid(fit) > 0
s3d$points3d(x[wh], z[wh], y[wh], pch = 19)



########################
#Regressao multipla no R
#Transformando todas as variaveis em z
#Permite ver a importancia relativa das variáveis
#dados
x1 <- rnorm(100, 100, 20)
x2 <- rnorm(100, 80, 10)
x3 <- rnorm(100, 200, 40)
x4 <- rnorm(100, 50, 5)
x5 <- rnorm(100, 120, 30)
y <- 4*x3 + 6*x4 + rnorm(100,0,20)

#Banco de dados com explanatorias
d <- data.frame(x1,x2,x3,x4,x5) 
View(d) 

#Verificando a ortogonalidade
chart.Correlation(d, histogram=TRUE, pch=19)

#Transformando as variáveis
zx1 <- scale(x1)
zx2 <- scale(x2)
zx3 <- scale(x3)
zx4 <- scale(x4)
zx5 <- scale(x5)
zy <- scale(y)

#Banco de dados transformado z
zd <- data.frame(zx1,zx2,zx3,zx4,zx5,zy)

#Modelo de regressao multipla
zmodel <- lm(zy ~ zx1 + zx2 + zx3 + zx4 + zx5, data = zd)
summary(zmodel)
Anova(zmodel, type="III")[-1,] #ANOVA COM MAIUSCULA - pacote car

#Figuras Regressoes Parciais
#Regressao parcial x3
Py_x3 <- resid(lm(y~x1+x2+x4+x5))
Px3 <- resid(lm(x3~x1+x2+x4+x5))
d_x3 <- data.frame(Py_x3, Px3)
ggplot(d_x3, aes(Px3, Py_x3)) +
   geom_point(size = 3) +
   geom_smooth(method = "lm") +
   labs(x = "x3 | Others", y = "y | Others")

#Regressao parcial x4
Py_x4 <- resid(lm(y~x1+x2+x3+x5))
Px4 <- resid(lm(x4~x1+x2+x3+x5))
d_x4 <- data.frame(Py_x4, Px4)
ggplot(d_x4, aes(Px4, Py_x4)) +
        geom_point(size = 3) +
        geom_smooth(method = "lm") +
        labs(x = "x4 | Others", y = "y | Others")

#Analise dos residuos
plot(zmodel)

