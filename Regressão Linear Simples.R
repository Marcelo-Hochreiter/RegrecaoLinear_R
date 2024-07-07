# Carregar os pacotes

if(!require(pacman)) install.packages("pacman")
library(pacman)
pacman::p_load(dplyr, ggplot2, car, rstatix, lmtest, ggpmisc)


#Leitura do Banco de Dados
dadosDesmata <- read.csv('pecyear.csv', dec ='.')
glimpse(dadosDesmata)


plot(dadosDesmata$bovino, dadosDesmata$area)

mod <- lm(area ~ bovino,dadosDesmata)



par(mfrow=c(2,2))
plot(mod)

par(mfrow=c(1,1))

shapiro.test(mod$residuals)

summary(rstandard(mod))

durbinWatsonTest(mod)

bptest(mod)

summary(mod)

ggplot(data = dadosDesmata, mapping = aes(x = bovino, y = area)) +
  geom_point()+
  geom_smooth(method= "lm", col = "red")+

stat_regline_equation(aes(label = paste(..eq.label.., ..adj.rr.label..,sep = "*plain(\",\")~~")), label.x = 0 , label.y = 400)+
  theme_classic()