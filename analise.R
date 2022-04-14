library(readxl)
base <- read_excel("RESIDUOS nEVES E bOLSONARO BANCO DE DADOS.xlsx")
base$Bolsonaro2018 <- 100*base$Bolsonaro2018
base$Neves14 <- 100*base$Neves14
summary(base)

fit <- lm(Bolsonaro2018 ~ Neves14, data=base)
summary(fit)

cor.test(base$Bolsonaro2018, base$Neves14)
base$predicted <- predict(fit)   # Save the predicted values
base$residuals <- residuals(fit) # Save the residual values


cor(base$Bolsonaro2018, base$predicted)
cor(base$Bolsonaro2018, base$residuals)

plot(base$Bolsonaro2018, base$predicted)
plot(base$Bolsonaro2018, base$Neves14)

library(ggplot2)
ggplot(base, aes(x =Neves14,  y = Bolsonaro2018)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
  geom_segment(aes(xend = Neves14, yend = predicted), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()


ggplot(base, aes(x = Neves14, y =predicted)) + geom_point(colour = "red", size = 3) + geom_line()

ggplot(base, aes(x = Neves14, y =Bolsonaro2018)) + geom_point(colour = "red", size = 3) + geom_text(label=base$Cidade)


h <- ggplot(base, aes(x = Neves14, y =Bolsonaro2018)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") + geom_text(label=base$Cidade)+       
  geom_segment(aes(xend = Neves14, yend = predicted), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()


h

ggplot(base, aes(x = Bolsonaro2018, y =predicted)) + geom_point(colour = "red", size = 3) + geom_text(label=base$Cidade)


library(tidyverse)
library(knitr)
library(kableExtra)

b5 <- base %>% 
  dplyr::select(Cidade, residuals) %>% 
  arrange(residuals)
b5 %>%
  kbl(caption = "Resíduos por Cidade") %>%
  kable_classic(full_width = F, html_font = "Garamond")


b5 <- base %>% 
  dplyr::select(Cidade, Bolsonaro2018, predicted, residuals) %>% 
  arrange(Cidade)
b5 %>%
  kbl(caption = "Resíduos por Cidade: Bolsonaro 2018 ~ Neves 14") %>%
  kable_classic(full_width = F, html_font = "Garamond")

library(coefplot)
coefplot(fit, intercept=FALSE, interactive=FALSE)
library(sjPlot)
tab_model(fit, show.ci = F, auto.label = T, show.se = T,
          collapse.se = T, wrap.labels = 60, p.style = "stars") 
summary(base)

library(BAS)
#??bas.lm
superior.bic =  bas.lm(Bolsonaro2018 ~ Neves14 ,data=base, n.models=2^15, prior="BIC",modelprior=beta.binomial(1,1),initprobs= "eplogp") 
summary(superior.bic)
#plot(superior.bic)
image(superior.bic, subset=-1)


g <- ggplot(base, aes(Bolsonaro2018))
g + geom_boxplot() + coord_flip()

g <- ggplot(base, aes(Neves14))
g + geom_boxplot() + coord_flip()



g <- ggplot(base, aes( predicted))
g + geom_boxplot() + coord_flip()

summary(base)

h


# 27 Cidades (tirar Rio do Sul)

base <- base[-c(21),] # rm Rio do Sul
fit <- lm(Bolsonaro2018 ~ Neves14, data=base)
summary(fit)
h1 <- ggplot(base, aes(x = Neves14, y =Bolsonaro2018)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") + geom_text(label=base$Cidade)+       
  geom_segment(aes(xend = Neves14, yend = predicted), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()


h1

b6 <- base %>% 
  dplyr::select(Cidade, Bolsonaro2018, predicted, residuals) %>% 
  arrange(Cidade)
b6 %>%
  kbl(caption = "Resíduos por Cidade: Bolsonaro 2018 ~ Neves14 - Sem Rio do Sul(27 cidades)") %>%
  kable_classic(full_width = F, html_font = "Garamond")
