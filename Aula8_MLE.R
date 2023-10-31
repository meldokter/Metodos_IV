#Aula 8 - MLE 

#9.3 regressão

#modelo
reg <- lm(leave ~ age, data=bes, na.action = na.omit )

stargazer::stargazer(reg, type =  "html", style = "ajps",
                     title = "Regressão linear - Brexit", omit.stat = "f")

#previsões
20*.007*100 # 20 anos
40*.007*100 # 40 anos
80*.007*100 # 40 anos

#p-valor
z <- coef(reg)[2]/summary(reg)$coefficients[2 , 2]
print(z)

p_valor <- 1 - pnorm(z)

print(p_valor)

hist(rnorm(10000))

#Intervalo de confiança
40*.007*100 # 40 anos estimativa pontual
40*.003*100 # 40 anos limite inferior
40*.01*100 # 40 anos limite superior