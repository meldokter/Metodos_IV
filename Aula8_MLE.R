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

#aula 7/11 - regressão
install.packages("data.table")
library(data.table)

#carregando os dados
library(readr)
BES <- read_csv("BES.csv")
View(BES)
bes <- BES

#regressão
reg <- lm(leave ~ age, data=bes, na.action = na.omit )

stargazer::stargazer(reg, type =  "html", style = "ajps",
                     title = "Regressão linear - Brexit", omit.stat = "f")
summary(reg)

#teste de hipótese 
sim_nula <- rnorm(10000, mean = 0, sd= 0.0001739)
options(scipen = 99)
hist(sim_nula)
#p-valor = <2e-16 =0 = rejeita-se HO de que B=0 

#intervalo de confiança 
0.0072082 + 2*0.0001739
0.0072082 - 2*0.0001739
#[0.0068604 - 0.007556]
0.007556 - 0.0068604 # 0.0006956

#IC para 40 anos
0.1194782 + (0.0072082 + 0.0001739)*40
0.1194782 + (0.0072082 - 0.0001739)*40
#40% de confiança 