#Mel Dokter Palomo - vespertino - graduação - 12507543
#Lista 2 

#Exercício 1: 
# Instalar o pacote
install.packages("PNADcIBGE")
# Carregar o pacote
library(PNADcIBGE)
# Importar os dados desejados
data <- get_pnadc(year=2017,
                  quarter=4,
                  selected=FALSE,
                  vars=c("Ano", "Trimestre", "UF", "V2007", "VD4020", "VD4035"),
                  design=FALSE,savedir=tempdir())
# Selecionar apenas as variáveis úteis para esta lista:
library(tidyverse)
install.packages("tidylog")
library(tidylog)
data <- data %>%
  select(Ano, Trimestre, UF, V2007, VD4020, VD4035)
# Renomear as variáveis:
data <- data %>%
  rename(Sexo = V2007,
         Renda = VD4020,
         Horas_trabalhadas = VD4035)
glimpse(data)

#Exercício 2: 
#Calcular renda média: 
renda_media <- mean(data$Renda, na.rm = TRUE)
print(renda_media)
#renda média = 1931.283
#Calcular variância da renda: 
var_renda <- var(data$Renda, na.rm = TRUE)
print(var_renda)
#variância da renda = 9543677
# Calcular a  renda média dos homens e das mulheres:
homens <- data[data$Sexo == "Homem", ]
media_homens <- mean(homens$Renda, na.rm = TRUE)
mulheres <- data[data$Sexo == "Mulher", ]
media_mulheres <- mean(mulheres$Renda, na.rm = TRUE)
print("Média para Homens:")
print(media_homens)
print("Média para Mulheres:")
print(media_mulheres)
#renda média homens = 2077.956
#renda média mulheres = 1720.783
#Calcular renda média em cada Estado brasileiro 
library(dplyr)
rendamedia_estados <- data %>% 
  group_by(UF) %>%
  summarise(renda_media_BR = mean(Renda, na.rm = TRUE)) 
View(rendamedia_estados)
#Calcular a covariância entre a renda e o número de horas trabalhadas: 
dataclean <- na.omit(data)
covar <- cov(dataclean$Renda, dataclean$Horas_trabalhadas)
print(covar)
#5776.864

#Respostas
install.packages("knitr")
install.packages("kableExtra")
library(knitr)
library(dplyr)
library(kableExtra)
dados_tabela <- data.frame(
  "Estatística"= c("Renda média", "Variância da renda", "Renda média homens",+ 
                     "Renda média mulheres", "Covariância entre renda e horas trabalhadas"),
  "Valor"= c(renda_media, var_renda, media_homens, media_mulheres, covar))
tabela_formatada <- kable(dados_tabela, 
                          col.names = c("Estatística", "Valor"),
                          format = "html", 
                          caption = "Exercício 2")
tabela_formatada %>%
  kable_styling(bootstrap_options = "striped")
kable(col.names = c("Renda média", "Variância da Renda", "Renda média homens",+ 
                      "Renda média mulheres", "Covariância entre renda e horas trabalhadas"))
kable_stylling(bootstrap_options = "striped")

library(knitr)
library(dplyr)
library(kableExtra)
tabela_formatada1 <- kable(rendamedia_estados, 
                          col.names = c("UF", "Renda_media_BR"),
                          format = "html", 
                          caption = "Renda média por Estado")
tabela_formatada1 %>%
  kable_styling(bootstrap_options = "striped")

#Exercício 3
#E[aX + bY ] = a × E[X] + b × E[Y ]
#X=renda 
#Y= Horas trabalhadas
#a=2
#b=3

mean(2*dataclean$Renda+3*dataclean$Horas_trabalhadas) #3975.141

2*mean(dataclean$Renda)+3*mean(dataclean$Horas_trabalhadas) #3975.141

#Exercício 4 
dataclean$UF <- as.character(dataclean$UF)
library(dplyr)
dataclean <- dataclean %>% 
  mutate(Estado = case_when(
    UF == "Acre" ~ "AC",
    UF == "Alagoas" ~ "AL",
    UF == "Amapá" ~ "AP",
    UF == "Amazonas" ~ "AM",
    UF == "Bahia" ~ "BA",
    UF == "Ceará" ~ "CE",
    UF == "Distrito Federal" ~ "DF",
    UF == "Espírito Santo" ~ "ES",
    UF == "Goiás" ~ "GO",
    UF == "Maranhão" ~ "MA",
    UF == "Mato Grosso" ~ "MT",
    UF == "Mato Grosso do Sul" ~ "MS",
    UF == "Minas Gerais" ~ "MG",
    UF == "Pará" ~ "PA",
    UF == "Paraíba" ~ "PB",
    UF == "Paraná" ~ "PR",
    UF == "Pernambuco" ~ "PE",
    UF == "Piauí" ~ "PI",
    UF == "Rio de Janeiro" ~ "RJ",
    UF == "Rio Grande do Norte" ~ "RN",
    UF == "Rio Grande do Sul" ~ "RS",
    UF == "Rondônia" ~ "RO",
    UF == "Roraima" ~ "RR",
    UF == "Santa Catarina" ~ "SC",
    UF == "São Paulo" ~ "SP",
    UF == "Sergipe" ~ "SE",
    UF == "Tocantins" ~ "TO",
    TRUE ~ UF))

library(ggplot2)
grafico <- ggplot(dataclean, aes(x= Estado, y=Renda, fill=Sexo))+
  geom_bar(stat = "summary", fun = "mean", position = "stack")+
  labs(title = "Renda média por Estado e Sexo", 
       x = "Estado",
       y = "Média da renda")+
  scale_fill_manual(values = c("light blue", "pink"))+
  theme_minimal()
print(grafico)

#Exercício 5
#X=Renda 
#Y= Horas trabalhadas 

#i) E[X|10 ≤ Y ≤ 20] (média de X quando Y está no intervalo de 10 a 20)
Y <- dataclean$Horas_trabalhadas
X <- dataclean$Renda
ex5i <- dataclean %>%
  filter(Y >= 10 & Y <= 20)
Econdi <- mean(ex5i$Renda)
Econdi #939.7574

#ii) E[X|Y ≥ 20]
ex5ii <- dataclean %>% 
  filter(Y >= 20)
Econdii <- mean(ex5ii$Renda)
Econdii #2015.483

#Exercício 6 
dataclean <- dataclean %>% 
  filter(Renda <= 10000)
#i 
library(ggplot2)
grafico1 <- ggplot(dataclean, aes(x = Renda))+
  geom_density(fill = "pink", alpha = 0.5)+ 
  labs(title = "Densidade da variável renda", x= "Renda", y = "Densidade")
print(grafico1)
# Percebe-se que, quanto menor a renda, maior a densidade, isto é, nota-se uma maior, 
#concentração de indivíduos nas faixas mais baixas de renda. 

#ii
dataclean <- dataclean %>% 
  filter(Renda <= 10000)
num_obs_1000_2000 <- sum(dataclean$Renda > 1000 & dataclean$Renda < 2000)
total_obs <- nrow(dataclean)
prob <- num_obs_1000_2000 / total_obs
prob #0.2714579

#iii
dataclean <- na.omit(data)
datacleangrafico2 <- dataclean %>% 
  filter(Renda <= 10000, Horas_trabalhadas <= 20)
datacleangrafico2$Horas_trabalhadas <- 
  as.factor(datacleangrafico2$Horas_trabalhadas)

library(ggplot2)
grafico2 <- ggplot(datacleangrafico2, aes(x = Renda))+ 
  geom_density(fill = "pink", alpha = 0.5)+ 
  labs (title = "Densidade da renda por horas trabalhadas <= 20", 
          x = "Renda por horas trabalhadas", 
          y = "Densidade")
print(grafico2)

#iv 
ex6iv <- dataclean %>%
  filter(Horas_trabalhadas <= 20, Renda <= 10000)
prob_cond <- sum(ex6iv$Renda > 1000 & ex6iv$Renda < 2000) / nrow(ex6iv)
prob_cond #0.1244861
