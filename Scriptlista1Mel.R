#Metodos Quantitativos IV - Lista 1
#Mel Dokter Palomo - 12507543 - graduação - vespertino 

#Exercicio 1 
install.packages("devtools")
devtools::install_github("tbrugz/ribge")
library(ribge)
pop2020 <- populacao_municipios(2020)
View(pop2020)

#R: A unidade de analise desse banco de dados sao os municipios 

#Exercicio 2 
#Selecao das observacoes do Estado de Sao Paulo 
install.packages("dplyr")
library(dplyr)
dados_SaoPaulo <- pop2020 %>%
  filter(uf=="SP")
View(dados_SaoPaulo)
#Limpeza da base de dados
SaoPaulo <- dados_SaoPaulo %>%
  select(-codigo_uf, -populacao_str)
View(SaoPaulo)
SaoPaulo <- SaoPaulo %>%
rename(municipio = nome_munic)  
SaoPaulo$municipio <- tolower(SaoPaulo$municipio)

#R1: No Estado de Sao Paulo há 645 municipios 
#R2: O menor municipio do Estado de Sao Paulo é Borá e tem 838 habitantes
R2 <- min(SaoPaulo$populacao)

#Exercicio 3
#Calculo da media populacao 
mediapop <- mean(SaoPaulo$populacao)
#Media = 71766.40
#Calculo da mediana populacao 
medianapop <- median(SaoPaulo$populacao)
#Mediana = 14141
#Calculo desvio padrao populacao 
dppop <- sd(SaoPaulo$populacao)
#Desvio padrao = 498489.8
#Calculo da variancia 
variancapop <- var(SaoPaulo$populacao)
#Variancia = 248492160624.32