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

#Exercicio 4 
install.packages("ggplot2")
library(ggplot2)
SaoPaulo <- data.frame(SaoPaulo)
ggplot(data = SaoPaulo, aes(x=populacao)) +
  geom_density(fill = "pink", alpha = 0.5)+
                 labs(title = "Densidade populacional dos municípios de São Paulo", x = "População", y= "densidade")+ scale_x_log10()
#R1: É possível observar uma leve assimetria para a esquerda do gráfico. Assim, quanto menor a população de um município, maior a densidade. 
#R2: A medida mais adequada de tendência central nesse caso parece ser a mediana, pois essa não é afetada por valores atípicos(observação que esteja bem acima ou bem abaixo da maioria dos dados).

#Exercício 5
install.packages("dplyr")
library(dplyr)
SaoPaulo_filtradopop <- SaoPaulo %>%
  filter(populacao < 50000)
library(ggplot2)
ggplot(data = SaoPaulo_filtradopop, aes(x=populacao))+
  geom_density(fill = "pink", alpha = 0.5)+
  labs(title = "Densidade populacional dos municípios de São Paulo com menos de 50.000 habitantes", x = "População", y = "Densidade")
#R1: Há 504 municípios com menos de 50.000 habitantes no Estado de São Paulo.
#R2: Observa-se uma maior assimetria para a esquerda com relação ao gráfico anterior.Isso pode indicar que a maioria dos municípios com população baixa tem alta densidade populacional.

#Exercício 6
View(pop2020)
library(dplyr)
media_pop_estados <- pop2020 %>%
  group_by(uf) %>%
  summarise(Media_populacaoBR = mean(populacao))
View(media_pop_estados)
#R1: Maior população média por municípo: Distrito Federal (DF) 
R1 <- max(media_pop_estados$Media_populacaoBR)
#R2: Menor população média por município: Tocantins (TO)
R22 <- min(media_pop_estados$Media_populacaoBR)

