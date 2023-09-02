#Lista 3 
#Mel Dokter Palomo - 12507543 - graduação - vespertino 

#Exercício 1 
#Rodar ?rnorm para acessar o help
?rnorm 
# Conforme o help e o EcoR da USP a função rnorm faz a simulação de amostras aleatórias 
#de uma distribuição normal e tem como argumentos n (tamanho da amostra), mean (média) 
# e sd (desvio padrão), que são os parâmetros de uma função normal. 
#Simulando uma amostra 100 com a função: 
teste <- rnorm(100, mean=0, sd=1)
print(teste)
mediateste <- mean(teste)
print(mediateste)

#Exercício 2
x <- rnorm(100, mean=2, sd=1)
print (x)
mediax <- mean(x)
#A média atribuída à distribuição (2) é diferente da média que obtem-se com "mean(x)", 
#(1.95072), pois os números escolhidos pela função rnorm são aleatórios, logo, a média
# dos valores será uma aproximação da média atribuída à distribuição normal 2. 

#Exercício 3 
x2 <- rnorm(100, mean=2, sd=1)
print (x2)
mediax2 <- mean(x2)
# Obtera-se um valor um pouco diferente da outra simulação por conta da aleatoriedade 
#na seleção dos valores que compõe a amostra, que gerará uma amostra diferente. 

#Exercío 4 
vetor_medias <- numeric()
vetor_medias[1] <- mean(rnorm(100, mean=2, sd=1))
vetor_medias[2] <- mean(rnorm(100, mean=2, sd=1))
print(vetor_medias)

#Exercício 5 
vetor_medias <- numeric(30)
for(i in 1:30){ vetor_medias[i] <- mean(rnorm(100, mean=2, sd=1))
}
print(vetor_medias)

#Exercício 6
df <- data.frame(medias = vetor_medias, sim_id = 1:30)
library(ggplot2)
grafico_ex6 <- ggplot(data = df, aes(x= medias)) +
  geom_histogram(binwidth = 0.2, fill = "pink", color = "black") +
  labs(title = "Histograma das Médias X ~ N(μ = 2, σ = 1)", x = "Média", y = "Frequência")
print(grafico_ex6)
#A distribuição é uma distribuição normal.  
# média e o desvio padrão: observando-se o gráfico, estima-se que, a priori, a média 
#seria 2 e o desvio padrão 0.2 (2-1.8).

#Exercício 7
possibilidades <- c("cara", "coroa")
n <- 1000
set.seed(123)
simulacao <- sample(possibilidades, n, replace = T)
resultados <- table(simulacao)
print(simulacao)
prob_cara <- sum(simulacao == "cara")/n
print(prob_cara)
prob_coroa <- sum(simulacao == "coroa")/n
print(prob_coroa)
resultados1 <- table(prob_cara, prob_coroa)
print(resultados1)

#Exercício 8 
set.seed(123)
n10 <- rnorm(10, mean = 0, sd=1) 
n100 <- rnorm(100, mean = 0, sd=1)
n1000 <- rnorm(1000, mean = 0, sd=1)
n10000 <- rnorm(10000, mean = 0, sd=1)

set.seed(123)
b10 <-  rbinom(10, size=20, prob = 0.7)
b100 <- rbinom(100, size=20, prob = 0.7)
b1000 <- rbinom(1000, size=20, prob = 0.7)
b10000 <- rbinom(10000, size=20, prob = 0.7)

dfn10 <- data.frame(valores=n10, sim_id = 1:10)
dfn100 <- data.frame(valores=n100, sim_id = 1:100)
dfn1000 <- data.frame(valores=n1000, sim_id = 1:1000)
dfn10000 <- data.frame(valores=n10000, sim_id = 1:10000)

dfb10 <- data.frame(valores=b10, sim_id = 1:10)
dfb100 <- data.frame(valores=b100, sim_id = 1:100)
dfb1000 <- data.frame(valores=b1000, sim_id = 1:1000)
dfb10000 <- data.frame(valores=b10000, sim_id = 1:10000)

library(ggplot2)
grafico_dfn10 <- ggplot(data = dfn10, aes(x= valores)) +
  geom_histogram(binwidth = 0.2, fill = "green", color = "black") +
  labs(title = "Histograma das Médias X ~ 10(μ = 0, σ = 1)", x = "Média", y = "Frequência")
print(grafico_dfn10)

grafico_dfn100 <- ggplot(data = dfn100, aes(x= valores)) +
  geom_histogram(binwidth = 0.2, fill = "green", color = "black") +
  labs(title = "Histograma das Médias X ~ 100(μ = 0, σ = 1)", x = "Média", y = "Frequência")
print(grafico_dfn100)

grafico_dfn1000 <- ggplot(data = dfn1000, aes(x= valores)) +
  geom_histogram(binwidth = 0.2, fill = "green", color = "black") +
  labs(title = "Histograma das Médias X ~ 1000(μ = 0, σ = 1)", x = "Média", y = "Frequência")
print(grafico_dfn1000)

grafico_dfn10000 <- ggplot(data = dfn10000, aes(x= valores)) +
  geom_histogram(binwidth = 0.2, fill = "green", color = "black") +
  labs(title = "Histograma das Médias X ~ 10000(μ = 0, σ = 1)", x = "Média", y = "Frequência")
print(grafico_dfn10000)


grafico_dfb10 <- ggplot(data=dfb10, aes(x= valores))+ 
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black")+ 
  labs(title = "Histograma das médias X~10(20, 0.7)", x= "Média", y="Frequência")
print(grafico_dfb10)

grafico_dfb100 <- ggplot(data=dfb100, aes(x= valores))+ 
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black")+ 
  labs(title = "Histograma das médias X~100(20, 0.7)", x= "Média", y="Frequência")
print(grafico_dfb100)

grafico_dfb1000 <- ggplot(data=dfb1000, aes(x= valores))+ 
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black")+ 
  labs(title = "Histograma das médias X~1000(20, 0.7)", x= "Média", y="Frequência")
print(grafico_dfb1000)

grafico_dfb10000 <- ggplot(data=dfb10000, aes(x= valores))+ 
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black")+ 
  labs(title = "Histograma das médias X~10000(20, 0.7)", x= "Média", y="Frequência")
print(grafico_dfb10000)