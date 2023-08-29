#Aula 4: O modelo de regressão 
#5.5 Distribuição dos salários 
#PNAD contínua me permite fazer inferências acerca da população antes do censo sair
library(ggplot2)

df <- data %>%
  filter(!is.na(renda)) %>%
  filter(!is.na(horas_trabalhadas)) %>%
  filter(renda > 0) %>%
  filter(horas_trabalhadas > 0) %>%
  mutate(salario = renda/(4.5*horas_trabalhadas)) %>%
  mutate(log_salario = log(salario)) %>%
  mutate(genero = as.character(genero))

p1 <- df %>%
  ggplot(aes(salario)) + geom_density(aes(weight=V1028)) + theme_bw(base_size = 22)

print(p1)
#esse gráfico tem dados muito assimétricos = passar o logaritimo nos dados para ele aproximar da normal
p2 <- df %>%
  ggplot(aes(log_salario)) + geom_density(aes(weight=V1028)) + theme_bw(base_size = 22)

print(p2)
#usa-se o log para aproximar de  uma normal por conta do Teorema do Limite Central: log 
#é a soma das multiplicações das variáveis independentes
#E[logsalário] 2.15 salário por hora ou 14 reais por hora 

#5.6 
# E[salário|gênero = mulher]
#E[salário|gênero = homem] 
p3 <- df %>%
  ggplot(aes(x=log_salario)) +
  geom_density(aes(weight = V1028, group=genero, colour=genero)) + theme_bw(base_size = 22)

print(p3)
#filtrar casos para deixar a diferença salarial mais clara
p4 <- df %>%
  dplyr::filter(log_salario > -2.5) %>%
  dplyr::filter(log_salario < 7.5) %>%
  ggplot(aes(x=log_salario)) +
  geom_density(aes(weight = V1028, group=genero, colour=genero)) + theme_bw(base_size = 22)

print(p4)
#colocar horas trabalhadas para pensar a diferença
p5 <- df %>%
  mutate(log_renda = log(renda)) %>%
  ggplot(aes(x=log_renda)) +
  geom_density(aes(weight = V1028, group=genero, colour=genero)) + theme_bw(base_size = 22)

print(p5)
