#Aula 4: O modelo de regressão 
library(PNADcIBGE)
 # Importe os dados desejados
 data <- get_pnadc(year=2017,
                      +                   quarter=4,
                      +                   vars=c("Ano", "Trimestre", "UF", "V2007", 
                                                 +                          "VD4020", "VD4035", "V1028"),design=FALSE,
                      +                   savedir=tempdir())
 library(tidyverse)
 library(tidylog)
 data <- data %>%
   select(Ano, Trimestre, UF, V2007, VD4020, VD4035, V1028)
 
 data <- data %>%
   rename(genero = V2007,
          renda = VD4020,
          horas_trabalhadas = VD4035)
 df <- data %>%
   rename(horas_trabalhadas = horaS_trabalhadas) %>%
   filter(!is.na(renda)) %>%
   filter(!is.na(horas_trabalhadas)) %>%
   filter(renda > 0) %>%
   filter(horas_trabalhadas > 0) %>%
   mutate(salario = renda/(4.5*horas_trabalhadas)) %>%
   mutate(log_salario = log(salario)) %>%
   mutate(genero = as.character(genero))

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

#5.7 logaritmos - diferença do log é aproximado da diferença percentual (se a diferença percentual for pequena, até 40%)
100 * (14.7 - 12.5) / 12.57 #17.50199 - 17%
log(14.7) - log(12.5) #0.1621188 - 16%

#5.8 Gênero e raça 
table1 <- df %>%
  mutate(log_renda = log(renda),
         raca1 = ifelse(raca %in% c("Preta", "Parda"), "Negra",as.character(raca))) %>%
  group_by(raca1, genero) %>%
  summarise(salario = round(weighted.mean(salario, w=V1028),2))

kable(table1)

table2 <- df %>%
  mutate(total_n = sum(V1028),
         log_renda = log(renda)) %>%
  group_by(raca, genero) %>%
  summarise(salario = round(weighted.mean(salario, w=V1028),2))

kable(table2)

#5.9 Prevendo a partir da esperança condicional 
df %>%
  ggplot(aes(y=log_salario, x=genero)) + geom_point(shape = 1) +
  scale_y_continuous(labels = scales::dollar) +
  stat_summary(
    geom = "point",
    fun.y = "mean",
    col = "black",
    size = 3,
    shape = 24,
    fill = "red"
  ) + theme_bw(base_size = 22)
df_erro <- df %>%
  group_by(genero) %>%
  mutate(cond_exp = mean(log_salario)) %>%
  ungroup() %>%
  mutate(erro = log_salario - cond_exp)

df_erro %>%
  select(log_salario, genero, cond_exp, erro) %>%
  head() %>%
  kable()

df_erro %>%
  summarise(eq = round(sum(erro),4),
            eqm =  sum(erro^2)) %>%
  kable()

#Exercício de aula 
data <- get_pnadc(year = 2017, 
                  quarter = 4, 
                  vars =c("Ano", "Trimestre", "UF", "V2007", "VD4020", "VD4035", "V2010", "V1028"), design = FALSE, 
                  savedir = tempdir())
data <- data %>% 
 select(Ano, Trimestre, UF, V2007, VD4020, VD4035, V2010, V1028)
data <- data %>% 
  rename(genero = V2007,
         renda = VD4020,
         raca = V2010,
         horas_trabalhadas = VD4035)

view(data)