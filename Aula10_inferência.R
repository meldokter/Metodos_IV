#Aula 10 - Inferência 

#Exercício de aula - t-student
qt(p=0.025,10) #0.25% dos dados e graus de liberdade na t-student, -0.6998121
qt(p=0.025,2)
qt(p=0.025,30)
qt(p=0.025,100)
qt(p=0.025,1000) #-1.962339 aproxima-se da normal quanto maior a amostra

# a medida que os graus de liberdade crescem, a distribuição t vai convergindo 
# para a normal
 
normal <- c()
student <- c()

i <- 10 

while (i <= 2000) {
  x <- qt(p=0.025, i)
  student <- c(student,x)
  y <- qnorm(p=0.025)
  normal <- c(normal,y)
  i <- i+50
}

data <- data.frame(normal, student)
library(ggplot2)
p <- data %>%
  ggplot(aes(x=n, y=normal)) + geom_line()
p + geom_line()
#completar o código do gráfico 

#exercício de classe: atualização de modelo
library(data.table)
library(here)
presid_al18 <- fread(here("votacao_secao_2018_BR.csv"), 
                     encoding = "Latin-1")

install.packages("janitor")
library(janitor)
library(dplyr)
library(tidyr)
df_resultados <- presid_al18 %>%
  dplyr::filter(!NR_VOTAVEL %in% c(95,96)) %>%
  group_by(NR_ZONA, CD_MUNICIPIO, SG_UF, NR_VOTAVEL, NR_TURNO) %>%
  summarise(total_votos = sum(QT_VOTOS)) %>%
  pivot_wider(names_from = NR_TURNO, values_from = total_votos, 
              values_fill = 0) %>%
  clean_names() %>%
  group_by(nr_zona, cd_municipio, sg_uf) %>%
  mutate(total_validos_1t = sum(x1),
         total_validos_2t = sum(x2)) %>%
  dplyr::filter(nr_votavel == 17) %>%
  mutate(percentual_bolso_1t = x1 /total_validos_1t ,
         percentual_bolso_2t = x2 / total_validos_2t)

# remove
rm(presid_al18)

# modelo de regressão

reg1 <- lm(percentual_bolso_2t ~ percentual_bolso_1t, data = df_resultados)
summary(reg1)

df_resultados %>%
  ggplot(aes(x=percentual_bolso_1t, y=percentual_bolso_2t)) + geom_point() + 
  geom_abline(slope = coef(reg1)[2] , intercept = coef(reg1)[1], 
              colour  = "blue")

# dados de 2022

#read  into data frame
presid_22 <- fread(here("votacao_secao_2022_BR.csv"), 
                   encoding = "Latin-1")

df_resultados_22 <- presid_22 %>%
  dplyr::filter(!NR_VOTAVEL %in% c(95,96)) %>%
  group_by(NR_ZONA, CD_MUNICIPIO, SG_UF, NR_VOTAVEL, NR_TURNO) %>%
  summarise(total_votos = sum(QT_VOTOS)) %>%
  pivot_wider(names_from = NR_TURNO, values_from = total_votos, 
              values_fill = 0) %>%
  clean_names() %>%
  group_by(nr_zona, cd_municipio, sg_uf) %>%
  mutate(total_validos_1t = sum(x1),
         total_validos_2t = sum(x2)) %>%
  dplyr::filter(nr_votavel == 22) %>%
  dplyr::filter(total_validos_1t >0) %>%
  mutate(percentual_bolso_1t = x1 /total_validos_1t ,
         percentual_bolso_2t = x2 / total_validos_2t)

df_previsão <- df_resultados_22 %>%
  mutate(y_previsto = coef(reg1) [1] + coef(reg1) [2]* percentual_bolso_1t)

df_resultados_22 <- df_resultados_22 %>%
  mutate(y_prev_2t = coef(reg1)[1] + coef(reg1)[2]*percentual_bolso_1t,
         validos_previsto = total_validos_1t*y_prev_2t)

# previsão do resultado eleitoral antes de observar apuração do 2t, 
#supondo comparecimento igual ao 1t
df_resultados_22 %>%
  ungroup() %>%
  summarise(total_bolso = sum(validos_previsto),
            total_valido_previsto = sum(total_validos_1t),
            perc_previsto = total_bolso/total_valido_previsto) #0.518, 118229715     

# previsão do resultado eleitoral com o comparecimento do 2o turno real
df_resultados_22 <- df_resultados_22 %>%
  mutate(y_prev_2t_alt = coef(reg1)[1] + coef(reg1)[2]*percentual_bolso_1t,
         validos_previsto_alt = total_validos_2t*y_prev_2t_alt) 

df_resultados_22 %>%
  ungroup() %>%
  summarise(total_bolso = sum(validos_previsto_alt),
            total_valido_previsto = sum(total_validos_2t),
            perc_previsto = total_bolso/total_valido_previsto) #0.519, 118552342

#E incerteza nas previsões?

previsoes <- predict(reg1, newdata = df_resultados_22, 
                     interval = "prediction", level = .95) %>%
  as.data.frame()

df_resultados_22 <- df_resultados_22 %>%
  ungroup() %>%
  mutate(prev_perc = previsoes$fit,
         prev_perc_lower = previsoes$lwr,
         prev_perc_upper = previsoes$upr,
         validos_prev = total_validos_1t*prev_perc,
         validos_prev_lower = total_validos_1t*prev_perc_lower,
         validos_prev_upper = total_validos_1t*prev_perc_upper)

df_resultados_22 %>%
  summarise(perc_previsto = sum(validos_prev)/sum(total_validos_2t),
            perc_previsto_lower = sum(validos_prev_lower)/sum(total_validos_2t),
            perc_previsto_upper = sum(validos_prev_upper)/sum(total_validos_2t))

#perc_previsto perc_previsto_lower perc_previsto_upper
#<dbl>               <dbl>               <dbl>
# 0.516               0.447               0.586
