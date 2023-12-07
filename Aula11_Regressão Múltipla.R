#Aula 11 - Regressão Múltipla 

library(ggplot2)
library(knitr)
library(tidyverse)
library(here)
library(tidyr)
library(data.table)
library(janitor)

#Modelo sem interação 
presid_18 <- library(readr)
votacao_secao_2018_BR <- read_csv("votacao_secao_2018_BR.csv")
View(votacao_secao_2018_BR)

presid_18 <- votacao_secao_2018_BR

# Supondo que seu dataframe seja chamado df
df_resultados <- presid_18 %>%
  dplyr::filter(!NR_VOTAVEL %in% c(95,96)) %>%
  group_by(NR_ZONA, CD_MUNICIPIO, SG_UF, NR_VOTAVEL, NR_TURNO) %>%
  summarise(total_votos = sum(QT_VOTOS)) %>%
  pivot_wider(names_from = NR_TURNO, values_from = total_votos, 
              values_fill = 0) %>%
  clean_names() %>%
  group_by(nr_zona, cd_municipio, sg_uf) %>%
  mutate(total_validos_1t = sum(x1),
         total_validos_2t = sum(x2)) %>%
  dplyr::filter(nr_votavel %in% c(13,17)) %>%
  group_by(nr_votavel) %>%
  mutate(percentual_1t = x1 /total_validos_1t,
         percentual_2t = x2 / total_validos_2t) %>%
  ungroup() %>%
  dplyr::select(-c(x1, x2, total_validos_1t, total_validos_2t)) %>%
  pivot_wider(names_from = nr_votavel, 
              values_from = c(percentual_1t, percentual_2t))

rm(presid_18)

df_resultados %>%
  ggplot(aes(x=percentual_1t_17, y=percentual_2t_17)) + geom_point() + 
  facet_wrap(~sg_uf) + geom_smooth(method="lm", se=F, linewidth = .5)

reg1 <- lm(percentual_2t_17 ~ percentual_1t_17 + percentual_1t_13 + sg_uf, 
           data = df_resultados)
summary(reg1) # no caso o R usou o Acre como intercepto, 
#               Quando todas as minhas UFs não são o Acre, eu espero que o 
#               Bolsonaro tenha 12% de votos no segundo turno se ele teve zero 
#               no primeiro

#Escolhendo a categoria de referência do modelo: 
mutate (sg_uf = as.factor(sg_uf), sg_uf = relevel(sg_uf, ref= "SP"))