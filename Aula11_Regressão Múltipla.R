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

#Multicolinearidade 
n <-100
x1 <- rnorm(n)
x2 <- x1*2 + 10 
y <- x1 + rnorm (n)
reg <- lm (y ~ x1 + x2)
summary (reg)

x3 <- rnorm(n)
x4 <- 7*x1 + 5*x3 - 10
reg <- lm(y ~ x1 + x3 + x4)
summary(reg)

#relação não linear 
x5 <- x1^2 
reg <- lm(y ~ x1 + x3 + x5)
summary(reg)

#erro padrão robusto - criação do toy model 
library(ggplot2)
library(tidyverse)
set.seed(123)

x <- c(1:8, 10, 15)
y <- c(5 + rnorm(8,sd = 1.5), 40, 65)
df <- data.frame(y=y, x=x)

df %>%
  ggplot(aes(x=x, y=y)) + geom_point()

fit <- lm(y ~x)
summary(fit)
#R acha que existi aassociação entre as variáveis por conta de outliers 

#erro padrão usual 
n <- length(y)
sigma2 <- sigma(fit)^2
mat_I <- diag(n)
X <- model.matrix(fit)
omega <- sigma2*mat_I
bread <- solve(t(X)%*%X)
meat <- (t(X) %*% omega %*% X)
vce <- bread %*% meat %*% bread
sqrt(diag(vce))
#o erro padrão da o mesmo de quando o R dá summary em fit 

sigma2_hc1 <- residuals(fit)^2*(n/(n-2))
omega <- sigma2_hc1*mat_I
bread <- solve(t(X)%*%X)
meat <- (t(X) %*% omega %*% X)
vce <- bread %*% meat %*% bread
sqrt(diag(vce))
#ao aplicar o erro padrão robusto o erro padrão muda, mas bem pouco 

sigma2_hc3 <- residuals(fit)^2/(1 - hatvalues(fit))^2
omega <- sigma2_hc3*mat_I
bread <- solve(t(X)%*%X)
meat <- (t(X) %*% omega %*% X)
vce <- bread %*% meat %*% bread
sqrt(diag(vce))
#erro aumentou muito com HC3 

#pacotes para calcular o erro padrão 
install.packages("lmtest")
install.packages("sandwich")
library(lmtest)
library(sandwich)

coeftest(fit, vcovHC(fit, "HC3"))

#ver influência dos valores chapéu no modelo (que estão fora das linhas cinza)
plot(fit, which = 5)
#onde está fit vai o nome do modelo 

#exemplo de heterocedasticidade 
x <- rnorm(10, mean=3)
e <- rnorm(10, 0, x^2) # o DP do erro é igual a .5*x^2
a <- 2
b <- -2
y <- a + b*x + e

df <- data.frame(y=y, x=x)

df %>%
  ggplot(aes(y=y, x=x)) + geom_point() + geom_smooth(method="lm")

m_sim <- lm(y ~ x, data=df)
# plots de checagem do modelo
library(easystats)
check_model(m_sim)

#R^2 
library(here)
library(data.table)
library(tidyverse)
library(sjlabelled) # pra remover labelled variables
library(haven)
library(janitor)
library(lubridate)

## dados
# https://www.latinobarometro.org/latContents.jsp

lat_bar23 <- sjlabelled::read_spss(here("Dados", 
                                        "Latinobarometro_2023_Eng_Spss_v1_0.sav"),
                                   drop.labels = TRUE) %>%
  mutate(S17 = as.Date(as.character(S17), "%Y%m%d")) %>%
  clean_names()

# get_label(lat_bar23)

lat_bar23 <- lat_bar23 %>%
  mutate(data_base = as.Date(paste(diareal, mesreal, "2023", sep="-"), 
                             "%d-%m-%Y"),
         idade = year(as.period(interval(s17,data_base))),
         econ_12_meses = ifelse(p6stgbs %in% c(1,2), "better", 
                                ifelse(p6stgbs == 8, NA, "other")),
         econ_12_meses = relevel(as.factor(econ_12_meses), ref = "other"),
         aprovacao_presidente = ifelse(p15stgbs == 0, NA, p15stgbs),
         ideologia = ifelse(p16st %in% c(97, 98, 99), NA, p16st),
         votaria_governo = ifelse(perpart == 4, NA,
                                  ifelse(perpart == 1, 1, 0)),
         genero = factor(sexo, labels = c("homem", "mulher")),
         evangelico = ifelse(s1 %in% c(0,98), NA,
                             ifelse(s1 %in% c(2,3,4,5), 1, 0))) 
# não considera adventista, testemunha Jeová, Mórmon

br_latbar_23 <- lat_bar23 %>%
  mutate(idenpa = remove_all_labels(idenpa)) %>% # haven_labelled problems
  filter(idenpa == 76) %>% ## seelciona brasil
  filter(!is.na(votaria_governo) & !is.na(evangelico) & !is.na(ideologia) & 
           !is.na(econ_12_meses))

glimpse(br_latbar_23)

reg_full <- lm(votaria_governo ~ ideologia + idade + genero + econ_12_meses + evangelico, data=br_latbar_23 )
summary(reg_full)

#cálculo do r^2 
residuos_full <- resid(reg_full)

reg_res_evan <- lm(votaria_governo ~ ideologia + idade + genero + econ_12_meses, data=br_latbar_23 )
summary(reg_res_evan)

residuos_res_evan <- resid(reg_res_evan)

partial_R2_evangelico <- (sum(residuos_res_evan^2) - sum(residuos_full^2))/sum(residuos_res_evan^2)
print(partial_R2_evangelico)

# alternative
partial_R2_evangelico_alt <-  1 - var(resid(reg_full))/var(resid(reg_res_evan))
print(partial_R2_evangelico_alt)

# another alternative
reg_evan <- lm(evangelico ~ ideologia + idade + genero + econ_12_meses , data=br_latbar_23)

partial_R2_evangelico_alt1 <- cor(resid(reg_evan), resid(reg_res_evan))^2
print(partial_R2_evangelico_alt1)

## Cohen partial f2
r_squared_full <- summary(reg_full)$r.squared
r_squared_res <- summary(reg_res_evan)$r.squared

partial_f2 <- (r_squared_full - r_squared_res)/(1-r_squared_full)
print(partial_f2) 