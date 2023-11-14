#Aula 9 - Checagem 

install.packages("data.table")
library(data.table)
install.packages("here")
library(here)

# lista o nome do arquivo em csv
# unzip(here("dados", "votacao_secao_2018_BR.zip"), list = TRUE)

#read data1.csv into data frame
presid_al18 <- fread(here("votacao_secao_2018_BR.csv"), 
                     encoding = "Latin-1")

# filtrando só AL
library(tidyverse)
presid_al18 <- presid_al18 %>%
  filter(SG_UF == "AL")

# modelo voto em Bolsonaro 1t prediz voto no 2t

# descobre o que é voto nulo e branco
presid_al18 %>%
  group_by(NM_VOTAVEL) %>%
  summarise(max(NR_VOTAVEL))

# 95 e 96
presid_al18_valido <- presid_al18 %>%
  filter(!NR_VOTAVEL %in% c(95,96)) %>%
  group_by(NR_SECAO,NR_ZONA, CD_MUNICIPIO, NR_TURNO, NR_VOTAVEL ) %>%
  summarise(validos = sum(QT_VOTOS)) %>%
  mutate(bol_bolsonaro = NR_VOTAVEL == 17,
         validos_bolsonaro = sum(validos*bol_bolsonaro)) %>%
  summarise(total_validos = sum(validos),
            validos_bolsonaro = max(validos_bolsonaro),
            perc_bolsonaro = validos_bolsonaro/total_validos) %>%
  dplyr::select(-total_validos) %>%
  pivot_wider(id_cols = c(NR_SECAO, NR_ZONA, CD_MUNICIPIO), 
              names_from = NR_TURNO, values_from = perc_bolsonaro) %>%
  rename(perc_bolso_turno1 = '1',
         perc_bolso_turno2 = '2')
summarise(presid_al18_valido)

# modelo de regressão
reg1 <- lm(perc_bolso_turno2 ~ perc_bolso_turno1, data = presid_al18_valido)
summary(reg1)

#plot coeficientes 
presid_al18_valido %>%
  ggplot(aes(x=perc_bolso_turno1, y=perc_bolso_turno2)) + geom_point() + 
  geom_abline(slope = coef(reg1)[2] , intercept = coef(reg1)[1], 
              colour  = "blue")

#resíduos contra o preditor
df <- data.frame(residuos = residuals(reg1), 
                 preditor = presid_al18_valido$perc_bolso_turno1)
df %>%
  ggplot(aes(x=preditor, y = residuos)) + geom_point() + 
  geom_smooth(method="lm", se=F)

#magnitude dos resíduos
df <- data.frame(residuos_sq = residuals(reg1)^2, 
                 preditor = presid_al18_valido$perc_bolso_turno1)
df %>%
  ggplot(aes(x=preditor, 
             y = residuos_sq)) + geom_point() + geom_smooth(method="lm", se=F)

#valor absoluto dos resíduos (não ao quadrado)
df <- data.frame(residuos_abs = abs(residuals(reg1)), 
                 preditor = presid_al18_valido$perc_bolso_turno1)
df %>%
  ggplot(aes(x=preditor,
             y = residuos_abs)) + geom_point() + geom_smooth(method="lm", se=F)

#resíduos com correlção espacial
presid_al18_valido <- presid_al18_valido %>%
  mutate(id_secao = paste0(NR_SECAO, NR_ZONA , CD_MUNICIPIO))

df <- data.frame(residuos = residuals(reg1), 
                 id = as.numeric(presid_al18_valido$id_secao))
df %>%
  ggplot(aes(x=id, y = residuos)) + geom_point() + 
  geom_smooth(method="lm", se=F)

#permutation test 
install.packages("nullabor")
library(nullabor)
set.seed(1234)  # Aleatoriza do mesmo jeito sempre
elec_reg <- data.frame(presid_al18_valido, 
                       .resid = residuals(reg1), .fitted = fitted(reg1))
shuffled_residuals <- lineup(null_lm(perc_bolso_turno2 ~ perc_bolso_turno1,
                                     method = "rotate"), true = elec_reg,
                             n = 9)
## decrypt("ve5B DEyE l6 GuClylu6 dT") #True data in position  2"
ggplot(shuffled_residuals, aes(x = .fitted, y = .resid)) +
  geom_point() +
  facet_wrap(vars(.sample))

#normalidade dos resíduos 
df <- data.frame(residuos = residuals(reg1), 
                 preditor = presid_al18_valido$perc_bolso_turno1, 
                 density_points = rnorm(length(residuals(reg1)) , 
                                        0, sd(residuals(reg1))))
print(sd(residuals(reg1))) #0.02884366
df %>%
  ggplot(aes(residuos)) + geom_histogram(aes(y=..density..)) + 
  geom_density(aes(density_points), colour = "pink")

#Q Q plot 
x <- rnorm( 1000)
q50 <- quantile(x, .5)
q025 <- quantile(x, .025)
q975 <-quantile(x, .975)
print(c(q50, q025, q975))

#ver se duas variáveis tem a mesma distribuição 
y <- rnorm(1000)
df <- data.frame(y=y, x=x)
# plot de x contra y
df %>%
  ggplot(aes(x=x, y=y)) + geom_point()
# plot de x ordenado contra y ordenado
df %>%
  ggplot(aes(x=sort(x), y=sort(y))) + 
  geom_point() +  geom_abline(intercept = 0, slope = 1, colour ="blue")

df <- data.frame(residuos = residuals(reg1), 
                 preditor = presid_al18_valido$perc_bolso_turno1, 
                 density_points = rnorm(length(residuals(reg1)) , 
                                        0, sd(residuals(reg1))),
                 fi_percentil = 1:length(residuals(reg1))/
                   1:length(residuals(reg1)))
print(sd(residuals(reg1)))
# plot de x ordenado contra y ordenado
df %>%
  ggplot(aes(y=sort(residuos), x=sort(density_points))) + geom_point() +
  geom_abline(intercept = 0, slope = 1, colour ="blue")

qqnorm(residuals(reg1))
qqline(residuals(reg1))

#exercício
presid_al18 <- fread(here("votacao_secao_2018_BR.csv"), 
                     encoding = "Latin-1")
library(tidyverse)
presid_al18 <- presid_al18 %>%
  filter(SG_UF == "PE")

presid_al18 %>%
  group_by(NM_VOTAVEL) %>%
  summarise(max(NR_VOTAVEL))

presid_al18_valido <- presid_al18 %>%
  filter(!NR_VOTAVEL %in% c(95,96)) %>%
  group_by(NR_SECAO,NR_ZONA, CD_MUNICIPIO, NR_TURNO, NR_VOTAVEL ) %>%
  summarise(validos = sum(QT_VOTOS)) %>%
  mutate(haddad = NR_VOTAVEL == 13,
         validos_haddad = sum(validos*haddad)) %>%
  summarise(total_validos = sum(validos),
            validos_haddad = max(validos_haddad),
            perc_haddad = validos_haddad/total_validos) %>%
  dplyr::select(-total_validos) %>%
  pivot_wider(id_cols = c(NR_SECAO, NR_ZONA, CD_MUNICIPIO), 
              names_from = NR_TURNO, values_from = perc_haddad) %>%
  rename(perc_haddad_turno1 = '1',
         perc_haddad_turno2 = '2')

glimpse(presid_al18_valido)
head(presid_al18_valido)

reg2 <- lm(perc_haddad_turno2 ~ perc_haddad_turno1, data = presid_al18_valido)
summary(reg2) 