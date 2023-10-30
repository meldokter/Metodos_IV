#Aula 6: Estimação 

#Simulando de uma normal bivariada 
library(ggplot2)
library(tidyverse)

# vetor de médias
vetor_media <- c(0,0)

sigma_x <- 2
sigma_y <- 2
rho <- .6

#criando a matriz de variância-covariância
matriz_var_cov <- matrix(c(sigma_x^2, rho*sigma_x*sigma_y, rho*sigma_x*sigma_y, 
                           sigma_y^2 ), byrow=T, nrow=2)

matriz_var_cov %>%
  kable()
#| 4.0| 2.4|
#| 2.4| 4.0|

#Simulação da normal
set.seed(345)
norm_bivariada <- MASS::mvrnorm(n = 10000, mu = vetor_media, 
                                Sigma  = matriz_var_cov)

bivariada_tibble1 <- as_tibble(norm_bivariada, .name_repair = "universal") %>%
  rename(x = '...1', 
         y = '...2')
#x
bivariada_tibble1 %>%
  ggplot(aes(x)) + geom_density()
#y
bivariada_tibble1 %>%
  ggplot(aes(y)) + geom_density()
#x e y em duas dimensões - curvas de nível 
bivariada_tibble1 %>%
  ggplot(aes(x, y)) + geom_density_2d()
#scatterplot 
bivariada_tibble1 %>%
  ggplot(aes(x, y)) + geom_point()

#Regressão 
reg1 <- lm( y ~x , data = bivariada_tibble1)

summary(reg1)

#Residuals:
  #Min      1Q  Median      3Q     Max 
#-5.7942 -1.0639 -0.0067  1.0740  6.4187 

#Coefficients:
 #Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -0.014913   0.015907  -0.937    0.349    
#x            0.588264   0.008073  72.870   <2e-16 ***
  ---
  #Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 1.591 on 9998 degrees of freedom
#Multiple R-squared:  0.3469,	Adjusted R-squared:  0.3468 
#F-statistic:  5310 on 1 and 9998 DF,  p-value: < 2.2e-16
  
#Simulação 2 
library(ggplot2)
library(tidyverse)

# vetor de médias
vetor_media <- c(0,0)

# cov menor, dp o mesmo
sigma_x <- 2
sigma_y <- 2
rho <- .3

matriz_var_cov <- matrix(c(sigma_x^2, rho*sigma_x*sigma_y, rho*sigma_x*sigma_y,  
                           sigma_y^2 ), byrow=T, nrow=2)

matriz_var_cov %>%
  kable()
#| 4.0| 1.2|
#| 1.2| 4.0|

set.seed(345)
norm_bivariada <- MASS::mvrnorm(n = 10000, mu = vetor_media, 
                                Sigma  = matriz_var_cov)

bivariada_tibble2 <- as_tibble(norm_bivariada, .name_repair = "universal") %>%
  rename(x = '...1', 
         y = '...2')

bivariada_tibble2 %>%
  ggplot(aes(x, y)) + geom_point()

#Simulação 3 
# cov igual, dp de x maior
rho <- .3
sigma_x <- 4

matriz_var_cov <- matrix(c(sigma_x^2, rho*sigma_x*sigma_y, rho*sigma_x*sigma_y,  
                           sigma_y^2 ), byrow=T, nrow=2)

matriz_var_cov %>%
  kable()
#| 16.0| 2.4|
#|  2.4| 4.0|

set.seed(345)
norm_bivariada <- MASS::mvrnorm(n = 10000, mu = vetor_media, 
                                Sigma  = matriz_var_cov)

bivariada_tibble3 <- as_tibble(norm_bivariada, .name_repair = "universal") %>%
  rename(x = '...1', 
         y = '...2')

bivariada_tibble3 %>%
  ggplot(aes(x, y)) + geom_point()

#Simulação 4
# cov maor, dp de x maior
rho <- .15
sigma_x <- 4
sigma_y <- 2

matriz_var_cov <- matrix(c(sigma_x^2, rho*sigma_x*sigma_y, rho*sigma_x*sigma_y,  
                           sigma_y^2 ), byrow=T, nrow=2)

matriz_var_cov %>%
  kable()
#| 16.0| 1.2|
#|  1.2| 4.0|

set.seed(345)
norm_bivariada <- MASS::mvrnorm(n = 10000, mu = vetor_media, 
                                Sigma  = matriz_var_cov)

bivariada_tibble4 <- as_tibble(norm_bivariada, .name_repair = "universal") %>%
  rename(x = '...1', 
         y = '...2')

bivariada_tibble4 %>%
  ggplot(aes(x, y)) + geom_point()

#Resultados regressão bonitinhos 
reg2 <- lm( y ~x , data = bivariada_tibble2)

reg3 <- lm( y ~x , data = bivariada_tibble3)

reg4 <- lm( y ~x , data = bivariada_tibble4)

install.packages("stargazer")
library("stargazer")

stargazer::stargazer(list(reg1, reg2, reg3, reg4), type =  "html", 
                     style = "ajps",
                     title = "Regressão linear", omit.stat = "f",
                     column.labels=c("cov = 2.4, s_x = 2", "cov = 1.2, s_x = 2", 
                                     "cov = 2.4, s_x = 4", 
                                     "cov = 1.2, s_x = 4" ))

#7.6 Estimando um modelo de regressão real-Censo da Educação

# instalando a biblioteca
install.packages('basedosdados')

# carregando a biblioteca na sessão
library(basedosdados)

# para importar os dadosdiretamente no R, 
#precisamos criar um id para acessar a base de dados
set_billing_id("aula-reg-manoel")

# checando que deu certo
get_billing_id()

# importando dados
query <- "SELECT * FROM `basedosdados.br_inep_censo_escolar.escola` where sigla_uf = 'AL' and id_municipio = '2704302' and ano = 2019" 
escola <- read_sql(query)

query <- "SELECT count(*) as contagem, id_municipio FROM `basedosdados.br_inep_censo_escolar.escola` where sigla_uf = 'AL' and ano = 2019 group by id_municipio" 
escola <- read_sql(query)

query <- "SELECT * FROM `basedosdados.br_inep_censo_escolar.dicionario`"
dicionario <- read_sql(query)

query <- "SELECT * FROM `basedosdados.br_inep_censo_escolar.matricula` where sigla_uf = 'AL' and id_municipio = '2704302' and ano = 2019"

aluno <- read_sql(query)

# dicionário de gênero e raça
# dicionario %>%
#   dplyr::filter(id_tabela == "matricula", nome_coluna %in% c("sexo", "raca_cor"))


aluno_gen <- aluno %>%
  dplyr::filter(regular == 1) %>%
  group_by(id_escola, sexo) %>%
  summarise(num_aluno_gen = n()) %>%
  mutate(sexo = gsub("1", "Male", sexo),
         sexo = gsub("2", "Female", sexo)) %>%
  mutate(total = sum(num_aluno_gen),
         percent = num_aluno_gen/total) %>%
  filter(sexo == "Female") %>%
  rename(percent_female = percent) %>%
  dplyr::select(id_escola, percent_female)

aluno_raca <- aluno %>%
  dplyr::filter(regular == 1) %>%
  group_by(id_escola, raca_cor) %>%
  summarise(num_aluno_raca = n()) %>%
  mutate(raca_cor = gsub("1", "Branca", raca_cor),
         raca_cor = gsub("2", "Preta", raca_cor),
         raca_cor = gsub("3", "Parda", raca_cor)) %>%
  mutate(total = sum(num_aluno_raca),
         percent = num_aluno_raca/total) %>%
  filter(raca_cor %in% c("Branca", "0", "Preta", "Parda")) %>%
  mutate(raca_cor = gsub("0","não_declarado", raca_cor)) %>%
  select(id_escola, raca_cor, percent) %>%
  pivot_wider(names_from = raca_cor , values_from = percent )



aluno_escola <- aluno_gen %>%
  inner_join(aluno_raca,  by = "id_escola") %>%
  inner_join(escola, by = "id_escola")

aluno_escola_reg <- aluno_escola %>%
  ungroup() %>%
  mutate(negra = Preta + Parda) %>%
  select(negra, percent_female, tipo_localizacao, agua_potavel, esgoto_rede_publica, 
         lixo_servico_coleta, area_verde, biblioteca, quantidade_profissional_psicologo) %>%
  filter(across(everything(), ~!is.na(.))) %>%
  mutate_if(bit64::is.integer64, as.factor)

reg <- lm(negra ~  percent_female + tipo_localizacao + agua_potavel  + esgoto_rede_publica + lixo_servico_coleta + area_verde + biblioteca + quantidade_profissional_psicologo, data= aluno_escola_reg)

summary(reg)

#limitações r-quadrado
x1 <- rnorm(1000, 0, 10)
x2 <- rnorm(1000, 0, 2)

y1 <- 2 + 3*x1 + rnorm(1000, 0, 2)
y2 <- 2 + 3*x2 + rnorm(1000, 0, 2)

summary(lm(y1 ~x1))

summary(lm(y2~x2))