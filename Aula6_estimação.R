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

matriz_var_cov <- matrix(c(sigma_x^2, rho*sigma_x*sigma_y, rho*sigma_x*sigma_y,  sigma_y^2 ), byrow=T, nrow=2)

matriz_var_cov %>%
  kable()
#| 16.0| 1.2|
#|  1.2| 4.0|

set.seed(345)
norm_bivariada <- MASS::mvrnorm(n = 10000, mu = vetor_media, Sigma  = matriz_var_cov)

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
