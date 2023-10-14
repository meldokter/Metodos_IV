#Aula 5: CEF

#6.2 Simulando para entender a CEF - prever Y pelo valor de X
# Y=Xˆ2 + U 
set.seed(234)
n <- 10000
hist(rnorm(n,0,1))

x <- rnorm(n)
u <- rnorm(n)
y <- x^2 + u

df %>% 
  ggplot(aes(x=x, y=y)) + geom_point()

m1 <- mean(y)

m2 <- median(y)
erro1 <- y - m1
erro2 <- y - m2

print(sum(erro1^2)) # 29617.43
print(sum(erro2^2)) #30395.5
#EQM da média é menor que o EQM da mediana 

#Regressão 
df <- data.frame(x=x, y=y)

reg <-  lm ( y~ x , data= df) #regressão
summary(reg) #estatísticas do modelo

#6.3.3 O erro da CEF é não-correlacionado com X
table2 <- df %>%
  mutate(total_n = sum(V1028),
         log_renda = log(renda)) %>%
  group_by(genero) %>%
  summarise(salario = round(weighted.mean(salario, w=V1028),2), freq=sum(V1028)/total_n)

table2 %>%
  head(10) %>%
  kable()

#6.4.1.1 Equação da Reta
library(ggplot2)
library(dplyr)
set.seed(1234)
n <- 1000
x <- rnorm(n)
u <- rnorm(n)
y <- 2 + 1.3*x + u

df <- data.frame(y=y, x=x)
df %>%
  ggplot(aes(x, y)) + geom_point() + geom_smooth(se=F, method="lm") +
  geom_abline(slope= .5, intercept = 1, colour="red") +
  geom_abline(slope= 3, intercept = 3, colour="green", size=1) +
  geom_abline(slope= 0, intercept = 2, colour="grey", size=1)
#No gráfico, vemos que a reta azul é melhor do que a verde e vermelha.

#encontrando os parâmetros da fórmula 
library(knitr)
df %>%
  summarise(cov_yx = cov(y,x),
            var_x = var(x),
            beta = cov_yx/var_x,
            alpha = mean(y) - beta*mean(x)) %>%
  kable(digits=3)
#| cov_yx| var_x|  beta| alpha|
#|------:|-----:|-----:|-----:|
#|  1.349| 0.995| 1.356| 2.016|

#Estimando a equação linear no R: função LM (linear model)
lm(y ~x, df)
#alpha:       beta    
#2.016        1.356  

#df nesse caso é o meu banco de dados 


