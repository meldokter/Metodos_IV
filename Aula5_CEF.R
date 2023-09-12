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

