#1.Calculo de medias 

x <- c(1,2,3,4,5,6,7,8,9,10)
(media_x <- sum(x)/length(x))
x <- c(5,5,5,5,5,5,5)
(media_x <-  sum(x)/length(x))
x <- c(1,3,5,7,9,11)
(media_x <-  sum(x)/length(x))
x <- c(-5,-4,-3,-2,-1,1,2,3,4,5)
(media_x <-  sum(x)/length(x))

# ou podemos simplemsnte usar mean(x)
mean(x)

#3.4: Algebra com covariância

#Linearidade da esperança 
x <- 1:10 
y <- 10:1 
#mean(x+y) = mean(x)+mean (y)
mean(x+y) #11
mean (x) #5.5
mean (y) #5.5
5.5 + 5.5 #11
#mean (ax+by) = a*mean(x)+ b*mean(y)
ax <- x*2
by <- y*4
mean(ax+by) #33
2*mean(x) #11
4*mean(y) #22
11+22 #33

#3.5 Distribuição de Probabilidade Conjunta
install.packages("knitr")
install.packages("kableExtra")
library(knitr)
library(dplyr)
library(kableExtra)
#Definir o espaço amostral

espaco_amostral <- expand.grid(1:4, 1:4)
espaco_amostral$X <- espaco_amostral$Var1 + espaco_amostral$Var2
espaco_amostral$Y <- pmax(espaco_amostral$Var1, espaco_amostral$Var2)

# Criar a tabela
kable(espaco_amostral, col.names = c("resultado do primeiro dado", "resultado do segundo dado", "X", "Y"),
      caption = "Tabela representando a soma (X) and o maior valor (Y) do lançamento de dois dados de quatro lados") %>%
  kable_styling(bootstrap_options = "striped")

# Definir os valores de (x, y) e P(X = x, Y = y)
valores <- c("(2, 1)", "(3, 2)", "(4, 2)", "(4, 3)", "(5, 3)", "(5, 4)", "(6, 3)", "(6, 4)", "(7, 4)", "(8, 4)")
probabilidades <- c(0.0625, 0.1250, 0.0625, 0.1250, 0.1250, 0.1250, 0.0625, 0.1250, 0.1250, 0.0625)

# Criar a tabela
tabela <- data.frame("(x, y)" = valores, "P(X = x, Y = y)" = probabilidades)

# Formatar a tabela
kable(tabela, caption = "Tabela representando a distribuição conjunta da soma (X) e o maior (Y) de dois lançamentos de um dado de quatro faces",
      col.names = c("$(X,Y)$", "$P(X=x, Y=y)$")) %>%
  kable_styling(bootstrap_options = "striped")

#3.6 Pobabilidade Condicional
# Definir os valores de (x, y) e P(X = x, Y = y)
valores <- c("(2, 1)", "(3, 2)", "(4, 2)", "(4, 3)", "(5, 3)", "(5, 4)", "(6, 3)", "(6, 4)", "(7, 4)", "(8, 4)")
probabilidades <- c(0, 2/3, 1/3, 0, 0, 0, 0, 0, 0, 0)

# Criar a tabela
tabela <- data.frame("(x, y)" = valores, "P(X = x| Y = 2)" = probabilidades)

# Formatar a tabela
kable(tabela, caption = "Tabela representando a distribuição conjunta da soma (X) e o maior (Y) de dois lançamentos de um dado de quatro faces",
      col.names = c("$(X,Y)$", "$P(X=x| Y=2)$")) %>%
  kable_styling(bootstrap_options = "striped")