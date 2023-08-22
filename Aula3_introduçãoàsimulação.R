#Aula 3: Introdução à simulação

#usar simulação para a)recriar amostras controladas b)verificar resultados matemáticos e c)aproximar valores matemáticos 
sum(1:10)

#Exemplo para calcular 10! 
# especificando semente, para simulação ser reproduzível
set.seed(2) #semente para gerar números "aleatórios"
# número de amostras
stirling_aprox <- function(n) {
  sqrt(2*pi)*n^(n+1/2)*exp(-n)
}
print(stirling_aprox(10)) #[1] 3598696
# razão da aproximação para o valor correto
stirling_aprox(10)/3628800 #99% próximo do valor objeto
# erro percentual
1 - stirling_aprox(10)/3628800 # 0,8%
#ou
factorial(10) #3628800

#para calcular raiz quadrada usar sqrt:
sqrt(4)

#Criando minha função 
strilling_formula <- function(n){sqrt(2*pi*n)*(n/exp(1)^n)}
strilling_formula(10)
strilling_formula(100)

#simular a probabilidade do número 6 sair em um dado de seis lados
# especificando semente, para simulação ser reproduzível
set.seed(234)
# número de amostras
n <- 10000

# 1000 amostras aleatórias de uma lançamento de dado de 6 lados
resultado <- sample(1:6, n, TRUE)
sample(1:6, 1) 
sample(1:6, 10, replace=T) #com reposição de sorteio
x <- sample(1:6, 10, replace=T)
sum(x==6) #quantidade de vezes que saiu o 6 (freq. real é 1/6)
# frequência relativade 6 é dada por número de 6 / total de amostras
prob_6 <- sum(resultado == 6)/n
# 16,89%
# 1/6 = 16.6666

#criando função que calcula a prob de sair um número em um dado de 6 faces 
prob_x_dado6 <- function(x, n=10000) {resultado <- sample(1:6, n, replace=TRUE)
sum(resultado == x)/n}
set.seed(234)
prob_x_dado6(2) #0.1638

#análise de sensibilidade
# especificando semente, para simulação ser reproduzível
set.seed(234)
# número de amostras

vec_amostra <- c(100, 1000, 10000, 100000, 1000000)

# lista vazia para armazenar os resultados das simulações
resultado_lista <- list()

# vetor vazio para armazenar a frequência relativa de 6
vec_prob6 <- numeric()

set.seed(234)
# loop sobre os tamanhos das amostrar
for ( i in 1:length(vec_amostra)) {
  # n amostras de uma lançamento de dado de 6 lados
  resultado_lista[[i]] <- sample(1:6, vec_amostra[i], TRUE)
  
  # frequência relativade 6 é dada por número de 6 / total de amostras
  vec_prob6[i] <- sum(resultado_lista[[i]] == 6)/vec_amostra[i]
  
}

print(vec_prob6)

#explicação laço
sum(1:10)
1+2+3+4+5+6+7+8+9+10
  # os valores da soma são i
  #laço é iterar os is (soma dos is e os valores que eu pedi)- laço é repetir procedimento iterativo
x <- 0
for(i in 1:10) {x <- x+i}
#laço aberto
x <- x+1
x <- x+2
x <- x+3
x <- x+4
x <- x+5
x <- x+6
x <- x+7
x <- x+8 
x <- x+9 
x <- x+10 
