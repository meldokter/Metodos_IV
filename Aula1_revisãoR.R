# - Revisão de R

---
  
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, dpi=96)

```

## R e Rstudio

O R é uma linguagem de programação voltada para análise de dados. O Rstudio é uma IDE (interface de desenvolvimento), que nos ajuda a programar em R. No curso utilizaremos o Rstudio para facilitar programar em R.

Normalmente, iremos escrever um comando aqui no Script, clicar em executar (run) ou apertar ctrl + enter, e o Rstudio vai copiar o comando, colar no console e executá-los para nós.

## R como calculadora

o R pode funcionar como calculadora.


```{r, results='asis', message=FALSE}
2+2
3*4
10/2
```

## Objetos no R

Tudo no R é um objeto. Isso significa que um número é um objeto.

```{r, results='asis', message=FALSE}
pi
```

Isso significa que funções (comandos) também são objetos

```{r, results='asis', message=FALSE}
sum(c(1,2,3))
sum
```

E nós podemos criar nossos próprios objetos, dando os nomes que quisermos (exceto se já existe um objeto no R com aquele nome, como por exemplo o objeto "sum").

```{r, results='asis', message=FALSE}
x <- 3
y <- 7
x+y
```

## Tipos de objetos

O R tem muitos tipos de objetos. Vamos listar aqui apenas os mais básicos.

### Numeric

Objetos do tipo numeric são ... números ("reais").

```{r, results='asis', message=FALSE}
# Exemplos
pi
1/3
4
```

### character

Objetos do tipo character são do tipo texto. Sempre são escritos entre aspas (simples ou duplas, tanto faz)

```{r, results='asis', message=FALSE}
# Exemplos
"Manoel Galdino"
'abc'
"7"
```

## Armazenando dados

Para armazenar dados, usualmente teremos 4 tipos de objetos: 1. vetor, 2. Matriz. 3. data.frame, 4. lista. Não vou falar de lista agora (nem de array, que é uma generalização da matriz para mais de duas dimensões).

### Vetor
Um vetor é uma sequência de objetos.

```{r, results='asis', message=FALSE}
# Exemplos
c(1,2,3)
1:3
c("Manoel", "Hugo", "Lia", "Juliana", "Jéssica")
c(c(1,2,3), c(2,3,1))
```

os elementos de um vetor devem ser todos do mesmo tipo:
  ```{r, results='asis', message=FALSE}
# Exemplos
x <- c("1", 1)
x[2]
# sedundo elemento fica armazenado como character

# não é possível somar texto
# x[2] + x[2] # erro
```

### Matriz

Uma matriz são vetores organizados por coluna, todas as colunas (vetores) só podem ser de um tipo, ou seja, não posso ter uma coluna numeric e outra de character, por exemplo.

```{r, results='asis', message=FALSE}
library(knitr)
library(kableExtra)

# Exemplos
mat <- matrix(1:6, nrow=3, ncol=2)

kable(mat)
```

### Data Frame
O data.frame é uma tabela/planilha, e é onde normalmente armazenamos nossos bancos de dados no R.


```{r, results='asis', message=FALSE}


# Exemplos
df <- data.frame(x=1:3, y=c("a", "b", "c"))
kable(df)
```


#### datas
```{r, results='asis', message=FALSE}
## explicando data rapidamente
data_ex <- "2020-10-23"
data_ex1 <- as.Date(data_ex)

data_ex1 + 0:9
## fim da explicação rápida de data
```


```{r, results='asis', message=FALSE}

## criando data de forma repetitiva e tediosa. O que queremos evitar!
minha_data <- c(as.Date('2009-01-01'), as.Date('2009-01-02'), as.Date('2009-01-03'),
                as.Date('2009-01-04'), as.Date('2009-01-05'), as.Date('2009-01-06'),
                as.Date('2009-01-07'), as.Date('2009-01-08'), as.Date('2009-01-09'),
                as.Date('2009-01-10'))

acoes <- data.frame(
  tempo = minha_data,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)
```

```{r, echo=F, results='asis', message=FALSE}
kable(acoes)
```

```{r, results='asis', message=FALSE}
# criando data.frame de maneira mais inteligente
acoes <- data.frame(
  tempo = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)
```

```{r, echo=F, results='asis', message=FALSE}
kable(acoes)
```


## Bibliotecas/pacotes

O R permite que a gente importe comandos que não vêm por padrão no R. Em gerla esses comandos es~toa agrupados sob um pacote. PAra usar esses comandos, primeiro a gente instala o pacote, e depois carrega a biblioteca.


```{r, results='asis', message=FALSE}
# Exemplos
#install.packages("data.table")
#library(data.table)
```

## importando dados

Para importar dados, vamos usar a bilioteca "data.table"
Então, instalem ela se ainda não instalaram (usando o comando install.packages("data.table"))
E depois carreguem a biblioteca: library(data.table)

Para importar, usaremos o comando fread do pacote data.table.

```{r import_dados, results='asis', message=FALSE, cache=TRUE}

# vamos importar uma base de dados de pib municipais de 2013, do IBGE

# o arquivo está em formato RDS, que é um formato do R, e disponível no meu github. Para importá-lo direto no R, vamos ler o arquivo com a função url e depois import´-lo com a função readRDS. 

pib_cid <- readRDS(url("https://github.com/mgaldino/book-regression/raw/main/dados/pib_cid.RDS"))

# para visualizar os dados que forma importados, temos várias funções
# glimpse, head e View

library(dplyr) # para glimpse

 glimpse(pib_cid)

 head(pib_cid)

View(pib_cid)


```

## Data wrangling

Para manipulação, limpeza e processamento de dados, iremos utilizar o chamado "tidyverse".

```{r, results='asis', message=FALSE}
library(tidyverse)

# digamos que quero o pib total médio e o pib per capita médio
# basta usar o comando summarise, que resume os dados e escolher a função mean.

pib_cid %>%
  summarize(pib_medio=mean(pib_total),
             pib_per_capita_medio=mean(pib_per_capita))

df <- pib_cid %>%
  summarise(pib_medio = mean(pib_total),
            pib_per_capita_medio = mean(pib_per_capita))

```

```{r, results='asis', message=FALSE}
# se eu quiser a soma dos pibs municipais
df <- pib_cid %>%
  summarise(soma_pib = sum(pib_total)) %>%
  head()
```

```{r, results='asis', message=FALSE}
# maior e menos pibs e pibs per capita entre municípios
df <- pib_cid %>%
  summarise(pib_max = max(pib_total),
            pib_min = min(pib_total),
            pib_per_capita_max = max(pib_per_capita),
            pib_per_capita_min = min(pib_per_capita)) %>%
  head()

kable(df)
```

```{r, results='asis', message=FALSE}
# se eu quiser apenas dos municípios od estado de SP?
# basta filtrar pelo estado de SP, com o comando filter
df <- pib_cid %>%
  filter(sigla_uf == "SP") %>%
  summarise(soma_pib = sum(pib_total)) %>%
  head()

kable(df)
```

```{r, results='asis', message=FALSE}
df <- pib_cid %>%
  filter(sigla_uf == "SP") %>%
  summarise(pib_medio = mean(pib_total),
            pib_per_capita_medio = mean(pib_per_capita)) %>%
  head()

kable(df)
```

```{r, results='asis', message=FALSE}
# se eu quiser esse cálculo por uf (or cada umas das ufs?)
# Aí é melhor aguprar por uf
# ideia é: split by, apply (function), combine (summarise?)
df <- pib_cid %>%
  group_by(sigla_uf) %>%
  summarise(pib_medio = mean(pib_total),
            pib_per_capita_medio = mean(pib_per_capita)) %>%
  head()

kable(df)
```

```{r, results='asis', message=FALSE}
# agora, quero criar uma nova variável, que é o pib estadual
df <- pib_cid %>%
  group_by(sigla_uf) %>%
  mutate(pib_uf = sum(pib_total)) %>%
  head()

kable(df)

```

Coisas estranhas. O maior pib per capita municipal deu muito alto. vamos ver qual município é?
  Vamos filtrar e depois selecionar apenas algumas colunas
```{r, results='asis', message=FALSE}
# agora, quero criar uma nova variável, que é o pib estadual
df <- pib_cid %>%
  filter(pib_per_capita > 700000) %>%
  select(sigla_uf, nome_munic, pib_per_capita, pib_total) %>%
  head()

kable(df)

```

Vamos entrar na Wiki do município ou perguntar pra chatGPT o que explica isso aí?
  Veremos que "faz sentido", embora na verdade não faça. 

Exercício em sala de aula: veja os impostos desse município. 

## Visualização

Para visualizarmos os dados com gráficos, utilizaremos a biblioteca ggplot2

```{r start_visualizacao, results='asis', message=FALSE, cache=T}
# gráficos

library(ggplot2)
pib_cid %>%
  ggplot(aes(y=pib_total, x=impostos)) + geom_point()


```

A lgócia geral de um grtáfico com ggplot2 é como no exemplo acima. Primeiro passamos as variáveis por meio do comando ggplot, dentro de aes (de aesthetics), depois combinamos com o tipo de plot que queremos faze,r nesse caso, pontos, com geom_point. É possívle customizar o gráfico para ele ficar mais bonito. Vamos fazer isso agora.

```{r, results='asis', message=FALSE}
# gráficos  mais bonitos


pib_cid %>%
  ggplot(aes(y=pib_total, x=impostos)) + geom_point() +
  scale_y_continuous(labels = scales::dollar) + theme_light() + theme(text=element_text(size=20)) +
  xlab("impostos municipais") + ggtitle("PIB municipal de 2013 x impostos municipais")


```


Podemos usar vários temas feitos pela comunidade. Por exemplo, Barbie:
  ```{r, results='asis', message=FALSE}
# gráficos  mais bonitos

install.packages("remotes")
remotes::install_github("MatthewBJane/theme_park")
library(ThemePark)

pib_cid %>%
  ggplot(aes(y=pib_total, x=impostos)) + geom_point() +
  scale_y_continuous(labels = scales::dollar) + theme(text=element_text(size=20)) + theme_barbie() +
  xlab("impostos municipais") + ggtitle("PIB municipal de 2013 x impostos municipais")


```

Vocês podem ver outros temas de filmes no github do autor do pacote: https://github.com/MatthewBJane/theme_park
E, claro, há muito mais na internet.

Para fazer outro tipo de gráfico, é só variar o geom. Por exemplo, um histograma do PIB per capita.

```{r, results='asis', message=FALSE}
# Histograma


pib_cid %>%
  ggplot(aes(x=pib_per_capita)) + geom_histogram() +
  theme_light() + theme(text=element_text(size=20)) + ggtitle("PIB per capita municipal")


```
#Notas de Aula

pib_cid %>%
  mutate(pib_max = max(pib_total))

pib_cid %>%
  mutate(pib_min = min(pib_total))

# agora, quero criar uma nova variável, que é o pib estadual
pib_cid %>%
  group_by(sigla_uf) %>%
  mutate(pib_uf = sum(pib_total)) %>%
  head()

# agora, quero criar uma nova variável, que é o pib estadual
pib_cid %>%
  group_by(sigla_uf) %>%
  summarise(pib_uf = sum(pib_total)) %>%
  head()