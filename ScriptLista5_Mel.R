#Lista 5 - Mel Dokter Palomo - vespertino - 12507543

#1. Importação de dados PNAD 2017
# Instalar e carregar o pacote
install.packages("PNADcIBGE")
library(PNADcIBGE)
# Importar os dados desejados
data <- get_pnadc(year=2017,
                  quarter=4,
                  selected=FALSE,
                  vars=c("Ano", "Trimestre", "UF", "V2007", "VD4020", "VD4035"),
                  design=FALSE,
                  savedir=tempdir())
# Seleção das variáveis para a lista: 
install.packages("tidyverse")
install.packages("tidylog")
library(tidyverse)
library(tidylog)
data <- data %>%
  select(Ano, Trimestre, UF, V2007, VD4020, VD4035)
# Renomear as variáveis:
data <- data %>%
  rename(Sexo = V2007,
         Renda = VD4020,
         Horas_trabalhadas = VD4035)

#2.  
#Regressão 
reg <- lm(Renda ~ Sexo, data=data)
summary(reg)

#Equação da regressão 
# Y = a + B*x + e 
# Renda = 2077.956 + -357.173*SexoMulher + e 

#Interpretação dos coeficientes: 
#O intercepto da regressão é 2077.956, o que significa que quando SexoMulher=0,
#ou seja, quando o sexo é masculino, a renda média é de R$2077,956. Já o B1 é 
#-357.173, o que significa que quando SexoMulher=1, ou seja, quando o sexo é
#feminino, espera-se uma diminuição de R$-357, 173 na renda em comparação ao 
#grupo masculino. Ambos os coeficientes tem um alto nível de significância esta
#tística, sendo o p-valor < 0.001. 

#Resultados: 
install.packages("stargazer")
library(stargazer)
tabelareg <- stargazer(reg, type = "html", style = "ajps",
                       title = "Regressão Linear ex.2",
          column.labels = c("Coeficientes"), omit.stat = "f")

#3. 
intercepto <- 2077.956
SexoMulher0 <- -357.173

# Renda média mulheres (SexoMulher=1)
rm_mulheres <- intercepto + SexoMulher0 * 1 

# Renda média homens (SexoMulher=0)
rm_homens <- intercepto + SexoMulher0 * 0

rm_mulheres #1720.783
rm_homens # 2077.956

#Confirmaçao dos resultados: 
library(dplyr)

rm_conf <- data %>%
  filter(!is.na(Renda)) %>%
  group_by(Sexo) %>%
  summarise(Renda_Media = mean(Renda))

rm_conf
# Sexo   Renda_Media
#<fct>        <dbl>
  #1 Homem        2078.
#2 Mulher       1721.

#4. 
#Regressão 
reg2 <- lm(Renda ~ Horas_trabalhadas, data=data)
summary(reg2)

#Equação da regressão 
# Y = a + B*x + e 
# Renda = 846.7765 + 28.9009*Horas_trabalhadas + e 

#Interpretação dos coeficientes: 
# O intercepto da regressão é 846.7765, o que significa que, quando o número de
#horas trabalhadas é "igual a 0", a renda média é de R$846.7765. O B1 é 28.9009
#o que significa que a renda aumenta R$28.9009 para cada hora trabalhada. Ambos 
#os coeficientes tem um alto nível de significância estatística, 
#sendo o p-valor < 0.001. 

#Renda para trabalho de 40 horas semanais
intercepto2 <-  846.7765
Horas_trabalhadas0 <- 28.9009

rm40h <- intercepto2 + Horas_trabalhadas0*40
rm40h #2002.812

#Apresentação dos resultados
library(stargazer)
tabelareg2 <- stargazer(reg2, type = "html", style = "ajps",
                       title = "Regressão Linear ex.4",
                       column.labels = c("Coeficientes"), omit.stat = "f")

#5. 
#Intervalo de confiança questão 2
-357.173 - 1.96 * 13.127 #-382.9019
-357.173 + 1.96 * 13.127 #-331.4441
#ICreg [-382.9019, -331.4441]

#intervalo de confiança questão 4 
28.9009 - 1.96 *  0.4535 #28.01204
28.9009 + 1.96 *  0.4535 #29.78976
#ICreg2 [28.01204, 29.78976]

#Os intervalos de confiança são construídos, nesse caso, em torno do coeficiente 
#estimado pela regressão e representam o intervalo numérico no qual o parâmetro 
#populacional que a estimativa procurava medir se encontra a determinado nível 
#de confiança, isto é, a determinada probabilidade.

#Um nível de confiança de 95% mostra que 95% das amostras aleatórias da minha 
#população conterão o parâmetro dentro do intervalo de confiança, e 5% delas não 
#conterão. Como ambos os meus coeficientes estimados (em reg e em reg2) encontram
#-se contidos nos intervalos de confiança calculados (ICreg e ICreg2) e ambos os 
#intervalos de confiança não contém o número zero (0), meus coeficientes 
#estimados são estatisticamente significativos, isto é, apenas não estarão 
#contidos nos intervalos de confiança apenas em 5% das amostras aleatórias 
#geradas.

#Dizer que os coeficientes são estatisticamente significantes ou insignificantes 
#significa dizer que os coeficientes estimados para a minha amostra são 
#aleatórios, isto é, ocorreram apenas para essa amostra, ou se valem também para 
#outras amostras aleatórias da mesma população. Ao dizer que um coeficiente é 
#estatisticamente significativo a um nível de confiança de 95%, por exemplo, 
#estou dizendo que em 95% das amostras aleatórias de minha população esse 
#coeficiente também será estimado, conforme os resultados do teste de hipótese 
#(intervalo de confiança). Se meus coeficientes não estivessem contidos nos 
#intervalos de confiança ou se esses contivessem o número zero, poderíamos dizer 
#que meus coeficientes não são estatisticamente significativos, isto é, são 
#intrínsecos à minha amostra.

