#Aula 7 - Causalidade 

#8.3 Simulação de Resultados Potenciais e Causalidade

## potential outcomes

n1 <- 5000
tau <- 2 # ATE Average Treatment Effect (ATE) or Average Causal Effect (ACE)
alpha <- 1
eta_0 <- rnorm(n1)
eta_1 <- rnorm(n1)


##
n <- n1+n1
x <- rnorm(n1)

gamma <- 2
nu_0 <- rnorm(n1)
nu_1 <- rnorm(n1)
eta_0 <- gamma*x + nu_0
eta_1 <- gamma*x + nu_1

treatment <- rep(1:0, each=n1)

# potential outcomes
y_1 <- alpha + tau*treatment[1:n1] + eta_1
y_0 <- alpha + tau*treatment[(n1+1):n] + eta_0

# ATT ou ACE
mean(y_1 - y_0)

amostra <- sample(1:length(treatment), size = 1000)
df <- data.frame(y = c(y_1, y_0)[amostra], x=c(x, x)[amostra], 
                 treatment = treatment[amostra])

reg <- lm(y ~ 1, data=df)
summary(reg)

reg1 <- lm(y ~ x, data=df)
summary(reg1)

## Ignorability apenas com CIA (x)
library(arm)
x <- rnorm(n)
treatment <- ifelse(x > 0, 1,0)

gamma <- 2
tau <- -2 # ATE Average Treatment Effect (ATE) or Average Causal Effect (ACE)
alpha <- 1
nu <- rnorm(n)
eta <- gamma*x + nu

y <- alpha + tau*treatment + eta

df <- data.frame(y=y, x=x, treatment = treatment)
reg3 <- lm(y ~ treatment, data=df)
summary(reg3)

reg4 <- lm(y ~ treatment +  x, data=df)
summary(reg4)

library(tidyverse)
library(ggplot2)

df %>%
  group_by(treatment) %>%
  mutate(uncond_y = mean(y),
         erro = (y - uncond_y),
         sqs = sum(erro)^2) %>%
  ggplot(aes(x, erro )) + geom_point()

df %>%
  mutate(bol_x = ifelse(x > 0 ,1,0)) %>%
  group_by(treatment, bol_x) %>%
  mutate(cond_y = mean(y)) %>%
  ungroup() %>%
  mutate(erro = (y - cond_y),
         sqs = sum(erro)^2) %>%
  ggplot(aes(x, erro )) + geom_point()

# -5 ocorre 10%,02 10% etc. até 5, 10% das vezes
x <- sample(1:11, size=n, replace=T) - 6
treatment <- ifelse(x > 0, 1,0)

gamma <- 2
tau <- -2 # ATE Average Treatment Effect (ATE) or Average Causal Effect (ACE)
alpha <- 1
nu <- rnorm(n)
eta <- gamma*x + nu

y <- alpha + tau*treatment + eta


df %>%
  mutate(bol_x = ifelse(x > 0 ,1,0)) %>%
  group_by(treatment, bol_x) %>%
  mutate(cond_y = alpha + .5*tau + gamma*x ) %>%
  ungroup() %>%
  mutate(erro = (y - cond_y),
         sqs = sum(erro)^2) %>%
  ggplot(aes(x, erro )) + geom_point()

df %>%
  mutate(bol_x = ifelse(x > 0 ,1,0),
         cond_y = alpha + tau*treatment + gamma*x,
         erro = (y - cond_y),
         sqs = sum(erro)^2) %>%
  ggplot(aes(y=erro, x=x )) + geom_point() + geom_smooth(method="lm")

df %>%
  mutate(bol_x = ifelse(x > 0 ,1,0),
         cond_y = alpha + tau*treatment + gamma*x,
         erro = (y - cond_y),
         sqs = sum(erro)^2) %>%
  summarise(cor(erro, x))