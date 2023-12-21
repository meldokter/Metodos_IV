#Aula 12 - GLM 

library(here)
library(data.table)
library(tidyverse)
library(sjlabelled) # pra remover labelled variables
library(haven)
library(janitor)
library(lubridate)
library(knitr)
library(broom)

## dados
# https://www.latinobarometro.org/latContents.jsp

lat_bar23 <- sjlabelled::read_spss(here("Dados", 
                                        "Latinobarometro_2023_Eng_Spss_v1_0.sav"),
                                   drop.labels = TRUE) %>%
  mutate(S17 = as.Date(as.character(S17), "%Y%m%d")) %>%
  clean_names()

# get_label(lat_bar23)

lat_bar23 <- lat_bar23 %>%
  mutate(data_base = as.Date(paste(diareal, mesreal, "2023", sep="-"), 
                             "%d-%m-%Y"),
         idade = year(as.period(interval(s17,data_base))),
         econ_12_meses = ifelse(p6stgbs %in% c(1,2), "better", 
                                ifelse(p6stgbs == 8, NA, "other")),
         econ_12_meses = relevel(as.factor(econ_12_meses), ref = "other"),
         aprovacao_presidente = ifelse(p15stgbs == 0, NA, p15stgbs),
         ideologia = ifelse(p16st %in% c(97, 98, 99), NA, p16st),
         votaria_governo = ifelse(perpart == 4, NA,
                                  ifelse(perpart == 1, 1, 0)),
         genero = factor(sexo, labels = c("homem", "mulher")),
         evangelico = ifelse(s1 %in% c(0,98), NA,
                             ifelse(s1 %in% c(2,3,4,5), 1, 0))) 
# não considera adventista, testemunha Jeová, Mórmon

br_latbar_23 <- lat_bar23 %>%
  mutate(idenpa = remove_all_labels(idenpa)) %>% # haven_labelled problems
  filter(idenpa == 76) %>% ## seelciona brasil
  filter(!is.na(votaria_governo) & !is.na(evangelico) & !is.na(ideologia) 
         & !is.na(econ_12_meses))

reg_logistica <- glm(votaria_governo ~ ideologia, data=br_latbar_23,
                     family=binomial(link= "logit"))

reg_logistica %>%
  tidy() %>%
  
  # plotando # usando base R
  # código adaptado de Regression and Other Stories (Gelman et. al.)
  library(arm)

n <- nrow(br_latbar_23)
ideologia_jitt <- br_latbar_23$ideologia + runif(n, -.2, .2)
vote_jitt <- br_latbar_23$votaria_governo + 
  ifelse(br_latbar_23$votaria_governo==0, 
         runif(n, .005, .05), runif(n, -.05, -.005))
curve(invlogit(reg_logistica$coef[1] + reg_logistica$coef[2]*x), 
      from = -5,to=24, ylim=c(0,1), xlim=c(-5, 24), xaxt="n", xaxs="i", 
      ylab="Pr (voto governo)", xlab="Ideologia", lwd=.5, yaxs="i")
curve(invlogit(reg_logistica$coef[1] + reg_logistica$coef[2]*x), 1, 11, 
      lwd=3, add=TRUE)
axis(1, 0:10)
mtext("(left)", side=1, line=1.7, at=0, adj=.5)
mtext("(right)", 1, 1.7, at=11, adj=.5)
points(ideologia_jitt, vote_jitt, pch=20, cex=.1)
  kable()
  
newdata <- data.frame(ideologia = 1:11)
previsao <- predict(reg_logistica, newdata =newdata, type = "response")
print(round(previsao, 2))

#Probit 
reg_logistica <- glm(votaria_governo ~ ideologia, data=br_latbar_23,
                     family=binomial(link= "logit"))

reg_probit <- glm(votaria_governo ~ ideologia, data=br_latbar_23,
                  family=binomial(link= "probit"))

library(stargazer)

stargazer(reg_logistica, reg_probit, type = "html")

#logística com múltiplos preditores 
reg_logistica1 <- glm(votaria_governo ~ ideologia + evangelico, data=br_latbar_23,
                      family=binomial(link= "logit"))

reg_logistica1 %>%
  tidy() %>%
  kable()

br_latbar_23 <- br_latbar_23 %>%
  mutate(ideologia_pad = (ideologia - mean(ideologia))/sd(ideologia))

reg_logistica2 <- glm(votaria_governo ~ ideologia_pad + evangelico, data=br_latbar_23,
                      family=binomial(link= "logit"))

reg_logistica2 %>%
  tidy() %>%
  kable()

jitter_binary <- function(a, jitt=0.05){
  ifelse(a==0, runif(length(a), 0, jitt), runif(length(a), 1 - jitt, 1))
}

br_latbar_23$votaria_governo_jitter <- jitter_binary(br_latbar_23$votaria_governo)

ideologia_jitt <- br_latbar_23$ideologia_pad + runif(n, -.05, .05)

plot(ideologia_jitt, br_latbar_23$votaria_governo_jitter, xlim=c(0,max(ideologia_jitt)))

curve(invlogit(cbind(1, x, 0) %*% coef(reg_logistica2)), add=TRUE)
curve(invlogit(cbind(1, x, 1.0) %*% coef(reg_logistica2)), add=TRUE)

#ajuste do modelo
y_hat <- predict(reg_logistica, type="response")
y_obs <- br_latbar_23$votaria_governo

log_likelihood <- sum(y_obs*log(y_hat) + (1-y_obs)*log(1 - y_hat))
print(log_likelihood)

print(sum(y_obs*log(mean(y_obs)) + (1-y_obs)*log(1 - mean(y_obs))))

print(nrow(br_latbar_23)*log(.5))

stargazer(reg_logistica, reg_logistica1, type = "html")