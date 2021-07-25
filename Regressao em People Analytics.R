#' Modelo de Regressão em People Analitcs

#' O matarial é baseado no livro texto 
#' https://www.routledge.com/Handbook-of-Regression-Modeling-in-People-Analytics
#' -With-Examples-in-R/McNulty/p/book/9781032041742

# data example

url <- "https://peopleanalytics-regression-book.org/data/Galton.txt"
galton <- read.delim(url)

head(galton)

#' criando a media de altura
library(tidyverse)
galton <- galton %>% mutate(altura_pais_media = (Father + Mother)/2)

#' regressao simples altura e altura media dos pais

regsimples <- lm(formula = Height~altura_pais_media, data = galton)

#' O quanto explica - r quadrado
summary(regsimples)$r.squared

#' Galton percebeu que a altura das crianças se afasta da media de alutra
#' dos pais e se aproxima da média populacional.

#' Regrassão Logistica/Probit
#' Space Shuttle explodiu logo após a decolagem. Ivestigações estimam que
#' O-rings não funcionam bem em temperaturas baixas. Vamos rodar os dados
#' abaixo.
#' 
shuttles <- read.table(
  "https://archive.ics.uci.edu/ml/machine-learning-databases/space-shuttle/o-ring-erosion-only.data")

colnames(shuttles) <- c("total_orings", "distressed_orings", "temp",
                        "leakcheck_psi", "order")

head(shuttles)

#' Criando a variável Binária, baseado onde o-rings deram problemas

shuttles <- shuttles %>% 
  mutate(incident = ifelse(distressed_orings > 0, 1, 0))

#' Regressão simples para relacionar temperatura e vazamento.

model <- glm("incident ~ temp + leakcheck_psi", data = shuttles,
             family = "binomial")

summary(model)

#' Analisando resultados em relação ao tempo
#' No exemplo, y = 1 ou y = 0. 
#' Em people analitycs, varios dos resultados podem ocorrer em qualquer tempo
#' Portanto, além dos efeitos o efeito do tempo também é de interesse.

#' *Análise de sobrevivência* 
#' 1. constroi a sobrevivência de um resultado
#' 2. Calcula o Kaplan-Meier taxas e curvas de sobrevivência
#' 3. Roda e checa o Cox Proportional Hazard modelo de regressão. 

#'*Exemplo* 
#' Dropping Like Flies é uma empresa que possui alto desgates de funcionarios
#' O Diretor do RH acrecdita que a experiência do funcionário não é um fator
#' e sim a concorrência agressiva.

#' É possível que a experiência ruim na afete o desgaste de funcionários?

#' dados da pesquisa de RH

survey_responses <- read.csv("https://peopleanalytics-regression-book.org/data/survey_responses.csv")

head(survey_responses)

#' sentimento 1 a 10, 10 = feliz com o trabalho 
#' intenção 1 a 10, 10 = pretende procurar um novo trabalho.
library(ggplot2)
survey_responses %>% pivot_longer(c(sentiment,intention),
                                  values_to = "values") %>%
  ggplot(aes(x = values, fill = name))+
  geom_bar(alpha = .3, position = "identity")

#' sentimento ou intenção tem impacto significante sobre pedido de demissão
#' pelos próximos 2 anos?

departure_dates <- read.csv("https://peopleanalytics-regression-book.org/data/departure_dates.csv")

head(departure_dates)

#' Somente empregadores que saíram estão presente na base
#'  O Evento observado: Saiu da empresa ou não
#'  Tempo: 24 meses.

#' Juntando as duas bases
survival_data <- survey_responses %>%
  dplyr::left_join(departure_dates, by = "iid") %>%
  dplyr::select(-iid) %>%
  dplyr::mutate(departure_event = ifelse(is.na(departure_date), 0, 1))

head(survival_data)

#' Criando a variável mês que capta a duração no emprego.

library(lubridate)
survival_data <- survival_data %>% 
  dplyr::mutate(month = dplyr::case_when(
    year(departure_date) == 2019 ~ month(departure_date),
    year(departure_date) == 2020 ~ month(departure_date) + 12,
    departure_event == 0 ~ 24,
    TRUE ~ NA_real_)) %>%
  dplyr::select(-departure_date)

head(survival_data)

#' Criando o resultado de sobrevivência - Survival
#' O survival é gerado a partir de `departure_event` e `month` 

library(survival)
survival_outcome <- Surv(event = survival_data$departure_event, 
                         time = survival_data$month)
unique(survival_outcome)

#' Kaplan-Meier taxa de sobreviência
#' $$S_i = S_{i-1} \cdot (1 - \frac{l_i}{n_i})$$
#' Onde $l_i$ é a quantidade de saida no mes $i$
#' E $n_i$ é a quantidade de trabalhadores que ainda estão na empresa
#' e $S_0 = 1$

#' Vamos ver como os sentimentos afetam a taxa de sobrevivência
#' Categorizando as variáveis discreta

# create a new field to define high, medium and low sentiment (>= 7)
survival_data$sentiment_category <- dplyr::case_when(
  survival_data$sentiment >= 7 ~ "High",
  survival_data$sentiment >= 4 ~ "Medium",
  survival_data$sentiment < 4 ~ "Low",
  TRUE ~ NA_character_
)

# Relacionando a variavel categorica com a taxa de sobrevivência

# generate survival rates by sentiment category
kmestimate_sentimentcat <- survival::survfit(
  formula = Surv(event = departure_event, time = month) ~ sentiment_category,
  data = survival_data
)

# Curva Survival Kaplan-Meier

library(survminer)
# show survival curves with p-value estimate and confidence intervals
survminer::ggsurvplot(
  kmestimate_sentimentcat, pval = TRUE, conf.int = TRUE,
  palette = c("blue", "green", "red"),
  linetype = c("solid", "dashed", "dotted"), xlab = "Month",
  ylab = "Retention Rate"
)

#' Regressão tuilizando Modelo Cox Proporção Hazard

# run cox model against survival outcome
cox_model <- survival::coxph(
  formula = Surv(event = departure_event, time = month) ~ gender + 
    department + level + sentiment + intention,
  data = survival_data
)
summary(cox_model)
