library(tidyverse)

base_sobrevivencia <- readRDS("data/survival_data.rds")

base_sobrevivencia |>
  ungroup() |>
  count(evento) |>
  mutate(
    p = n/sum(n)
  ) |> arrange(desc(p))

library(survival)
library(ggfortify)

modelo <- survfit(Surv(Time, evento) ~ 1, data = base_sobrevivencia)

grafico <- modelo |>
  autoplot()

grafico$data |>
  filter(time > 0) |>
  ggplot(aes(x = time, y = pstate, color = event)) +
  geom_step() +
  theme_bw() +
  labs(x = "Time in days", color = "Outcome", y = "Probability of each event")

library(tidyverse)

dados <- readRDS("data/base_parseada.rds")

gera_valor <- function(x){
  x |>
    readr::parse_number(
      locale = readr::locale(
        decimal_mark = ",",
        grouping_mark = "."
      ))
}

covariavel = dados |>
  mutate(
    dataRecebimento = lubridate::dmy(dataRecebimento),
    valor_numerico = gera_valor(valor)
  ) |>
  select(numeProcesso, valor_numerico)

modelo_regressao = coxph(
  Surv(Time, evento) ~ log(valor_numerico),
  data = base_sobrevivencia |> left_join(covariavel),
  id = numeProcesso)

modelo_regressao |>
  stargazer::stargazer()

library(nnet)

dados = base_sobrevivencia |>
  filter(evento != "censor") |>
  left_join(covariavel)

modelo <- multinom(evento ~ log(valor_numerico), data = dados)

library(stargazer)

modelo|>
  stargazer::stargazer()

