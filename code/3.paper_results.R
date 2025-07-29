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

cox.zph(modelo_regressao)

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

base_com_covariaveis <- base_sobrevivencia |>
  left_join(covariavel) |>
  mutate(l_val = log(valor_numerico),
         indicador = 1)

eventos <- as.character(sort(unique(base_com_covariaveis$evento))[-1])
coeficientes_finegray <- purrr::map_dfr(eventos, function(evento){

  cox_data <- finegray(
    Surv(Time, evento) ~ l_val,
    data=base_com_covariaveis,
    etype=evento)

  coeficientes <- coxph(Surv(fgstart, fgstop, fgstatus) ~ l_val,
      data=cox_data,
      weights= fgwt) |>
    broom::tidy() |>
    mutate(
      event = evento, .before = "term"
    )
})

modelo_regressao <- coxph(
  Surv(Time, evento) ~ l_val,
  data = base_com_covariaveis,
  id = numeProcesso)

s <- survfit(modelo_regressao, newdata = data.frame(l_val = c(0)))
pstates <- s$pstate
last_time <- nrow(pstates)

final_cif <- pstates[last_time,,2:(1+length(eventos))]

s2 <- survfit(modelo_regressao, newdata = data.frame(l_val = log(10000)))
plot(s2)

pstates <- s2$pstate
last_time <- nrow(pstates)

final_cif_cox <- pstates[last_time,,2:(1+length(eventos))]

coefficients_table <- coeficientes_finegray |>
  mutate(
    baseline_cif = final_cif,
    cif_10k_brl = final_cif_cox,
    marginal = cif_10k_brl-baseline_cif
  ) |>
  select(-statistic)
