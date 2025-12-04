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

teste <- survfit(Surv(Time, evento) ~ 1, data = base_sobrevivencia |> mutate(evento = evento != "censor"))

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
  select(numeProcesso, dataRecebimento, valor_numerico, assunto_info, nmForo, classe_info)

base_sobrevivencia |> left_join(covariavel) |> writexl::write_xlsx("20250806_dados_civeis.xlsx")

modelo_regressao = coxph(
  Surv(Time, evento) ~ log(valor_numerico),
  data = base_sobrevivencia |> left_join(covariavel),
  id = numeProcesso)

cox.zph(modelo_regressao)

modelo_regressao |>
  stargazer::stargazer()

base_grafico_dinheiro <- base_sobrevivencia |> left_join(covariavel) |>
  mutate(
    faixa_valor = case_when(
      valor_numerico <= 2000 ~ "< 2000",
      valor_numerico <= 5000 ~ "< 5000",
      valor_numerico <= 15000 ~ "< 15000",
      valor_numerico <= 20000 ~ "< 20000",
      valor_numerico <= 30000 ~ "< 30000",
      TRUE ~ "> 40000"
    )
  )

survfit(
  Surv(Time, evento) ~ year(dataRecebimento),
  data = base_grafico_dinheiro |>
  mutate(evento = evento != "censor"))

modelo2 <- survfit(Surv(Time, evento) ~ faixa_valor, data = base_grafico_dinheiro |>
                     mutate(evento = evento != "censor"))

dados_adicionais <- base_grafico_dinheiro |>
  filter(evento != "censor") |>
  group_by(faixa_valor) |>
  summarise(
    time = median(Time)
  ) |>
  ungroup() |>
  mutate(
    faixa_valor = as.numeric(str_remove_all(faixa_valor, "(<|>) "))
  )

modelo2 |> broom::tidy() |>
  group_by(strata) |>
  filter(estimate == estimate[which.min((estimate/(max(estimate)) - 0.5)^2)]) |>
  slice_head(n = 1) |>
  ungroup() |>
  mutate(
    faixa_valor = as.numeric(str_remove_all(strata, "faixa_valor=(<|>)")),
    estimate_type = "Survival Analysis"
  ) |>
  bind_rows(
    dados_adicionais |>
    mutate(
      estimate_type = "Complete Case"
    )
  ) |>
  ggplot(aes(x = faixa_valor, y = time, linetype = estimate_type)) +
  geom_line() +
  geom_point(size =2) +
  theme_bw() +
  labs(x = "Case Amount (BRL)", y = "Mean Time until Trial (days)", color = "Trial outcome",
       linetype = "") +
  theme(legend.position = "bottom")

grafico2 <- modelo2 |>
  autoplot()

grafico2$data |>
  filter(time > 0) |>
  ggplot(aes(x = time, y = pstate, color = event, linetype = strata)) +
  geom_step() +
  theme_bw() +
  labs(x = "Time in days", color = "Outcome", y = "Probability of each event")


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

prevalencia = base_sobrevivencia |> ungroup() |> filter(evento != "censor") |>   count(evento) |> mutate(p = n/sum(n))

tibble(
  evento = prevalencia$evento,
  `Complete case probability` = prevalencia$p,
  `Survival adjusted probability` = final_cif_cox
) |>
  pivot_longer(-evento) |>
  mutate(
    name = fct_relevel(name,
                      c("Survival adjusted probability",
                      "Complete case probability"))
  ) |>
  ggplot(aes(x = evento, y = value, fill = name)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x = "", y = "", fill = "")
