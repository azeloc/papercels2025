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

movimentos <- dados |>
  mutate(
    dataRecebimento = lubridate::dmy(dataRecebimento),
    valor_numerico = gera_valor(valor)
  ) |>
  unnest(movimentacao) |>
  mutate(
    dtMovimento = lubridate::dmy(dtMovimento)
  ) |>
  arrange(numeProcesso, dtMovimento)

regex_acordo <- regex("acordo", ignore_case = TRUE)
regex_remessa <- regex("remetidos? os autos", ignore_case = TRUE)
regex_arquivo <- regex("arquivado|baixa definitiva", ignore_case = TRUE)
regex_dependencia <- regex("depend[êe]ncia", ignore_case = TRUE)
regex_suspenso <- regex("suspens[oaã]", ignore_case = TRUE)
regex_irdr <- regex("irdr|incidente de resolução de demandas repetitivas", ignore_case = TRUE)
regex_entranhado <- regex("entranhado", ignore_case = TRUE)
regex_prescricao <- regex("Prescri[cç][aã]o", ignore_case = TRUE)
regex_159 <- regex(" 159| litig[âa]ncia abusiva|litig[aâ]ncia predat[óo]ria", ignore_case = TRUE)

dados_com_censura = movimentos |>
  #filter(numeProcesso == '1000515-14.2020.8.26.0100') |>
  group_by(numeProcesso, dataRecebimento) |>
  summarise(
    recomendacao_159 = any(str_detect(descricao, regex_159)),
    processo_por_dependencia = any(str_detect(movimento, regex_dependencia)),
    data_movimentacao_sentenca = min(
      dtMovimento[(
        (str_detect(movimento, regex_suspenso) & str_detect(movimento, regex_irdr)) |
        (str_detect(descricao, regex_suspenso) & str_detect(descricao, regex_irdr)) |
        str_detect(movimento, regex_entranhado) |
        str_detect(movimento, regex_prescricao) |
        str_detect(movimento, regex_arquivo) |
        str_detect(movimento, regex_remessa) |
        str_detect(movimento, regex_acordo) |
        str_detect(movimento, "[Pp]roced|[Ss]em [rR]esolu|Senten") |
        str_detect(movimento, "[Ss]enten[ç]|Julgo")) &
        !str_detect(movimento, "[Cc]onclu")]),
    movimentacao_sentenca = first(
      movimento[(
        (str_detect(movimento, regex_suspenso) & str_detect(movimento, regex_irdr)) |
          (str_detect(descricao, regex_suspenso) & str_detect(descricao, regex_irdr)) |
          str_detect(movimento, regex_entranhado) |
          str_detect(movimento, regex_prescricao) |
          str_detect(movimento, regex_arquivo) |
          str_detect(movimento, regex_remessa) |
          str_detect(movimento, regex_acordo) |
          str_detect(movimento, "[Pp]roced|[Ss]em [rR]esolu|Senten") |
          str_detect(movimento, "[Ss]enten[ç]|Julgo")) &
          !str_detect(movimento, "[Cc]onclu")]),
    ultima_movimentacao = max(dtMovimento)
  ) |>
  ungroup()

base_sobrevivencia <- dados_com_censura |>
  mutate(
    evento = factor(case_when(
      str_detect(movimentacao_sentenca, "Homologação") ~ 1,
      str_detect(movimentacao_sentenca, "[Ii]mproced") ~ 2,
      str_detect(movimentacao_sentenca, "Proced") ~ 3,
      str_detect(movimentacao_sentenca, "[dD]esistência") ~ 5,
      str_detect(movimentacao_sentenca, "[Ss]em [rR]eso|Ausência de pressupostos") ~ 4,
      str_detect(movimentacao_sentenca, "[Rr]emetid|[Rr]edist") ~ 6,
      is.na(movimentacao_sentenca) ~ 0,
      TRUE ~ 7
    ),
    0:7,
    c(
      "censor", "Settlement", "Defendant win", "Plaintiff win",
      "Mistrial", "Withdraw", "Redistribution", "Suspension")
    )
  ) |>
  mutate(
    Time = ifelse(
      !is.na(movimentacao_sentenca),
      as.numeric(data_movimentacao_sentenca-dataRecebimento),
      as.numeric(Sys.Date()-dataRecebimento)
    )
  ) |>
  filter(!processo_por_dependencia) |>
  mutate(

  )

base_sobrevivencia |>
  filter(!processo_por_dependencia) |>
  count(evento) |> with(n[1]/sum(n))

library(survival)
library(ggfortify)

modelo <- survfit(Surv(Time, evento) ~ 1, data = base_sobrevivencia)
grafico <- modelo |>
  autoplot()

saveRDS(base_sobrevivencia, "data/survival_data.rds")
