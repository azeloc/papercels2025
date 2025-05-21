library(httr)
library(tidyverse)

#Substituir <API Key> pela Chave Pública
headers = c(
  'Authorization' = 'ApiKey cDZHYzlZa0JadVREZDJCendQbXY6SkJlTzNjLV9TRENyQk1RdnFKZGRQdw==',
  'Content-Type' = 'application/json'
)

body = '{
  "size": 10000,
  "query": {
        "bool": {
            "filter": [
                {"match": {"orgaoJulgador.codigo": 10565}},
                {"range": {"dataAjuizamento": {"gte": "2020-01-01"}}}
            ]
        }
   }
}';

res <- VERB("POST", url = "https://api-publica.datajud.cnj.jus.br/api_publica_tjsp/_search", body = body, add_headers(headers))

processos <- content(res)

corrige_1 <- function(source){
  tibble(
    numeroProcesso = source$numeroProcesso,
    classe = source$classe$nome,
    formato = source$formato$nome,
    ultima_atualizacao = source$dataHoraUltimaAtualizacao,
    dataAjuizamneto = source$dataAjuizamento,
    assuntos = list(source$assuntos),
    movimentos = list(source$movimentos),
    orgaoJulgador = source$orgaoJulgador$nome
  )
}

tabela_demais_varas <- processos$hits$hits |>
  purrr::map_dfr(function(x){
    x$`_source` |>
      corrige_1() |>
      mutate(sort = x$sort[[1]])
  })

processa_movimentos = function(x){
  tibble(
    codigo = x$codigo,
    nome = x$nome,
    data = x$dataHora
  )
}

movimentos = tabela_demais_varas |>
  filter(
    classe == "Procedimento Comum Cível"
  ) |>
  unnest(movimentos) |>
  mutate(
    movimentos_limpos = map(movimentos, processa_movimentos)
  ) |>
  unnest(movimentos_limpos)

movimentos_decisao = c(193, 385, 11877, 11876, 198,
                       15028, 871, 15030, 15026, 12615,
                       242, 241, 240, 14099, 12187, 466, 12738,
                       12649, 220, 11409, 11407, 11408, 15029, 12650,
                       200, 239, 901, 12331, 12329, 12330, 219,
                       11795, 15211, 11403, 11401, 11402, 15212,
                       221, 15213, 11406, 11404, 11405, 15214,
                       471, 237, 238, 972, 456, 228, 473, 472, 218,
                       11373, 458, 464, 461, 463, 454, 457, 460,
                       236, 459, 22, 246)

movimentos_mais_limpos = movimentos

analise_final = movimentos |>
  group_by(numeroProcesso) |>
  filter(
    (data == min(data[codigo %in% movimentos_decisao])) |
    (data == min(data[codigo == "26"]))
  ) |>
  ungroup()

basona_sobrevivencia = analise_final |>
  group_by(numeroProcesso) |>
  summarise(
    data_inicial = min(data[codigo == "26"]),
    data_julgamento = min(data[codigo %in% movimentos_decisao]),
    texto_final = first(nome[codigo %in% movimentos_decisao]),
    final_acompanhamento = max(ultima_atualizacao)
  ) |>
  ungroup() |>
  mutate(
    data_inicial = ymd_hms(data_inicial),
    data_julgamento = ymd_hms(data_julgamento),
    final_acompanhamento = ymd_hms(final_acompanhamento)
  ) |>
  filter(data_inicial >= as.Date("2020-01-01"))

base_analise = basona_sobrevivencia |>
  filter(data_inicial >= as.Date("2020-01-01")) |>
  mutate(
    tempo_final = ifelse(!is.na(data_julgamento),
                         as.numeric(as.Date(data_julgamento) - as.Date(data_inicial)),
                         as.numeric(as.Date(final_acompanhamento) - as.Date(data_inicial))),
    indicador_evento = !is.na(data_julgamento)
  )

library(survival)
library(ggfortify)

km_fit <- survfit(Surv(tempo_final, indicador_evento) ~ 1, data = filter(
  base_analise, data_inicial >= as.Date("2023-01-01")))

km_fit

autoplot(km_fit)

summary(km_fit)$table

km_fit <- survfit(Surv(tempo_final, indicador_evento) ~ 1, data = filter(base_analise, indicador_evento))

km_fit

autoplot(km_fit)

summary(km_fit)$table

analise_final |>
  filter(!(numeroProcesso %in% unique(movimentos_mais_limpos$numeroProcesso)))

setdiff(movimentos_mais_limpos$numeroProcesso |> unique(),
        analise_final |> filter(codigo == "26") |> with(numeroProcesso) |> unique())
