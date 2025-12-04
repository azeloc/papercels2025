da_report <- data |>
  mutate(
    disposition_date = mdy(TERMDATE),
    filling_date = mdy(FILEDATE)
  ) |>
  mutate(
    time = ifelse(
      STATUSCD == "L", as.numeric(lubridate::mdy(TERMDATE)-lubridate::mdy(FILEDATE)),
      as.numeric(as.Date("2025-07-25")-lubridate::mdy(FILEDATE))
    ),
    evento = STATUSCD == "L",
    court_action = PROCPROG %in% c(1),
    #evento = factor(case_when(
    #  PROCPROG %in% c(1) ~ "no court action",
    #  STATUSCD == "L" ~ "court action",
    #  TRUE ~ "censor"
    #  ), labels = c("censor", "no court action", "court action")),
    CIRCUIT = as.character(CIRCUIT),
    JURIS = (str_detect(PLT, "COMMONWEALTH|USA|UNITED +STATES|STATE +OF|FEDERAL TRADE COMMISSIO") | str_detect(DEF, "COMMONWEALTH|USA|UNITED +STATES|STATE +OF|FEDERAL TRADE COMMISSIO"))
  ) |>
  filter(filling_date >= "2002-01-01")

# complete case -----------------------------------------------------------

da_report |>
  filter(disposition_date >= "2024-04-01", disposition_date <= "2025-03-31") |>
  group_by(a = PROCPROG %in% c(1)) |>
  summarise(
    freq = n(),
    time = median(as.numeric(disposition_date-filling_date))/30)

# survival ----------------------------------------------------------------

library(survival)

modelo <- survfit(Surv(time, evento) ~ court_action, data = da_report)

modelo |>
  broom::tidy()

library(ggfortify)

autoplot(modelo)
