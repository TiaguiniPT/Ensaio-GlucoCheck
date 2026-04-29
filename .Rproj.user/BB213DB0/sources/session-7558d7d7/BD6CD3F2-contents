library(here)
library(readr)
library(gtsummary)
library(ggplot2)

# Carregar dados
dados <- read_csv(here("data/raw/glucocheck.csv"))

# Converter variáveis categóricas
dados$sexo  <- factor(dados$sexo, levels = c("M", "F"))
dados$grupo <- factor(dados$grupo, levels = c("intervencao", "controlo"))

# ── Tabela 1: Características basais ──────────────────────────────────────
tabela1 <- dados |>
  dplyr::select(grupo, idade, sexo,
                hba1c_baseline, glicemia_jejum_baseline,
                peso_baseline, des_sf, satisfacao) |>
  tbl_summary(
    by = grupo,
    label = list(
      idade                    ~ "Idade (anos)",
      sexo                     ~ "Sexo",
      hba1c_baseline           ~ "HbA1c inicial (%)",
      glicemia_jejum_baseline  ~ "Glicemia jejum inicial (mg/dL)",
      peso_baseline            ~ "Peso inicial (kg)",
      des_sf                   ~ "Desempenho SF",
      satisfacao               ~ "Satisfação"
    )
  ) |>
  add_p() |>
  modify_header(label ~ "**Variável**")

tabela1

# ── Gráfico: HbA1c aos 6 meses por grupo ──────────────────────────────────
grafico_outcome <- ggplot(dados, aes(x = grupo, y = hba1c_6meses, fill = grupo)) +
  geom_boxplot(alpha = 0.7, outlier.shape = 21) +
  geom_jitter(width = 0.1, alpha = 0.5, size = 2) +
  scale_fill_manual(values = c("intervencao" = "#2196F3", "controlo" = "#FF9800")) +
  labs(
    title    = "HbA1c aos 6 meses por grupo",
    subtitle = "Ensaio GlucoCheck",
    x        = "Grupo",
    y        = "HbA1c (%)",
    fill     = "Grupo"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

grafico_outcome
