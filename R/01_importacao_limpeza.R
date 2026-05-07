# ==============================================================================
# Trabalho Final de Estatística II
# Script 01 — Importação, limpeza e recorte cross-section
# ==============================================================================

source(file.path("R", "00_setup.R"))

cat("\nScript 01 — Importação e limpeza dos dados\n")

# Indicadores do World Bank usados na análise
indicadores <- c(
  life_exp = "SP.DYN.LE00.IN",    # Expectativa de vida ao nascer (anos)
  gdp_pc = "NY.GDP.PCAP.PP.CD",   # PIB per capita PPP (US$ internacionais correntes)
  co2_pc = "EN.ATM.CO2E.PC"       # Emissões de CO2 per capita (toneladas métricas)
)

arquivo_bruto <- file.path(paths$data_raw, "wdi_worldbank_2020_2022.rds")
arquivo_limpo_rds <- file.path(paths$data_processed, "base_limpa.rds")
arquivo_limpo_csv <- file.path(paths$data_processed, "base_limpa.csv")

# Baixar a base bruta apenas se ela ainda não existir localmente
if (file.exists(arquivo_bruto)) {
  dados_brutos <- readRDS(arquivo_bruto)
  cat("Base bruta carregada de data/raw.\n")
} else {
  dados_brutos <- WDI::WDI(
    indicator = indicadores,
    start = 2020,
    end = 2022,
    extra = TRUE
  )
  saveRDS(dados_brutos, arquivo_bruto)
  cat("Base bruta baixada do World Bank e salva em data/raw.\n")
}

# Converter para data.table e padronizar nomes
setDT(dados_brutos)
dados_brutos <- janitor::clean_names(dados_brutos)

# Limpeza principal
# 1. Remover agregados regionais/mundiais.
# 2. Remover observações sem as variáveis necessárias.
# 3. Manter o ano mais recente disponível para cada país.
dados_limpos <- dados_brutos[
  region != "Aggregates" &
    !is.na(life_exp) &
    !is.na(gdp_pc) &
    !is.na(co2_pc)
]

setorder(dados_limpos, country, iso2c, -year)
dados_limpos <- dados_limpos[, .SD[1], by = .(country, iso2c)]

dados_limpos <- dados_limpos[, .(
  country,
  iso2c,
  year,
  region,
  income,
  life_exp,
  gdp_pc,
  co2_pc
)]

# Criar variáveis úteis para os testes
dados_limpos[, alta_renda := fifelse(income == "High income", "Alta renda", "Demais países")]
dados_limpos[, alta_renda := factor(alta_renda, levels = c("Demais países", "Alta renda"))]
dados_limpos[, vida_acima_75 := fifelse(life_exp > 75, "Acima de 75 anos", "Até 75 anos")]
dados_limpos[, vida_acima_75 := factor(vida_acima_75, levels = c("Até 75 anos", "Acima de 75 anos"))]
dados_limpos[, log_gdp_pc := log(gdp_pc)]

# Tabelas de checagem do recorte cross-section
distribuicao_ano <- dados_limpos[, .N, by = year][order(year)]
distribuicao_income <- dados_limpos[, .N, by = income][order(income)]
checagem_paises <- dados_limpos[, .N, by = country][N > 1]

readr::write_csv(distribuicao_ano, file.path(paths$output_tables, "distribuicao_ano.csv"))
readr::write_csv(distribuicao_income, file.path(paths$output_tables, "distribuicao_income.csv"))

if (nrow(checagem_paises) > 0) {
  readr::write_csv(checagem_paises, file.path(paths$output_tables, "paises_duplicados.csv"))
  warning("Há países com mais de uma observação. Verifique output/tables/paises_duplicados.csv.")
}

# Salvar base limpa
saveRDS(dados_limpos, arquivo_limpo_rds)
readr::write_csv(dados_limpos, arquivo_limpo_csv)

cat("Base limpa salva em data/processed.\n")
cat("Número de países na base final:", nrow(dados_limpos), "\n")
cat("Distribuição por ano:\n")
print(distribuicao_ano)
cat("Distribuição por grupo de renda:\n")
print(distribuicao_income)
