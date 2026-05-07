# ==============================================================================
# Trabalho Final de Estatística II
# Script 02 — Análise descritiva
# ==============================================================================

source(file.path("R", "00_setup.R"))

cat("\nScript 02 — Análise descritiva\n")

base_limpa <- file.path(paths$data_processed, "base_limpa.rds")
if (!file.exists(base_limpa)) {
  stop("Base limpa não encontrada. Rode primeiro R/01_importacao_limpeza.R.")
}

dados <- readRDS(base_limpa)
setDT(dados)

# Estatísticas descritivas gerais
tabela_descritiva_geral <- dados[, .(
  variavel = c("Expectativa de vida", "PIB per capita PPP", "CO2 per capita"),
  media = c(mean(life_exp), mean(gdp_pc), mean(co2_pc)),
  mediana = c(median(life_exp), median(gdp_pc), median(co2_pc)),
  desvio_padrao = c(sd(life_exp), sd(gdp_pc), sd(co2_pc)),
  minimo = c(min(life_exp), min(gdp_pc), min(co2_pc)),
  q1 = c(quantile(life_exp, 0.25), quantile(gdp_pc, 0.25), quantile(co2_pc, 0.25)),
  q3 = c(quantile(life_exp, 0.75), quantile(gdp_pc, 0.75), quantile(co2_pc, 0.75)),
  maximo = c(max(life_exp), max(gdp_pc), max(co2_pc)),
  n = .N
)]

# Estatísticas por grupo usado no teste principal
tabela_descritiva_por_grupo <- dados[, .(
  n = .N,
  media_life_exp = mean(life_exp),
  mediana_life_exp = median(life_exp),
  dp_life_exp = sd(life_exp),
  erro_padrao = sd(life_exp) / sqrt(.N),
  min_life_exp = min(life_exp),
  max_life_exp = max(life_exp)
), by = alta_renda]

# Proporções auxiliares para eventual discussão complementar
tabela_vida_75 <- dados[, .N, by = .(alta_renda, vida_acima_75)]
tabela_vida_75[, proporcao := N / sum(N), by = alta_renda]

readr::write_csv(tabela_descritiva_geral, file.path(paths$output_tables, "tabela_descritiva_geral.csv"))
readr::write_csv(tabela_descritiva_por_grupo, file.path(paths$output_tables, "tabela_descritiva_por_grupo.csv"))
readr::write_csv(tabela_vida_75, file.path(paths$output_tables, "tabela_vida_acima_75_por_grupo.csv"))

cat("Tabela descritiva geral:\n")
print(tabela_descritiva_geral)
cat("\nTabela descritiva por grupo:\n")
print(tabela_descritiva_por_grupo)
