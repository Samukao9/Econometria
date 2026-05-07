# ==============================================================================
# Trabalho Final de Estatística II
# Script 05 — Modelo OLS opcional e complementar
# ==============================================================================

source(file.path("R", "00_setup.R"))

cat("\nScript 05 — Regressão OLS complementar\n")
cat("Atenção: esta análise é complementar e não substitui o teste de hipótese principal.\n")

base_limpa <- file.path(paths$data_processed, "base_limpa.rds")
if (!file.exists(base_limpa)) {
  stop("Base limpa não encontrada. Rode primeiro R/01_importacao_limpeza.R.")
}

dados <- readRDS(base_limpa)
setDT(dados)

# Modelo complementar 1: diferença de médias expressa como regressão com dummy
modelo_dummy <- lm(life_exp ~ alta_renda, data = dados)

# Modelo complementar 2: associação entre log do PIB per capita e expectativa de vida
modelo_log_pib <- lm(life_exp ~ log_gdp_pc, data = dados)

resultado_modelo_dummy <- broom::tidy(modelo_dummy)
resultado_modelo_dummy$modelo <- "life_exp ~ alta_renda"
resultado_modelo_log_pib <- broom::tidy(modelo_log_pib)
resultado_modelo_log_pib$modelo <- "life_exp ~ log_gdp_pc"

resumos_modelos <- data.table::rbindlist(list(
  data.table::as.data.table(resultado_modelo_dummy),
  data.table::as.data.table(resultado_modelo_log_pib)
), fill = TRUE)

ajuste_modelos <- data.table::rbindlist(list(
  data.table::as.data.table(broom::glance(modelo_dummy))[, modelo := "life_exp ~ alta_renda"],
  data.table::as.data.table(broom::glance(modelo_log_pib))[, modelo := "life_exp ~ log_gdp_pc"]
), fill = TRUE)

readr::write_csv(resumos_modelos, file.path(paths$output_tables, "resultados_ols_complementar.csv"))
readr::write_csv(ajuste_modelos, file.path(paths$output_tables, "ajuste_ols_complementar.csv"))

cat("Resultados OLS complementares salvos em output/tables.\n")
cat("\nModelo com dummy de alta renda:\n")
print(summary(modelo_dummy))
cat("\nModelo com log do PIB per capita:\n")
print(summary(modelo_log_pib))
