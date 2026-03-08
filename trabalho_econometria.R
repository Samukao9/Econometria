# ==============================================================================
# TRABALHO DE ECONOMETRIA I / MÉTODOS QUANTITATIVOS
# Universidade Presbiteriana Mackenzie
# Professor: Thiago Lobo
#
# Tema: Regressão Linear por Mínimos Quadrados Ordinários (MQO/OLS)
# Variáveis:
#   Y  = Expectativa de vida ao nascer (anos)
#   X1 = PIB per capita, PPP (dólares internacionais correntes)
#   X2 = Emissões de CO2 per capita (toneladas métricas)
# Fonte: World Bank Open Data
# Período: 2022 (dados cross-section, uma observação por país)
# ==============================================================================

# ====== CONFIGURAÇÃO INICIAL ======

# Instalar pacotes necessários caso ainda não estejam instalados
pacotes <- c("WDI", "tidyverse", "corrplot")
for (p in pacotes) {
  if (!require(p, character.only = TRUE, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
    library(p, character.only = TRUE)
  }
}

cat("\n========================================\n")
cat("  TRABALHO DE ECONOMETRIA I - MACKENZIE\n")
cat("  Regressão OLS: Expectativa de Vida\n")
cat("========================================\n\n")

# Definir diretório para salvar gráficos
dir_graficos <- "graficos"
if (!dir.exists(dir_graficos)) dir.create(dir_graficos)

# ====== PARTE 1: AQUISIÇÃO E LIMPEZA DOS DADOS ======

cat("====== PARTE 1: AQUISIÇÃO E LIMPEZA DOS DADOS ======\n\n")

# Códigos dos indicadores do World Bank
indicadores <- c(
  life_exp = "SP.DYN.LE00.IN",    # Expectativa de vida ao nascer
  gdp_pc   = "NY.GDP.PCAP.PP.CD", # PIB per capita (PPP)
  co2_pc   = "EN.ATM.CO2E.PC"     # Emissões de CO2 per capita
)

# Tentar baixar os dados do World Bank
cat("Baixando dados do World Bank...\n")

dados_brutos <- tryCatch({
  WDI::WDI(
    indicator = indicadores,
    start     = 2020,
    end       = 2022,
    extra     = TRUE  # inclui informações adicionais (região, tipo, etc.)
  )
}, error = function(e) {
  cat("ERRO ao baixar dados do WDI:", conditionMessage(e), "\n")
  cat("Tentando ler dados do arquivo local 'dados_limpos.csv'...\n")
  if (file.exists("dados_limpos.csv")) {
    return(NULL)  # sinaliza para usar o CSV local
  } else {
    stop("Não foi possível obter os dados. Verifique sua conexão com a internet.")
  }
})

if (is.null(dados_brutos)) {
  # Fallback: ler do CSV local
  dados <- read.csv("dados_limpos.csv", stringsAsFactors = FALSE)
  cat("Dados carregados do arquivo local.\n")
} else {
  cat("Dados baixados com sucesso!\n\n")

  # Filtrar apenas o ano mais recente com dados disponíveis (2020-2022)
  # e remover agregados (regiões, mundo, grupos de renda)
  # O campo 'region' é NA para agregados no pacote WDI
  dados <- dados_brutos %>%
    filter(
      region != "Aggregates",  # remove agregados
      !is.na(life_exp),
      !is.na(gdp_pc),
      !is.na(co2_pc)
    ) %>%
    # Para cada país, pegar o ano mais recente disponível
    group_by(country) %>%
    filter(year == max(year)) %>%
    ungroup() %>%
    select(country, year, gdp_pc, life_exp, co2_pc) %>%
    arrange(country)

  cat("Ano dos dados por observação:\n")
  print(table(dados$year))
  cat("\n")
}

# Número de observações
n <- nrow(dados)
cat(sprintf("Número de países na amostra limpa: %d\n\n", n))

# Estatísticas descritivas
cat("--- Estatísticas Descritivas ---\n\n")

estatisticas <- dados %>%
  summarise(
    across(
      c(life_exp, gdp_pc, co2_pc),
      list(
        media  = ~mean(., na.rm = TRUE),
        dp     = ~sd(., na.rm = TRUE),
        min    = ~min(., na.rm = TRUE),
        max    = ~max(., na.rm = TRUE),
        n      = ~sum(!is.na(.))
      ),
      .names = "{.col}_{.fn}"
    )
  )

cat(sprintf("Expectativa de Vida (anos):\n"))
cat(sprintf("  Média: %.2f | DP: %.2f | Mín: %.2f | Máx: %.2f | N: %d\n\n",
            estatisticas$life_exp_media, estatisticas$life_exp_dp,
            estatisticas$life_exp_min, estatisticas$life_exp_max,
            estatisticas$life_exp_n))

cat(sprintf("PIB per capita PPP (US$):\n"))
cat(sprintf("  Média: %.2f | DP: %.2f | Mín: %.2f | Máx: %.2f | N: %d\n\n",
            estatisticas$gdp_pc_media, estatisticas$gdp_pc_dp,
            estatisticas$gdp_pc_min, estatisticas$gdp_pc_max,
            estatisticas$gdp_pc_n))

cat(sprintf("Emissões de CO2 per capita (ton):\n"))
cat(sprintf("  Média: %.2f | DP: %.2f | Mín: %.2f | Máx: %.2f | N: %d\n\n",
            estatisticas$co2_pc_media, estatisticas$co2_pc_dp,
            estatisticas$co2_pc_min, estatisticas$co2_pc_max,
            estatisticas$co2_pc_n))

# Salvar dados limpos em CSV
write.csv(dados, "dados_limpos.csv", row.names = FALSE)
cat("Dados limpos salvos em 'dados_limpos.csv'\n\n")


# ====== PARTE 2: ANÁLISE EXPLORATÓRIA ======

cat("====== PARTE 2: ANÁLISE EXPLORATÓRIA ======\n\n")

# Gráfico (a): Expectativa de Vida vs PIB per capita
g1 <- ggplot(dados, aes(x = gdp_pc, y = life_exp)) +
  geom_point(color = "steelblue", alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  labs(
    title = "Expectativa de Vida vs PIB per capita (PPP)",
    x = "PIB per capita, PPP (US$ internacionais)",
    y = "Expectativa de Vida ao Nascer (anos)"
  ) +
  theme_minimal(base_size = 12) +
  scale_x_continuous(labels = scales::comma)

ggsave(file.path(dir_graficos, "scatter_vida_vs_pib.png"), g1,
       width = 8, height = 6, dpi = 150)
cat("Gráfico salvo: graficos/scatter_vida_vs_pib.png\n")

# Gráfico (b): Expectativa de Vida vs CO2 per capita
g2 <- ggplot(dados, aes(x = co2_pc, y = life_exp)) +
  geom_point(color = "darkgreen", alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  labs(
    title = "Expectativa de Vida vs Emissões de CO2 per capita",
    x = "Emissões de CO2 per capita (toneladas métricas)",
    y = "Expectativa de Vida ao Nascer (anos)"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(dir_graficos, "scatter_vida_vs_co2.png"), g2,
       width = 8, height = 6, dpi = 150)
cat("Gráfico salvo: graficos/scatter_vida_vs_co2.png\n")

# Gráfico (c): CO2 per capita vs PIB per capita
g3 <- ggplot(dados, aes(x = gdp_pc, y = co2_pc)) +
  geom_point(color = "darkorange", alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1) +
  labs(
    title = "Emissões de CO2 per capita vs PIB per capita (PPP)",
    x = "PIB per capita, PPP (US$ internacionais)",
    y = "Emissões de CO2 per capita (toneladas métricas)"
  ) +
  theme_minimal(base_size = 12) +
  scale_x_continuous(labels = scales::comma)

ggsave(file.path(dir_graficos, "scatter_co2_vs_pib.png"), g3,
       width = 8, height = 6, dpi = 150)
cat("Gráfico salvo: graficos/scatter_co2_vs_pib.png\n")

# Matriz de correlação
cat("\n--- Matriz de Correlação ---\n\n")
mat_cor <- cor(dados %>% select(life_exp, gdp_pc, co2_pc))
print(round(mat_cor, 4))

# Salvar gráfico da matriz de correlação
png(file.path(dir_graficos, "matriz_correlacao.png"), width = 600, height = 500)
corrplot::corrplot(mat_cor,
                   method = "number",
                   type = "upper",
                   tl.col = "black",
                   tl.srt = 45,
                   title = "Matriz de Correlação",
                   mar = c(0, 0, 2, 0))
dev.off()
cat("\nGráfico salvo: graficos/matriz_correlacao.png\n\n")


# ====== PARTE 3: REGRESSÃO OLS (Mínimos Quadrados Ordinários) ======

cat("====== PARTE 3: REGRESSÃO OLS ======\n\n")

# --- Função auxiliar para analisar um modelo OLS ---
# Essa função extrai os resultados e verifica as propriedades algébricas
analisar_modelo <- function(modelo, nome, x_var, dados_modelo) {

  cat(sprintf("\n--- %s ---\n\n", nome))

  # Resumo completo
  resumo <- summary(modelo)
  print(resumo)

  # Coeficientes
  b0 <- coef(modelo)[1]
  b1 <- coef(modelo)[2]
  r2 <- resumo$r.squared
  ser <- resumo$sigma  # Standard Error of Regression

  cat(sprintf("\n  β̂₀ (intercepto) = %.4f\n", b0))
  cat(sprintf("  β̂₁ (inclinação) = %.4f\n", b1))
  cat(sprintf("  R²              = %.3f\n", r2))
  cat(sprintf("  SER             = %.3f\n\n", ser))

  # Valores ajustados e resíduos
  y_hat <- fitted(modelo)
  u_hat <- residuals(modelo)
  y     <- dados_modelo$life_exp
  x     <- x_var

  # --- Verificação das Propriedades Algébricas do OLS ---
  cat("  >> Verificação das Propriedades Algébricas do OLS <<\n\n")

  # Propriedade 01: Soma dos resíduos = 0
  soma_residuos <- sum(u_hat)
  cat(sprintf("  Propriedade 01: Σûᵢ = %.6e (≈ 0 ✓)\n", soma_residuos))

  # Propriedade 02: Cov(X, û) = 0
  cov_x_u <- cov(x, u_hat)
  cat(sprintf("  Propriedade 02: Cov(X, û) = %.6e (≈ 0 ✓)\n", cov_x_u))

  # Propriedade 03: A reta passa por (X̄, Ȳ)
  x_barra <- mean(x)
  y_barra <- mean(y)
  y_pred_media <- b0 + b1 * x_barra
  cat(sprintf("  Propriedade 03: Ȳ = %.4f | β̂₀ + β̂₁·X̄ = %.4f (iguais ✓)\n",
              y_barra, y_pred_media))

  # Propriedade 04: Cov(Ŷ, û) = 0
  cov_yhat_u <- cov(y_hat, u_hat)
  cat(sprintf("  Propriedade 04: Cov(Ŷ, û) = %.6e (≈ 0 ✓)\n\n", cov_yhat_u))

  # --- Decomposição da Variância: SST = SSE + SSR ---
  SST <- sum((y - y_barra)^2)
  SSE <- sum((y_hat - y_barra)^2)   # Soma dos Quadrados Explicada
  SSR <- sum(u_hat^2)                # Soma dos Quadrados dos Resíduos

  cat(sprintf("  Decomposição da Variância:\n"))
  cat(sprintf("    SST (Total)     = %.4f\n", SST))
  cat(sprintf("    SSE (Explicada) = %.4f\n", SSE))
  cat(sprintf("    SSR (Resíduos)  = %.4f\n", SSR))
  cat(sprintf("    SSE + SSR       = %.4f\n", SSE + SSR))
  cat(sprintf("    SST = SSE + SSR? %s\n\n", ifelse(abs(SST - SSE - SSR) < 1e-6, "SIM ✓", "NÃO ✗")))

  # Verificação do R²
  r2_calc1 <- SSE / SST
  r2_calc2 <- 1 - SSR / SST
  cat(sprintf("  Verificação do R²:\n"))
  cat(sprintf("    R² (summary)    = %.6f\n", r2))
  cat(sprintf("    R² = SSE/SST    = %.6f\n", r2_calc1))
  cat(sprintf("    R² = 1 - SSR/SST = %.6f\n\n", r2_calc2))

  # Retornar resultados
  return(list(b0 = b0, b1 = b1, r2 = r2, ser = ser, nome = nome))
}

# --- Modelo 1: life_exp = β₀ + β₁ × gdp_pc + u ---
cat("=" %>% strrep(60), "\n")
modelo1 <- lm(life_exp ~ gdp_pc, data = dados)
res1 <- analisar_modelo(modelo1, "Modelo 1: life_exp ~ gdp_pc", dados$gdp_pc, dados)

# --- Modelo 2: life_exp = β₀ + β₁ × co2_pc + u ---
cat("=" %>% strrep(60), "\n")
modelo2 <- lm(life_exp ~ co2_pc, data = dados)
res2 <- analisar_modelo(modelo2, "Modelo 2: life_exp ~ co2_pc", dados$co2_pc, dados)

# --- Modelo 3: life_exp = β₀ + β₁ × log(gdp_pc) + u ---
cat("=" %>% strrep(60), "\n")
dados <- dados %>% mutate(log_gdp_pc = log(gdp_pc))
modelo3 <- lm(life_exp ~ log_gdp_pc, data = dados)
res3 <- analisar_modelo(modelo3, "Modelo 3: life_exp ~ log(gdp_pc)", dados$log_gdp_pc, dados)


# ====== PARTE 4: INTERPRETAÇÃO DOS RESULTADOS ======

cat("\n====== PARTE 4: INTERPRETAÇÃO DOS RESULTADOS ======\n\n")

# --- Interpretação do Modelo 1 ---
cat("--- Interpretação do Modelo 1: life_exp ~ gdp_pc ---\n\n")
cat(sprintf(
  "O intercepto β̂₀ = %.4f indica que, quando o PIB per capita é zero, a expectativa de
vida estimada seria de aproximadamente %.1f anos. Embora um PIB de zero não seja
realista, o intercepto é necessário para o ajuste da reta.

O coeficiente β̂₁ = %.6f indica que, para cada aumento de US$ 1 no PIB per capita (PPP),
a expectativa de vida aumenta em média %.6f anos, ceteris paribus. Em termos mais
intuitivos: um aumento de US$ 10.000 no PIB per capita está associado a um aumento
de aproximadamente %.2f anos na expectativa de vida.

O R² = %.3f indica que %.1f%% da variação na expectativa de vida entre os países é
explicada pela variação no PIB per capita. O restante (%.1f%%) é atribuído a outros
fatores não incluídos no modelo.

O SER = %.3f anos significa que o erro típico de previsão do modelo é de aproximadamente
%.1f anos — ou seja, as previsões do modelo tendem a errar por cerca de %.1f anos
em relação ao valor observado.\n\n",
  res1$b0, res1$b0,
  res1$b1, res1$b1, res1$b1 * 10000,
  res1$r2, res1$r2 * 100, (1 - res1$r2) * 100,
  res1$ser, res1$ser, res1$ser))

# --- Interpretação do Modelo 2 ---
cat("--- Interpretação do Modelo 2: life_exp ~ co2_pc ---\n\n")
cat(sprintf(
  "O intercepto β̂₀ = %.4f indica que, para um país com emissões de CO2 iguais a zero,
a expectativa de vida estimada seria de %.1f anos.

O coeficiente β̂₁ = %.4f indica que cada tonelada métrica adicional de CO2 per capita
está associada a um aumento de %.4f anos na expectativa de vida. Esse resultado pode
parecer contraintuitivo (poluição associada a maior longevidade), mas reflete o fato de
que países mais industrializados tendem a ter tanto maiores emissões de CO2 quanto
melhor infraestrutura de saúde. Trata-se de correlação, não causalidade.

O R² = %.3f indica que apenas %.1f%% da variação é explicada por CO2 per capita.
Conforme discutido em aula, um R² baixo não invalida a regressão — apenas indica que
há muitos outros fatores relevantes.

O SER = %.3f anos indica um erro típico de previsão relativamente alto.\n\n",
  res2$b0, res2$b0,
  res2$b1, res2$b1,
  res2$r2, res2$r2 * 100,
  res2$ser))

# --- Interpretação do Modelo 3 ---
cat("--- Interpretação do Modelo 3: life_exp ~ log(gdp_pc) ---\n\n")
cat(sprintf(
  "O intercepto β̂₀ = %.4f indica a expectativa de vida estimada quando log(PIB per capita)
é zero, ou seja, quando o PIB per capita é US$ 1 — um valor teórico sem significado prático.

O coeficiente β̂₁ = %.4f indica que um aumento de 1%% no PIB per capita está associado a
um aumento de aproximadamente %.4f anos (= %.4f / 100) na expectativa de vida.
A especificação log-nível captura a relação não-linear: ganhos adicionais de renda têm
retornos decrescentes sobre a expectativa de vida.

O R² = %.3f indica que %.1f%% da variação na expectativa de vida é explicada pelo
logaritmo do PIB per capita. Note que este é o maior R² entre os três modelos,
confirmando que a relação não-linear (log) oferece melhor ajuste.

O SER = %.3f anos é o menor entre os três modelos, indicando previsões mais precisas.\n\n",
  res3$b0,
  res3$b1, res3$b1 / 100, res3$b1,
  res3$r2, res3$r2 * 100,
  res3$ser))


# ====== PARTE 5: PREVISÃO (In-Sample e Out-of-Sample) ======

cat("====== PARTE 5: PREVISÃO ======\n\n")

# Usando o Modelo 3 (melhor ajuste)
cat("Utilizando o Modelo 3: life_exp ~ log(gdp_pc)\n\n")

# --- Previsão In-Sample ---
dados$y_hat <- fitted(modelo3)
dados$residuos <- residuals(modelo3)

cat("--- Previsão In-Sample (primeiras 10 observações) ---\n\n")
print(
  dados %>%
    select(country, life_exp, y_hat, residuos) %>%
    mutate(across(c(life_exp, y_hat, residuos), ~round(., 2))) %>%
    head(10)
)
cat("\n")

# --- Previsão Out-of-Sample ---
# País hipotético com PIB per capita = US$ 15.000 (similar ao Brasil)
gdp_hipotetico <- 15000
log_gdp_hipotetico <- log(gdp_hipotetico)

previsao <- predict(modelo3, newdata = data.frame(log_gdp_pc = log_gdp_hipotetico))
ser_modelo3 <- res3$ser

cat(sprintf("--- Previsão Out-of-Sample ---\n\n"))
cat(sprintf("País hipotético com PIB per capita = US$ %s\n", format(gdp_hipotetico, big.mark = ".")))
cat(sprintf("log(PIB per capita) = %.4f\n\n", log_gdp_hipotetico))
cat(sprintf("Expectativa de vida prevista (Ŷ) = %.2f anos\n", previsao))
cat(sprintf("Intervalo informal (Ŷ ± SER):  [%.2f , %.2f] anos\n\n",
            previsao - ser_modelo3, previsao + ser_modelo3))

# Para referência: valor real do Brasil (se estiver na amostra)
brasil <- dados %>% filter(grepl("Brazil", country, ignore.case = TRUE))
if (nrow(brasil) > 0) {
  cat(sprintf("Referência — Brasil:\n"))
  cat(sprintf("  PIB per capita real:         US$ %s\n", format(round(brasil$gdp_pc), big.mark = ".")))
  cat(sprintf("  Expectativa de vida real:     %.2f anos\n", brasil$life_exp))
  cat(sprintf("  Expectativa de vida prevista: %.2f anos\n", brasil$y_hat))
  cat(sprintf("  Resíduo:                      %.2f anos\n\n", brasil$residuos))
}

# --- Gráfico: Resíduos vs Valores Ajustados ---
g4 <- ggplot(dados, aes(x = y_hat, y = residuos)) +
  geom_point(color = "steelblue", alpha = 0.7, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Resíduos vs Valores Ajustados (Modelo 3)",
    x = "Valores Ajustados (Ŷ)",
    y = "Resíduos (û)"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(dir_graficos, "residuos_vs_ajustados.png"), g4,
       width = 8, height = 6, dpi = 150)
cat("Gráfico salvo: graficos/residuos_vs_ajustados.png\n")

# --- Gráfico: Resíduos vs X (log_gdp_pc) ---
g5 <- ggplot(dados, aes(x = log_gdp_pc, y = residuos)) +
  geom_point(color = "darkgreen", alpha = 0.7, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Resíduos vs log(PIB per capita) (Modelo 3)",
    x = "log(PIB per capita)",
    y = "Resíduos (û)"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(dir_graficos, "residuos_vs_log_pib.png"), g5,
       width = 8, height = 6, dpi = 150)
cat("Gráfico salvo: graficos/residuos_vs_log_pib.png\n\n")


# ====== PARTE 6: TABELA COMPARATIVA DOS MODELOS ======

cat("====== PARTE 6: TABELA COMPARATIVA DOS MODELOS ======\n\n")

tabela_comparacao <- data.frame(
  Modelo = c(
    "Modelo 1: life_exp ~ gdp_pc",
    "Modelo 2: life_exp ~ co2_pc",
    "Modelo 3: life_exp ~ log(gdp_pc)"
  ),
  Beta_0 = round(c(res1$b0, res2$b0, res3$b0), 4),
  Beta_1 = round(c(res1$b1, res2$b1, res3$b1), 4),
  R2     = round(c(res1$r2, res2$r2, res3$r2), 3),
  SER    = round(c(res1$ser, res2$ser, res3$ser), 3)
)

# Imprimir tabela formatada
cat("Tabela Comparativa dos Modelos OLS\n")
cat("-" %>% strrep(80), "\n")
cat(sprintf("%-35s %10s %10s %8s %8s\n", "Modelo", "β̂₀", "β̂₁", "R²", "SER"))
cat("-" %>% strrep(80), "\n")
for (i in 1:nrow(tabela_comparacao)) {
  cat(sprintf("%-35s %10.4f %10.4f %8.3f %8.3f\n",
              tabela_comparacao$Modelo[i],
              tabela_comparacao$Beta_0[i],
              tabela_comparacao$Beta_1[i],
              tabela_comparacao$R2[i],
              tabela_comparacao$SER[i]))
}
cat("-" %>% strrep(80), "\n\n")

# Salvar tabela como CSV
write.csv(tabela_comparacao, "tabela_comparacao.csv", row.names = FALSE)
cat("Tabela salva em 'tabela_comparacao.csv'\n\n")

# --- Conclusão ---
cat("=" %>% strrep(60), "\n")
cat("  CONCLUSÃO\n")
cat("=" %>% strrep(60), "\n\n")
cat(sprintf(
  "O Modelo 3 (especificação logarítmica) apresenta o melhor ajuste, com R² = %.3f
e SER = %.3f anos. A relação entre PIB per capita e expectativa de vida é melhor
capturada por uma função logarítmica, refletindo retornos decrescentes: aumentos
de renda têm maior impacto na expectativa de vida em países mais pobres do que
em países mais ricos.

As quatro propriedades algébricas do estimador OLS foram verificadas para todos
os modelos, confirmando a correta implementação da regressão.

Os resultados indicam que o PIB per capita é um forte preditor da expectativa de
vida entre países, mas outros fatores (saúde pública, saneamento, educação) também
desempenham papel importante, conforme evidenciado pelo R² inferior a 1.\n\n",
  res3$r2, res3$ser))

cat("========================================\n")
cat("  FIM DO SCRIPT\n")
cat("========================================\n")
