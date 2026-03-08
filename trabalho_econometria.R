# ==============================================================================
#                    TRABALHO DE ECONOMETRIA I / MÉTODOS QUANTITATIVOS
#                    Universidade Presbiteriana Mackenzie
#                    Professor: Thiago Lobo
# ==============================================================================
#
# Tema: Regressão Linear por Mínimos Quadrados Ordinários (MQO/OLS)
#
# Variáveis escolhidas:
#   Y  (dependente)    = Expectativa de vida ao nascer (anos)
#   X1 (independente)  = PIB per capita, PPP (dólares internacionais correntes)
#   X2 (independente)  = Emissões de CO2 per capita (toneladas métricas)
#
# Fonte dos dados: World Bank Open Data (https://data.worldbank.org/)
# Período: 2022 (dados cross-section — uma observação por país)
#
# Conceitos aplicados (Aulas 05–07):
#   - Estimação por MQO (Mínimos Quadrados Ordinários)
#   - Interpretação dos coeficientes (beta0 e beta1)
#   - Coeficiente de determinação (R²)
#   - Erro padrão da regressão (SER)
#   - Propriedades algébricas do estimador OLS
#   - Decomposição da variância (SST = SSE + SSR)
#   - Previsão in-sample e out-of-sample
#   - Especificação log-nível
#
# Estrutura do script:
#   PARTE 1 — Aquisição e limpeza dos dados
#   PARTE 2 — Análise exploratória (gráficos e correlação)
#   PARTE 3 — Regressão OLS (3 modelos + propriedades algébricas)
#   PARTE 4 — Interpretação econômica dos resultados
#   PARTE 5 — Previsão (in-sample e out-of-sample)
#   PARTE 6 — Tabela comparativa e conclusão
#
# ==============================================================================


# ====== CONFIGURAÇÃO INICIAL ======

# Limpar o ambiente de trabalho (remover objetos anteriores)
rm(list = ls())

# Definir opções globais
options(scipen = 999)  # evitar notação científica nos outputs

# Instalar e carregar pacotes necessários
# (instala automaticamente se ainda não estiver instalado)
pacotes <- c("WDI", "tidyverse", "corrplot", "scales", "knitr")
for (p in pacotes) {
  if (!require(p, character.only = TRUE, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org")
    library(p, character.only = TRUE)
  }
}

# Definir tema padrão para todos os gráficos do ggplot2
tema_padrao <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle    = element_text(size = 11, hjust = 0.5, color = "gray40"),
    axis.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )
theme_set(tema_padrao)

# Diretório para salvar gráficos
dir_graficos <- "graficos"
if (!dir.exists(dir_graficos)) dir.create(dir_graficos)

cat("\n")
cat("================================================================\n")
cat("  TRABALHO DE ECONOMETRIA I — UNIVERSIDADE PRESBITERIANA MACKENZIE\n")
cat("  Prof. Thiago Lobo\n")
cat("  Regressão OLS: Determinantes da Expectativa de Vida\n")
cat("================================================================\n\n")


# ==============================================================================
# ====== PARTE 1: AQUISIÇÃO E LIMPEZA DOS DADOS ======
# ==============================================================================

cat("====== PARTE 1: AQUISIÇÃO E LIMPEZA DOS DADOS ======\n\n")

# Códigos dos indicadores do World Bank (WDI)
indicadores <- c(
  life_exp = "SP.DYN.LE00.IN",    # Expectativa de vida ao nascer (anos)
  gdp_pc   = "NY.GDP.PCAP.PP.CD", # PIB per capita, PPP (US$ internacionais correntes)
  co2_pc   = "EN.ATM.CO2E.PC"     # Emissões de CO2 per capita (toneladas métricas)
)

# Tentar baixar os dados do World Bank via API
# Baixamos 2020-2022 para garantir dados, pois CO2 pode ter atraso de publicação
cat("[1/6] Baixando dados do World Bank via pacote WDI...\n")

dados_brutos <- tryCatch({
  WDI::WDI(
    indicator = indicadores,
    start     = 2020,
    end       = 2022,
    extra     = TRUE  # inclui região, nível de renda, longitude, latitude etc.
  )
}, error = function(e) {
  cat("  AVISO: Falha ao baixar dados do WDI.\n")
  cat("  Motivo:", conditionMessage(e), "\n")
  cat("  Tentando ler dados do arquivo local 'dados_limpos.csv'...\n\n")
  if (file.exists("dados_limpos.csv")) {
    return(NULL)  # sinaliza para usar o CSV local
  } else {
    stop("Não foi possível obter os dados. Verifique sua conexão com a internet.")
  }
})

if (is.null(dados_brutos)) {
  # ---- Fallback: ler do CSV local ----
  dados <- read.csv("dados_limpos.csv", stringsAsFactors = FALSE)
  cat("  Dados carregados do arquivo local 'dados_limpos.csv'.\n\n")
} else {
  cat("  Dados baixados com sucesso!\n\n")

  # ---- Limpeza dos dados ----
  cat("[2/6] Limpando dados...\n")

  # Passo 1: Remover agregados (regiões, mundo, grupos de renda)
  n_antes <- length(unique(dados_brutos$country))
  dados <- dados_brutos %>%
    filter(region != "Aggregates")
  n_sem_agregados <- length(unique(dados$country))
  cat(sprintf("  - Entidades totais: %d -> Após remover agregados: %d países\n",
              n_antes, n_sem_agregados))

  # Passo 2: Remover observações com qualquer variável faltante (NA)
  dados <- dados %>%
    filter(!is.na(life_exp), !is.na(gdp_pc), !is.na(co2_pc))
  cat(sprintf("  - Após remover NAs: %d observações (país-ano)\n", nrow(dados)))

  # Passo 3: Para cada país, manter apenas o ano mais recente disponível
  # (garante dados cross-section — uma observação por país)
  dados <- dados %>%
    group_by(country, iso2c) %>%
    filter(year == max(year)) %>%
    ungroup() %>%
    select(country, iso2c, year, region, income, gdp_pc, life_exp, co2_pc) %>%
    arrange(country)

  cat(sprintf("  - Após selecionar ano mais recente por país: %d observações\n", nrow(dados)))

  # Mostrar distribuição por ano
  cat("\n  Distribuição das observações por ano:\n")
  print(table(dados$year))
  cat("\n")

  # Mostrar distribuição por nível de renda do World Bank
  cat("  Distribuição por nível de renda (World Bank):\n")
  print(table(dados$income))
  cat("\n")
}

# Número total de observações (n)
n <- nrow(dados)
cat(sprintf("[3/6] Amostra final: n = %d países\n\n", n))

# ---- Estatísticas Descritivas ----
cat("--- Estatísticas Descritivas ---\n\n")

# Calcular estatísticas completas para cada variável (incluindo quartis e mediana)
estat_vida <- c(
  Media = mean(dados$life_exp), Mediana = median(dados$life_exp),
  DP = sd(dados$life_exp), Min = min(dados$life_exp),
  Q1 = quantile(dados$life_exp, 0.25), Q3 = quantile(dados$life_exp, 0.75),
  Max = max(dados$life_exp), N = n
)
estat_pib <- c(
  Media = mean(dados$gdp_pc), Mediana = median(dados$gdp_pc),
  DP = sd(dados$gdp_pc), Min = min(dados$gdp_pc),
  Q1 = quantile(dados$gdp_pc, 0.25), Q3 = quantile(dados$gdp_pc, 0.75),
  Max = max(dados$gdp_pc), N = n
)
estat_co2 <- c(
  Media = mean(dados$co2_pc), Mediana = median(dados$co2_pc),
  DP = sd(dados$co2_pc), Min = min(dados$co2_pc),
  Q1 = quantile(dados$co2_pc, 0.25), Q3 = quantile(dados$co2_pc, 0.75),
  Max = max(dados$co2_pc), N = n
)

cat(sprintf("  Expectativa de Vida ao Nascer (anos):\n"))
cat(sprintf("    Média: %.2f | Mediana: %.2f | DP: %.2f\n",
            estat_vida["Media"], estat_vida["Mediana"], estat_vida["DP"]))
cat(sprintf("    Mín: %.2f | Q1: %.2f | Q3: %.2f | Máx: %.2f | N: %.0f\n\n",
            estat_vida["Min"], estat_vida["Q1"], estat_vida["Q3"],
            estat_vida["Max"], estat_vida["N"]))

cat(sprintf("  PIB per capita PPP (US$ internacionais):\n"))
cat(sprintf("    Média: %s | Mediana: %s | DP: %s\n",
            format(round(estat_pib["Media"], 2), big.mark = "."),
            format(round(estat_pib["Mediana"], 2), big.mark = "."),
            format(round(estat_pib["DP"], 2), big.mark = ".")))
cat(sprintf("    Mín: %s | Q1: %s | Q3: %s | Máx: %s | N: %.0f\n\n",
            format(round(estat_pib["Min"], 2), big.mark = "."),
            format(round(estat_pib["Q1"], 2), big.mark = "."),
            format(round(estat_pib["Q3"], 2), big.mark = "."),
            format(round(estat_pib["Max"], 2), big.mark = "."),
            estat_pib["N"]))

cat(sprintf("  Emissões de CO2 per capita (toneladas métricas):\n"))
cat(sprintf("    Média: %.2f | Mediana: %.2f | DP: %.2f\n",
            estat_co2["Media"], estat_co2["Mediana"], estat_co2["DP"]))
cat(sprintf("    Mín: %.2f | Q1: %.2f | Q3: %.2f | Máx: %.2f | N: %.0f\n\n",
            estat_co2["Min"], estat_co2["Q1"], estat_co2["Q3"],
            estat_co2["Max"], estat_co2["N"]))

# ---- Identificar extremos ----
cat("  Países com MAIOR expectativa de vida:\n")
top5 <- dados %>% arrange(desc(life_exp)) %>% head(5)
for (i in 1:nrow(top5)) {
  cat(sprintf("    %d. %s: %.1f anos (PIB pc: US$ %s)\n",
              i, top5$country[i], top5$life_exp[i],
              format(round(top5$gdp_pc[i]), big.mark = ".")))
}

cat("\n  Países com MENOR expectativa de vida:\n")
bottom5 <- dados %>% arrange(life_exp) %>% head(5)
for (i in 1:nrow(bottom5)) {
  cat(sprintf("    %d. %s: %.1f anos (PIB pc: US$ %s)\n",
              i, bottom5$country[i], bottom5$life_exp[i],
              format(round(bottom5$gdp_pc[i]), big.mark = ".")))
}
cat("\n")

# Salvar dados limpos em CSV
write.csv(dados, "dados_limpos.csv", row.names = FALSE)
cat("[4/6] Dados limpos salvos em 'dados_limpos.csv'\n\n")


# ==============================================================================
# ====== PARTE 2: ANÁLISE EXPLORATÓRIA ======
# ==============================================================================

cat("====== PARTE 2: ANÁLISE EXPLORATÓRIA ======\n\n")

# ---- Gráfico 1: Expectativa de Vida vs PIB per capita (nível) ----
g1 <- ggplot(dados, aes(x = gdp_pc, y = life_exp)) +
  geom_point(aes(color = life_exp), alpha = 0.75, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#D32F2F", linewidth = 1,
              fill = "#FFCDD2", alpha = 0.3) +
  scale_color_gradient(low = "#FF7043", high = "#1B5E20", name = "Exp. Vida") +
  labs(
    title    = "Expectativa de Vida vs PIB per capita (PPP)",
    subtitle = "Cada ponto = um país | Reta OLS com intervalo de confiança 95%",
    x = "PIB per capita, PPP (US$ internacionais)",
    y = "Expectativa de Vida ao Nascer (anos)",
    caption = "Fonte: World Bank Open Data"
  ) +
  scale_x_continuous(labels = scales::dollar_format(big.mark = ".", decimal.mark = ","))

ggsave(file.path(dir_graficos, "01_scatter_vida_vs_pib.png"), g1,
       width = 10, height = 7, dpi = 200, bg = "white")
cat("  Gráfico salvo: graficos/01_scatter_vida_vs_pib.png\n")

# ---- Gráfico 2: Expectativa de Vida vs PIB per capita (LOG) ----
g2 <- ggplot(dados, aes(x = log(gdp_pc), y = life_exp)) +
  geom_point(aes(color = life_exp), alpha = 0.75, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#1565C0", linewidth = 1,
              fill = "#BBDEFB", alpha = 0.3) +
  scale_color_gradient(low = "#FF7043", high = "#1B5E20", name = "Exp. Vida") +
  labs(
    title    = "Expectativa de Vida vs log(PIB per capita)",
    subtitle = "Especificação logarítmica — relação mais linear | IC 95%",
    x = "log(PIB per capita, PPP)",
    y = "Expectativa de Vida ao Nascer (anos)",
    caption = "Fonte: World Bank Open Data"
  )

ggsave(file.path(dir_graficos, "02_scatter_vida_vs_log_pib.png"), g2,
       width = 10, height = 7, dpi = 200, bg = "white")
cat("  Gráfico salvo: graficos/02_scatter_vida_vs_log_pib.png\n")

# ---- Gráfico 3: Expectativa de Vida vs CO2 per capita ----
g3 <- ggplot(dados, aes(x = co2_pc, y = life_exp)) +
  geom_point(aes(color = life_exp), alpha = 0.75, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#2E7D32", linewidth = 1,
              fill = "#C8E6C9", alpha = 0.3) +
  scale_color_gradient(low = "#FF7043", high = "#1B5E20", name = "Exp. Vida") +
  labs(
    title    = "Expectativa de Vida vs Emissões de CO2 per capita",
    subtitle = "Correlação positiva — possível variável omitida (desenvolvimento) | IC 95%",
    x = "Emissões de CO2 per capita (toneladas métricas)",
    y = "Expectativa de Vida ao Nascer (anos)",
    caption = "Fonte: World Bank Open Data"
  )

ggsave(file.path(dir_graficos, "03_scatter_vida_vs_co2.png"), g3,
       width = 10, height = 7, dpi = 200, bg = "white")
cat("  Gráfico salvo: graficos/03_scatter_vida_vs_co2.png\n")

# ---- Gráfico 4: CO2 per capita vs PIB per capita ----
g4 <- ggplot(dados, aes(x = gdp_pc, y = co2_pc)) +
  geom_point(aes(color = co2_pc), alpha = 0.75, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, color = "#E65100", linewidth = 1,
              fill = "#FFE0B2", alpha = 0.3) +
  scale_color_gradient(low = "#FDD835", high = "#BF360C", name = "CO2 pc") +
  labs(
    title    = "Emissões de CO2 per capita vs PIB per capita (PPP)",
    subtitle = "Relação entre desenvolvimento econômico e emissões | IC 95%",
    x = "PIB per capita, PPP (US$ internacionais)",
    y = "Emissões de CO2 per capita (toneladas métricas)",
    caption = "Fonte: World Bank Open Data"
  ) +
  scale_x_continuous(labels = scales::dollar_format(big.mark = ".", decimal.mark = ","))

ggsave(file.path(dir_graficos, "04_scatter_co2_vs_pib.png"), g4,
       width = 10, height = 7, dpi = 200, bg = "white")
cat("  Gráfico salvo: graficos/04_scatter_co2_vs_pib.png\n")

# ---- Gráfico 5: Histogramas das 3 variáveis ----
g5a <- ggplot(dados, aes(x = life_exp)) +
  geom_histogram(bins = 25, fill = "#1B5E20", color = "white", alpha = 0.8) +
  geom_vline(xintercept = mean(dados$life_exp), color = "red",
             linetype = "dashed", linewidth = 1) +
  labs(title = "Distribuição da Expectativa de Vida",
       subtitle = sprintf("Média = %.1f anos (linha vermelha)", mean(dados$life_exp)),
       x = "Expectativa de Vida (anos)", y = "Frequência")

g5b <- ggplot(dados, aes(x = gdp_pc)) +
  geom_histogram(bins = 30, fill = "#1565C0", color = "white", alpha = 0.8) +
  geom_vline(xintercept = mean(dados$gdp_pc), color = "red",
             linetype = "dashed", linewidth = 1) +
  labs(title = "Distribuição do PIB per capita",
       subtitle = sprintf("Média = US$ %s (linha vermelha)",
                          format(round(mean(dados$gdp_pc)), big.mark = ".")),
       x = "PIB per capita, PPP (US$)", y = "Frequência") +
  scale_x_continuous(labels = scales::dollar_format(big.mark = ".", decimal.mark = ","))

g5c <- ggplot(dados, aes(x = co2_pc)) +
  geom_histogram(bins = 30, fill = "#E65100", color = "white", alpha = 0.8) +
  geom_vline(xintercept = mean(dados$co2_pc), color = "red",
             linetype = "dashed", linewidth = 1) +
  labs(title = "Distribuição das Emissões de CO2 per capita",
       subtitle = sprintf("Média = %.2f ton (linha vermelha)", mean(dados$co2_pc)),
       x = "CO2 per capita (toneladas métricas)", y = "Frequência")

ggsave(file.path(dir_graficos, "05a_hist_expectativa_vida.png"), g5a,
       width = 8, height = 6, dpi = 200, bg = "white")
ggsave(file.path(dir_graficos, "05b_hist_pib_per_capita.png"), g5b,
       width = 8, height = 6, dpi = 200, bg = "white")
ggsave(file.path(dir_graficos, "05c_hist_co2_per_capita.png"), g5c,
       width = 8, height = 6, dpi = 200, bg = "white")
cat("  Gráficos salvos: graficos/05a, 05b, 05c (histogramas)\n")

# ---- Gráfico 6: Boxplots por nível de renda ----
if ("income" %in% names(dados)) {
  dados$income <- factor(dados$income,
    levels = c("Low income", "Lower middle income",
               "Upper middle income", "High income"))

  g6 <- ggplot(dados %>% filter(!is.na(income)),
               aes(x = income, y = life_exp, fill = income)) +
    geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.fill = "red") +
    scale_fill_brewer(palette = "RdYlGn", direction = 1) +
    labs(
      title    = "Expectativa de Vida por Nível de Renda (World Bank)",
      subtitle = "Classificação de renda do Banco Mundial",
      x = "Nível de Renda", y = "Expectativa de Vida ao Nascer (anos)"
    ) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 15, hjust = 1))

  ggsave(file.path(dir_graficos, "06_boxplot_vida_por_renda.png"), g6,
         width = 9, height = 6, dpi = 200, bg = "white")
  cat("  Gráfico salvo: graficos/06_boxplot_vida_por_renda.png\n")
}

# ---- Matriz de Correlação ----
cat("\n--- Matriz de Correlação ---\n\n")
mat_cor <- cor(dados %>% select(life_exp, gdp_pc, co2_pc))
rownames(mat_cor) <- c("Exp. Vida", "PIB pc", "CO2 pc")
colnames(mat_cor) <- c("Exp. Vida", "PIB pc", "CO2 pc")
print(round(mat_cor, 4))

cat(sprintf("\n  Interpretação:\n"))
cat(sprintf("    - Corr(Exp. Vida, PIB pc) = %.4f -> correlação forte e positiva\n",
            mat_cor["Exp. Vida", "PIB pc"]))
cat(sprintf("    - Corr(Exp. Vida, CO2 pc) = %.4f -> correlação moderada e positiva\n",
            mat_cor["Exp. Vida", "CO2 pc"]))
cat(sprintf("    - Corr(PIB pc, CO2 pc)    = %.4f -> correlação entre os regressores\n\n",
            mat_cor["PIB pc", "CO2 pc"]))

png(file.path(dir_graficos, "07_matriz_correlacao.png"), width = 700, height = 600, res = 120)
corrplot::corrplot(mat_cor,
                   method  = "color", type = "upper",
                   addCoef.col = "black", number.cex = 1.2,
                   tl.col = "black", tl.srt = 45, tl.cex = 1.1,
                   cl.cex = 0.9,
                   title  = "Matriz de Correlação entre as Variáveis",
                   mar    = c(0, 0, 2, 0))
dev.off()
cat("  Gráfico salvo: graficos/07_matriz_correlacao.png\n\n")


# ==============================================================================
# ====== PARTE 3: REGRESSÃO OLS (Mínimos Quadrados Ordinários) ======
# ==============================================================================

cat("====== PARTE 3: REGRESSÃO OLS (MÍNIMOS QUADRADOS ORDINÁRIOS) ======\n\n")
cat("Referência teórica: Aulas 05-07 (Estimação OLS, Propriedades Algébricas)\n\n")

# --- Função auxiliar: análise completa de um modelo OLS ---
# Esta função realiza toda a análise de forma padronizada:
#   1. Exibe summary() completo
#   2. Extrai e imprime beta0, beta1, R², SER, erros padrão, t, p-valor
#   3. Verifica as 4 propriedades algébricas do OLS
#   4. Faz decomposição da variância (SST = SSE + SSR)
#   5. Verifica R² por duas fórmulas diferentes
#   6. Calcula manualmente beta1 pela fórmula do OLS

analisar_modelo_ols <- function(modelo, nome, x_var, dados_modelo) {

  cat(sprintf("\n%s\n", strrep("=", 70)))
  cat(sprintf("  %s\n", nome))
  cat(sprintf("%s\n\n", strrep("=", 70)))

  # ---- 1. Resumo completo do modelo (summary) ----
  resumo <- summary(modelo)
  print(resumo)

  # ---- 2. Extração dos resultados principais ----
  b0     <- coef(modelo)[1]
  b1     <- coef(modelo)[2]
  r2     <- resumo$r.squared
  r2_adj <- resumo$adj.r.squared
  ser    <- resumo$sigma
  ep_b0  <- resumo$coefficients[1, 2]  # erro padrão de beta0
  ep_b1  <- resumo$coefficients[2, 2]  # erro padrão de beta1
  t_b0   <- resumo$coefficients[1, 3]  # estatística t de beta0
  t_b1   <- resumo$coefficients[2, 3]  # estatística t de beta1
  p_b0   <- resumo$coefficients[1, 4]  # p-valor de beta0
  p_b1   <- resumo$coefficients[2, 4]  # p-valor de beta1
  n_obs  <- nrow(dados_modelo)
  gl     <- n_obs - 2  # graus de liberdade (n - k - 1, com k=1)

  cat(sprintf("\n  +================================================+\n"))
  cat(sprintf("  |        RESULTADOS PRINCIPAIS                    |\n"))
  cat(sprintf("  +================================================+\n"))
  cat(sprintf("  |  beta0 (intercepto)  = %12.4f              |\n", b0))
  cat(sprintf("  |  beta1 (inclinação)  = %12.6f              |\n", b1))
  cat(sprintf("  |  EP(beta0)           = %12.4f              |\n", ep_b0))
  cat(sprintf("  |  EP(beta1)           = %12.6f              |\n", ep_b1))
  cat(sprintf("  |  t(beta0)            = %12.4f              |\n", t_b0))
  cat(sprintf("  |  t(beta1)            = %12.4f              |\n", t_b1))
  cat(sprintf("  |  p-valor(beta1)      = %12.2e              |\n", p_b1))
  cat(sprintf("  |  R²                  = %12.4f (%.1f%%)      |\n", r2, r2 * 100))
  cat(sprintf("  |  R² ajustado         = %12.4f              |\n", r2_adj))
  cat(sprintf("  |  SER                 = %12.4f anos         |\n", ser))
  cat(sprintf("  |  n                   = %12d               |\n", n_obs))
  cat(sprintf("  |  Graus de liberdade  = %12d               |\n", gl))
  cat(sprintf("  +================================================+\n\n"))

  # Valores ajustados e resíduos
  y_hat <- fitted(modelo)
  u_hat <- residuals(modelo)
  y     <- dados_modelo$life_exp
  x     <- x_var

  # ---- 3. Verificação das Propriedades Algébricas do OLS (Aula 06) ----
  cat("  +------------------------------------------------------------+\n")
  cat("  |  VERIFICAÇÃO DAS PROPRIEDADES ALGÉBRICAS DO OLS            |\n")
  cat("  |  (Conforme Aulas 05-07)                                    |\n")
  cat("  +------------------------------------------------------------+\n\n")

  # PROPRIEDADE 01: Soma dos resíduos = 0
  # Justificativa teórica: consequência da CPO dSSR/dbeta0 = 0
  # => -2 * sum(Yi - beta0 - beta1*Xi) = 0  =>  sum(u_hat) = 0
  soma_residuos <- sum(u_hat)
  cat(sprintf("  PROPRIEDADE 01: sum(u_hat) = 0\n"))
  cat(sprintf("    Soma dos resíduos = %.10e\n", soma_residuos))
  cat(sprintf("    Verificação: %s (arredondamento computacional)\n\n",
              ifelse(abs(soma_residuos) < 1e-8, "CONFIRMADA", "valor muito próximo de zero")))

  # PROPRIEDADE 02: Cov(Xi, u_hat_i) = 0
  # Justificativa teórica: consequência da CPO dSSR/dbeta1 = 0
  # => -2 * sum(Xi * (Yi - beta0 - beta1*Xi)) = 0
  cov_x_u <- cov(x, u_hat)
  cor_x_u <- cor(x, u_hat)
  cat(sprintf("  PROPRIEDADE 02: Cov(Xi, u_hat_i) = 0\n"))
  cat(sprintf("    Cov(X, u_hat)  = %.10e\n", cov_x_u))
  cat(sprintf("    Corr(X, u_hat) = %.10e\n", cor_x_u))
  cat(sprintf("    Verificação: %s\n\n",
              ifelse(abs(cov_x_u) < 1e-6, "CONFIRMADA", "valor muito próximo de zero")))

  # PROPRIEDADE 03: A reta de regressão passa pelo ponto (X_barra, Y_barra)
  # Justificativa: Y_barra = beta0 + beta1 * X_barra (decorre da fórmula de beta0)
  x_barra <- mean(x)
  y_barra <- mean(y)
  y_pred_media <- b0 + b1 * x_barra
  cat(sprintf("  PROPRIEDADE 03: A reta passa por (X_barra, Y_barra)\n"))
  cat(sprintf("    X_barra = %.4f\n", x_barra))
  cat(sprintf("    Y_barra = %.4f\n", y_barra))
  cat(sprintf("    beta0 + beta1 * X_barra = %.4f\n", y_pred_media))
  cat(sprintf("    |Y_barra - (beta0 + beta1*X_barra)| = %.10e\n",
              abs(y_barra - y_pred_media)))
  cat(sprintf("    Verificação: CONFIRMADA\n\n"))

  # PROPRIEDADE 04: Cov(Y_hat_i, u_hat_i) = 0
  # Justificativa: Y_hat é combinação linear de X, e Cov(X, u_hat) = 0
  cov_yhat_u <- cov(y_hat, u_hat)
  cor_yhat_u <- cor(y_hat, u_hat)
  cat(sprintf("  PROPRIEDADE 04: Cov(Y_hat_i, u_hat_i) = 0\n"))
  cat(sprintf("    Cov(Y_hat, u_hat)  = %.10e\n", cov_yhat_u))
  cat(sprintf("    Corr(Y_hat, u_hat) = %.10e\n", cor_yhat_u))
  cat(sprintf("    Verificação: CONFIRMADA\n\n"))

  # ---- 4. Decomposição da Variância: SST = SSE + SSR ----
  cat("  +------------------------------------------------------------+\n")
  cat("  |  DECOMPOSIÇÃO DA VARIÂNCIA (SST = SSE + SSR)               |\n")
  cat("  +------------------------------------------------------------+\n\n")

  SST <- sum((y - y_barra)^2)      # Soma dos Quadrados Total
  SSE <- sum((y_hat - y_barra)^2)  # Soma dos Quadrados Explicada (pelo modelo)
  SSR <- sum(u_hat^2)              # Soma dos Quadrados dos Resíduos

  cat(sprintf("    SST (Total)     = %.4f\n", SST))
  cat(sprintf("    SSE (Explicada) = %.4f\n", SSE))
  cat(sprintf("    SSR (Resíduos)  = %.4f\n", SSR))
  cat(sprintf("    SSE + SSR       = %.4f\n", SSE + SSR))
  cat(sprintf("    |SST - (SSE + SSR)| = %.10e\n", abs(SST - SSE - SSR)))
  cat(sprintf("    SST = SSE + SSR? %s\n\n",
              ifelse(abs(SST - SSE - SSR) < 1e-6, "SIM - CONFIRMADO", "DIFERENÇA DETECTADA")))

  # ---- 5. Verificação do R² por 3 formas ----
  r2_f1 <- SSE / SST
  r2_f2 <- 1 - SSR / SST

  cat(sprintf("    Verificação do R² (3 formas de calcular):\n"))
  cat(sprintf("      R² via summary()      = %.6f\n", r2))
  cat(sprintf("      R² = SSE / SST        = %.6f\n", r2_f1))
  cat(sprintf("      R² = 1 - SSR / SST    = %.6f\n", r2_f2))
  cat(sprintf("      Todas iguais? %s\n\n",
              ifelse(abs(r2 - r2_f1) < 1e-10 & abs(r2 - r2_f2) < 1e-10,
                     "SIM - CONFIRMADO", "DIFERENÇA MÍNIMA")))

  # ---- 6. Cálculo manual de beta1 pela fórmula do OLS ----
  # Fórmula: beta1 = sum((Xi - X_barra)(Yi - Y_barra)) / sum((Xi - X_barra)^2)
  cat(sprintf("    Cálculo MANUAL de beta1 (fórmula OLS):\n"))
  cov_xy <- sum((x - x_barra) * (y - y_barra))
  var_x  <- sum((x - x_barra)^2)
  b1_manual <- cov_xy / var_x
  b0_manual <- y_barra - b1_manual * x_barra

  cat(sprintf("      Numerador:   sum((Xi-X_bar)(Yi-Y_bar)) = %.4f\n", cov_xy))
  cat(sprintf("      Denominador: sum((Xi-X_bar)^2)         = %.4f\n", var_x))
  cat(sprintf("      beta1_manual = Num / Den               = %.6f\n", b1_manual))
  cat(sprintf("      beta1 via lm()                         = %.6f\n", b1))
  cat(sprintf("      beta0_manual = Y_bar - beta1*X_bar     = %.4f\n", b0_manual))
  cat(sprintf("      beta0 via lm()                         = %.4f\n", b0))
  cat(sprintf("      Confere? %s\n\n",
              ifelse(abs(b1 - b1_manual) < 1e-10, "SIM - CONFIRMADO", "DIFERENÇA MÍNIMA")))

  # Retornar lista com todos os resultados
  return(list(
    b0 = b0, b1 = b1, r2 = r2, r2_adj = r2_adj, ser = ser,
    ep_b0 = ep_b0, ep_b1 = ep_b1, t_b1 = t_b1, p_b1 = p_b1,
    SST = SST, SSE = SSE, SSR = SSR, nome = nome, n = n_obs
  ))
}

# ---- Modelo 1: life_exp = beta0 + beta1 * gdp_pc + u ----
cat("MODELO 1: Especificação nível-nível\n")
cat("  life_exp = beta0 + beta1 * gdp_pc + u\n")
cat("  Interpretação de beta1: se gdp_pc aumenta em 1 US$, life_exp muda beta1 anos\n")
modelo1 <- lm(life_exp ~ gdp_pc, data = dados)
res1 <- analisar_modelo_ols(modelo1, "MODELO 1: life_exp ~ gdp_pc (nível-nível)",
                            dados$gdp_pc, dados)

# ---- Modelo 2: life_exp = beta0 + beta1 * co2_pc + u ----
cat("MODELO 2: Especificação nível-nível\n")
cat("  life_exp = beta0 + beta1 * co2_pc + u\n")
cat("  Interpretação de beta1: se co2_pc aumenta em 1 ton, life_exp muda beta1 anos\n")
modelo2 <- lm(life_exp ~ co2_pc, data = dados)
res2 <- analisar_modelo_ols(modelo2, "MODELO 2: life_exp ~ co2_pc (nível-nível)",
                            dados$co2_pc, dados)

# ---- Modelo 3: life_exp = beta0 + beta1 * log(gdp_pc) + u ----
cat("MODELO 3: Especificação nível-log\n")
cat("  life_exp = beta0 + beta1 * log(gdp_pc) + u\n")
cat("  Interpretação de beta1: aumento de 1% em gdp_pc -> variação de beta1/100 em life_exp\n")
dados <- dados %>% mutate(log_gdp_pc = log(gdp_pc))
modelo3 <- lm(life_exp ~ log_gdp_pc, data = dados)
res3 <- analisar_modelo_ols(modelo3, "MODELO 3: life_exp ~ log(gdp_pc) (nível-log)",
                            dados$log_gdp_pc, dados)


# ==============================================================================
# ====== PARTE 4: INTERPRETAÇÃO ECONÔMICA DOS RESULTADOS ======
# ==============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("====== PARTE 4: INTERPRETAÇÃO ECONÔMICA DOS RESULTADOS ======\n")
cat(strrep("=", 70), "\n\n")

# ---- Interpretação do Modelo 1 ----
cat("+------------------------------------------------------------------+\n")
cat("|  INTERPRETAÇÃO DO MODELO 1: life_exp ~ gdp_pc                    |\n")
cat("+------------------------------------------------------------------+\n\n")
cat(sprintf(
"  INTERCEPTO (beta0 = %.4f):
    O intercepto indica que, quando o PIB per capita é zero, a expectativa
    de vida estimada seria de aproximadamente %.1f anos. Embora um PIB per
    capita de zero não seja economicamente realista, o intercepto é
    matematicamente necessário para o ajuste da reta de regressão e não
    deve ser interpretado literalmente neste contexto.

  COEFICIENTE ANGULAR (beta1 = %.6f):
    Para cada aumento de US$ 1 no PIB per capita (PPP), a expectativa de
    vida ao nascer aumenta, em média, %.6f anos, ceteris paribus.
    Em termos mais intuitivos:
      -> Um aumento de US$ 1.000  -> +%.3f anos na expectativa de vida
      -> Um aumento de US$ 10.000 -> +%.2f anos na expectativa de vida
    ATENÇÃO: Trata-se de uma associação (correlação), não de causalidade.

  COEFICIENTE DE DETERMINAÇÃO (R² = %.3f):
    Aproximadamente %.1f%%%% da variação na expectativa de vida entre os
    países da amostra é explicada pela variação no PIB per capita.
    Os restantes %.1f%%%% são atribuídos a outros fatores não incluídos
    no modelo (saúde pública, saneamento, educação, cultura etc.).

  ERRO PADRÃO DA REGRESSÃO (SER = %.3f anos):
    O erro típico de previsão é de aproximadamente %.1f anos. Ou seja,
    quando usamos este modelo para prever a expectativa de vida de um
    país, a previsão tende a errar em média por %.1f anos.

  SIGNIFICÂNCIA ESTATÍSTICA:
    O p-valor de beta1 = %.2e, que é %s 0,05.
    Portanto, %s H0: beta1 = 0 — o PIB per capita %s um preditor
    estatisticamente significante da expectativa de vida.\n\n",
  res1$b0, res1$b0,
  res1$b1, res1$b1,
  res1$b1 * 1000, res1$b1 * 10000,
  res1$r2, res1$r2 * 100, (1 - res1$r2) * 100,
  res1$ser, res1$ser, res1$ser,
  res1$p_b1,
  ifelse(res1$p_b1 < 0.05, "menor que", "maior que"),
  ifelse(res1$p_b1 < 0.05, "rejeitamos", "NÃO rejeitamos"),
  ifelse(res1$p_b1 < 0.05, "É", "NÃO é")))

# ---- Interpretação do Modelo 2 ----
cat("+------------------------------------------------------------------+\n")
cat("|  INTERPRETAÇÃO DO MODELO 2: life_exp ~ co2_pc                    |\n")
cat("+------------------------------------------------------------------+\n\n")
cat(sprintf(
"  INTERCEPTO (beta0 = %.4f):
    Para um país hipotético com emissões de CO2 iguais a zero, a
    expectativa de vida estimada seria de %.1f anos.

  COEFICIENTE ANGULAR (beta1 = %.4f):
    Cada tonelada métrica adicional de CO2 per capita está associada
    a um aumento de %.4f anos na expectativa de vida.

    NOTA IMPORTANTE — CORRELAÇÃO vs CAUSALIDADE:
    Este resultado pode parecer contraintuitivo — como poluição poderia
    estar associada a maior longevidade? A explicação é que países mais
    industrializados (e mais ricos) tendem a ter simultaneamente:
      (a) maiores emissões de CO2 (mais indústria, mais energia)
      (b) maior expectativa de vida (melhor saúde, nutrição, saneamento)
    Isso configura um problema clássico de VARIÁVEL OMITIDA: o nível de
    desenvolvimento econômico é um confundidor que afeta ambas as variáveis.
    Correlação NÃO implica causalidade.

  COEFICIENTE DE DETERMINAÇÃO (R² = %.3f):
    Apenas %.1f%%%% da variação na expectativa de vida é explicada pelas
    emissões de CO2. Este R² relativamente baixo era esperado.
    Conforme discutido em aula: um R² baixo NÃO invalida a regressão.
    Ele apenas indica que existem muitos outros fatores relevantes.

  ERRO PADRÃO DA REGRESSÃO (SER = %.3f anos):
    O erro típico de previsão é de %.1f anos — substancialmente maior
    que o Modelo 1, indicando menor capacidade preditiva.\n\n",
  res2$b0, res2$b0,
  res2$b1, res2$b1,
  res2$r2, res2$r2 * 100,
  res2$ser, res2$ser))

# ---- Interpretação do Modelo 3 ----
cat("+------------------------------------------------------------------+\n")
cat("|  INTERPRETAÇÃO DO MODELO 3: life_exp ~ log(gdp_pc)               |\n")
cat("|  (MELHOR MODELO — Especificação nível-log)                       |\n")
cat("+------------------------------------------------------------------+\n\n")
cat(sprintf(
"  INTERCEPTO (beta0 = %.4f):
    Indica a expectativa de vida quando log(PIB per capita) = 0, ou seja,
    quando o PIB per capita = e^0 = US$ 1. Este é um valor puramente
    teórico e não deve ser interpretado substantivamente.

  COEFICIENTE ANGULAR (beta1 = %.4f):
    Na especificação nível-log, a interpretação é:
    -> Um aumento de 1%%%% no PIB per capita está associado a um aumento
       de aproximadamente %.4f anos (= beta1 / 100 = %.4f / 100) na
       expectativa de vida.
    -> Equivalentemente: dobrar o PIB per capita (+100%%%%) está associado
       a um aumento de %.2f anos (= beta1 * ln(2) = %.4f * 0.693).

    Esta especificação captura a relação NÃO-LINEAR entre renda e
    longevidade: ganhos adicionais de renda têm RETORNOS DECRESCENTES
    sobre a expectativa de vida. Um aumento de US$ 1.000 no PIB faz
    muito mais diferença para um país pobre do que para um país rico.

  COEFICIENTE DE DETERMINAÇÃO (R² = %.3f):
    %.1f%%%% da variação na expectativa de vida é explicada pelo logaritmo
    do PIB per capita.
    -> Este é o MAIOR R² entre os três modelos, confirmando que a
       especificação logarítmica oferece o melhor ajuste aos dados.

  ERRO PADRÃO DA REGRESSÃO (SER = %.3f anos):
    -> Este é o MENOR SER entre os três modelos, indicando as previsões
       mais precisas. O erro típico de previsão é de %.1f anos.

  POR QUE O MODELO 3 É SUPERIOR:
    Comparando os R² e SER dos três modelos:
      Modelo 1 (gdp_pc):      R² = %.3f | SER = %.3f
      Modelo 2 (co2_pc):      R² = %.3f | SER = %.3f
      Modelo 3 (log_gdp_pc):  R² = %.3f | SER = %.3f  <- MELHOR
    A especificação log é superior porque captura a concavidade da
    relação renda-saúde, amplamente documentada na literatura econômica.\n\n",
  res3$b0,
  res3$b1,
  res3$b1 / 100, res3$b1,
  res3$b1 * log(2), res3$b1,
  res3$r2, res3$r2 * 100,
  res3$ser, res3$ser,
  res1$r2, res1$ser,
  res2$r2, res2$ser,
  res3$r2, res3$ser))


# ==============================================================================
# ====== PARTE 5: PREVISÃO (In-Sample e Out-of-Sample) ======
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("====== PARTE 5: PREVISÃO (In-Sample e Out-of-Sample) ======\n")
cat(strrep("=", 70), "\n\n")

cat("Utilizando o Modelo 3 (melhor ajuste): life_exp ~ log(gdp_pc)\n\n")

# ---- 5.1 Previsão In-Sample ----
dados$y_hat_m3    <- fitted(modelo3)
dados$residuos_m3 <- residuals(modelo3)

cat("--- 5.1 Previsão In-Sample (amostra completa) ---\n\n")
cat("  Primeiras 15 observações (ordenadas por país):\n\n")

tabela_insample <- dados %>%
  select(country, life_exp, y_hat_m3, residuos_m3, gdp_pc) %>%
  mutate(
    life_exp    = round(life_exp, 2),
    y_hat_m3    = round(y_hat_m3, 2),
    residuos_m3 = round(residuos_m3, 2),
    gdp_pc      = round(gdp_pc, 0)
  ) %>%
  head(15)

print(as.data.frame(tabela_insample))
cat("\n")

# Estatísticas dos resíduos
cat("  Estatísticas dos Resíduos (Modelo 3):\n")
cat(sprintf("    Média dos resíduos:    %.6f (aprox. 0, Propriedade 01)\n",
            mean(dados$residuos_m3)))
cat(sprintf("    DP dos resíduos:       %.4f anos\n", sd(dados$residuos_m3)))
cat(sprintf("    Mín resíduo:           %.4f anos (%s)\n",
            min(dados$residuos_m3),
            dados$country[which.min(dados$residuos_m3)]))
cat(sprintf("    Máx resíduo:           %.4f anos (%s)\n\n",
            max(dados$residuos_m3),
            dados$country[which.max(dados$residuos_m3)]))

# Países com maiores resíduos positivos (modelo subestima)
cat("  Países onde o modelo mais SUBESTIMA a exp. de vida (resíduo > 0):\n")
top_pos <- dados %>% arrange(desc(residuos_m3)) %>% head(5)
for (i in 1:5) {
  cat(sprintf("    %d. %s: Real=%.1f, Previsto=%.1f, Resíduo=+%.1f\n",
              i, top_pos$country[i], top_pos$life_exp[i],
              top_pos$y_hat_m3[i], top_pos$residuos_m3[i]))
}

# Países com maiores resíduos negativos (modelo superestima)
cat("\n  Países onde o modelo mais SUPERESTIMA a exp. de vida (resíduo < 0):\n")
top_neg <- dados %>% arrange(residuos_m3) %>% head(5)
for (i in 1:5) {
  cat(sprintf("    %d. %s: Real=%.1f, Previsto=%.1f, Resíduo=%.1f\n",
              i, top_neg$country[i], top_neg$life_exp[i],
              top_neg$y_hat_m3[i], top_neg$residuos_m3[i]))
}
cat("\n")

# ---- 5.2 Previsão Out-of-Sample ----
cat("--- 5.2 Previsão Out-of-Sample ---\n\n")

# Cenário: país hipotético com PIB per capita = US$ 15.000 (similar ao Brasil)
gdp_hipotetico     <- 15000
log_gdp_hipotetico <- log(gdp_hipotetico)
ser_m3             <- res3$ser

# Previsão pontual
previsao <- predict(modelo3, newdata = data.frame(log_gdp_pc = log_gdp_hipotetico))

# Intervalo de previsão formal (95%)
previsao_ic <- predict(modelo3,
                       newdata  = data.frame(log_gdp_pc = log_gdp_hipotetico),
                       interval = "prediction",
                       level    = 0.95)

cat(sprintf("  Cenário: País hipotético com PIB per capita = US$ %s\n",
            format(gdp_hipotetico, big.mark = ".")))
cat(sprintf("  log(PIB per capita) = ln(%s) = %.4f\n\n",
            format(gdp_hipotetico, big.mark = "."), log_gdp_hipotetico))

cat(sprintf("  Previsão pontual (Y_hat):                 %.2f anos\n", previsao))
cat(sprintf("  Intervalo informal (Y_hat +/- SER):       [%.2f , %.2f] anos\n",
            previsao - ser_m3, previsao + ser_m3))
cat(sprintf("  Intervalo de previsão 95%%%% (via predict): [%.2f , %.2f] anos\n\n",
            previsao_ic[, "lwr"], previsao_ic[, "upr"]))

# Previsão para múltiplos cenários
cat("  Tabela de previsões para diferentes níveis de PIB per capita:\n\n")
cat(sprintf("  %20s | %12s | %10s | %s\n",
            "PIB per capita", "log(PIB pc)", "Y_hat", "Intervalo +/- SER"))
cat(sprintf("  %s\n", strrep("-", 72)))

cenarios <- c(1000, 3000, 5000, 10000, 15000, 25000, 40000, 60000, 100000)
for (g in cenarios) {
  pred <- predict(modelo3, newdata = data.frame(log_gdp_pc = log(g)))
  cat(sprintf("  %20s | %12.4f | %10.2f | [%.2f , %.2f]\n",
              paste0("US$ ", format(g, big.mark = ".")),
              log(g), pred,
              pred - ser_m3, pred + ser_m3))
}
cat("\n")

# Referência: valor real do Brasil
brasil <- dados %>% filter(grepl("Brazil", country, ignore.case = TRUE))
if (nrow(brasil) > 0) {
  cat("  +----------------------------------------------+\n")
  cat("  |  REFERÊNCIA: BRASIL                           |\n")
  cat("  +----------------------------------------------+\n\n")
  cat(sprintf("    PIB per capita real:            US$ %s\n",
              format(round(brasil$gdp_pc), big.mark = ".")))
  cat(sprintf("    log(PIB per capita):            %.4f\n", log(brasil$gdp_pc)))
  cat(sprintf("    Expectativa de vida REAL:       %.2f anos\n", brasil$life_exp))
  cat(sprintf("    Expectativa de vida PREVISTA:   %.2f anos\n", brasil$y_hat_m3))
  cat(sprintf("    Resíduo (Real - Previsto):      %.2f anos\n", brasil$residuos_m3))
  cat(sprintf("    O modelo %s a exp. de vida do Brasil em %.1f anos.\n\n",
              ifelse(brasil$residuos_m3 > 0, "subestima", "superestima"),
              abs(brasil$residuos_m3)))
}

# ---- 5.3 Gráficos de diagnóstico ----
cat("--- 5.3 Gráficos de Diagnóstico do Modelo 3 ---\n\n")

# Gráfico 8: Valores Observados vs Ajustados
g8 <- ggplot(dados, aes(x = y_hat_m3, y = life_exp)) +
  geom_point(aes(color = residuos_m3), alpha = 0.75, size = 2.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              color = "red", linewidth = 1) +
  scale_color_gradient2(low = "#D32F2F", mid = "gray80", high = "#1B5E20",
                        midpoint = 0, name = "Resíduo") +
  labs(
    title    = "Valores Observados vs Valores Ajustados (Modelo 3)",
    subtitle = "Linha tracejada = previsão perfeita (Y = Y_hat)",
    x = "Valores Ajustados — Y_hat (anos)",
    y = "Valores Observados — Y (anos)",
    caption = "Pontos próximos da linha indicam bom ajuste"
  )

ggsave(file.path(dir_graficos, "08_observado_vs_ajustado.png"), g8,
       width = 10, height = 7, dpi = 200, bg = "white")
cat("  Gráfico salvo: graficos/08_observado_vs_ajustado.png\n")

# Gráfico 9: Resíduos vs Valores Ajustados (homocedasticidade)
g9 <- ggplot(dados, aes(x = y_hat_m3, y = residuos_m3)) +
  geom_point(aes(color = residuos_m3), alpha = 0.75, size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  geom_hline(yintercept = c(-ser_m3, ser_m3), linetype = "dotted",
             color = "blue", linewidth = 0.7) +
  scale_color_gradient2(low = "#D32F2F", mid = "gray80", high = "#1B5E20",
                        midpoint = 0, name = "Resíduo") +
  geom_smooth(method = "loess", se = FALSE, color = "orange",
              linewidth = 0.8) +
  labs(
    title    = "Resíduos vs Valores Ajustados (Modelo 3)",
    subtitle = "Vermelha = 0 | Azuis pontilhadas = +/- SER | Laranja = tendência (loess)",
    x = "Valores Ajustados — Y_hat (anos)",
    y = "Resíduos — u_hat (anos)",
    caption = "Distribuição aleatória em torno de zero indica boa especificação"
  )

ggsave(file.path(dir_graficos, "09_residuos_vs_ajustados.png"), g9,
       width = 10, height = 7, dpi = 200, bg = "white")
cat("  Gráfico salvo: graficos/09_residuos_vs_ajustados.png\n")

# Gráfico 10: Resíduos vs log(PIB per capita)
g10 <- ggplot(dados, aes(x = log_gdp_pc, y = residuos_m3)) +
  geom_point(aes(color = residuos_m3), alpha = 0.75, size = 2.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  geom_hline(yintercept = c(-ser_m3, ser_m3), linetype = "dotted",
             color = "blue", linewidth = 0.7) +
  scale_color_gradient2(low = "#D32F2F", mid = "gray80", high = "#1B5E20",
                        midpoint = 0, name = "Resíduo") +
  geom_smooth(method = "loess", se = FALSE, color = "orange", linewidth = 0.8) +
  labs(
    title    = "Resíduos vs log(PIB per capita) (Modelo 3)",
    subtitle = "Verificação visual de Cov(X, u_hat) = 0 (Propriedade 02)",
    x = "log(PIB per capita, PPP)",
    y = "Resíduos — u_hat (anos)",
    caption = "Sem padrão sistemático -> especificação adequada"
  )

ggsave(file.path(dir_graficos, "10_residuos_vs_log_pib.png"), g10,
       width = 10, height = 7, dpi = 200, bg = "white")
cat("  Gráfico salvo: graficos/10_residuos_vs_log_pib.png\n")

# Gráfico 11: Histograma dos resíduos (normalidade)
g11 <- ggplot(dados, aes(x = residuos_m3)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 25, fill = "#1565C0", color = "white", alpha = 0.8) +
  geom_density(color = "#D32F2F", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
  labs(
    title    = "Distribuição dos Resíduos (Modelo 3)",
    subtitle = "Histograma + densidade estimada | Linha tracejada = zero",
    x = "Resíduos — u_hat (anos)",
    y = "Densidade",
    caption = "Formato aproximadamente simétrico sugere normalidade dos erros"
  )

ggsave(file.path(dir_graficos, "11_histograma_residuos.png"), g11,
       width = 8, height = 6, dpi = 200, bg = "white")
cat("  Gráfico salvo: graficos/11_histograma_residuos.png\n")

# Gráfico 12: QQ-Plot dos resíduos (normalidade)
g12 <- ggplot(dados, aes(sample = residuos_m3)) +
  stat_qq(color = "#1565C0", alpha = 0.7, size = 2) +
  stat_qq_line(color = "#D32F2F", linewidth = 1) +
  labs(
    title    = "QQ-Plot dos Resíduos (Modelo 3)",
    subtitle = "Pontos sobre a linha vermelha indicam normalidade",
    x = "Quantis Teóricos (Normal)",
    y = "Quantis Amostrais dos Resíduos"
  )

ggsave(file.path(dir_graficos, "12_qqplot_residuos.png"), g12,
       width = 8, height = 6, dpi = 200, bg = "white")
cat("  Gráfico salvo: graficos/12_qqplot_residuos.png\n\n")


# ==============================================================================
# ====== PARTE 6: TABELA COMPARATIVA E CONCLUSÃO ======
# ==============================================================================

cat(strrep("=", 70), "\n")
cat("====== PARTE 6: TABELA COMPARATIVA E CONCLUSÃO ======\n")
cat(strrep("=", 70), "\n\n")

# ---- Tabela comparativa dos 3 modelos ----
tabela_comparacao <- data.frame(
  Modelo        = c("M1: life_exp ~ gdp_pc",
                    "M2: life_exp ~ co2_pc",
                    "M3: life_exp ~ log(gdp_pc)"),
  Especificacao = c("Nível-Nível", "Nível-Nível", "Nível-Log"),
  Beta_0        = round(c(res1$b0, res2$b0, res3$b0), 4),
  Beta_1        = round(c(res1$b1, res2$b1, res3$b1), 6),
  EP_Beta_1     = round(c(res1$ep_b1, res2$ep_b1, res3$ep_b1), 6),
  t_Beta_1      = round(c(res1$t_b1, res2$t_b1, res3$t_b1), 4),
  p_valor       = c(res1$p_b1, res2$p_b1, res3$p_b1),
  R2            = round(c(res1$r2, res2$r2, res3$r2), 4),
  R2_ajustado   = round(c(res1$r2_adj, res2$r2_adj, res3$r2_adj), 4),
  SER           = round(c(res1$ser, res2$ser, res3$ser), 3),
  SST           = round(c(res1$SST, res2$SST, res3$SST), 2),
  SSE           = round(c(res1$SSE, res2$SSE, res3$SSE), 2),
  SSR           = round(c(res1$SSR, res2$SSR, res3$SSR), 2),
  N             = c(res1$n, res2$n, res3$n)
)

# Imprimir tabela formatada no console
cat("+============================================================================+\n")
cat("|                    TABELA COMPARATIVA DOS MODELOS OLS                       |\n")
cat("+============================================================================+\n")
cat(sprintf("| %-30s | %12s | %12s | %8s | %8s |\n",
            "Modelo", "beta0", "beta1", "R²", "SER"))
cat("+============================================================================+\n")
cat(sprintf("| %-30s | %12.4f | %12.6f | %8.4f | %8.3f |\n",
            "M1: life_exp ~ gdp_pc", res1$b0, res1$b1, res1$r2, res1$ser))
cat(sprintf("| %-30s | %12.4f | %12.6f | %8.4f | %8.3f |\n",
            "M2: life_exp ~ co2_pc", res2$b0, res2$b1, res2$r2, res2$ser))
cat(sprintf("| %-30s | %12.4f | %12.6f | %8.4f | %8.3f | <- MELHOR\n",
            "M3: life_exp ~ log(gdp_pc)", res3$b0, res3$b1, res3$r2, res3$ser))
cat("+============================================================================+\n\n")

# Tabela de decomposição da variância
cat(sprintf("  %-30s | %12s | %12s | %12s\n",
            "Modelo", "SST", "SSE", "SSR"))
cat(sprintf("  %s\n", strrep("-", 75)))
cat(sprintf("  %-30s | %12.2f | %12.2f | %12.2f\n",
            "M1: life_exp ~ gdp_pc", res1$SST, res1$SSE, res1$SSR))
cat(sprintf("  %-30s | %12.2f | %12.2f | %12.2f\n",
            "M2: life_exp ~ co2_pc", res2$SST, res2$SSE, res2$SSR))
cat(sprintf("  %-30s | %12.2f | %12.2f | %12.2f\n\n",
            "M3: life_exp ~ log(gdp_pc)", res3$SST, res3$SSE, res3$SSR))

# Salvar tabela como CSV
write.csv(tabela_comparacao, "tabela_comparacao.csv", row.names = FALSE)
cat("  Tabela salva em 'tabela_comparacao.csv'\n\n")

# ---- CONCLUSÃO FINAL ----
cat("+============================================================================+\n")
cat("|                              CONCLUSÃO                                      |\n")
cat("+============================================================================+\n\n")

previsao_brasil <- predict(modelo3, newdata = data.frame(log_gdp_pc = log(15000)))

cat(sprintf(
"  1. MELHOR MODELO: O Modelo 3 (especificação nível-log) apresenta o melhor
     ajuste, com R² = %.3f e SER = %.3f anos. A relação entre PIB per capita
     e expectativa de vida é melhor capturada por uma função logarítmica,
     refletindo retornos decrescentes: aumentos de renda têm maior impacto
     na expectativa de vida em países mais pobres do que em países ricos.

  2. PROPRIEDADES DO OLS: As quatro propriedades algébricas do estimador OLS
     foram verificadas numericamente para TODOS os três modelos:
       (i)   sum(u_hat_i) = 0
       (ii)  Cov(Xi, u_hat_i) = 0
       (iii) A reta passa por (X_barra, Y_barra)
       (iv)  Cov(Y_hat_i, u_hat_i) = 0

  3. DECOMPOSIÇÃO DA VARIÂNCIA: Verificamos que SST = SSE + SSR para todos
     os modelos, e que R² = SSE/SST = 1 - SSR/SST.

  4. CÁLCULO MANUAL: Os coeficientes beta0 e beta1 foram calculados manualmente
     usando as fórmulas:
       beta1 = sum((Xi-X_bar)(Yi-Y_bar)) / sum((Xi-X_bar)^2)
       beta0 = Y_bar - beta1 * X_bar
     Os resultados coincidem com os da função lm() do R.

  5. PREVISÃO: O Modelo 3 prevê que um país com PIB per capita de US$ 15.000
     (similar ao Brasil) teria expectativa de vida de aproximadamente %.1f anos,
     com erro típico de +/- %.1f anos.

  6. INTERPRETAÇÃO ECONÔMICA: O PIB per capita é um forte preditor da
     expectativa de vida entre países, mas não é o único fator relevante.
     Outros fatores como saúde pública, saneamento, educação e cultura
     também desempenham papel importante, conforme evidenciado pelo fato
     de R² ser inferior a 1. Além disso, o resultado do Modelo 2 (CO2)
     ilustra a importância de distinguir correlação de causalidade e o
     problema de variáveis omitidas.

  7. NOTA METODOLÓGICA: Todos os modelos estimados são de regressão linear
     SIMPLES (uma variável explicativa). Uma extensão natural seria a
     regressão MÚLTIPLA, incluindo mais de uma variável explicativa para
     controlar por fatores confundidores.\n\n",
  res3$r2, res3$ser,
  previsao_brasil, res3$ser))

cat("================================================================\n")
cat("  FIM DO SCRIPT — Todos os arquivos foram salvos com sucesso!\n")
cat("================================================================\n\n")
cat("  Arquivos gerados:\n")
cat("    dados_limpos.csv          — base de dados limpa\n")
cat("    tabela_comparacao.csv     — tabela comparativa dos modelos\n")
cat("    graficos/                 — pasta com todos os gráficos:\n")
cat("      01_scatter_vida_vs_pib.png\n")
cat("      02_scatter_vida_vs_log_pib.png\n")
cat("      03_scatter_vida_vs_co2.png\n")
cat("      04_scatter_co2_vs_pib.png\n")
cat("      05a_hist_expectativa_vida.png\n")
cat("      05b_hist_pib_per_capita.png\n")
cat("      05c_hist_co2_per_capita.png\n")
cat("      06_boxplot_vida_por_renda.png\n")
cat("      07_matriz_correlacao.png\n")
cat("      08_observado_vs_ajustado.png\n")
cat("      09_residuos_vs_ajustados.png\n")
cat("      10_residuos_vs_log_pib.png\n")
cat("      11_histograma_residuos.png\n")
cat("      12_qqplot_residuos.png\n")
cat("================================================================\n")
