# ==============================================================================
# Trabalho Final de Estatística II
# Script 04 — Gráficos
# ==============================================================================

source(file.path("R", "00_setup.R"))

cat("\nScript 04 — Gráficos\n")

base_limpa <- file.path(paths$data_processed, "base_limpa.rds")
if (!file.exists(base_limpa)) {
  stop("Base limpa não encontrada. Rode primeiro R/01_importacao_limpeza.R.")
}

dados <- readRDS(base_limpa)
setDT(dados)

# Gráfico 1: boxplot da expectativa de vida por grupo
g_boxplot <- ggplot2::ggplot(dados, ggplot2::aes(x = alta_renda, y = life_exp, fill = alta_renda)) +
  ggplot2::geom_boxplot(alpha = 0.75, width = 0.55, outlier.alpha = 0.65) +
  ggplot2::geom_jitter(width = 0.12, alpha = 0.45, size = 1.6) +
  ggplot2::scale_fill_manual(values = c("Demais países" = "#6BAED6", "Alta renda" = "#2171B5"), guide = "none") +
  ggplot2::labs(
    title = "Expectativa de vida por grupo de renda",
    subtitle = "Cada ponto representa um país",
    x = "Grupo de países",
    y = "Expectativa de vida ao nascer (anos)",
    caption = "Fonte: World Bank Open Data."
  )

ggplot2::ggsave(
  filename = file.path(paths$output_figures, "boxplot_expectativa_vida_grupo.png"),
  plot = g_boxplot,
  width = 9,
  height = 6,
  dpi = 300,
  bg = "white"
)

# Gráfico 2: média com intervalo de confiança aproximado de 95%
tabela_media_ic <- dados[, .(
  n = .N,
  media = mean(life_exp),
  dp = sd(life_exp),
  erro_padrao = sd(life_exp) / sqrt(.N)
), by = alta_renda]

tabela_media_ic[, ic_inf := media - qt(0.975, df = n - 1) * erro_padrao]
tabela_media_ic[, ic_sup := media + qt(0.975, df = n - 1) * erro_padrao]

readr::write_csv(tabela_media_ic, file.path(paths$output_tables, "media_ic_expectativa_vida_grupo.csv"))

g_media_ic <- ggplot2::ggplot(tabela_media_ic, ggplot2::aes(x = alta_renda, y = media, fill = alta_renda)) +
  ggplot2::geom_col(alpha = 0.82, width = 0.55) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = ic_inf, ymax = ic_sup), width = 0.15, linewidth = 0.8) +
  ggplot2::geom_text(ggplot2::aes(label = round(media, 1)), vjust = -0.8, fontface = "bold") +
  ggplot2::scale_fill_manual(values = c("Demais países" = "#9ECAE1", "Alta renda" = "#08519C"), guide = "none") +
  ggplot2::labs(
    title = "Média da expectativa de vida por grupo",
    subtitle = "Barras verticais indicam intervalo de confiança de 95% da média",
    x = "Grupo de países",
    y = "Expectativa de vida média (anos)",
    caption = "Fonte: World Bank Open Data."
  )

ggplot2::ggsave(
  filename = file.path(paths$output_figures, "media_ic_expectativa_vida_grupo.png"),
  plot = g_media_ic,
  width = 9,
  height = 6,
  dpi = 300,
  bg = "white"
)

# Gráfico 3: dispersão complementar entre log do PIB per capita e expectativa de vida
g_scatter <- ggplot2::ggplot(dados, ggplot2::aes(x = log_gdp_pc, y = life_exp, color = alta_renda)) +
  ggplot2::geom_point(alpha = 0.75, size = 2) +
  ggplot2::geom_smooth(method = "lm", se = TRUE, color = "#B2182B", linewidth = 0.9) +
  ggplot2::scale_color_manual(values = c("Demais países" = "#6BAED6", "Alta renda" = "#08519C")) +
  ggplot2::labs(
    title = "Expectativa de vida e log do PIB per capita",
    subtitle = "Análise visual complementar; não substitui o teste de hipótese principal",
    x = "Log do PIB per capita PPP",
    y = "Expectativa de vida ao nascer (anos)",
    color = "Grupo",
    caption = "Fonte: World Bank Open Data."
  )

ggplot2::ggsave(
  filename = file.path(paths$output_figures, "scatter_life_exp_log_gdp_pc.png"),
  plot = g_scatter,
  width = 9,
  height = 6,
  dpi = 300,
  bg = "white"
)

cat("Gráficos salvos em output/figures.\n")
