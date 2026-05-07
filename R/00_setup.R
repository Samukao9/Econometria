# ==============================================================================
# Trabalho Final de Estatística II
# Script 00 — Configuração inicial
# ==============================================================================

# Limpar ambiente de trabalho
rm(list = ls())

# Evitar notação científica nos outputs
options(scipen = 999)

# Definir diretórios do projeto com caminhos relativos
paths <- list(
  data_raw = file.path("data", "raw"),
  data_processed = file.path("data", "processed"),
  output_tables = file.path("output", "tables"),
  output_figures = file.path("output", "figures"),
  report = "report",
  slides = "slides"
)

# Criar pastas necessárias, caso ainda não existam
for (pasta in paths) {
  if (!dir.exists(pasta)) {
    dir.create(pasta, recursive = TRUE)
  }
}

# Pacotes usados no projeto
pacotes <- c(
  "WDI",
  "data.table",
  "ggplot2",
  "readr",
  "janitor",
  "broom",
  "knitr"
)

# Instalar pacotes ausentes, se necessário, e carregar todos
pacotes_ausentes <- pacotes[!pacotes %in% rownames(installed.packages())]
if (length(pacotes_ausentes) > 0) {
  install.packages(pacotes_ausentes, repos = "https://cloud.r-project.org")
}

invisible(lapply(pacotes, library, character.only = TRUE))

# Tema padrão dos gráficos
tema_padrao <- ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
    plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray35"),
    axis.title = ggplot2::element_text(face = "bold"),
    panel.grid.minor = ggplot2::element_blank()
  )

ggplot2::theme_set(tema_padrao)

cat("Configuração concluída. Pastas e pacotes prontos.\n")
