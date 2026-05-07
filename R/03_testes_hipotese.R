# ==============================================================================
# Trabalho Final de Estatística II
# Script 03 — Teste de hipótese principal
# ==============================================================================

source(file.path("R", "00_setup.R"))

cat("\nScript 03 — Teste de hipótese principal\n")

base_limpa <- file.path(paths$data_processed, "base_limpa.rds")
if (!file.exists(base_limpa)) {
  stop("Base limpa não encontrada. Rode primeiro R/01_importacao_limpeza.R.")
}

dados <- readRDS(base_limpa)
setDT(dados)

# Pergunta de pesquisa:
# A expectativa de vida média difere entre países de alta renda e demais países?
#
# H0: mu_alta_renda = mu_demais
# H1: mu_alta_renda != mu_demais
# alpha = 0,05

alpha <- 0.05

# Separar grupos
grupo_demais <- dados[alta_renda == "Demais países", life_exp]
grupo_alta <- dados[alta_renda == "Alta renda", life_exp]

# Cálculo manual do teste t de Welch
n_demais <- length(grupo_demais)
n_alta <- length(grupo_alta)
media_demais <- mean(grupo_demais)
media_alta <- mean(grupo_alta)
var_demais <- var(grupo_demais)
var_alta <- var(grupo_alta)

diferenca_medias <- media_alta - media_demais
erro_padrao_dif <- sqrt(var_alta / n_alta + var_demais / n_demais)
t_stat <- diferenca_medias / erro_padrao_dif

gl_welch <- (var_alta / n_alta + var_demais / n_demais)^2 /
  ((var_alta / n_alta)^2 / (n_alta - 1) + (var_demais / n_demais)^2 / (n_demais - 1))

p_valor_manual <- 2 * pt(abs(t_stat), df = gl_welch, lower.tail = FALSE)
t_critico <- qt(1 - alpha / 2, df = gl_welch)
ic_inferior <- diferenca_medias - t_critico * erro_padrao_dif
ic_superior <- diferenca_medias + t_critico * erro_padrao_dif

decisao <- ifelse(p_valor_manual < alpha, "Rejeitar H0", "Não rejeitar H0")

resultado_manual <- data.table::data.table(
  teste = "Teste t de Welch - cálculo manual",
  h0 = "mu_alta_renda = mu_demais",
  h1 = "mu_alta_renda != mu_demais",
  alpha = alpha,
  n_demais = n_demais,
  n_alta_renda = n_alta,
  media_demais = media_demais,
  media_alta_renda = media_alta,
  diferenca_medias_alta_menos_demais = diferenca_medias,
  erro_padrao_diferenca = erro_padrao_dif,
  estatistica_t = t_stat,
  graus_liberdade_welch = gl_welch,
  p_valor = p_valor_manual,
  ic_95_inferior = ic_inferior,
  ic_95_superior = ic_superior,
  decisao = decisao
)

# Teste usando função pronta do R para validação
# A ordem x = alta renda e y = demais países mantém a diferença como
# media_alta_renda - media_demais, igual ao cálculo manual.
teste_t_r <- t.test(x = grupo_alta, y = grupo_demais, var.equal = FALSE, conf.level = 0.95)
resultado_r <- broom::tidy(teste_t_r)
resultado_r$h0 <- "mu_alta_renda = mu_demais"
resultado_r$h1 <- "mu_alta_renda != mu_demais"
resultado_r$alpha <- alpha
resultado_r$decisao <- ifelse(resultado_r$p.value < alpha, "Rejeitar H0", "Não rejeitar H0")

readr::write_csv(resultado_manual, file.path(paths$output_tables, "resultado_teste_t_manual.csv"))
readr::write_csv(resultado_r, file.path(paths$output_tables, "resultado_teste_t_welch.csv"))

cat("Resultado manual do teste t de Welch:\n")
print(resultado_manual)
cat("\nResultado do t.test() no R:\n")
print(teste_t_r)
