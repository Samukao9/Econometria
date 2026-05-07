# Trabalho Final de Estatística II

Este projeto organiza a análise empírica do Trabalho Final de Estatística II.

## Tema provisório

Diferença de expectativa de vida média entre países de alta renda e países que não são de alta renda, usando dados cross-section do World Bank Open Data.

## Pergunta de pesquisa

> A expectativa de vida média difere entre países de alta renda e países que não são de alta renda?

## Estratégia estatística

- Base: World Bank Open Data.
- Unidade de observação: país.
- Recorte: uma observação por país, usando o ano mais recente disponível entre 2020 e 2022.
- Variável de interesse: expectativa de vida ao nascer (`life_exp`).
- Grupo de comparação: países de alta renda versus demais países (`alta_renda`).
- Teste principal: teste t de Welch para duas médias independentes.
- Nível de significância: 5%.

## Estrutura do projeto

```text
.
├── data/
│   ├── raw/                 # Bases brutas
│   └── processed/           # Bases limpas
├── R/
│   ├── 00_setup.R
│   ├── 01_importacao_limpeza.R
│   ├── 02_analise_descritiva.R
│   ├── 03_testes_hipotese.R
│   ├── 04_graficos.R
│   └── 05_modelo_ols_opcional.R
├── output/
│   ├── tables/              # Tabelas geradas pelos scripts
│   └── figures/             # Gráficos gerados pelos scripts
├── report/
│   └── relatorio_final.Rmd  # Esqueleto do relatório final
├── slides/
│   └── roteiro_slides.md    # Roteiro inicial da apresentação
├── README.md
└── trabalho_econometria.R   # Script antigo usado como referência
```

## Ordem de execução no RStudio

Abra o projeto no RStudio e rode os scripts nesta ordem:

1. `R/00_setup.R`
2. `R/01_importacao_limpeza.R`
3. `R/02_analise_descritiva.R`
4. `R/03_testes_hipotese.R`
5. `R/04_graficos.R`
6. `R/05_modelo_ols_opcional.R` apenas se quiser a análise complementar por OLS.

## Outputs esperados

Depois da execução, os principais arquivos estarão em:

- `data/raw/wdi_worldbank_2020_2022.rds`
- `data/processed/base_limpa.rds`
- `data/processed/base_limpa.csv`
- `output/tables/tabela_descritiva_geral.csv`
- `output/tables/tabela_descritiva_por_grupo.csv`
- `output/tables/resultado_teste_t_welch.csv`
- `output/tables/resultado_teste_t_manual.csv`
- `output/figures/boxplot_expectativa_vida_grupo.png`
- `output/figures/media_ic_expectativa_vida_grupo.png`

## Observação metodológica

O teste principal é um teste de diferença de médias. Os resultados devem ser interpretados como evidência estatística de diferença entre grupos, não como evidência causal de que renda causa maior expectativa de vida.
