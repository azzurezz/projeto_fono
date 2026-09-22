#!/usr/bin/env Rscript
# ==============================================================================
# Script para Compilação Automatizada do Relatório MACb em PDF
# ==============================================================================

cat("Iniciando compilação do relatório MACb em PDF...\n")

# Garante que as análises e gráficos foram gerados
if (!file.exists("data/results/resultados_macb.rds")) {
  cat("Executando análise de dados R (analise_macb.R)...\n")
  source("src/analise_macb.R")
}

# Compila o documento R Markdown para PDF
cat("Renderizando o relatório R Markdown para PDF...\n")
rmarkdown::render(
  input = "report/relatorio_macb.Rmd",
  output_file = "../data/relatorio_estatistico_macb.pdf",
  quiet = FALSE
)

cat("\n==============================================================================\n")
cat("RELATÓRIO PDF GERADO COM SUCESSO!\n")
cat("Caminho do arquivo: data/relatorio_estatistico_macb.pdf\n")
cat("==============================================================================\n")
