#!/usr/bin/env Rscript
# ==============================================================================
# Script para compilação do relatório MACb em PDF
# ==============================================================================

cat("Iniciando compilação do relatório MACb em PDF...\n")

# Garante que as análises e gráficos foram gerados
if (!file.exists("data/results/resultados_macb.rds")) {
  cat("Executando análise de dados em R (analise_macb.R)...\n")
  source("src/analise_macb.R")
}

# Compila o documento R Markdown para PDF único em data/
cat("Renderizando o relatório R Markdown para PDF...\n")
rmarkdown::render(
  input = "report/relatorio_macb.Rmd",
  output_file = "../data/relatorio_macb.pdf",
  quiet = TRUE
)

# Remove arquivos intermediários do LaTeX no diretório report
file.remove(list.files("report", pattern = "\\.(log|tex|knit\\.md|utf8\\.md|aux)$", full.names = TRUE))

cat("==============================================================================\n")
cat("Relatório PDF único gerado com sucesso!\n")
cat("Caminho do arquivo: data/relatorio_macb.pdf\n")
cat("==============================================================================\n")
