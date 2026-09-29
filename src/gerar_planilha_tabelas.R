# ==============================================================================
# Script: gerar_planilha_tabelas.R
# Objetivo: Exportar todas as tabelas dos relatórios relatorio_macb.Rmd e
#           relatorio_macb_escolaridade.Rmd para uma planilha Excel estilizada (.xlsx)
# ==============================================================================

suppressPackageStartupMessages({
  library(openxlsx)
  library(dplyr)
})

# 1. Carregar resultados processados
res1 <- readRDS("data/results/resultados_macb.rds")
res2 <- readRDS("data/results/resultados_macb_escolaridade.rds")

# Criar workbook
wb <- createWorkbook()

# Definir estilos visuais profissionais
style_header_sumario <- createStyle(
  fontName = "Calibri", fontSize = 11, fontColour = "#FFFFFF",
  fgFill = "#1F3864", halign = "center", valign = "center",
  textDecoration = "bold", border = "TopBottomLeftRight", borderColour = "#000000"
)

style_header_r1 <- createStyle(
  fontName = "Calibri", fontSize = 11, fontColour = "#FFFFFF",
  fgFill = "#2F5597", halign = "center", valign = "center",
  textDecoration = "bold", border = "TopBottomLeftRight", borderColour = "#000000"
)

style_header_r2 <- createStyle(
  fontName = "Calibri", fontSize = 11, fontColour = "#FFFFFF",
  fgFill = "#2E75B6", halign = "center", valign = "center",
  textDecoration = "bold", border = "TopBottomLeftRight", borderColour = "#000000"
)

style_data_center <- createStyle(
  fontName = "Calibri", fontSize = 10, halign = "center", valign = "center",
  border = "TopBottomLeftRight", borderColour = "#D9D9D9"
)

style_data_left <- createStyle(
  fontName = "Calibri", fontSize = 10, halign = "left", valign = "center",
  border = "TopBottomLeftRight", borderColour = "#D9D9D9"
)

# ------------------------------------------------------------------------------
# Lista de tabelas a serem exportadas
# ------------------------------------------------------------------------------

# --- RELATÓRIO 1: relatorio_macb.Rmd ---

# T1: Resumo Friedman
df_r1_friedman <- res1$res_friedman_summary
colnames(df_r1_friedman) <- c("Bloco / Domínio", "Estatística Chi2", "gl", "p-valor", "W de Kendall", "Resultado")

# T2: Ranking Geral de Dificuldade
df_r1_ranking <- res1$df_ranking[, c("Codigo", "Categoria", "Media", "Mediana", "Minimo", "Maximo", "Desvio_Padrao", "Pct_Dificuldade_Media")]
colnames(df_r1_ranking) <- c("Tarefa", "Categoria", "Média", "Mediana", "Mínimo", "Máximo", "Desvio-Padrão", "Dificuldade (%)")

# T3: Descritiva MACb Completo
df_r1_comp_desc <- res1$desc_comp[, c("Codigo", "Media", "Mediana", "Minimo", "Maximo", "Desvio_Padrao", "Pct_Dificuldade_Media")]
colnames(df_r1_comp_desc) <- c("Tarefa", "Média", "Mediana", "Mínimo", "Máximo", "Desvio-Padrão", "Dificuldade (%)")

# T4: Friedman MACb Completo
df_r1_comp_friedman <- data.frame(
  Chi2 = round(res1$res_f_comp$chi2, 4),
  GL = res1$res_f_comp$df,
  p_valor = formatC(res1$res_f_comp$p_value, format="f", digits=5),
  Kendall_W = round(res1$res_f_comp$kendall_w, 4),
  Resultado = ifelse(res1$res_f_comp$p_value < 0.05, "Significativo", "Não significativo")
)
colnames(df_r1_comp_friedman) <- c("Estatística Chi2", "gl", "p-valor", "W de Kendall", "Resultado")

# T5: Post-hoc MACb Completo
df_r1_comp_posthoc <- res1$res_f_comp$posthoc[, c("Par", "Media_Tarefa1", "Media_Tarefa2", "Diferenca_Media", "p_bruto", "Resultado")]
colnames(df_r1_comp_posthoc) <- c("Comparação", "Tarefa 1 (%)", "Tarefa 2 (%)", "Diferença (%)", "p-valor", "Resultado")

# T6: Descritiva Discurso Narrativo
df_r1_narr_desc <- res1$desc_narrativo[, c("Codigo", "Media", "Mediana", "Minimo", "Maximo", "Desvio_Padrao", "Pct_Dificuldade_Media")]
colnames(df_r1_narr_desc) <- c("Tarefa", "Média", "Mediana", "Mínimo", "Máximo", "Desvio-Padrão", "Dificuldade (%)")

# T7: Friedman Discurso Narrativo
df_r1_narr_friedman <- data.frame(
  Chi2 = round(res1$res_f_narr$chi2, 4),
  GL = res1$res_f_narr$df,
  p_valor = formatC(res1$res_f_narr$p_value, format="f", digits=5),
  Kendall_W = round(res1$res_f_narr$kendall_w, 4),
  Resultado = ifelse(res1$res_f_narr$p_value < 0.05, "Significativo", "Não significativo")
)
colnames(df_r1_narr_friedman) <- c("Estatística Chi2", "gl", "p-valor", "W de Kendall", "Resultado")

# T8: Post-hoc Discurso Narrativo
df_r1_narr_posthoc <- res1$res_f_narr$posthoc[, c("Par", "Media_Tarefa1", "Media_Tarefa2", "Diferenca_Media", "p_bruto", "Resultado")]
colnames(df_r1_narr_posthoc) <- c("Comparação", "Tarefa 1 (%)", "Tarefa 2 (%)", "Diferença (%)", "p-valor", "Resultado")

# T9: Descritiva Discurso Inicial
df_r1_inic_desc <- res1$desc_inicial[, c("Codigo", "Media", "Mediana", "Minimo", "Maximo", "Desvio_Padrao", "Pct_Dificuldade_Media")]
colnames(df_r1_inic_desc) <- c("Tarefa", "Média", "Mediana", "Mínimo", "Máximo", "Desvio-Padrão", "Dificuldade (%)")

# T10: Friedman Discurso Inicial
df_r1_inic_friedman <- data.frame(
  Chi2 = round(res1$res_f_inic$chi2, 4),
  GL = res1$res_f_inic$df,
  p_valor = formatC(res1$res_f_inic$p_value, format="f", digits=5),
  Kendall_W = round(res1$res_f_inic$kendall_w, 4),
  Resultado = ifelse(res1$res_f_inic$p_value < 0.05, "Significativo", "Não significativo")
)
colnames(df_r1_inic_friedman) <- c("Estatística Chi2", "gl", "p-valor", "W de Kendall", "Resultado")

# T11: Post-hoc Discurso Inicial
df_r1_inic_posthoc <- res1$res_f_inic$posthoc[, c("Par", "Media_Tarefa1", "Media_Tarefa2", "Diferenca_Media", "p_bruto", "Resultado")]
colnames(df_r1_inic_posthoc) <- c("Comparação", "Tarefa 1 (%)", "Tarefa 2 (%)", "Diferença (%)", "p-valor", "Resultado")

# T12: Descritiva Fluência Verbal
df_r1_flue_desc <- res1$desc_fluencia
colnames(df_r1_flue_desc) <- c("Intervalo", "Média", "Mediana", "Desvio-Padrão", "Mínimo", "Máximo", "Q1", "Q3")

# T13: Friedman Fluência Verbal
df_r1_flue_friedman <- data.frame(
  Chi2 = round(res1$res_f_fluen$chi2, 4),
  GL = res1$res_f_fluen$df,
  p_valor = formatC(res1$res_f_fluen$p_value, format="f", digits=5),
  Kendall_W = round(res1$res_f_fluen$kendall_w, 4),
  Resultado = ifelse(res1$res_f_fluen$p_value < 0.05, "Significativo", "Não significativo")
)
colnames(df_r1_flue_friedman) <- c("Estatística Chi2", "gl", "p-valor", "W de Kendall", "Resultado")

# T14: Post-hoc Fluência Verbal
df_r1_flue_posthoc <- res1$res_f_fluen$posthoc[, c("Par", "Media_Tarefa1", "Media_Tarefa2", "Diferenca_Media", "p_bruto", "Resultado")]
colnames(df_r1_flue_posthoc) <- c("Intervalos Consecutivos", "Média 1", "Média 2", "Diferença", "p-valor", "Resultado")


# --- RELATÓRIO 2: relatorio_macb_escolaridade.Rmd ---

# T15: Resumo Kruskal-Wallis por Escolaridade
df_r2_kw_resumo <- res2$kw_resumo[, c("Bloco", "Subteste", "H", "GL", "p_fmt", "Resultado")]
colnames(df_r2_kw_resumo) <- c("Bloco", "Subteste", "Estatística H", "gl", "p-valor", "Resultado")

# T16: Descritiva MACb Completo por Escolaridade
df_r2_comp_desc <- res2$desc_comp[, c("Subteste", "Escolaridade", "Media", "DP", "Mediana", "Q1", "Q3", "Corte")]
colnames(df_r2_comp_desc) <- c("Subteste", "Escolaridade", "Média", "Desvio-Padrão", "Mediana", "Q1", "Q3", "Ponto de corte")

# T17: Post-hoc MACb Completo por Escolaridade
df_r2_comp_posthoc <- res2$kw_comp$posthoc[, c("Subteste", "Par", "Media_G1", "Media_G2", "Diferenca", "p_fmt", "Resultado")]
colnames(df_r2_comp_posthoc) <- c("Subteste", "Comparação", "Média G1", "Média G2", "Diferença", "p-valor (Holm)", "Resultado")

# T18: Descritiva Discurso Inicial por Escolaridade
df_r2_inic_desc <- res2$desc_inic[, c("Subteste", "Escolaridade", "Media", "DP", "Mediana", "Q1", "Q3", "Corte")]
colnames(df_r2_inic_desc) <- c("Subteste", "Escolaridade", "Média", "Desvio-Padrão", "Mediana", "Q1", "Q3", "Ponto de corte")

# T19: Post-hoc Discurso Inicial por Escolaridade
df_r2_inic_posthoc <- res2$kw_inic$posthoc[, c("Subteste", "Par", "Media_G1", "Media_G2", "Diferenca", "p_fmt", "Resultado")]
colnames(df_r2_inic_posthoc) <- c("Subteste", "Comparação", "Média G1", "Média G2", "Diferença", "p-valor (Holm)", "Resultado")

# T20: Descritiva Fluência Verbal por Escolaridade
df_r2_flue_desc <- res2$desc_flue[, c("Subteste", "Escolaridade", "Media", "DP", "Mediana", "Q1", "Q3", "Corte")]
colnames(df_r2_flue_desc) <- c("Intervalo / Subteste", "Escolaridade", "Média", "Desvio-Padrão", "Mediana", "Q1", "Q3", "Ponto de corte")

# T21: Post-hoc Fluência Verbal por Escolaridade
df_r2_flue_posthoc <- res2$kw_flue$posthoc[, c("Subteste", "Par", "Media_G1", "Media_G2", "Diferenca", "p_fmt", "Resultado")]
colnames(df_r2_flue_posthoc) <- c("Subteste", "Comparação", "Média G1", "Média G2", "Diferença", "p-valor (Holm)", "Resultado")

# T22: Descritiva Discurso Narrativo por Escolaridade
df_r2_narr_desc <- res2$desc_narr[, c("Subteste", "Escolaridade", "Media", "DP", "Mediana", "Q1", "Q3", "Corte")]
colnames(df_r2_narr_desc) <- c("Subteste", "Escolaridade", "Média", "Desvio-Padrão", "Mediana", "Q1", "Q3", "Ponto de corte")

# T23: Post-hoc Discurso Narrativo por Escolaridade
df_r2_narr_posthoc <- res2$kw_narr$posthoc[, c("Subteste", "Par", "Media_G1", "Media_G2", "Diferenca", "p_fmt", "Resultado")]
colnames(df_r2_narr_posthoc) <- c("Subteste", "Comparação", "Média G1", "Média G2", "Diferença", "p-valor (Holm)", "Resultado")

# T24: Subteste Inferência - Contingência e Fisher
tab_raw <- res2$fisher_infer$tabela
df_r2_infer_fisher <- data.frame(
  Escolaridade = rownames(tab_raw),
  Erro_0 = as.numeric(tab_raw[, "0"]),
  Acerto_1 = as.numeric(tab_raw[, "1"]),
  Total = as.numeric(tab_raw[, "0"] + tab_raw[, "1"]),
  Pct_Acerto = round(as.numeric(tab_raw[, "1"]) / (as.numeric(tab_raw[, "0"] + tab_raw[, "1"])) * 100, 2),
  Fisher_p_valor = c(res2$fisher_infer$p_fmt, "", ""),
  Fisher_Resultado = c(res2$fisher_infer$resultado, "", ""),
  stringsAsFactors = FALSE
)
colnames(df_r2_infer_fisher) <- c("Escolaridade", "Erro (0)", "Acerto (1)", "Total", "Taxa de Acerto (%)", "p-valor (Fisher)", "Resultado")

# T25: Prevalência de Alertas Clínicos
df_r2_alertas <- res2$alertas_all[, c("Bloco", "Subteste", "Escolaridade", "N_total", "N_alerta", "Pct_alerta")]
colnames(df_r2_alertas) <- c("Bloco funcional", "Subteste", "Escolaridade", "Total (N)", "Alertas (n)", "Prevalência (%)")


# ------------------------------------------------------------------------------
# Mapeamento do Sumário
# ------------------------------------------------------------------------------

sumario_df <- data.frame(
  ID = 1:25,
  Relatorio = c(rep("Relatório 1 (MACb Geral)", 14), rep("Relatório 2 (Escolaridade)", 11)),
  Nome_Aba = c(
    "R1_Friedman_Resumo",
    "R1_Ranking_Dificuldade",
    "R1_Comp_Descritiva",
    "R1_Comp_Friedman",
    "R1_Comp_PostHoc",
    "R1_Narr_Descritiva",
    "R1_Narr_Friedman",
    "R1_Narr_PostHoc",
    "R1_Inic_Descritiva",
    "R1_Inic_Friedman",
    "R1_Inic_PostHoc",
    "R1_Flue_Descritiva",
    "R1_Flue_Friedman",
    "R1_Flue_PostHoc",
    "R2_KW_Resumo",
    "R2_Comp_Descritiva",
    "R2_Comp_PostHoc",
    "R2_Inic_Descritiva",
    "R2_Inic_PostHoc",
    "R2_Flue_Descritiva",
    "R2_Flue_PostHoc",
    "R2_Narr_Descritiva",
    "R2_Narr_PostHoc",
    "R2_Inferencia_Fisher",
    "R2_Alertas_Clinicos"
  ),
  Titulo_Tabela = c(
    "Resumo dos testes de Friedman e tamanhos do efeito por bloco",
    "Ranking geral de dificuldade das tarefas do MACb (% de erro)",
    "Estatística descritiva: MACb completo",
    "Resultado do teste de Friedman: MACb completo",
    "Comparações pareadas post-hoc (Wilcoxon): MACb completo",
    "Estatística descritiva: discurso narrativo",
    "Resultado do teste de Friedman: discurso narrativo",
    "Comparações pareadas post-hoc (Wilcoxon): discurso narrativo",
    "Estatística descritiva: discurso inicial",
    "Resultado do teste de Friedman: discurso inicial",
    "Comparações pareadas post-hoc (Wilcoxon): discurso inicial",
    "Estatística descritiva: fluência verbal livre por intervalo temporal",
    "Resultado do teste de Friedman: fluência verbal livre",
    "Comparações pareadas post-hoc de intervalos consecutivos: fluência verbal",
    "Resumo geral dos testes de Kruskal-Wallis por escolaridade",
    "Estatística descritiva e pontos de corte: MACb completo por escolaridade",
    "Comparações pareadas post-hoc (Wilcoxon-Holm): MACb completo",
    "Estatística descritiva e pontos de corte: discurso inicial por escolaridade",
    "Comparações pareadas post-hoc (Wilcoxon-Holm): discurso inicial",
    "Estatística descritiva e pontos de corte: fluência verbal por escolaridade",
    "Comparações pareadas post-hoc (Wilcoxon-Holm): fluência verbal",
    "Estatística descritiva e pontos de corte: discurso narrativo por escolaridade",
    "Comparações pareadas post-hoc (Wilcoxon-Holm): discurso narrativo",
    "Distribuição de frequências e teste exato de Fisher: subteste Inferência",
    "Prevalência de classificações de alerta clínico por subteste e escolaridade"
  ),
  Linhas = c(
    nrow(df_r1_friedman), nrow(df_r1_ranking), nrow(df_r1_comp_desc), nrow(df_r1_comp_friedman), nrow(df_r1_comp_posthoc),
    nrow(df_r1_narr_desc), nrow(df_r1_narr_friedman), nrow(df_r1_narr_posthoc), nrow(df_r1_inic_desc), nrow(df_r1_inic_friedman),
    nrow(df_r1_inic_posthoc), nrow(df_r1_flue_desc), nrow(df_r1_flue_friedman), nrow(df_r1_flue_posthoc),
    nrow(df_r2_kw_resumo), nrow(df_r2_comp_desc), nrow(df_r2_comp_posthoc), nrow(df_r2_inic_desc), nrow(df_r2_inic_posthoc),
    nrow(df_r2_flue_desc), nrow(df_r2_flue_posthoc), nrow(df_r2_narr_desc), nrow(df_r2_narr_posthoc), nrow(df_r2_infer_fisher),
    nrow(df_r2_alertas)
  ),
  stringsAsFactors = FALSE
)


# ------------------------------------------------------------------------------
# Adicionar aba Sumário ao Workbook
# ------------------------------------------------------------------------------

addWorksheet(wb, "Sumario", tabColour = "#1F3864", gridLines = TRUE)
writeData(wb, "Sumario", sumario_df, startRow = 1, startCol = 1)
addStyle(wb, "Sumario", style_header_sumario, rows = 1, cols = 1:ncol(sumario_df), gridExpand = TRUE)
addStyle(wb, "Sumario", style_data_center, rows = 2:(nrow(sumario_df) + 1), cols = c(1, 3, 5), gridExpand = TRUE)
addStyle(wb, "Sumario", style_data_left, rows = 2:(nrow(sumario_df) + 1), cols = c(2, 4), gridExpand = TRUE)
freezePane(wb, "Sumario", firstRow = TRUE)
addFilter(wb, "Sumario", rows = 1, cols = 1:ncol(sumario_df))
setColWidths(wb, "Sumario", cols = 1:ncol(sumario_df), widths = "auto")


# ------------------------------------------------------------------------------
# Função auxiliar para adicionar tabelas estilizadas
# ------------------------------------------------------------------------------

adicionar_aba <- function(wb, sheet_name, df, header_style, tab_color) {
  addWorksheet(wb, sheet_name, tabColour = tab_color, gridLines = TRUE)
  writeData(wb, sheet_name, df, startRow = 1, startCol = 1)
  
  # Estilizar cabeçalho
  addStyle(wb, sheet_name, header_style, rows = 1, cols = 1:ncol(df), gridExpand = TRUE)
  
  # Estilizar células de dados
  for (c in 1:ncol(df)) {
    if (is.character(df[[c]]) || is.factor(df[[c]])) {
      # se for texto curto ou cod/sig, centraliza; senao alinha a esquerda
      max_len <- max(nchar(as.character(df[[c]])), na.rm = TRUE)
      if (max_len <= 15) {
        addStyle(wb, sheet_name, style_data_center, rows = 2:(nrow(df) + 1), cols = c, gridExpand = TRUE)
      } else {
        addStyle(wb, sheet_name, style_data_left, rows = 2:(nrow(df) + 1), cols = c, gridExpand = TRUE)
      }
    } else {
      addStyle(wb, sheet_name, style_data_center, rows = 2:(nrow(df) + 1), cols = c, gridExpand = TRUE)
    }
  }
  
  freezePane(wb, sheet_name, firstRow = TRUE)
  addFilter(wb, sheet_name, rows = 1, cols = 1:ncol(df))
  setColWidths(wb, sheet_name, cols = 1:ncol(df), widths = "auto")
}


# ------------------------------------------------------------------------------
# Adicionar abas do Relatório 1 (Azul Médio: #2F5597)
# ------------------------------------------------------------------------------

tab_col_r1 <- "#2F5597"
adicionar_aba(wb, "R1_Friedman_Resumo", df_r1_friedman, style_header_r1, tab_col_r1)
adicionar_aba(wb, "R1_Ranking_Dificuldade", df_r1_ranking, style_header_r1, tab_col_r1)
adicionar_aba(wb, "R1_Comp_Descritiva", df_r1_comp_desc, style_header_r1, tab_col_r1)
adicionar_aba(wb, "R1_Comp_Friedman", df_r1_comp_friedman, style_header_r1, tab_col_r1)
adicionar_aba(wb, "R1_Comp_PostHoc", df_r1_comp_posthoc, style_header_r1, tab_col_r1)
adicionar_aba(wb, "R1_Narr_Descritiva", df_r1_narr_desc, style_header_r1, tab_col_r1)
adicionar_aba(wb, "R1_Narr_Friedman", df_r1_narr_friedman, style_header_r1, tab_col_r1)
adicionar_aba(wb, "R1_Narr_PostHoc", df_r1_narr_posthoc, style_header_r1, tab_col_r1)
adicionar_aba(wb, "R1_Inic_Descritiva", df_r1_inic_desc, style_header_r1, tab_col_r1)
adicionar_aba(wb, "R1_Inic_Friedman", df_r1_inic_friedman, style_header_r1, tab_col_r1)
adicionar_aba(wb, "R1_Inic_PostHoc", df_r1_inic_posthoc, style_header_r1, tab_col_r1)
adicionar_aba(wb, "R1_Flue_Descritiva", df_r1_flue_desc, style_header_r1, tab_col_r1)
adicionar_aba(wb, "R1_Flue_Friedman", df_r1_flue_friedman, style_header_r1, tab_col_r1)
adicionar_aba(wb, "R1_Flue_PostHoc", df_r1_flue_posthoc, style_header_r1, tab_col_r1)


# ------------------------------------------------------------------------------
# Adicionar abas do Relatório 2 (Azul Claro: #2E75B6)
# ------------------------------------------------------------------------------

tab_col_r2 <- "#2E75B6"
adicionar_aba(wb, "R2_KW_Resumo", df_r2_kw_resumo, style_header_r2, tab_col_r2)
adicionar_aba(wb, "R2_Comp_Descritiva", df_r2_comp_desc, style_header_r2, tab_col_r2)
adicionar_aba(wb, "R2_Comp_PostHoc", df_r2_comp_posthoc, style_header_r2, tab_col_r2)
adicionar_aba(wb, "R2_Inic_Descritiva", df_r2_inic_desc, style_header_r2, tab_col_r2)
adicionar_aba(wb, "R2_Inic_PostHoc", df_r2_inic_posthoc, style_header_r2, tab_col_r2)
adicionar_aba(wb, "R2_Flue_Descritiva", df_r2_flue_desc, style_header_r2, tab_col_r2)
adicionar_aba(wb, "R2_Flue_PostHoc", df_r2_flue_posthoc, style_header_r2, tab_col_r2)
adicionar_aba(wb, "R2_Narr_Descritiva", df_r2_narr_desc, style_header_r2, tab_col_r2)
adicionar_aba(wb, "R2_Narr_PostHoc", df_r2_narr_posthoc, style_header_r2, tab_col_r2)
adicionar_aba(wb, "R2_Inferencia_Fisher", df_r2_infer_fisher, style_header_r2, tab_col_r2)
adicionar_aba(wb, "R2_Alertas_Clinicos", df_r2_alertas, style_header_r2, tab_col_r2)


# ------------------------------------------------------------------------------
# Salvar planilha Excel nos diretórios data/ e report/
# ------------------------------------------------------------------------------

out_data <- "data/tabelas_relatorios_macb.xlsx"
out_report <- "report/tabelas_relatorios_macb.xlsx"

saveWorkbook(wb, out_data, overwrite = TRUE)
saveWorkbook(wb, out_report, overwrite = TRUE)

cat("Planilha criada com sucesso em:\n")
cat(" -", out_data, "\n")
cat(" -", out_report, "\n")
cat("Total de abas criadas:", length(sheets(wb)), "\n")
