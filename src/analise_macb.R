#!/usr/bin/env Rscript
# ==============================================================================
# Script de Análise Estatística MACb em R
# - Análise Descritiva (Média, Mediana, Desvio-Padrão, Mínimo e Máximo)
# - Testes Não Paramétricos de Friedman e Kendall's W
# - Comparações Pareadas Post-Hoc (Wilcoxon com ajuste de Holm)
# - Visualizações de Dificuldade com ggplot2
# ==============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(knitr)
})

# Caminhos de arquivos
EXCEL_PATH  <- "data/Tabulação MACb - quantitativo reformulada.xlsx"
PLOTS_DIR   <- "data/plots"
RESULTS_DIR <- "data/results"

dir.create(PLOTS_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)

# Definição dos subtestes e tetos máximos
SUBTESTS_COMPLETO <- list(
  IME = list(o = "IME o", m = "IME m", name = "Interpretação de Metáfora - Explicação"),
  IMA = list(o = "IMA o", m = "lMA m", name = "Interpretação de Metáfora - Alternativas"),
  PEP = list(o = "PEP o", m = "PEP m", name = "Prosódia Emocional - Produção"),
  JSi = list(o = "JSi o", m = "JSi m", name = "Julgamento Semântico - Identificação"),
  JSe = list(o = "JSe o", m = "JSe m", name = "Julgamento Semântico - Explicação"),
  ATe = list(o = "ATe o", m = "ATe m", name = "Atos de Fala - Explicação"),
  ATa = list(o = "ATa o", m = "ATa m", name = "Atos de Fala - Alternativas")
)

SUBTESTS_INICIAL <- list(
  E   = list(o = "E o",   m = "E m",   name = "Expressão"),
  C   = list(o = "C o",   m = "C m",   name = "Compreensão"),
  CNV = list(o = "CNV o", m = "CNV m", name = "Comportamento Não Verbal"),
  PLE = list(o = "PLE o", m = "PLE m", name = "Prosódia Linguística Emocional")
)

SUBTESTS_NARRATIVO <- list(
  IP    = list(o = "IP o",    m = "IP m",    name = "Ideias Principais"),
  InfL  = list(o = "InfL o",  m = "InfL m",  name = "Informações Lembradas"),
  CompT = list(o = "CompT o", m = "CompT m", name = "Compreensão do Texto"),
  Infer = list(o = "Infer o", m = "Infer m", name = "Inferência")
)

# Função para estatística descritiva completa
calc_descritiva <- function(df, subtests_list = NULL) {
  if (!is.null(subtests_list)) {
    res <- lapply(names(subtests_list), function(code) {
      info <- subtests_list[[code]]
      vec_o <- df[[info$o]]
      vec_m <- df[[info$m]]
      teto  <- vec_m[1]
      
      mean_val <- mean(vec_o, na.rm = TRUE)
      std_val  <- sd(vec_o, na.rm = TRUE)
      med_val  <- median(vec_o, na.rm = TRUE)
      q1_val   <- quantile(vec_o, 0.25, na.rm = TRUE)
      q3_val   <- quantile(vec_o, 0.75, na.rm = TRUE)
      min_val  <- min(vec_o, na.rm = TRUE)
      max_val  <- max(vec_o, na.rm = TRUE)
      pct_mean <- (mean_val / teto) * 100
      
      data.frame(
        Codigo = code,
        Subteste = info$name,
        Pontuacão_Maxima = teto,
        Media = round(mean_val, 2),
        Mediana = round(med_val, 2),
        Desvio_Padrao = round(std_val, 2),
        Minimo = round(min_val, 2),
        Maximo = round(max_val, 2),
        Q1 = round(q1_val, 2),
        Q3 = round(q3_val, 2),
        Pct_Acerto_Medio = round(pct_mean, 2),
        Pct_Dificuldade_Media = round(100 - pct_mean, 2),
        stringsAsFactors = FALSE
      )
    })
    return(bind_rows(res))
  } else {
    num_cols <- names(df)[names(df) != "ID"]
    res <- lapply(num_cols, function(col) {
      vec <- df[[col]]
      data.frame(
        Variavel = col,
        Media = round(mean(vec, na.rm = TRUE), 2),
        Mediana = round(median(vec, na.rm = TRUE), 2),
        Desvio_Padrao = round(sd(vec, na.rm = TRUE), 2),
        Minimo = round(min(vec, na.rm = TRUE), 2),
        Maximo = round(max(vec, na.rm = TRUE), 2),
        Q1 = round(quantile(vec, 0.25, na.rm = TRUE), 2),
        Q3 = round(quantile(vec, 0.75, na.rm = TRUE), 2),
        stringsAsFactors = FALSE
      )
    })
    return(bind_rows(res))
  }
}

# Função para Teste de Friedman e Post-Hoc
executar_friedman_posthoc <- function(pct_df) {
  mat <- as.matrix(pct_df)
  f_test <- friedman.test(mat)
  N <- nrow(mat)
  k <- ncol(mat)
  kendall_w <- f_test$statistic / (N * (k - 1))
  
  cols <- colnames(pct_df)
  pairs_list <- list()
  idx <- 1
  for (i in 1:(length(cols) - 1)) {
    for (j in (i + 1):length(cols)) {
      c1 <- cols[i]
      c2 <- cols[j]
      w_test <- suppressWarnings(wilcox.test(pct_df[[c1]], pct_df[[c2]], paired = TRUE))
      m1 <- mean(pct_df[[c1]], na.rm = TRUE)
      m2 <- mean(pct_df[[c2]], na.rm = TRUE)
      pairs_list[[idx]] <- data.frame(
        Par = paste(c1, "vs", c2),
        Media_Subteste1 = round(m1, 2),
        Media_Subteste2 = round(m2, 2),
        Diferenca_Media = round(m1 - m2, 2),
        p_bruto = w_test$p.value,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }
  posthoc_df <- bind_rows(pairs_list)
  p_adj <- p.adjust(posthoc_df$p_bruto, method = "holm")
  posthoc_df$Significativo <- p_adj < 0.05
  posthoc_df$Resultado <- ifelse(posthoc_df$Significativo, "Significativo", "Não significativo")
  posthoc_df$p_bruto <- formatC(posthoc_df$p_bruto, format = "e", digits = 3)
  posthoc_df$p_ajustado_Holm <- formatC(p_adj, format = "e", digits = 3)
  
  list(
    chi2 = as.numeric(f_test$statistic),
    df = as.numeric(f_test$parameter),
    p_value = f_test$p.value,
    kendall_w = as.numeric(kendall_w),
    posthoc = posthoc_df
  )
}

# ==============================================================================
# Execução da Análise
# ==============================================================================
cat("Lendo dados do arquivo Excel...\n")
df_comp      <- read_excel(EXCEL_PATH, sheet = "MACB completo")
df_inicial   <- read_excel(EXCEL_PATH, sheet = "MACB - discurso inicial")
df_fluencia  <- read_excel(EXCEL_PATH, sheet = "MACB - fluência verbal")
df_narrativo <- read_excel(EXCEL_PATH, sheet = "MACB - discurso narrativo")

# 1. Tabelas Descritivas
desc_comp      <- calc_descritiva(df_comp, SUBTESTS_COMPLETO)
desc_inicial   <- calc_descritiva(df_inicial, SUBTESTS_INICIAL)
desc_narrativo <- calc_descritiva(df_narrativo, SUBTESTS_NARRATIVO)
desc_fluencia  <- calc_descritiva(df_fluencia)

# 2. DataFrames em Porcentagem
pct_comp <- as.data.frame(sapply(SUBTESTS_COMPLETO, function(x) (df_comp[[x$o]] / df_comp[[x$m]]) * 100))
pct_inic <- as.data.frame(sapply(SUBTESTS_INICIAL,  function(x) (df_inicial[[x$o]] / df_inicial[[x$m]]) * 100))
pct_narr <- as.data.frame(sapply(SUBTESTS_NARRATIVO,function(x) (df_narrativo[[x$o]] / df_narrativo[[x$m]]) * 100))

# 3. Testes não paramétricos
res_f_comp  <- executar_friedman_posthoc(pct_comp)
res_f_inic  <- executar_friedman_posthoc(pct_inic)
res_f_narr  <- executar_friedman_posthoc(pct_narr)

fv_cols     <- c("0-30", "30-60", "60-90", "90-120", "120-150")
res_f_fluen <- executar_friedman_posthoc(df_fluencia[fv_cols])

# 4. Ranking Geral de Dificuldade
all_subtests <- list()
idx <- 1
for (grp in list(list(sub = SUBTESTS_COMPLETO, df = df_comp, cat = "MACb Completo"),
                 list(sub = SUBTESTS_INICIAL,  df = df_inicial, cat = "Discurso Inicial"),
                 list(sub = SUBTESTS_NARRATIVO,df = df_narrativo, cat = "Discurso Narrativo"))) {
  for (code in names(grp$sub)) {
    info <- grp$sub[[code]]
    pct_vec <- (grp$df[[info$o]] / grp$df[[info$m]]) * 100
    vec_o <- grp$df[[info$o]]
    all_subtests[[idx]] <- data.frame(
      Codigo = code,
      Subteste = paste0(info$name, " (", code, ")"),
      Categoria = grp$cat,
      Media = round(mean(vec_o, na.rm = TRUE), 2),
      Mediana = round(median(vec_o, na.rm = TRUE), 2),
      Desvio_Padrao = round(sd(vec_o, na.rm = TRUE), 2),
      Minimo = round(min(vec_o, na.rm = TRUE), 2),
      Maximo = round(max(vec_o, na.rm = TRUE), 2),
      Pct_Acerto_Medio = round(mean(pct_vec, na.rm = TRUE), 2),
      Pct_Dificuldade_Media = round(100 - mean(pct_vec, na.rm = TRUE), 2),
      stringsAsFactors = FALSE
    )
    idx <- idx + 1
  }
}

df_ranking <- bind_rows(all_subtests) %>%
  arrange(desc(Pct_Dificuldade_Media)) %>%
  mutate(Rank_Dificuldade = row_number())

# ==============================================================================
# Gráficos com ggplot2
# ==============================================================================
cat("Gerando gráficos com ggplot2...\n")

# Theme base
theme_custom <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray30"),
    axis.title = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank()
  )

# 1. MACb Completo
LIGHT_GRAY <- "#9CA3AF"

p1_df <- desc_comp %>% arrange(Pct_Dificuldade_Media)
p1 <- ggplot(p1_df, aes(x = reorder(Codigo, Pct_Dificuldade_Media), y = Pct_Dificuldade_Media)) +
  geom_col(width = 0.7, fill = LIGHT_GRAY, show.legend = FALSE) +
  geom_text(aes(label = paste0(Pct_Dificuldade_Media, "%")), hjust = -0.15, fontface = "bold", size = 3.8, color = "#1F2937") +
  coord_flip(ylim = c(0, 105)) +
  labs(title = "MACb completo: porcentagem média de dificuldade por subteste",
       subtitle = "Subtestes principais normalizados pelo valor máximo",
       x = "Subteste", y = "Dificuldade média (%)") +
  theme_custom

ggsave(file.path(PLOTS_DIR, "dificuldade_macb_completo.png"), plot = p1, width = 8, height = 5, dpi = 300)

# 2. Discurso Narrativo
p2_df <- desc_narrativo %>% arrange(Pct_Dificuldade_Media)
p2 <- ggplot(p2_df, aes(x = reorder(Codigo, Pct_Dificuldade_Media), y = Pct_Dificuldade_Media)) +
  geom_col(width = 0.7, fill = LIGHT_GRAY, show.legend = FALSE) +
  geom_text(aes(label = paste0(Pct_Dificuldade_Media, "%")), hjust = -0.15, fontface = "bold", size = 3.8, color = "#1F2937") +
  coord_flip(ylim = c(0, 105)) +
  labs(title = "MACb discurso narrativo: porcentagem média de dificuldade por subteste",
       subtitle = "Sub-dimensões de discurso narrativo normalizadas pelo valor máximo",
       x = "Subteste", y = "Dificuldade média (%)") +
  theme_custom

ggsave(file.path(PLOTS_DIR, "dificuldade_discurso_narrativo.png"), plot = p2, width = 8, height = 4.5, dpi = 300)

# 3. Discurso Inicial
p3_df <- desc_inicial %>% arrange(Pct_Dificuldade_Media)
p3 <- ggplot(p3_df, aes(x = reorder(Codigo, Pct_Dificuldade_Media), y = Pct_Dificuldade_Media)) +
  geom_col(width = 0.7, fill = LIGHT_GRAY, show.legend = FALSE) +
  geom_text(aes(label = paste0(Pct_Dificuldade_Media, "%")), hjust = -0.15, fontface = "bold", size = 3.8, color = "#1F2937") +
  coord_flip(ylim = c(0, 105)) +
  labs(title = "MACb discurso inicial: porcentagem média de dificuldade por subteste",
       subtitle = "Sub-dimensões de discurso inicial normalizadas pelo valor máximo",
       x = "Subteste", y = "Dificuldade média (%)") +
  theme_custom

ggsave(file.path(PLOTS_DIR, "dificuldade_discurso_inicial.png"), plot = p3, width = 8, height = 4.5, dpi = 300)

# 4. Fluência Verbal (Curva Temporal)
p4_df <- data.frame(
  Intervalo = factor(fv_cols, levels = fv_cols),
  Media = sapply(fv_cols, function(c) mean(df_fluencia[[c]])),
  SD    = sapply(fv_cols, function(c) sd(df_fluencia[[c]]))
)

p4 <- ggplot(p4_df, aes(x = Intervalo, y = Media, group = 1)) +
  geom_ribbon(aes(ymin = Media - SD/2, ymax = Media + SD/2, fill = "Dispersão (Desvio-Padrão)"), alpha = 0.35) +
  geom_line(aes(color = "Média de palavras"), linewidth = 1.2) +
  geom_point(aes(color = "Média de palavras"), size = 3.5) +
  geom_text(aes(label = round(Media, 2)), vjust = -1, fontface = "bold", color = "#1F2937") +
  scale_y_continuous(limits = c(0, max(p4_df$Media + p4_df$SD))) +
  scale_color_manual(values = c("Média de palavras" = "#374151"), name = NULL) +
  scale_fill_manual(values = c("Dispersão (Desvio-Padrão)" = LIGHT_GRAY), name = NULL) +
  labs(title = "Curva temporal da fluência verbal livre",
       subtitle = "Evocação lexical média por intervalo de 30 segundos",
       x = "Intervalo de tempo (segundos)", y = "Número médio de palavras") +
  theme_custom +
  theme(legend.position = "bottom", legend.box = "horizontal")

ggsave(file.path(PLOTS_DIR, "curva_fluencia_verbal.png"), plot = p4, width = 8, height = 4.5, dpi = 300)

# 5. Ranking Geral de Dificuldade
p5_df <- df_ranking %>% arrange(Pct_Dificuldade_Media)
p5 <- ggplot(p5_df, aes(x = reorder(Codigo, Pct_Dificuldade_Media), y = Pct_Dificuldade_Media, fill = Categoria)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(Pct_Dificuldade_Media, "%")), hjust = -0.15, fontface = "bold", size = 3.5, color = "#1F2937") +
  coord_flip(ylim = c(0, 105)) +
  scale_fill_manual(values = c("MACb Completo" = "#4B5563", "Discurso Narrativo" = "#71717A", "Discurso Inicial" = "#A1A1AA"), name = "Categoria") +
  labs(title = "Ranking geral de dificuldade das tarefas do MACb",
       subtitle = "Porcentagem média de erro/dificuldade por tarefa",
       x = "Subteste", y = "Dificuldade média (%)") +
  theme_custom +
  theme(legend.position = "bottom", legend.title = element_text(face = "bold"))

ggsave(file.path(PLOTS_DIR, "ranking_geral_dificuldade.png"), plot = p5, width = 9, height = 7, dpi = 300)

# Save RDS object for report compilation
saveRDS(list(
  desc_comp = desc_comp,
  desc_inicial = desc_inicial,
  desc_narrativo = desc_narrativo,
  desc_fluencia = desc_fluencia,
  res_f_comp = res_f_comp,
  res_f_inic = res_f_inic,
  res_f_narr = res_f_narr,
  res_f_fluen = res_f_fluen,
  df_ranking = df_ranking
), file.path(RESULTS_DIR, "resultados_macb.rds"))

cat("Análise R finalizada com sucesso! Resultados salvos em data/results/resultados_macb.rds\n")
