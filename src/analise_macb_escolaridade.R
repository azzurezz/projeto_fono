#!/usr/bin/env Rscript
# ==============================================================================
# Análise MACb por Escolaridade com Pontos de Corte
# - Estatística descritiva estratificada por grupo de escolaridade
# - Gráficos de médias com linhas de ponto de corte
# - Testes de Kruskal-Wallis + Post-hoc pairwise Wilcoxon (ajuste de Holm)
# - Teste exato de Fisher para Inferência (variável binária)
# ==============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(knitr)
})

# Caminhos
EXCEL_PATH  <- "data/MACB_escolaridade_corte.xlsx"
PLOTS_DIR   <- "data/plots/escolaridade"
RESULTS_DIR <- "data/results"

dir.create(PLOTS_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)

# Labels e paleta de cores
ESC_LABELS <- c("1" = "5-8 anos", "2" = "9-11 anos", "3" = "12+ anos")
ESC_COLORS <- c("5-8 anos" = "#1F2937", "9-11 anos" = "#6B7280", "12+ anos" = "#B0B8C4")

# ==============================================================================
# Definição da estrutura de cada sheet (colunas por posição, 1-indexed no R)
# Cada subteste: code, name, o (pontuação), m (máximo), corte, class
# ==============================================================================

SUBTESTS_COMP <- list(
  list(code="DC",  name="Discurso Conversacional",         o=3,  m=4,    corte=5,  cls=6),
  list(code="IMe", name="Interp. Metáfora - Explicação",   o=7,  m=8,    corte=9,  cls=10),
  list(code="IMa", name="Interp. Metáfora - Alternativas", o=11, m=12,   corte=13, cls=14),
  list(code="FVL", name="Fluência Verbal Livre",            o=15, m=NULL, corte=16, cls=17),
  list(code="PEP", name="Prosódia Emocional - Produção",    o=18, m=19,   corte=20, cls=21),
  list(code="JSi", name="Julgamento Semântico - Identif.",  o=22, m=23,   corte=24, cls=25),
  list(code="JSe", name="Julgamento Semântico - Explicação",o=26, m=27,   corte=28, cls=29),
  list(code="ATe", name="Atos de Fala - Explicação",        o=30, m=31,   corte=32, cls=33),
  list(code="ATa", name="Atos de Fala - Alternativas",      o=34, m=35,   corte=36, cls=37)
)

SUBTESTS_INIC <- list(
  list(code="E",   name="Expressão",                       o=3,  m=4,  corte=5,  cls=6),
  list(code="C",   name="Compreensão",                     o=7,  m=8,  corte=9,  cls=10),
  list(code="CNV", name="Comportamento Não Verbal",        o=11, m=12, corte=13, cls=14),
  list(code="PLE", name="Prosódia Linguística Emocional",  o=15, m=16, corte=17, cls=18),
  list(code="DC",  name="Discurso Conversacional",          o=19, m=20, corte=21, cls=22)
)

SUBTESTS_FLUE <- list(
  list(code="0-30",    name="0–30s",   o=3,  m=NULL, corte=4,  cls=5),
  list(code="30-60",   name="30–60s",  o=6,  m=NULL, corte=7,  cls=8),
  list(code="60-90",   name="60–90s",  o=9,  m=NULL, corte=10, cls=11),
  list(code="90-120",  name="90–120s", o=12, m=NULL, corte=13, cls=14),
  list(code="120-150", name="120–150s",o=15, m=NULL, corte=16, cls=17),
  list(code="Total",   name="Total",   o=18, m=NULL, corte=19, cls=20)
)

SUBTESTS_NARR <- list(
  list(code="IP",    name="Ideias Principais",       o=3,  m=4,  corte=5,  cls=6),
  list(code="InfL",  name="Informações Lembradas",   o=7,  m=8,  corte=9,  cls=10),
  list(code="CompT", name="Compreensão do Texto",    o=11, m=12, corte=13, cls=14),
  list(code="Infer", name="Inferência",              o=15, m=16, corte=NULL, cls=NULL)
)

# ==============================================================================
# Funções utilitárias
# ==============================================================================

# Extrair dados limpos de uma sheet usando posições de coluna
extract_clean_data <- function(path, sheet, subtests_def) {
  df <- read_excel(path, sheet = sheet)

  result <- data.frame(
    Participante = as.character(df[[1]]),
    Escolaridade = as.numeric(df[[2]]),
    stringsAsFactors = FALSE
  )

  for (st in subtests_def) {
    result[[paste0(st$code, "_o")]] <- as.numeric(df[[st$o]])
    if (!is.null(st$m)) {
      result[[paste0(st$code, "_m")]] <- as.numeric(df[[st$m]])
    }
    if (!is.null(st$corte)) {
      result[[paste0(st$code, "_corte")]] <- as.numeric(df[[st$corte]])
    }
    if (!is.null(st$cls)) {
      val <- as.character(df[[st$cls]])
      result[[paste0(st$code, "_class")]] <- ifelse(
        !is.na(val) & tolower(trimws(val)) == "alerta", "alerta", "normal"
      )
    }
  }
  result
}

# Estatística descritiva estratificada por escolaridade
calc_desc_estratificada <- function(data, subtests_def) {
  rows <- list()
  idx <- 1

  for (st in subtests_def) {
    for (esc in c(1, 2, 3)) {
      vec <- data[[paste0(st$code, "_o")]][data$Escolaridade == esc]
      vec <- vec[!is.na(vec)]

      corte_val <- NA
      corte_col <- paste0(st$code, "_corte")
      if (corte_col %in% names(data)) {
        cv <- data[[corte_col]][data$Escolaridade == esc]
        cv <- cv[!is.na(cv)]
        if (length(cv) > 0) corte_val <- cv[1]
      }

      rows[[idx]] <- data.frame(
        Subteste     = st$code,
        Tarefa       = st$name,
        Escolaridade = ESC_LABELS[as.character(esc)],
        Esc_num      = esc,
        N            = length(vec),
        Media        = round(mean(vec), 2),
        Mediana      = round(median(vec), 2),
        DP           = round(sd(vec), 2),
        Minimo       = round(min(vec), 2),
        Maximo       = round(max(vec), 2),
        Q1           = round(quantile(vec, 0.25), 2),
        Q3           = round(quantile(vec, 0.75), 2),
        Corte        = round(corte_val, 2),
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }
  bind_rows(rows)
}

# Kruskal-Wallis + Post-hoc pairwise Wilcoxon (Holm)
run_kw_posthoc <- function(data, subtests_def) {
  kw_rows <- list()
  ph_rows <- list()
  ki <- 1
  pi <- 1

  for (st in subtests_def) {
    scores <- data[[paste0(st$code, "_o")]]
    groups <- factor(data$Escolaridade, levels = c(1, 2, 3),
                     labels = c("5-8 anos", "9-11 anos", "12+ anos"))

    valid <- !is.na(scores) & !is.na(groups)
    sc <- scores[valid]
    gr <- groups[valid]

    kw <- kruskal.test(sc ~ gr)

    kw_rows[[ki]] <- data.frame(
      Subteste   = st$code,
      Tarefa     = st$name,
      H          = round(as.numeric(kw$statistic), 4),
      GL         = as.numeric(kw$parameter),
      p_valor    = kw$p.value,
      p_fmt      = formatC(kw$p.value, format = "f", digits = 5),
      Resultado  = ifelse(kw$p.value < 0.05, "Significativo", "Não significativo"),
      stringsAsFactors = FALSE
    )
    ki <- ki + 1

    # Post-hoc pairwise Wilcoxon com ajuste de Holm
    pw <- suppressWarnings(pairwise.wilcox.test(sc, gr, p.adjust.method = "holm"))

    for (r in rownames(pw$p.value)) {
      for (cc in colnames(pw$p.value)) {
        pv <- pw$p.value[r, cc]
        if (!is.na(pv)) {
          m1 <- mean(sc[gr == cc], na.rm = TRUE)
          m2 <- mean(sc[gr == r],  na.rm = TRUE)
          ph_rows[[pi]] <- data.frame(
            Subteste      = st$code,
            Par           = paste(cc, "vs", r),
            Media_G1      = round(m1, 2),
            Media_G2      = round(m2, 2),
            Diferenca     = round(m1 - m2, 2),
            p_valor       = pv,
            p_fmt         = formatC(pv, format = "f", digits = 5),
            Resultado     = ifelse(pv < 0.05, "Significativo", "Não significativo"),
            stringsAsFactors = FALSE
          )
          pi <- pi + 1
        }
      }
    }
  }

  list(kw = bind_rows(kw_rows), posthoc = bind_rows(ph_rows))
}

# Teste exato de Fisher para variável binária (Inferência)
run_fisher <- function(data, code = "Infer") {
  scores <- data[[paste0(code, "_o")]]
  groups <- factor(data$Escolaridade, levels = c(1, 2, 3),
                   labels = c("5-8 anos", "9-11 anos", "12+ anos"))

  valid <- !is.na(scores) & !is.na(groups)
  sc <- scores[valid]
  gr <- groups[valid]

  tab <- table(Escolaridade = gr, Pontuacao = sc)
  ft  <- fisher.test(tab)

  list(
    tabela    = tab,
    p_valor   = ft$p.value,
    p_fmt     = formatC(ft$p.value, format = "f", digits = 5),
    resultado = ifelse(ft$p.value < 0.05, "Significativo", "Não significativo")
  )
}

# ==============================================================================
# Função de gráfico: médias por escolaridade com linhas de corte
# ==============================================================================

theme_esc <- theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray30"),
    axis.title    = element_text(face = "bold", size = 10),
    axis.text.x   = element_text(angle = 25, hjust = 1, size = 8),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    strip.text = element_text(face = "bold", size = 9)
  )

plot_medias_corte <- function(desc_df, titulo, subtitulo, draw_corte = TRUE) {

  plot_df <- desc_df %>%
    mutate(
      Escolaridade = factor(Escolaridade, levels = c("5-8 anos", "9-11 anos", "12+ anos")),
      Subteste     = factor(Subteste, levels = unique(Subteste))
    )

  p <- ggplot(plot_df, aes(x = Escolaridade, y = Media, fill = Escolaridade)) +
    geom_col(width = 0.6, alpha = 0.85, color = NA) +
    geom_errorbar(aes(ymin = pmax(Media - DP, 0), ymax = Media + DP),
                  width = 0.2, linewidth = 0.45, color = "#374151") +
    geom_text(aes(label = Media, y = Media + DP),
              vjust = -0.6, size = 2.7, fontface = "bold", color = "#1F2937")

  if (draw_corte && "Corte" %in% names(plot_df) && any(!is.na(plot_df$Corte))) {
    p <- p +
      geom_crossbar(aes(y = Corte, ymin = Corte, ymax = Corte, linetype = "Ponto de corte"),
                    width = 0.55, color = "#E63946", fatten = 2, linewidth = 0.6,
                    show.legend = TRUE) +
      scale_linetype_manual(values = c("Ponto de corte" = "dashed"), name = NULL)
  }

  p <- p +
    facet_wrap(~ Subteste, scales = "free_y", ncol = 3) +
    scale_fill_manual(values = ESC_COLORS, name = "Escolaridade") +
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.20))) +
    labs(title = titulo, subtitle = subtitulo,
         x = "Grupo de Escolaridade", y = "Pontuação média") +
    theme_esc

  p
}

# ==============================================================================
# Execução principal
# ==============================================================================

cat("====================================================\n")
cat(" Análise MACb por Escolaridade com Pontos de Corte\n")
cat("====================================================\n\n")

cat("1. Lendo dados do arquivo Excel...\n")
data_comp <- extract_clean_data(EXCEL_PATH, "MACB completo",          SUBTESTS_COMP)
data_inic <- extract_clean_data(EXCEL_PATH, "MACB discurso inicial",  SUBTESTS_INIC)
data_flue <- extract_clean_data(EXCEL_PATH, "MACB fluência verbal",   SUBTESTS_FLUE)
data_narr <- extract_clean_data(EXCEL_PATH, "MACB discurso narrativo",SUBTESTS_NARR)

cat(sprintf("   N total = %d participantes\n", nrow(data_comp)))
cat(sprintf("   Escolaridade 1 (5-8 anos):  n = %d\n", sum(data_comp$Escolaridade == 1)))
cat(sprintf("   Escolaridade 2 (9-11 anos): n = %d\n", sum(data_comp$Escolaridade == 2)))
cat(sprintf("   Escolaridade 3 (12+ anos):  n = %d\n\n", sum(data_comp$Escolaridade == 3)))

# ------ Descritiva ------
cat("2. Calculando estatísticas descritivas por escolaridade...\n")
desc_comp <- calc_desc_estratificada(data_comp, SUBTESTS_COMP)
desc_inic <- calc_desc_estratificada(data_inic, SUBTESTS_INIC)
desc_flue <- calc_desc_estratificada(data_flue, SUBTESTS_FLUE)
desc_narr <- calc_desc_estratificada(data_narr, SUBTESTS_NARR)

# ------ Kruskal-Wallis + Post-hoc ------
cat("3. Executando Kruskal-Wallis e post-hoc pairwise Wilcoxon...\n")
kw_comp <- run_kw_posthoc(data_comp, SUBTESTS_COMP)
kw_inic <- run_kw_posthoc(data_inic, SUBTESTS_INIC)
kw_flue <- run_kw_posthoc(data_flue, SUBTESTS_FLUE)

# Narrativo: subtestes com corte (IP, InfL, CompT) + Infer separado
SUB_NARR_CORTE <- SUBTESTS_NARR[sapply(SUBTESTS_NARR, function(x) !is.null(x$corte))]
SUB_NARR_INFER <- SUBTESTS_NARR[sapply(SUBTESTS_NARR, function(x) is.null(x$corte))]

kw_narr  <- run_kw_posthoc(data_narr, SUB_NARR_CORTE)
kw_infer <- run_kw_posthoc(data_narr, SUB_NARR_INFER)

# Fisher exato para Inferência (binária)
cat("4. Teste exato de Fisher para Inferência...\n")
fisher_infer <- run_fisher(data_narr, "Infer")

# ------ Gráficos ------
cat("5. Gerando gráficos com linhas de corte...\n")

# 5.1 MACb Completo (9 subtestes, 3x3 grid)
p_comp <- plot_medias_corte(
  desc_comp,
  "MACb Completo: médias por escolaridade",
  "Barras = média ± DP  |  Linha tracejada vermelha = ponto de corte"
)
ggsave(file.path(PLOTS_DIR, "medias_macb_completo_esc.png"),
       plot = p_comp, width = 13, height = 11, dpi = 300)

# 5.2 Discurso Inicial (5 subtestes, 2x3 grid)
p_inic <- plot_medias_corte(
  desc_inic,
  "Discurso Inicial: médias por escolaridade",
  "Barras = média ± DP  |  Linha tracejada vermelha = ponto de corte"
)
ggsave(file.path(PLOTS_DIR, "medias_disc_inicial_esc.png"),
       plot = p_inic, width = 11, height = 8, dpi = 300)

# 5.3 Fluência Verbal (6 subtestes, 2x3 grid)
p_flue <- plot_medias_corte(
  desc_flue,
  "Fluência Verbal: médias por escolaridade",
  "Barras = média ± DP  |  Linha tracejada vermelha = ponto de corte"
)
ggsave(file.path(PLOTS_DIR, "medias_fluencia_verbal_esc.png"),
       plot = p_flue, width = 12, height = 8, dpi = 300)

# 5.4 Discurso Narrativo COM corte (IP, InfL, CompT)
desc_narr_corte <- desc_narr %>% filter(Subteste != "Infer")
p_narr <- plot_medias_corte(
  desc_narr_corte,
  "Discurso Narrativo: médias por escolaridade",
  "Barras = média ± DP  |  Linha tracejada vermelha = ponto de corte"
)
ggsave(file.path(PLOTS_DIR, "medias_disc_narrativo_esc.png"),
       plot = p_narr, width = 10, height = 5, dpi = 300)

# 5.5 Inferência SEM corte (gráfico separado)
desc_infer <- desc_narr %>% filter(Subteste == "Infer")
p_infer <- plot_medias_corte(
  desc_infer,
  "Inferência: médias por escolaridade",
  "Barras = média ± DP  (ponto de corte não disponível)",
  draw_corte = FALSE
)
ggsave(file.path(PLOTS_DIR, "medias_inferencia_esc.png"),
       plot = p_infer, width = 6, height = 4.5, dpi = 300)

# ------ Tabela resumo KW ------
cat("6. Compilando resumo dos testes de Kruskal-Wallis...\n")

kw_resumo <- bind_rows(
  kw_comp$kw %>% mutate(Bloco = "MACb Completo"),
  kw_inic$kw %>% mutate(Bloco = "Discurso Inicial"),
  kw_flue$kw %>% mutate(Bloco = "Fluência Verbal"),
  kw_narr$kw %>% mutate(Bloco = "Discurso Narrativo"),
  kw_infer$kw %>% mutate(Bloco = "Discurso Narrativo (Inferência)")
)

# ------ Prevalência de alertas ------
cat("7. Calculando prevalência de alertas por escolaridade...\n")

calc_alertas <- function(data, subtests_def, bloco) {
  rows <- list()
  idx <- 1
  for (st in subtests_def) {
    cls_col <- paste0(st$code, "_class")
    if (!cls_col %in% names(data)) next
    for (esc in c(1, 2, 3)) {
      sub <- data[data$Escolaridade == esc, ]
      n_total   <- nrow(sub)
      n_alerta  <- sum(sub[[cls_col]] == "alerta", na.rm = TRUE)
      pct <- round((n_alerta / n_total) * 100, 1)
      rows[[idx]] <- data.frame(
        Bloco        = bloco,
        Subteste     = st$code,
        Escolaridade = ESC_LABELS[as.character(esc)],
        N_total      = n_total,
        N_alerta     = n_alerta,
        Pct_alerta   = pct,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }
  bind_rows(rows)
}

alertas_comp <- calc_alertas(data_comp, SUBTESTS_COMP, "MACb Completo")
alertas_inic <- calc_alertas(data_inic, SUBTESTS_INIC, "Discurso Inicial")
alertas_flue <- calc_alertas(data_flue, SUBTESTS_FLUE, "Fluência Verbal")
alertas_narr <- calc_alertas(data_narr, SUB_NARR_CORTE, "Discurso Narrativo")

alertas_all <- bind_rows(alertas_comp, alertas_inic, alertas_flue, alertas_narr)

# ------ Salvar resultados ------
cat("8. Salvando resultados em RDS...\n")

saveRDS(list(
  # Dados limpos
  data_comp = data_comp,
  data_inic = data_inic,
  data_flue = data_flue,
  data_narr = data_narr,

  # Descritiva
  desc_comp = desc_comp,
  desc_inic = desc_inic,
  desc_flue = desc_flue,
  desc_narr = desc_narr,

  # Kruskal-Wallis + post-hoc
  kw_comp  = kw_comp,
  kw_inic  = kw_inic,
  kw_flue  = kw_flue,
  kw_narr  = kw_narr,
  kw_infer = kw_infer,
  kw_resumo = kw_resumo,

  # Fisher (Inferência)
  fisher_infer = fisher_infer,

  # Alertas
  alertas_all = alertas_all,

  # Definições de subtestes
  subtests_comp = SUBTESTS_COMP,
  subtests_inic = SUBTESTS_INIC,
  subtests_flue = SUBTESTS_FLUE,
  subtests_narr = SUBTESTS_NARR
), file.path(RESULTS_DIR, "resultados_macb_escolaridade.rds"))

cat("\n====================================================\n")
cat(" Análise finalizada com sucesso!\n")
cat(sprintf(" Resultados: %s\n", file.path(RESULTS_DIR, "resultados_macb_escolaridade.rds")))
cat(sprintf(" Gráficos:   %s\n", PLOTS_DIR))
cat("====================================================\n")
