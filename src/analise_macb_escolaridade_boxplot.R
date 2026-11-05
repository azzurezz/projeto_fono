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

# Converter dados em formato longo para boxplots
extract_long_data <- function(data, subtests_def) {
  rows <- list()
  idx <- 1
  for (st in subtests_def) {
    o_col <- paste0(st$code, "_o")
    corte_col <- paste0(st$code, "_corte")
    if (!o_col %in% names(data)) next

    for (i in 1:nrow(data)) {
      esc_num <- data$Escolaridade[i]
      if (is.na(esc_num)) next
      sc <- data[[o_col]][i]
      c_val <- if (corte_col %in% names(data)) data[[corte_col]][i] else NA

      rows[[idx]] <- data.frame(
        Participante = data$Participante[i],
        Escolaridade = ESC_LABELS[as.character(esc_num)],
        Esc_num      = esc_num,
        Subteste     = st$code,
        Tarefa       = st$name,
        Pontuacao    = sc,
        Corte        = c_val,
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }
  bind_rows(rows)
}

# ==============================================================================
# Função de gráfico: Boxplots em blocos de 4 (2x2)
# Média: Linha azul sólida com valor ao lado
# Corte: Linha vermelha sólida com valor ao lado
# ==============================================================================

theme_esc <- theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 9.5, hjust = 0.5, color = "gray30"),
    axis.title    = element_text(face = "bold", size = 10),
    axis.text.x   = element_text(angle = 20, hjust = 1, size = 9),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom",
    legend.box       = "horizontal",
    strip.text = element_text(face = "bold", size = 10)
  )

plot_boxplot_chunk <- function(long_df, titulo, subtitulo, draw_corte = TRUE) {

  plot_df <- long_df %>%
    filter(!is.na(Pontuacao)) %>%
    mutate(
      Escolaridade = factor(Escolaridade, levels = c("5-8 anos", "9-11 anos", "12+ anos")),
      Subteste     = factor(Subteste, levels = unique(Subteste))
    )

  means_df <- plot_df %>%
    group_by(Subteste, Escolaridade) %>%
    summarise(
      Media = mean(Pontuacao, na.rm = TRUE),
      Corte = ifelse(draw_corte && "Corte" %in% names(plot_df), suppressWarnings(mean(Corte, na.rm = TRUE)), NA),
      .groups = "drop"
    ) %>%
    mutate(
      Corte = ifelse(is.nan(Corte), NA, Corte),
      esc_num = as.numeric(factor(Escolaridade, levels = c("5-8 anos", "9-11 anos", "12+ anos"))),
      media_vjust = ifelse(!is.na(Corte) & Media < Corte, 1.35, -0.35),
      corte_vjust = ifelse(!is.na(Corte) & Media < Corte, -0.35, 1.35)
    )

  p <- ggplot(plot_df, aes(x = Escolaridade, y = Pontuacao)) +
    geom_boxplot(aes(fill = Escolaridade), width = 0.45, alpha = 0.35, color = "#4B5563",
                 outlier.shape = 21, outlier.size = 1.8, outlier.fill = "#9CA3AF") +
    geom_jitter(aes(color = Escolaridade), width = 0.10, alpha = 0.40, size = 1.2, show.legend = FALSE)

  has_corte <- draw_corte && any(!is.na(means_df$Corte))

  # 1. Linha azul sólida de MÉDIA + Valor ao lado
  p <- p +
    geom_segment(data = means_df,
                 aes(x = esc_num - 0.24, xend = esc_num + 0.24, y = Media, yend = Media, linetype = "Média"),
                 color = "#1D4ED8", linewidth = 0.9) +
    geom_text(data = means_df,
              aes(x = esc_num + 0.27, y = Media, label = sprintf("%.1f", Media), vjust = media_vjust),
              color = "#1D4ED8", size = 3.1, fontface = "bold", hjust = 0)

  # 2. Linha vermelha sólida de CORTE + Valor ao lado
  if (has_corte) {
    means_corte <- filter(means_df, !is.na(Corte))
    p <- p +
      geom_segment(data = means_corte,
                   aes(x = esc_num - 0.24, xend = esc_num + 0.24, y = Corte, yend = Corte, linetype = "Ponto de corte"),
                   color = "#DC2626", linewidth = 0.9) +
      geom_text(data = means_corte,
                aes(x = esc_num + 0.27, y = Corte, label = sprintf("%.1f", Corte), vjust = corte_vjust),
                color = "#DC2626", size = 3.1, fontface = "bold", hjust = 0)
  }

  lt_values  <- if (has_corte) c("Média" = "solid", "Ponto de corte" = "solid") else c("Média" = "solid")
  lt_colors  <- if (has_corte) c("#1D4ED8", "#DC2626") else "#1D4ED8"

  p <- p +
    facet_wrap(~ Subteste, scales = "free_y", ncol = 2) +
    scale_fill_manual(values = ESC_COLORS, name = "Escolaridade") +
    scale_color_manual(values = ESC_COLORS) +
    scale_linetype_manual(values = lt_values, name = NULL) +
    scale_x_discrete(expand = expansion(mult = c(0.15, 0.35))) +
    scale_y_continuous(expand = expansion(mult = c(0.08, 0.22))) +
    labs(title = titulo, subtitle = subtitulo,
         x = "Grupo de Escolaridade", y = "Pontuação") +
    theme_esc +
    guides(
      fill = guide_legend(order = 1),
      linetype = guide_legend(order = 2, override.aes = list(color = lt_colors, linewidth = 1.0))
    )

  p
}

generate_chunked_plots <- function(long_df, prefix_filename, title_prefix, draw_corte = TRUE, max_per_plot = 4) {
  unique_subtests <- unique(long_df$Subteste)
  n_sub <- length(unique_subtests)
  num_chunks <- ceiling(n_sub / max_per_plot)
  saved_files <- c()

  for (c in 1:num_chunks) {
    start_idx <- (c - 1) * max_per_plot + 1
    end_idx   <- min(c * max_per_plot, n_sub)
    sub_chunk <- unique_subtests[start_idx:end_idx]

    df_chunk <- long_df %>% filter(Subteste %in% sub_chunk)
    part_str <- if (num_chunks > 1) sprintf(" (Parte %d de %d)", c, num_chunks) else ""
    fname    <- if (num_chunks > 1) sprintf("%s_part%d.png", prefix_filename, c) else sprintf("%s.png", prefix_filename)

    p <- plot_boxplot_chunk(
      df_chunk,
      titulo = paste0(title_prefix, part_str),
      subtitulo = "Boxplot = Mediana/IQR  |  Linha azul = Média  |  Linha vermelha = Ponto de corte",
      draw_corte = draw_corte
    )

    h_val <- if (length(sub_chunk) <= 2) 5.5 else 9.5
    w_val <- 10.5
    filepath <- file.path(PLOTS_DIR, fname)
    ggsave(filepath, plot = p, width = w_val, height = h_val, dpi = 300)
    saved_files <- c(saved_files, fname)
  }
  saved_files
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

long_comp <- extract_long_data(data_comp, SUBTESTS_COMP)
long_inic <- extract_long_data(data_inic, SUBTESTS_INIC)
long_flue <- extract_long_data(data_flue, SUBTESTS_FLUE)
long_narr <- extract_long_data(data_narr, SUBTESTS_NARR)

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

# ------ Gráficos em blocos de 4 ------
cat("5. Gerando gráficos em blocos de 4 com Média e Corte em linhas sólidas...\n")

files_comp <- generate_chunked_plots(long_comp, "medias_macb_completo_esc", "MACb Completo: distribuição por escolaridade")
files_inic <- generate_chunked_plots(long_inic, "medias_disc_inicial_esc", "Discurso Inicial: distribuição por escolaridade")
files_flue <- generate_chunked_plots(long_flue, "medias_fluencia_verbal_esc", "Fluência Verbal: distribuição por escolaridade")

long_narr_corte <- long_narr %>% filter(Subteste != "Infer")
files_narr <- generate_chunked_plots(long_narr_corte, "medias_disc_narrativo_esc", "Discurso Narrativo: distribuição por escolaridade")

long_infer <- long_narr %>% filter(Subteste == "Infer")
files_infer <- generate_chunked_plots(long_infer, "medias_inferencia_esc", "Inferência: distribuição por escolaridade", draw_corte = FALSE)

# ------ Tabela resumo KW ------
cat("6. Compilando resumo dos testes de Kruskal-Wallis...\n")

kw_resumo <- bind_rows(
  kw_comp$kw %>% mutate(Bloco = "MACb Completo"),
  kw_inic$kw %>% mutate(Bloco = "Discurso Inicial"),
  kw_flue$kw %>% mutate(Bloco = "Fluência Verbal"),
  kw_narr$kw %>% mutate(Bloco = "Discurso Narrativo"),
  kw_infer$kw %>% mutate(Bloco = "Discurso Narrativo")
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
