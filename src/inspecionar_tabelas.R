# Inspecionar estruturas das tabelas dos dois relatorios
res1 <- readRDS("data/results/resultados_macb.rds")
res2 <- readRDS("data/results/resultados_macb_escolaridade.rds")

cat("=== OBJETOS EM RES1 (MACb Geral) ===\n")
print(names(res1))

cat("\n=== RES1: Friedman Summary ===\n")
print(res1$res_friedman_summary)

cat("\n=== RES1: Ranking (head) ===\n")
print(head(res1$df_ranking, 3))

cat("\n=== OBJETOS EM RES2 (Escolaridade) ===\n")
print(names(res2))
