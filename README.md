
# Projeto Fono - Análise Estatística MACb

Projeto de análise estatística descritiva e inferencial não paramétrica para dados quantitativos dos testes MACb em Fonoaudiologia e Neuropsicologia ($N = 47$ participantes).

---

## Objetivos do Projeto

1. **Estatística Descritiva Completa**: Cálculo de Média, Mediana, Desvio-Padrão, Valor Mínimo, Valor Máximo e Intervalos Interquartis (Q1 e Q3) para todas as tarefas e dimensões dos testes.
2. **Avaliação de Dificuldade das Tarefas**: Identificação das tarefas com maior e menor nível de dificuldade utilizando o **teste não paramétrico de Friedman** (para medidas repetidas) com tamanho de efeito (**W de Kendall**).
3. **Análise Post-Hoc**: Testes pareados de Wilcoxon com correção de **Holm-Bonferroni** para determinar quais pares de tarefas possuem diferenças significativas de desempenho.
4. **Relatório Profissional em PDF**: Automação da compilação de relatórios formatados em PDF via R Markdown e LaTeX.

---

## Estrutura do Repositório

```text
projeto_fono/
├── data/
│   ├── Tabulação MACb - quantitativo reformulada.xlsx   # Dados brutos em Excel (amostra geral)
│   ├── MACB_escolaridade_corte.xlsx                     # Dados com escolaridade e corte
│   ├── relatorio_macb.pdf                               # Relatório final geral em PDF
│   ├── relatorio_macb_escolaridade.pdf                  # Relatório de escolaridade e corte em PDF
│   ├── plots/                                           # Gráficos de alta resolução (ggplot2)
│   │   └── escolaridade/                                # Boxplots por escolaridade com corte
│   └── results/                                         # Resultados numéricos exportados (RDS)
│       ├── resultados_macb.rds
│       └── resultados_macb_escolaridade.rds
├── report/
│   ├── relatorio_macb.Rmd                               # Template R Markdown geral
│   └── relatorio_macb_escolaridade.Rmd                  # Template R Markdown por escolaridade
├── src/
│   ├── analise_macb.R                                   # Script principal de análise geral
│   └── analise_macb_escolaridade.R                      # Script de análise por escolaridade
└── README.md
```

---

## Como Executar os Scripts

### Pré-requisitos (Linguagem R)

Certifique-se de possuir o **R** (versão $\ge 4.0$) e o **pandoc** instalados.

Pacotes R necessários:

- `readxl`, `dplyr`, `tidyr`, `ggplot2`, `knitr`, `rmarkdown`

### Execução via R

1. **Executar a análise de dados geral:**

   ```bash
   Rscript src/analise_macb.R
   ```

2. **Executar a análise por escolaridade e pontos de corte:**

   ```bash
   Rscript src/analise_macb_escolaridade.R
   Rscript -e "rmarkdown::render('report/relatorio_macb_escolaridade.Rmd', output_file = '../data/relatorio_macb_escolaridade.pdf')"
   ```

   *Os relatórios em PDF `data/relatorio_macb.pdf` e `data/relatorio_macb_escolaridade.pdf` serão gerados na pasta `data/`.*
