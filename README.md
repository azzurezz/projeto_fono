# Projeto Fono - Análise Estatística MACb

Projeto de análise estatística descritiva e inferencial não paramétrica para dados quantitativos da **Bateria MACb** em Fonoaudiologia e Neuropsicologia ($N = 47$ participantes).

---

## Objetivos do Projeto

1. **Estatística Descritiva Completa**: Cálculo de Média, Mediana, Desvio-Padrão, Valor Mínimo, Valor Máximo e Intervalos Interquartis (Q1 e Q3) para todos os subtestes e dimensões da bateria.
2. **Avaliação de Dificuldade de Itens/Subtestes**: Identificação dos subtestes com maior e menor nível de dificuldade utilizando o **teste não paramétrico de Friedman** (para medidas repetidas) com tamanho de efeito (**W de Kendall**).
3. **Análise Post-Hoc**: Testes pareados de Wilcoxon com correção de **Holm-Bonferroni** para determinar quais pares de subtestes possuem diferenças significativas de desempenho.
4. **Relatório Profissional em PDF**: Automação da compilação de relatórios formatados em PDF via R Markdown e LaTeX.

---

## Estrutura do Repositório

```text
projeto_fono/
├── data/
│   ├── Tabulação MACb - quantitativo reformulada.xlsx   # Dados brutos em Excel
│   ├── relatorio_estatistico_macb.pdf                   # Relatório final compilado em PDF
│   ├── relatorio_estatistico_macb.md                    # Relatório estruturado em Markdown
│   ├── plots/                                           # Gráficos de alta resolução (ggplot2 / seaborn)
│   │   ├── ranking_geral_dificuldade.png
│   │   ├── dificuldade_macb_completo.png
│   │   ├── dificuldade_discurso_narrativo.png
│   │   ├── dificuldade_discurso_inicial.png
│   │   └── curva_fluencia_verbal.png
│   └── results/                                         # Resultados numéricos exportados
│       ├── analise_estatistica_macb.xlsx
│       └── resultados_macb.rds
├── report/
│   └── relatorio_macb.Rmd                               # Template R Markdown para PDF
├── src/
│   ├── analise_macb.R                                   # Script principal de análise em R
│   ├── gerar_relatorio_pdf.R                            # Script de geração do PDF
│   └── analise_macb.py                                  # Script alternativo em Python
└── README.md
```

---

## Como Executar os Scripts

### Pré-requisitos (Linguagem R)

Certifique-se de possuir o **R** (versão $\ge 4.0$) e o **pandoc** instalados.

Pacotes R necessários:

- `readxl`, `dplyr`, `tidyr`, `ggplot2`, `knitr`, `rmarkdown`

### Execução via R (Recomendado)

1. **Executar a análise de dados e gerar os gráficos:**

   ```bash
   Rscript src/analise_macb.R
   ```

2. **Compilar o relatório em PDF:**

   ```bash
   Rscript src/gerar_relatorio_pdf.R
   ```

   *O arquivo `data/relatorio_estatistico_macb.pdf` será gerado automaticamente.*
