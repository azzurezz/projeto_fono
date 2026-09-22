# Projeto Fono - Análise Estatística MACb

Projeto de análise estatística descritiva e inferencial não paramétrica para dados quantitativos da **Bateria MACb (Bateria MAC Breve)** em Fonoaudiologia e Neuropsicologia ($N = 47$ participantes).

---

## 📋 Objetivos do Projeto

1. **Estatística Descritiva Completa**: Cálculo de Média, Mediana, Desvio-Padrão, Valor Mínimo, Valor Máximo e Intervalos Interquartis (Q1 e Q3) para todos os subtestes e dimensões da bateria.
2. **Avaliação de Dificuldade de Itens/Subtestes**: Identificação dos subtestes com maior e menor nível de dificuldade utilizando o **teste não paramétrico de Friedman** (para medidas repetidas) com tamanho de efeito (**W de Kendall**).
3. **Análise Post-Hoc**: Testes pareados de Wilcoxon com correção de **Holm-Bonferroni** para determinar quais pares de subtestes possuem diferenças significativas de desempenho.
4. **Relatório Profissional em PDF**: Automação da compilação de relatórios formatados em PDF via R Markdown e LaTeX.

---

## 📊 Principais Resultados

### Ranking Geral de Dificuldade dos Subtestes

| Rank | Código | Subteste | Categoria | Média Obtida | Máximo | % Acerto Médio | % Dificuldade Média |
| :---: | :---: | :--- | :--- | :---: | :---: | :---: | :---: |
| **1** | `IP` | Ideias Principais | Discurso Narrativo | 5.30 | 18 | **29,43%** | **70,57%** |
| **2** | `InfL` | Informações Lembradas | Discurso Narrativo | 7.85 | 26 | **30,19%** | **69,81%** |
| **3** | `Infer` | Inferências (Sim/Não) | Discurso Narrativo | 0.36 | 1 | **36,17%** | **63,83%** |
| **4** | `CompT` | Compreensão do Texto | Discurso Narrativo | 6.64 | 16 | **41,49%** | **58,51%** |
| **5** | `IME` | Metáfora - Explicação | MACb Completo | 6.70 | 12 | **55,85%** | **44,15%** |
| **6** | `PEP` | Prosódia Emocional - Produção | MACb Completo | 3.81 | 6 | **63,48%** | **36,52%** |
| **7** | `JSe` | Julgamento Semântico - Explicação | MACb Completo | 3.91 | 6 | **65,25%** | **34,75%** |
| **8** | `E` | Expressão | Discurso Inicial | 11.23 | 16 | **70,21%** | **29,79%** |
| **9** | `IMA` | Metáfora - Alternativas | MACb Completo | 4.23 | 6 | **70,57%** | **29,43%** |
| **10** | `ATe` | Atos de Fala - Explicação | MACb Completo | 8.64 | 12 | **71,99%** | **28,01%** |
| **11** | `ATa` | Atos de Fala - Alternativas | MACb Completo | 4.85 | 6 | **80,85%** | **19,15%** |
| **12** | `JSi` | Julgamento Semântico - Identificação | MACb Completo | 5.15 | 6 | **85,82%** | **14,18%** |
| **13** | `PLE` | Prosódia Linguística Emocional | Discurso Inicial | 12.04 | 14 | **86,02%** | **13,98%** |
| **14** | `C` | Compreensão | Discurso Inicial | 6.94 | 8 | **86,70%** | **13,30%** |
| **15** | `CNV` | Comportamento Não Verbal | Discurso Inicial | 5.49 | 6 | **91,49%** | **8,51%** |

---

## 📁 Estrutura do Repositório

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

## 🚀 Como Executar os Scripts

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

### Execução Alternativa via Python

Se preferir utilizar Python (`pandas`, `scipy`, `statsmodels`, `seaborn`):

```bash
python3 src/analise_macb.py
```
