# ==============================================================================
# 0. PACOTES E LEITURA DOS DADOS
# ==============================================================================

# Instale os pacotes caso nao possua:
# install.packages(c("tidyverse", "corrplot", "patchwork"))

library("tidyverse")
library("corrplot")
library("rio")
options(scipen=9999)

# Leitura da base
# ==============================================================================
# 0. PACOTES E LEITURA DOS DADOS
# ==============================================================================

# Instale os pacotes caso nao possua:
# install.packages(c("tidyverse", "corrplot", "patchwork"))

# Leitura da base
credit_risk <- import("credit_risk_dataset.csv", headers = TRUE)

x <- credit_risk |> filter(person_age <= 40) |>
  na.omit() |>
  nrow()

# Separacao dos nomes das variaveis por tipo
resposta <- "loan_status"

categoricas <- c(
  "person_home_ownership",
  "loan_intent",
  "loan_grade",
  "cb_person_default_on_file"
)

numericas <- setdiff(
  names(select(dados, where(is.numeric))),
  resposta
)


# ==============================================================================
# 1. TABELA DE MEDIDAS DESCRITIVAS (NUMÉRICAS, EXCETO RESPOSTA)
# ==============================================================================

medidas_descritivas <- credit_risk |>
  select(numericas) |>
  pivot_longer(cols = everything(), names_to = "variavel", values_to = "valor") |>
  group_by(variavel) |>
  summarise(
    na = sum(is.na(valor)),
    mean = mean(valor, na.rm = TRUE),
    sd = sd(valor, na.rm = TRUE),
    min = min(valor, na.rm = TRUE),
    q1 = quantile(valor, 0.25, na.rm = TRUE),
    median = median(valor, na.rm = TRUE),
    q3 = quantile(valor, 0.75, na.rm = TRUE),
    max = max(valor, na.rm = TRUE)
  ) |>
  mutate(perc_na = na/nrow(credit_risk) * 100, .before = 3)

print("Medidas descritivas das variaveis numericas:")
print(medidas_descritivas)

# Análise da distribuição dos dados da amostra.
#
# Todos os dados são positivos. A faixa de valores é plausível em todos os dados, exceto em person_age, onde o valor máximo
# sugere existência de pontos discrepantes. Por meio de um box plot, focamos em verificar a distribuição desta variável específica.

ggplot(dados, aes(y = person_age)) +
  geom_boxplot(fill = "#9ecae1", color = "#08519c", outlier.color = "red", outlier.size = 1) +
  theme_minimal(base_size = 11) +
  labs(
    title = "Boxplots das Variáveis Numéricas",
    y = "Valor",
    x = NULL
  ) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Evidentemente, as observações de 144 anos são outliers, mais especificamente, pontos discrepantes frutos de erro de registro ou mensuração.
# 


dados |>
  ggplot(aes(x = person_age)) +
  geom_histogram(bins = 30, fill = "#3182bd", color = "white", alpha = 0.8) +
  theme_minimal(base_size = 11) +
  labs(
    title = "Histograma das Variáveis Numéricas",
    x = "Valor",
    y = "Frequência"
  )


# ==============================================================================
# 1. VALORES FALTANTES POR VARIÁVEL
# ==============================================================================

missing_summary <- dados %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variavel",
    values_to = "n_faltantes"
  ) %>%
  mutate(pct_faltante = round((n_faltantes / nrow(dados)) * 100, 2)) %>%
  arrange(desc(n_faltantes))

print("Valores faltantes por variavel:")
print(missing_summary)



# ==============================================================================
# 3. GRÁFICO DE BARRAS PARA A VARIÁVEL RESPOSTA (loan_status)
# ==============================================================================

credit_risk |>
  mutate(loan_status = factor(loan_status, labels = c("Não Inadimplente (0)", "Inadimplente (1)"))) %>%
  ggplot(aes(x = loan_status, fill = loan_status)) +
  geom_bar(color = "black", alpha = 0.85, show.legend = FALSE) +
  geom_text(
    stat = "count",
    aes(label = after_stat(paste0(count, " (", round(count / sum(count) * 100, 1), "%)"))),
    vjust = -0.5,
    size = 4
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = c("#2b8cbe", "#e34a33")) +
  theme_minimal(base_size = 13) +
  labs(
    title = "Distribuição da Variável Resposta (loan_status)",
    x = "Status do Empréstimo",
    y = "Frequência Absoluta"
  )


# ==============================================================================
# 4. HISTOGRAMAS DAS VARIÁVEIS NUMÉRICAS (EXCETO RESPOSTA)
# ==============================================================================

dados %>%
  select(all_of(vars_numericas)) %>%
  pivot_longer(cols = everything(), names_to = "variavel", values_to = "valor") %>%
  ggplot(aes(x = valor)) +
  geom_histogram(bins = 30, fill = "#3182bd", color = "white", alpha = 0.8) +
  facet_wrap(~ variavel, scales = "free", ncol = 3) +
  theme_minimal(base_size = 11) +
  labs(
    title = "Histograma das Variáveis Numéricas",
    x = "Valor",
    y = "Frequência"
  )


# ==============================================================================
# 5. BOXPLOTS DAS VARIÁVEIS NUMÉRICAS (EXCETO RESPOSTA)
# ==============================================================================

dados %>%
  select(all_of(vars_numericas)) %>%
  pivot_longer(cols = everything(), names_to = "variavel", values_to = "valor") %>%
  ggplot(aes(y = valor)) +
  geom_boxplot(fill = "#9ecae1", color = "#08519c", outlier.color = "red", outlier.size = 1) +
  facet_wrap(~ variavel, scales = "free", ncol = 3) +
  theme_minimal(base_size = 11) +
  labs(
    title = "Boxplots das Variáveis Numéricas",
    y = "Valor",
    x = NULL
  ) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())


# ==============================================================================
# 6. GRÁFICOS DE SETOR (PIZZA) PARA VARIÁVEIS CATEGÓRICAS
# ==============================================================================

# Gerando e exibindo um grafico de setor para cada categorica
for (var in vars_categoricas) {
  p <- dados %>%
    count(.data[[var]]) %>%
    mutate(pct = n / sum(n) * 100,
           rotulo = paste0(.data[[var]], "\n", round(pct, 1), "%")) %>%
    ggplot(aes(x = "", y = n, fill = .data[[var]])) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    geom_text(aes(label = rotulo), position = position_stack(vjust = 0.5), size = 3.5) +
    theme_void(base_size = 12) +
    theme(legend.position = "none") +
    labs(title = paste("Gráfico de Setores -", var))
  
  print(p)
}


# ==============================================================================
# 7. MATRIZ DE CORRELAÇÃO (VARIÁVEIS NUMÉRICAS)
# ==============================================================================

cor_matrix <- dados %>%
  select(all_of(vars_numericas)) %>%
  cor(use = "pairwise.complete.obs")

print("Matriz de correlacao numerica:")
print(round(cor_matrix, 2))

# Visualizacao grafica da correlacao
corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  number.cex = 0.75,
  col = colorRampPalette(c("#d73027", "#f7f7f7", "#1a9850"))(200),
  title = "Matriz de Correlação das Variáveis Numéricas",
  mar = c(0, 0, 2, 0)
)


# ==============================================================================
# 8. TRÊS TABELAS DE DUPLA ENTRADA (CROSSTABS)
# ==============================================================================

# Tabela 1: Tipo de Moradia vs. Grau do Empréstimo
tab1 <- table(dados$person_home_ownership, dados$loan_grade)
print("--- Tabela 1: Moradia vs. Grau do Empréstimo ---")
print(tab1)
print("Em proporcao marginal por linha (%):")
print(round(prop.table(tab1, margin = 1) * 100, 2))

# Tabela 2: Finalidade do Empréstimo vs. Grau do Empréstimo
tab2 <- table(dados$loan_intent, dados$loan_grade)
print("--- Tabela 2: Finalidade vs. Grau do Empréstimo ---")
print(tab2)
print("Em proporcao marginal por linha (%):")
print(round(prop.table(tab2, margin = 1) * 100, 2))

# Tabela 3: Histórico de Inadimplência vs. Tipo de Moradia
tab3 <- table(dados$cb_person_default_on_file, dados$person_home_ownership)
print("--- Tabela 3: Inadimplência Registrada vs. Moradia ---")
print(tab3)
print("Em proporcao marginal por linha (%):")
print(round(prop.table(tab3, margin = 1) * 100, 2))dados <- read_csv("credit_risk_dataset.csv")
