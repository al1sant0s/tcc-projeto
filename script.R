##############################################################################
# CONFIGURAÇÃO INICIAL
##############################################################################
#| echo: false
library("pacman")
p_load("tidyverse", "rio", "gt", "labelled", "moments", "janitor", "corrr", "ggcorrplot", "patchwork", "scales")
p_load("tidymodels", "dotwhisker", "skimr", "car", "forcats")
options(scipen = 999)

##############################################################################
# INTRODUÇÃO
##############################################################################

# ==============================================================================
# O conjunto de dados
# ==============================================================================

#| label: database-import

# Leitura da base
credit_risk <- import("credit_risk_dataset.csv", headers = TRUE) |>
    mutate(
        loan_status           = factor(loan_status, levels = c(0, 1), labels = c("Adimplente", "Inadimplente")),
        person_home_ownership = as.factor(person_home_ownership),
        loan_intent           = as.factor(loan_intent),
        loan_grade            = as.factor(loan_grade),
        cb_person_default_on_file = as.factor(cb_person_default_on_file)
    )


# Traduzir variáveis.
nomes_legiveis <- c(
    "person_age" = "Idade",
    "person_income" = "Renda anual",
    "person_home_ownership" = "Tipo de moradia",
    "person_emp_length" = "Tempo no emprego atual",
    "loan_intent" = "Finalidade do empréstimo",
    "loan_grade" = "Nível de risco",
    "loan_amnt" = "Valor do empréstimo",
    "loan_int_rate" = "Taxa de juros",
    "loan_status" = "Situação do empréstimo",
    "loan_percent_income" = "Comprometimento da renda",
    "cb_person_default_on_file" = "Registro de inadimplência anterior",
    "cb_person_cred_hist_length" = "Histórico de crédito (anos)"
)

var_label(credit_risk) <- nomes_legiveis

# Traduzir labels
credit_risk <- credit_risk |>
  mutate(
    # Fator com ordem explícita e rótulos legíveis
    person_home_ownership = factor(
      person_home_ownership,
      levels = c("RENT", "MORTGAGE", "OWN", "OTHER"),
      labels = c("Alugada", "Financiada", "Própria", "Outro")
    ),
    loan_intent = factor(
      loan_intent,
      levels = c("EDUCATION", "MEDICAL", "VENTURE", "PERSONAL", 
                 "DEBTCONSOLIDATION", "HOMEIMPROVEMENT"),
      labels = c("Educação", "Saúde", "Empreendimento", "Uso pessoal", 
                 "Consolidação de dívidas", "Reforma imobiliária")
    ),
    # Ordem natural mantida
    loan_grade = factor(loan_grade),
    # 'Não' como nível de referência para a regressão
    cb_person_default_on_file = factor(
      cb_person_default_on_file,
      levels = c("N", "Y"),
      labels = c("Não", "Sim")
    )
  )

# Separacao dos nomes das variaveis por tipo
resposta <- "loan_status"

categoricas <- c(
  "person_home_ownership",
  "loan_intent",
  "loan_grade",
  "cb_person_default_on_file"
)

numericas <- setdiff(
  names(select(credit_risk, where(is.numeric))),
  resposta
)

# ----------------------------------------------------------------------------
# Perfil do tomador
# ----------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# Características do empréstimo
# ----------------------------------------------------------------------------

# ----------------------------------------------------------------------------
# Informações de risco
# ----------------------------------------------------------------------------

##############################################################################
# ANÁLISE EXPLORATÓRIA
##############################################################################

# ==============================================================================
# Desbalanceamento da variável resposta
# ==============================================================================

# ==============================================================================
# Limpeza preliminar dos dados
# ==============================================================================

#| out-width: 100%
#| label: fig-piechart-loan-status-preview
#| fig-cap: Distribuição da situação do empréstimo

credit_risk |>
    count(loan_status) |>
    mutate(perc = n / sum(n) * 100) |>
    ggplot(aes(x = "", y = n, fill = fct_rev(loan_status))) +
    geom_bar(stat = "identity", width = 1, color = "white", linewidth = 1) +
    coord_polar("y", start = 1.5) +
    theme_void() +
    geom_text(
        aes(label = paste0(round(perc, 2), '%')),
        position = position_stack(vjust = 0.5),
        color = "white",
        fontface = "bold",
        size = 5
    ) +
    scale_fill_brewer(palette = "Set1") +
    labs(
        fill = "Situação do empréstimo",
    ) +
    theme(
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
    )

#| label: tbl-desc-analysis-summary-nondefault-preview
#| tbl-cap: Medidas descritivas para adimplentes

medidas_descritivas_nondefault <- credit_risk |>
    filter(loan_status == "Adimplente") |>
    select(all_of(numericas)) |>
    pivot_longer(cols = everything(), names_to = "variavel", values_to = "valor") |>
    group_by(variavel) |>
    summarise(
        mean = mean(valor, na.rm = TRUE),
        sd = sd(valor, na.rm = TRUE),
        min = min(valor, na.rm = TRUE),
        q1 = quantile(valor, 0.25, na.rm = TRUE),
        median = median(valor, na.rm = TRUE),
        q3 = quantile(valor, 0.75, na.rm = TRUE),
        max = max(valor, na.rm = TRUE)
    ) |>
    mutate(var_name = variavel) |>
    mutate(across(-c(variavel, var_name), \(x) round(x, 2))) |>
    mutate(variavel = coalesce(nomes_legiveis[variavel], variavel))

medidas_descritivas_nondefault |>
    select(-var_name) |>
    gt(rowname_col = "variavel") |>
    tab_stubhead("Variável") |>
    cols_label(
        mean = "Média",
        sd = "Desvio padrão",
        min = "Mínimo",
        q1 = md("$q_1$"),
        median = "Mediana",
        q3 = md("$q_3$"),
        max = "Máximo"
    ) |>
    cols_align(align = "center", columns = everything()) |>
    tab_options(
        table.width = "65%",
        column_labels.font.weight = "bold",
        column_labels.padding = "10px"
    ) |>
    tab_source_note(
        source_note = md("**Fonte**: base de dados.")
    )


#| label: tbl-desc-analysis-summary-default-preview
#| tbl-cap: Medidas descritivas para inadimplentes

medidas_descritivas_default <- credit_risk |>
    filter(loan_status == "Inadimplente") |>
    select(all_of(numericas)) |>
    pivot_longer(cols = everything(), names_to = "variavel", values_to = "valor") |>
    group_by(variavel) |>
    summarise(
        mean = mean(valor, na.rm = TRUE),
        sd = sd(valor, na.rm = TRUE),
        min = min(valor, na.rm = TRUE),
        q1 = quantile(valor, 0.25, na.rm = TRUE),
        median = median(valor, na.rm = TRUE),
        q3 = quantile(valor, 0.75, na.rm = TRUE),
        max = max(valor, na.rm = TRUE)
    ) |>
    mutate(var_name = variavel) |>
    mutate(across(-c(variavel, var_name), \(x) round(x, 2))) |>
    mutate(variavel = coalesce(nomes_legiveis[variavel], variavel))

medidas_descritivas_default |>
    select(-var_name) |>
    gt(rowname_col = "variavel") |>
    tab_stubhead("Variável") |>
    cols_label(
        mean = "Média",
        sd = "Desvio padrão",
        min = "Mínimo",
        q1 = md("$q_1$"),
        median = "Mediana",
        q3 = md("$q_3$"),
        max = "Máximo"
    ) |>
    cols_align(align = "center", columns = everything()) |>
    tab_options(
        table.width = "65%",
        column_labels.font.weight = "bold",
        column_labels.padding = "10px"
    ) |>
    tab_source_note(
        source_note = md("**Fonte**: base de dados.")
    )

#| out-width: 100%
#| label: fig-missing-values
#| fig-cap: Composição de valores faltantes por variável em relação à base de dados

missing_values <- credit_risk |>
  summarise(across(everything(), \(x) sum(is.na(x)))) |>
  pivot_longer(cols = everything(), names_to = "variavel", values_to = "na") |>
  filter(na > 0) |>
  mutate(
    perc_na  = na * 100 / nrow(credit_risk),
    variavel = coalesce(nomes_legiveis[variavel], variavel) # Traduz os nomes
  )

ggplot(missing_values, aes(x = reorder(variavel, -na), y = na)) +
    geom_col(fill = "steelblue", alpha = 0.85, show.legend = FALSE) +
    geom_text(aes(label = na, vjust = -0.5)) +
    geom_text(
        aes(label = paste0("(", round(perc_na, 2), "%)")),
        vjust = 1.5,
        color = "white"
    ) +
    ylim(0, max(missing_values$na) * 1.1) + # Garante espaço para o rótulo
    labs(x = "Variável", y = "Observações faltantes") +
    theme_minimal()

credit_risk_omit <- credit_risk |>
    na.omit()

#| label: fig-boxplot-age-and-person-emp-length-preview
#| fig-cap: "Distribuição das idades e tempos  de emprego dos tomadores de crédito (prévia)"
#| layout-ncol: 2
#| fig-subcap:
#|   - Distribuição das idades dos tomadores de crédito
#|   - Distribuição dos tempos de emprego dos tomadores de crédito

ggplot(credit_risk_omit, aes(y = person_age)) +
  geom_boxplot(
    outlier.color = "red",
    outlier.size = 1,
    fill = "steelblue",
    width = 0.5,
    show.legend = FALSE
  ) +
  scale_y_continuous(breaks = seq(20, 140, by = 20)) +
  labs(y = "Idade (anos)") +
  theme_minimal(base_size = 11)

ggplot(credit_risk_omit, aes(y = person_emp_length)) +
  geom_boxplot(
    outlier.color = "red",
    outlier.size = 1,
    fill = "steelblue",
    width = 0.5,
    show.legend = FALSE
  ) +
  scale_y_continuous(breaks = seq(20, 140, by = 20)) +
  labs(y = "Tempo de emprego (anos)") +
  theme_minimal(base_size = 11)

#| label: person-age-chunk

# 1. Filtro na base bruta
credit_risk_filtered <- credit_risk |>
  filter(
    person_age >= 20,
    person_age <= 40,
    person_age - person_emp_length >= 16
  )

new_person_age__lte_40 <- nrow(credit_risk_filtered)
new_person_age_prop__lte_40 <- new_person_age__lte_40 / nrow(credit_risk)

# 2. Remoção de NA e cálculo proporcional
credit_risk__omit <- na.omit(credit_risk)

credit_risk_clean <- credit_risk__omit |>
  filter(
    person_age >= 20,
    person_age <= 40,
    person_age - person_emp_length >= 16
  )

new_person_age__omit__lte_40 <- nrow(credit_risk_clean)
new_person_age_prop__omit__lte_40 <- new_person_age__omit__lte_40 / nrow(credit_risk__omit)

#| out-width: 100%
#| label: fig-piechart-loan-status
#| fig-cap: Distribuição da situação do empréstimo

credit_risk_clean |>
    count(loan_status) |>
    mutate(perc = n / sum(n) * 100) |>
    ggplot(aes(x = "", y = n, fill = fct_rev(loan_status))) +
    geom_bar(stat = "identity", width = 1, color = "white", linewidth = 1) +
    coord_polar("y", start = 1.5) +
    theme_void() +
    geom_text(
        aes(label = paste0(round(perc, 2), '%')),
        position = position_stack(vjust = 0.5),
        color = "white",
        fontface = "bold",
        size = 5
    ) +
    scale_fill_brewer(palette = "Set1") +
    labs(
        fill = "Situação do empréstimo",
    ) +
    theme(
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14)
    )

#| label: credit-risk-clean

age_lt_emp_length <- sum(credit_risk_clean$person_age < credit_risk_clean$person_emp_length)
age_lt_cb_person_cred_hist_length <- sum(credit_risk_clean$person_age < credit_risk_clean$cb_person_cred_hist_length)
max_diff_ratio_loan_percent_income <- max(abs(credit_risk$loan_amnt / credit_risk$person_income - credit_risk$loan_percent_income))

#| label: tbl-desc-analysis-summary-nondefault
#| tbl-cap: Medidas descritivas para adimplentes

medidas_descritivas_nondefault <- credit_risk_clean |>
    filter(loan_status == "Adimplente") |>
    select(all_of(numericas)) |>
    pivot_longer(cols = everything(), names_to = "variavel", values_to = "valor") |>
    group_by(variavel) |>
    summarise(
        mean = mean(valor, na.rm = TRUE),
        sd = sd(valor, na.rm = TRUE),
        min = min(valor, na.rm = TRUE),
        q1 = quantile(valor, 0.25, na.rm = TRUE),
        median = median(valor, na.rm = TRUE),
        q3 = quantile(valor, 0.75, na.rm = TRUE),
        max = max(valor, na.rm = TRUE)
    ) |>
    mutate(var_name = variavel) |>
    mutate(across(-c(variavel, var_name), \(x) round(x, 2))) |>
    mutate(variavel = coalesce(nomes_legiveis[variavel], variavel))

medidas_descritivas_nondefault |>
    select(-var_name) |>
    gt(rowname_col = "variavel") |>
    tab_stubhead("Variável") |>
    cols_label(
        mean = "Média",
        sd = "Desvio padrão",
        min = "Mínimo",
        q1 = md("$q_1$"),
        median = "Mediana",
        q3 = md("$q_3$"),
        max = "Máximo"
    ) |>
    cols_align(align = "center", columns = everything()) |>
    tab_options(
        table.width = "65%",
        column_labels.font.weight = "bold",
        column_labels.padding = "10px"
    ) |>
    tab_source_note(
        source_note = md("**Fonte**: base de dados.")
    )


#| label: tbl-desc-analysis-summary-default
#| tbl-cap: Medidas descritivas para inadimplentes

medidas_descritivas_default <- credit_risk_clean |>
    filter(loan_status == "Inadimplente") |>
    select(all_of(numericas)) |>
    pivot_longer(cols = everything(), names_to = "variavel", values_to = "valor") |>
    group_by(variavel) |>
    summarise(
        mean = mean(valor, na.rm = TRUE),
        sd = sd(valor, na.rm = TRUE),
        min = min(valor, na.rm = TRUE),
        q1 = quantile(valor, 0.25, na.rm = TRUE),
        median = median(valor, na.rm = TRUE),
        q3 = quantile(valor, 0.75, na.rm = TRUE),
        max = max(valor, na.rm = TRUE)
    ) |>
    mutate(var_name = variavel) |>
    mutate(across(-c(variavel, var_name), \(x) round(x, 2))) |>
    mutate(variavel = coalesce(nomes_legiveis[variavel], variavel))

medidas_descritivas_default |>
    select(-var_name) |>
    gt(rowname_col = "variavel") |>
    tab_stubhead("Variável") |>
    cols_label(
        mean = "Média",
        sd = "Desvio padrão",
        min = "Mínimo",
        q1 = md("$q_1$"),
        median = "Mediana",
        q3 = md("$q_3$"),
        max = "Máximo"
    ) |>
    cols_align(align = "center", columns = everything()) |>
    tab_options(
        table.width = "65%",
        column_labels.font.weight = "bold",
        column_labels.padding = "10px"
    ) |>
    tab_source_note(
        source_note = md("**Fonte**: base de dados.")
    )


#| label: tbl-desc-analysis-summary
#| tbl-cap: Medidas descritivas para tomadores de empréstimo

medidas_descritivas <- credit_risk_clean |>
    select(all_of(numericas)) |>
    pivot_longer(cols = everything(), names_to = "variavel", values_to = "valor") |>
    group_by(variavel) |>
    summarise(
        mean = mean(valor, na.rm = TRUE),
        sd = sd(valor, na.rm = TRUE),
        min = min(valor, na.rm = TRUE),
        q1 = quantile(valor, 0.25, na.rm = TRUE),
        median = median(valor, na.rm = TRUE),
        q3 = quantile(valor, 0.75, na.rm = TRUE),
        max = max(valor, na.rm = TRUE)
    ) |>
    mutate(var_name = variavel) |>
    mutate(across(-c(variavel, var_name), \(x) round(x, 2))) |>
    mutate(variavel = coalesce(nomes_legiveis[variavel], variavel))

medidas_descritivas |>
    select(-var_name) |>
    gt(rowname_col = "variavel") |>
    tab_stubhead("Variável") |>
    cols_label(
        mean = "Média",
        sd = "Desvio padrão",
        min = "Mínimo",
        q1 = md("$q_1$"),
        median = "Mediana",
        q3 = md("$q_3$"),
        max = "Máximo"
    ) |>
    cols_align(align = "center", columns = everything()) |>
    tab_options(
        table.width = "65%",
        column_labels.font.weight = "bold",
        column_labels.padding = "10px"
    ) |>
    tab_source_note(
        source_note = md("**Fonte**: base de dados.")
    )


# ==============================================================================
# Análise individual
# ==============================================================================

# ----------------------------------------------------------------------------
# Idade
# ----------------------------------------------------------------------------

#| out-width: 100%
#| label: fig-boxplot-age
#| fig-cap: Distribuição das idades por situação de empréstimo

ggplot(credit_risk_clean, aes(x = loan_status, y = person_age, fill = loan_status)) +
    geom_boxplot(
        outlier.color = "red",
        outlier.size = 1,
        width = 0.5,
        show.legend = FALSE
    ) +
    scale_y_continuous(breaks = seq(20, 140, by = 20)) +
    scale_fill_manual(values = c("Adimplente" = "#9ecae1", "Inadimplente" = "#6baed6")) +
    labs(
        x = "Situação do empréstimo",
        y = "Idade (anos)"
    ) +
    theme_minimal(base_size = 11)

#| out-width: 100%
#| label: fig-hist-person-age
#| fig-cap: Distribuição das idades

credit_risk_clean |>
    ggplot(aes(x = person_age)) +
    geom_histogram(binwidth = 5, fill = "steelblue", color = "white", alpha = 0.8) +
    theme_minimal(base_size = 11) +
    labs(
        x = "Idade",
        y = "Frequência"
    )

# ----------------------------------------------------------------------------
# Renda anual
# ----------------------------------------------------------------------------

#| out-width: 100%
#| label: fig-boxplot-person-income
#| fig-cap: Distribuição da renda anual por situação de empréstimo

credit_risk_clean |>
    na.omit() |>
    ggplot(aes(x = loan_status, y = person_income, fill = loan_status)) +
    geom_boxplot(
        outlier.color = "red",
        outlier.size = 1,
        width = 0.5,
        show.legend = FALSE
    ) +
    scale_fill_manual(values = c("Adimplente" = "#9ecae1", "Inadimplente" = "#6baed6")) +
    labs(
        x = "Situação do empréstimo",
        y = "Renda anual"
    ) +
    theme_minimal(base_size = 11)

#| out-width: 100%
#| label: fig-boxplot-log-person-income
#| fig-cap: Distribuição do log da renda anual por situação de empréstimo

credit_risk_clean |>
    na.omit() |>
    ggplot(aes(x = loan_status, y = log(person_income), fill = loan_status)) +
    geom_boxplot(
        outlier.color = "red",
        outlier.size = 1,
        width = 0.5,
        show.legend = FALSE
    ) +
    scale_fill_manual(values = c("Adimplente" = "#9ecae1", "Inadimplente" = "#6baed6")) +
    labs(
        x = "Situação do empréstimo",
        y = "ln(Renda anual)"
    ) +
    theme_minimal(base_size = 11)

#| out-width: 100%
#| label: fig-hist-person-income-versus
#| fig-cap: Distribuição da renda anual em escala original e logarítmica
#| fig-subcap:
#|  - "Escala bruta"
#|  - "Escala logarítmica (ln)"
#| layout-ncol: 2

gghist_person_income <- credit_risk_clean |>
    na.omit() |>
    ggplot(aes(x = person_income)) +
    geom_histogram(bins = 15, fill = "steelblue", color = "white", alpha = 0.8) +
    theme_minimal(base_size = 11) +
    labs(
        x = "Tempo em anos",
        y = "Frequência"
    )

gghist_log_person_income <- credit_risk_clean |>
    mutate(log_person_income = log(person_income)) |>
    na.omit() |>
    ggplot(aes(x = log_person_income)) +
    geom_histogram(bins = 15, fill = "steelblue", color = "white", alpha = 0.8) +
    theme_minimal(base_size = 11) +
    labs(
        x = "ln(renda anual)",
        y = "Frequência"
    )

gghist_person_income
gghist_log_person_income

# ----------------------------------------------------------------------------
# Tempo no emprego atual
# ----------------------------------------------------------------------------

#| out-width: 100%
#| label: fig-boxplot-person-emp-length
#| fig-cap: Distribuição dos tempos no emprego do tomador por situação de empréstimo

credit_risk_clean |>
    na.omit() |>
    ggplot(aes(x = loan_status, y = person_emp_length, fill = loan_status)) +
    geom_boxplot(
        outlier.color = "red",
        outlier.size = 1,
        width = 0.5,
        show.legend = FALSE
    ) +
    scale_fill_manual(values = c("Adimplente" = "#9ecae1", "Inadimplente" = "#6baed6")) +
    labs(
        x = "Situação do empréstimo",
        y = "Tempo no emprego atual (anos)"
    ) +
    theme_minimal(base_size = 11)

#| out-width: 100%
#| label: fig-bar-person-emp-length
#| fig-cap: Distribuição dos tempos no emprego do tomador

credit_risk_clean |>
    na.omit() |>
    ggplot(aes(x = person_emp_length)) +
    geom_bar(fill = "steelblue", color = "white", alpha = 0.8) +
    theme_minimal(base_size = 11) +
    labs(
        x = "Tempo em anos",
        y = "Frequência"
    )

# ----------------------------------------------------------------------------
# Valor do empréstimo
# ----------------------------------------------------------------------------

#| out-width: 100%
#| label: fig-boxplot-loan-amnt
#| fig-cap: Distribuição dos empréstimos por situação de empréstimo

credit_risk_clean |>
    ggplot(aes(x = loan_status, y = loan_amnt, fill = loan_status)) +
    geom_boxplot(
        outlier.color = "red",
        outlier.size = 1,
        width = 0.5,
        show.legend = FALSE
    ) +
    scale_fill_manual(values = c("Adimplente" = "#9ecae1", "Inadimplente" = "#6baed6")) +
    labs(
        x = "Situação do empréstimo",
        y = "Valor do empréstimo"
    ) +
    theme_minimal(base_size = 11)

#| out-width: 100%
#| label: fig-hist-loan-amnt
#| fig-cap: Distribuição dos empréstimos

credit_risk_clean |>
    ggplot(aes(x = loan_amnt)) +
    geom_histogram(bins = 15, fill = "steelblue", color = "white", alpha = 0.8) +
    theme_minimal(base_size = 11) +
    labs(
        x = "Valor do empréstimo",
        y = "Frequência"
    )

# ----------------------------------------------------------------------------
# Taxa de juros
# ----------------------------------------------------------------------------

#| out-width: 100%
#| label: fig-boxplot-loan-int-rate
#| fig-cap: Distribuição das taxas de juros dos empréstimos contratados por situação de empréstimo

credit_risk_clean |>
    na.omit() |>
    ggplot(aes(x = loan_status, y = loan_int_rate, fill = loan_status)) +
    geom_boxplot(
        outlier.color = "red",
        outlier.size = 1,
        width = 0.5,
        show.legend = FALSE
    ) +
    scale_fill_manual(values = c("Adimplente" = "#9ecae1", "Inadimplente" = "#6baed6")) +
    labs(
        x = "Situação do empréstimo",
        y = "Taxa de juros"
    ) +
    theme_minimal(base_size = 11)

#| out-width: 100%
#| label: fig-hist-loan-int-rate
#| fig-cap: Distribuição das taxas de juros dos empréstimos contratados

credit_risk_clean |>
    na.omit() |>
    ggplot(aes(x = loan_int_rate)) +
    geom_histogram(bins = 12, fill = "steelblue", color = "white", alpha = 0.8) +
    theme_minimal(base_size = 11) +
    labs(
        x = "Taxa de juros",
        y = "Frequência"
    )

# ----------------------------------------------------------------------------
# Comprometimento da renda
# ----------------------------------------------------------------------------

#| out-width: 100%
#| label: fig-boxplot-loan-percent-income
#| fig-cap: Distribuição do comprometimento da renda por situação de empréstimo

credit_risk_clean |>
    na.omit() |>
    ggplot(aes(x = loan_status, y = loan_percent_income, fill = loan_status)) +
    geom_boxplot(
        outlier.color = "red",
        outlier.size = 1,
        width = 0.5,
        show.legend = FALSE
    ) +
    scale_fill_manual(values = c("Adimplente" = "#9ecae1", "Inadimplente" = "#6baed6")) +
    labs(
        x = "Situação do empréstimo",
        y = "Comprometimento da renda"
    ) +
    theme_minimal(base_size = 11)

#| out-width: 100%
#| label: fig-hist-loan-percent-income
#| fig-cap: Distribuição do comprometimento da renda

credit_risk_clean |>
    na.omit() |>
    ggplot(aes(x = loan_percent_income)) +
    geom_histogram(bins = 12, fill = "steelblue", color = "white", alpha = 0.8) +
    theme_minimal(base_size = 11) +
    labs(
        x = "Comprometimento da renda",
        y = "Frequência"
    )

# ----------------------------------------------------------------------------
# Histórico de crédito
# ----------------------------------------------------------------------------

#| out-width: 100%
#| label: fig-boxplot-cb-person-cred-hist-length
#| fig-cap: Distribuição do histórico de crédito por situação de empréstimo

credit_risk_clean |>
    na.omit() |>
    ggplot(aes(x = loan_status, y = cb_person_cred_hist_length, fill = loan_status)) +
    geom_boxplot(
        outlier.color = "red",
        outlier.size = 1,
        width = 0.5,
        show.legend = FALSE
    ) +
    scale_fill_manual(values = c("Adimplente" = "#9ecae1", "Inadimplente" = "#6baed6")) +
    labs(
        x = "Situação do empréstimo",
        y = "Histórico de crédito (anos)"
    ) +
    theme_minimal(base_size = 11)

#| out-width: 100%
#| label: fig-hist-cb-person-cred-hist-length
#| fig-cap: Distribuição do histórico de crédito

credit_risk_clean |>
    na.omit() |>
    ggplot(aes(x = cb_person_cred_hist_length)) +
    geom_histogram(bins = 12, fill = "steelblue", color = "white", alpha = 0.8) +
    theme_minimal(base_size = 11) +
    labs(
        x = "Histórico de crédito (anos)",
        y = "Frequência"
    )

# ----------------------------------------------------------------------------
# Tipo de moradia
# ----------------------------------------------------------------------------

#| out-width: 100%
#| label: fig-hbar-person-home-ownership
#| fig-cap: Distribuição do tipo de moradia

#dist_freq_person_home_ownership <- credit_risk_clean |>
#    count(person_home_ownership) |>
#    mutate(ncum = cumsum(n)) |>
#    mutate(perc = round(n/sum(n) * 100, digits = 4)) |>
#    mutate(perccum = round(cumsum(perc), digits = 4))
#
#ggplot(dist_freq_person_home_ownership, aes(x = "", y = perc, fill = person_home_ownership)) +
#    geom_bar(stat = "identity", width = 1, color = "white") +
#    coord_polar("y", start = 1.5) +
#    theme_void() +
#    geom_text(aes(label = round(perc, 2)), position = position_stack(vjust = 0.5), color = "black", fontface = "bold") +
#    scale_fill_brewer(
#        palette="Dark2",
#        labels = c(
#            "RENT"     = "Alugada",
#            "OWN"      = "Própria",
#            "MORTGAGE" = "Financiada",
#            "OTHER"    = "Outro"
#        )
#    ) +
#    labs(fill = "Tipo de moradia")

ggplot(credit_risk_clean, aes(y = fct_rev(fct_infreq(person_home_ownership)))) +
  geom_bar(fill = "steelblue", width = 0.7) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = "Quantidade de empréstimos",
    y = "Tipo de moradia"
  )

#| out-width: 100%
#| label: tbl-dblentry-person-home-owner-ship
#| tbl-cap: Distribuição conjunta das proporções da situação de empréstimo conforme o tipo de moradia do tomador

tbl_contigency <- credit_risk_clean |>
    tabyl(person_home_ownership, loan_status) |> 
    adorn_totals("col", name = "Total") |>
    mutate(across(-1, \(x) round(x * 100 / Total, 2)))

tbl_contigency |>
    gt(rowname_col = "person_home_ownership") |>
    tab_stubhead("Tipo de moradia") |>
    tab_spanner(
        columns = c("Adimplente", "Inadimplente", "Total"),
        label = "Situação do empréstimo (%)"
    ) |>
    cols_align(align = "center", columns = everything()) |>
    tab_options(
        table.width = "65%",
        column_labels.font.weight = "bold",
        column_labels.padding = "10px"
    ) |>
    tab_source_note(
        source_note = md("**Fonte**: base de dados.")
    )

# ----------------------------------------------------------------------------
# Finalidade do empréstimo
# ----------------------------------------------------------------------------

#| out-width: 100%
#| label: fig-hbar-loan-intent
#| fig-cap: Distribuição da finalidade do empréstimo

ggplot(credit_risk_clean, aes(y = fct_rev(fct_infreq(loan_intent)))) +
  geom_bar(fill = "steelblue", width = 0.7) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = "Quantidade de empréstimos",
    y = "Finalidade"
  )

#| out-width: 100%
#| label: tbl-dblentry-loan-intent
#| tbl-cap: Distribuição conjunta das proporções da situação de empréstimo conforme a finalidade

tbl_contigency <- credit_risk_clean |>
    tabyl(loan_intent, loan_status) |> 
    adorn_totals("col", name = "Total") |>
    mutate(across(-1, \(x) round(x * 100 / Total, 2)))

tbl_contigency |>
    gt(rowname_col = "loan_intent") |>
    tab_stubhead("Finalidade") |>
    tab_spanner(
        columns = c("Adimplente", "Inadimplente", "Total"),
        label = "Situação do empréstimo (%)"
    ) |>
    cols_align(align = "center", columns = everything()) |>
    tab_options(
        table.width = "65%",
        column_labels.font.weight = "bold",
        column_labels.padding = "10px"
    ) |>
    tab_source_note(
        source_note = md("**Fonte**: base de dados.")
    )

# ----------------------------------------------------------------------------
# Classificação de risco
# ----------------------------------------------------------------------------

#| out-width: 100%
#| label: fig-hbar-loan-grade
#| fig-cap: Distribuição da classificação de risco

ggplot(credit_risk_clean, aes(y = fct_rev(fct_infreq(loan_grade)))) +
  geom_bar(fill = "steelblue", width = 0.7) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    x = "Quantidade de empréstimos",
    y = "Risco"
  )

#| out-width: 100%
#| label: tbl-dblentry-loan_grade
#| tbl-cap: Distribuição conjunta das proporções da situação de empréstimo conforme a classificação de risco

tbl_contigency <- credit_risk_clean |>
    tabyl(loan_grade, loan_status) |> 
    adorn_totals("col", name = "Total") |>
    mutate(across(-1, \(x) round(x * 100 / Total, 2)))

tbl_contigency |>
    gt(rowname_col = "loan_grade") |>
    tab_stubhead("Risco") |>
    tab_spanner(
        columns = c("Adimplente", "Inadimplente", "Total"),
        label = "Situação do empréstimo (%)"
    ) |>
    cols_align(align = "center", columns = everything()) |>
    tab_options(
        table.width = "65%",
        column_labels.font.weight = "bold",
        column_labels.padding = "10px"
    ) |>
    tab_source_note(
        source_note = md("**Fonte**: base de dados.")
    )

# ----------------------------------------------------------------------------
# Registro de inadimplência anterior
# ----------------------------------------------------------------------------

#| label: cb-person-default-on-file

count_cb_person_default_on_file <- credit_risk_clean |>
    count(cb_person_default_on_file) |>
    mutate(perc = n * 100 / sum(n))

count_cb_person_default_on_file_yes <- count_cb_person_default_on_file |>
    filter(cb_person_default_on_file == "Sim")

count_cb_person_default_on_file_no <- count_cb_person_default_on_file |>
    filter(cb_person_default_on_file == "Não")

#| out-width: 100%
#| label: tbl-dblentry-cb-person-default-on-file
#| tbl-cap: Distribuição conjunta das proporções da situação de empréstimo conforme o registro de inadimplência anterior

tbl_contigency <- credit_risk_clean |>
    mutate(cb_person_default_on_file = fct_rev(cb_person_default_on_file)) |>
    tabyl(cb_person_default_on_file, loan_status) |> 
    adorn_totals("col", name = "Total") |>
    mutate(across(-1, \(x) round(x * 100 / Total, 2)))

tbl_contigency |>
    gt(rowname_col = "cb_person_default_on_file") |>
    tab_stubhead("Registro") |>
    tab_spanner(
        columns = c("Adimplente", "Inadimplente", "Total"),
        label = "Situação do empréstimo (%)"
    ) |>
    cols_align(align = "center", columns = everything()) |>
    tab_options(
        table.width = "65%",
        column_labels.font.weight = "bold",
        column_labels.padding = "10px"
    ) |>
    tab_source_note(
        source_note = md("**Fonte**: base de dados.")
    )

# ----------------------------------------------------------------------------
# Situação do empréstimo
# ----------------------------------------------------------------------------

#| label: loan-status

count_loan_status <- credit_risk_clean |>
    count(loan_status) |>
    mutate(perc = n * 100 / sum(n))

count_loan_status_default <- count_loan_status |>
    filter(loan_status == "Inadimplente")

count_loan_status_paid <- count_loan_status |>
    filter(loan_status == "Adimplente")

# ==============================================================================
# Análise conjunta
# ==============================================================================

# ----------------------------------------------------------------------------
# Variáveis quantitativas
# ----------------------------------------------------------------------------

#| out-width: 100%
#| label: fig-corr-matrix
#| fig-cap: Matriz de correlação
#| warning: false

## Matriz de correlação.
Mcorr <- cor(select(credit_risk_clean, all_of(numericas)))
colnames(Mcorr) <- nomes_legiveis[colnames(Mcorr)]
rownames(Mcorr) <- nomes_legiveis[rownames(Mcorr)]

ggcorrplot(
    Mcorr,
    method = "square",
    type = "lower",          # Mostra apenas o triângulo inferior
    lab = TRUE,              # Exibe os coeficientes numéricos
    lab_size = 3.2,          # Tamanho legível dos números
    digits = 2,              # 2 casas decimais
    outline.col = "white",
    show.legend = TRUE,
    legend.title = "Correlação (r)",
    colors = c("#6D9EC1", "white", "#E46726") # Azul (negativa), Branco (nula), Laranja (positiva)
    ) +
    theme_minimal(base_size = 11) +
    theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.grid = element_blank() # Remove linhas de fundo redundantes
    ) +
    labs(x = NULL, y = NULL)

# ----------------------------------------------------------------------------
# Variáveis categóricas
# ----------------------------------------------------------------------------

#| label: fig-associacao-categoricas
#| fig-cap: Distribuição proporcional entre pares de variáveis categóricas explicativas
#| fig-width: 10
#| fig-height: 11
#| out-width: 100%


# 1. Função geradora aproveitando o vetor 'nomes_legiveis'
criar_grafico_100 <- function(dados, x_var, fill_var, paleta = "Blues") {
  ggplot(dados, aes(x = .data[[x_var]], fill = .data[[fill_var]])) +
    geom_bar(position = "fill", width = 0.7, color = "white", linewidth = 0.2) +
    scale_y_continuous(labels = label_percent(), expand = c(0, 0, 0.05, 0)) +
    scale_fill_brewer(palette = paleta) +
    labs(
      x = nomes_legiveis[x_var],
      y = "Proporção",
      fill = nomes_legiveis[fill_var]
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      axis.title = element_text(size = 9),
      axis.text.x = element_text(angle = 35, hjust = 1, size = 8),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
}

# 2. Geração dos 6 pares únicos C(4, 2) = 6
p1 <- criar_grafico_100(credit_risk_clean, "person_home_ownership", "loan_intent", "Set2")
p2 <- criar_grafico_100(credit_risk_clean, "person_home_ownership", "loan_grade", "YlOrRd")
p3 <- criar_grafico_100(credit_risk_clean, "person_home_ownership", "cb_person_default_on_file", "Blues")
p4 <- criar_grafico_100(credit_risk_clean, "loan_intent", "loan_grade", "YlOrRd")
p5 <- criar_grafico_100(credit_risk_clean, "loan_intent", "cb_person_default_on_file", "Blues")
p6 <- criar_grafico_100(credit_risk_clean, "loan_grade", "cb_person_default_on_file", "Blues")

# 3. Composição do painel 3x2
painel_pares_cat <- (p1 | p2) /
                    (p3 | p4) /
                    (p5 | p6)


painel_pares_cat




##############################################################################
# Ajuste de modelos
##############################################################################

# Split.
set.seed(2026)
split      <- initial_split(credit_risk_clean, prop = 0.7, strata = loan_status)
train_data <- training(split)
test_data  <- testing(split)

# ==============================================================================
# Logistic model.
# Variable selection with GVIF and CV.
# ==============================================================================

# Base model for GVIF verification.
glm_check_vif <- glm(loan_status ~ . - person_income + log(person_income),
  family = binomial,
  data = train_data
) |>
  vif()

# Problems detected here.
glm_check_vif |>
  as.data.frame()

# Remove loan_amnt and adjust two models, one without loan_int_rate and one without loan_grade
# and check which of the two has the best performance.

# Model, recip, workflow.
logit_mod <- logistic_reg() |>
  set_engine("glm")

rec_exclude_int_rate <- recipe(loan_status ~ ., data = train_data) |>
  step_log(person_income) |>
  step_rm(loan_amnt, loan_int_rate) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors())

wf_exclude_int_rate <-
  workflow() |>
  add_model(logit_mod) |>
  add_recipe(rec_exclude_int_rate)

# Repeated 10-fold CV.
set.seed(2026)
folds <- vfold_cv(train_data, v = 10, repeats = 5, strata = loan_status)

# Metrics used throughout.
risk_metrics <- metric_set(roc_auc, brier_class, mn_log_loss)

exclude_int_rate_rs <-
  wf_exclude_int_rate |>
  fit_resamples(
    folds,
    metrics = risk_metrics,
    control = control_resamples(event_level = "second")
  )

# Logit model without grade.
rec_exclude_grade <- recipe(loan_status ~ ., data = train_data) |>
  step_log(person_income) |>
  step_rm(loan_amnt, loan_grade) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors())

wf_exclude_grade <-
  workflow() |>
  add_model(logit_mod) |>
  add_recipe(rec_exclude_grade)

exclude_grade_rs <-
  wf_exclude_grade |>
  fit_resamples(
    folds,
    metrics = risk_metrics,
    control = control_resamples(event_level = "second")
  )

# Collect results.
# Without int_rate vs without grade.
bind_rows(
  collect_metrics(exclude_int_rate_rs) |> mutate(model = "Keeps loan_grade"),
  collect_metrics(exclude_grade_rs)    |> mutate(model = "Keeps loan_int_rate")
) |>
  select(model, .metric, mean, std_err)

# Now adjusting the default recipe
logit_rec <- recipe(loan_status ~ ., data = train_data) |>
  step_log(person_income) |>
  step_rm(loan_amnt, loan_int_rate) |>
  step_mutate(loan_grade = fct_collapse(loan_grade, "F-G" = c("F", "G"))) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors())

logit_wf <-
  workflow() |>
  add_model(logit_mod) |>
  add_recipe(logit_rec)

logit_fit_rs <-
  logit_wf |>
  fit_resamples(
    folds,
    metrics = risk_metrics,
    control = control_resamples(event_level = "second")
  )

logit_fit_rs_metrics <- collect_metrics(logit_fit_rs)
logit_fit_rs_metrics

# Final fit on training data, evaluated on test data.
logit_final <-
logit_wf |>
last_fit(
  split,
  metrics = risk_metrics,
  control = control_last_fit(event_level = "second")
)

collect_metrics(logit_final)

# Coefficients as odds ratios (for defaulting).
logit_final |>
extract_fit_parsnip() |>
tidy(exponentiate = TRUE, conf.int = TRUE)

# ROC curve.
collect_predictions(logit_final) |>
roc_curve(truth = loan_status, .pred_Inadimplente, event_level = "second") |>
autoplot()


# ==============================================================================
# Probit model.
# ==============================================================================

# Model, recipe, workflow.
probit_mod <-
  logistic_reg() |>
  set_engine("glm", family = binomial(link = "probit"))

probit_rec <- logit_rec

probit_wf <-
  workflow() |>
  add_model(probit_mod) |>
  add_recipe(probit_rec)

probit_fit_rs <-
  probit_wf |>
  fit_resamples(
    folds,
    metrics = risk_metrics,
    control = control_resamples(event_level = "second")
  )

probit_fit_rs_metrics <- collect_metrics(probit_fit_rs)
probit_fit_rs_metrics

# ==============================================================================
# Cloglog model.
# ==============================================================================

# Model, recipe, workflow.
cloglog_mod <-
  logistic_reg() |>
  set_engine("glm", family = binomial(link = "cloglog"))

cloglog_rec <- logit_rec

cloglog_wf <-
  workflow() |>
  add_model(cloglog_mod) |>
  add_recipe(cloglog_rec)

cloglog_fit_rs <-
  cloglog_wf |>
  fit_resamples(
    folds,
    metrics = risk_metrics,
    control = control_resamples(event_level = "second")
  )

# Investigate warning.
fit(cloglog_wf, data = train_data) |>
  extract_fit_parsnip() |>
  tidy() |>
  arrange(desc(abs(std.error)))

cloglog_preds <- fit(cloglog_wf, data = train_data) |>
  augment(train_data)

cloglog_preds |>
  filter(.pred_Inadimplente > 1 - 1e-10) |>
  count(loan_status)


# Collect CV metrics.
cloglog_fit_rs_metrics <- collect_metrics(cloglog_fit_rs)
cloglog_fit_rs_metrics

# ==============================================================================
# Compare and choose a final model.
# ==============================================================================

# Collect results.
# Without int_rate vs without grade.
bind_rows(
  logit_fit_rs_metrics |> mutate(model = "logit", .before = 1),
  probit_fit_rs_metrics |> mutate(model = "probit", .before = 1),
  cloglog_fit_rs_metrics |> mutate(model = "cloglog", .before = 1)
) |>
  select(model, .metric, mean, std_err) |>
  print(digits = 9)

# Final fit on training data, evaluated on test data.
probit_final <-
  probit_wf |>
  last_fit(
    split,
    metrics = risk_metrics,
    control = control_last_fit(event_level = "second")
  )

collect_metrics(probit_final)

probit_final |>
  extract_fit_parsnip() |>
  tidy(conf.int = TRUE)

# ROC curve.
collect_predictions(probit_final) |>
  roc_curve(truth = loan_status, .pred_Inadimplente, event_level = "second") |>
  autoplot()
