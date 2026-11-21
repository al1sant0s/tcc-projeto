##############################################################################
# Script R extraído de relatorio.qmd (apenas os blocos de código R)
##############################################################################

##############################################################################
# Configuração inicial
##############################################################################
library("pacman")
p_load("tidyverse", "rio", "gt", "labelled", "moments", "janitor", "corrr", "ggcorrplot", "patchwork", "scales", "hexbin")
p_load("tidymodels", "dotwhisker", "skimr", "car", "forcats")
options(scipen = 999)

##############################################################################
# O conjunto de dados
##############################################################################

# --- database-import ---
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
    "person_income" = "Renda pessoal",
    "person_home_ownership" = "Tipo de moradia",
    "person_emp_length" = "Tempo no emprego atual",
    "loan_intent" = "Finalidade do empréstimo",
    "loan_grade" = "Risco de inadimplência",
    "loan_amnt" = "Valor do empréstimo",
    "loan_int_rate" = "Taxa de juros",
    "loan_status" = "Situação do empréstimo",
    "loan_percent_income" = "Comprometimento da renda",
    "cb_person_default_on_file" = "Registro de inadimplência anterior",
    "cb_person_cred_hist_length" = "Histórico de crédito"
)

var_label(credit_risk) <- nomes_legiveis

# Traduzir labels
credit_risk <- credit_risk |>
  mutate(
    # Fator com ordem explícita e rótulos legíveis
    person_home_ownership = factor(
      person_home_ownership,
      levels = c("RENT", "MORTGAGE", "OWN", "OTHER"),
      labels = c("Alugada", "Hipoteca", "Própria", "Outro")
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

# Separação dos nomes das variáveis por tipo
resposta <- "loan_status"

preditoras <- credit_risk |>
    select(-all_of(resposta)) |>
    colnames()

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

##############################################################################
# Limpeza preliminar dos dados
##############################################################################

# --- fig-piechart-loan-status-preview ---
credit_risk |>
    count(loan_status) |>
    mutate(perc = n / sum(n) * 100) |>
    ggplot(aes(x = "", y = n, fill = fct_rev(loan_status))) +
    geom_bar(stat = "identity", width = 1, color = "white", linewidth = 1) +
    coord_polar("y", start = 1.5) +
    theme_void() +
    geom_text(
        aes(label = paste0(format(round(perc, 2), decimal.mark = ","), '%')),
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

# --- tbl-desc-analysis-summary-nondefault-preview ---
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
    tab_source_note(
        source_note = md("**Fonte**: base de dados.")
    ) |>
    cols_align(align = "right", columns = everything()) |>
    cols_align(align = "left", columns = variavel) |>
    tab_style(
        style = cell_text(align = "left"),
        locations = cells_stubhead()
    ) |>
    tab_style(
        style = cell_text(align = "left"),
        locations = list(cells_source_notes(), cells_footnotes())
    ) |>
    tab_options(
        table.width = "100%",
        column_labels.font.weight = "bold",
        column_labels.padding = "10px",
        source_notes.font.size = px(12),
        footnotes.font.size = px(12)
    ) |>
    fmt_number(
        columns = c(mean, sd, min, q1, median, q3, max),
        decimals = 2,
        dec_mark = ",",
        sep_mark = "."
    )

# --- tbl-desc-analysis-summary-default-preview ---
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
    tab_source_note(
        source_note = md("**Fonte**: base de dados.")
    ) |>
    cols_align(align = "right", columns = everything()) |>
    cols_align(align = "left", columns = variavel) |>
    tab_style(
        style = cell_text(align = "left"),
        locations = cells_stubhead()
    ) |>
    tab_style(
        style = cell_text(align = "left"),
        locations = list(cells_source_notes(), cells_footnotes())
    ) |>
    tab_options(
        table.width = "100%",
        column_labels.font.weight = "bold",
        column_labels.padding = "10px",
        source_notes.font.size = px(12),
        footnotes.font.size = px(12)
    ) |>
    fmt_number(
        columns = c(mean, sd, min, q1, median, q3, max),
        decimals = 2,
        dec_mark = ",",
        sep_mark = "."
    )

# --- fig-missing-values ---
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
    geom_text(aes(label = format(na, big.mark = "."), vjust = -0.5)) +
    geom_text(
        aes(label = paste0("(", format(round(perc_na, 2), decimal.mark = ","), "%)")),
        vjust = 1.5,
        color = "white"
    ) +
    ylim(0, max(missing_values$na) * 1.1) + # Garante espaço para o rótulo
    labs(x = "Variável", y = "Observações faltantes") +
    theme_minimal()

credit_risk_omit <- credit_risk |>
    na.omit()

# --- fig-boxplot-age-and-person-emp-length-preview ---
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

# --- person-age-chunk ---
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

# --- fig-piechart-loan-status ---
credit_risk_clean |>
    count(loan_status) |>
    mutate(perc = n / sum(n) * 100) |>
    ggplot(aes(x = "", y = n, fill = fct_rev(loan_status))) +
    geom_bar(stat = "identity", width = 1, color = "white", linewidth = 1) +
    coord_polar("y", start = 1.5) +
    theme_void() +
    geom_text(
        aes(label = paste0(format(round(perc, 2), decimal.mark = ","), '%')),
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

# --- credit-risk-clean ---
age_lt_emp_length <- sum(credit_risk_clean$person_age < credit_risk_clean$person_emp_length)
age_lt_cb_person_cred_hist_length <- sum(credit_risk_clean$person_age < credit_risk_clean$cb_person_cred_hist_length)
max_diff_ratio_loan_percent_income <- max(abs(credit_risk$loan_amnt / credit_risk$person_income - credit_risk$loan_percent_income))

# --- tbl-desc-analysis-summary-nondefault ---
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
    tab_source_note(
        source_note = md("**Fonte**: base de dados.")
    ) |>
    cols_align(align = "right", columns = everything()) |>
    cols_align(align = "left", columns = variavel) |>
    tab_style(
        style = cell_text(align = "left"),
        locations = cells_stubhead()
    ) |>
    tab_style(
        style = cell_text(align = "left"),
        locations = list(cells_source_notes(), cells_footnotes())
    ) |>
    tab_options(
        table.width = "100%",
        column_labels.font.weight = "bold",
        column_labels.padding = "10px",
        source_notes.font.size = px(12),
        footnotes.font.size = px(12)
    ) |>
    fmt_number(
        columns = c(mean, sd, min, q1, median, q3, max),
        decimals = 2,
        dec_mark = ",",
        sep_mark = "."
    )

# --- tbl-desc-analysis-summary-default ---
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
    tab_source_note(
        source_note = md("**Fonte**: base de dados.")
    ) |>
    cols_align(align = "right", columns = everything()) |>
    cols_align(align = "left", columns = variavel) |>
    tab_style(
        style = cell_text(align = "left"),
        locations = cells_stubhead()
    ) |>
    tab_style(
        style = cell_text(align = "left"),
        locations = list(cells_source_notes(), cells_footnotes())
    ) |>
    tab_options(
        table.width = "100%",
        column_labels.font.weight = "bold",
        column_labels.padding = "10px",
        source_notes.font.size = px(12),
        footnotes.font.size = px(12)
    ) |>
    fmt_number(
        columns = c(mean, sd, min, q1, median, q3, max),
        decimals = 2,
        dec_mark = ",",
        sep_mark = "."
    )

# --- tbl-desc-analysis-summary ---
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
    tab_source_note(
        source_note = md("**Fonte**: base de dados.")
    ) |>
    cols_align(align = "right", columns = everything()) |>
    cols_align(align = "left", columns = variavel) |>
    tab_style(
        style = cell_text(align = "left"),
        locations = cells_stubhead()
    ) |>
    tab_style(
        style = cell_text(align = "left"),
        locations = list(cells_source_notes(), cells_footnotes())
    ) |>
    tab_options(
        table.width = "100%",
        column_labels.font.weight = "bold",
        column_labels.padding = "10px",
        source_notes.font.size = px(12),
        footnotes.font.size = px(12)
    ) |>
    fmt_number(
        columns = c(mean, sd, min, q1, median, q3, max),
        decimals = 2,
        dec_mark = ",",
        sep_mark = "."
    )

##############################################################################
# Idade
##############################################################################

# --- fig-boxplot-age ---
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

# --- fig-hist-person-age ---
credit_risk_clean |>
    ggplot(aes(x = person_age)) +
    geom_histogram(binwidth = 5, fill = "steelblue", color = "white", alpha = 0.8) +
    theme_minimal(base_size = 11) +
    labs(
        x = "Idade",
        y = "Frequência"
    )

##############################################################################
# Renda pessoal
##############################################################################

# --- fig-boxplot-person-income ---
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
        y = "Renda pessoal"
    ) +
    theme_minimal(base_size = 11)

# --- fig-boxplot-log-person-income ---
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
        y = "ln(Renda pessoal)"
    ) +
    theme_minimal(base_size = 11)

# --- fig-hist-person-income-versus ---
gghist_person_income <- credit_risk_clean |>
    na.omit() |>
    ggplot(aes(x = person_income)) +
    geom_histogram(bins = 15, fill = "steelblue", color = "white", alpha = 0.8) +
    theme_minimal(base_size = 11) +
    labs(
        x = "Renda pessoal",
        y = "Frequência"
    )

gghist_log_person_income <- credit_risk_clean |>
    na.omit() |>
    ggplot(aes(x = log(person_income))) +
    geom_histogram(bins = 15, fill = "steelblue", color = "white", alpha = 0.8) +
    theme_minimal(base_size = 11) +
    labs(
        x = "ln(Renda pessoal)",
        y = "Frequência"
    )

gghist_person_income
gghist_log_person_income

##############################################################################
# Tempo no emprego atual
##############################################################################

# --- fig-boxplot-person-emp-length ---
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

# --- fig-bar-person-emp-length ---
credit_risk_clean |>
    na.omit() |>
    ggplot(aes(x = person_emp_length)) +
    geom_bar(fill = "steelblue", color = "white", alpha = 0.8) +
    theme_minimal(base_size = 11) +
    labs(
        x = "Tempo em anos",
        y = "Frequência"
    )

##############################################################################
# Valor do empréstimo
##############################################################################

# --- fig-boxplot-loan-amnt ---
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

# --- fig-hist-loan-amnt ---
credit_risk_clean |>
    ggplot(aes(x = loan_amnt)) +
    geom_histogram(bins = 15, fill = "steelblue", color = "white", alpha = 0.8) +
    theme_minimal(base_size = 11) +
    labs(
        x = "Valor do empréstimo",
        y = "Frequência"
    )

##############################################################################
# Taxa de juros
##############################################################################

# --- fig-boxplot-loan-int-rate ---
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

# --- fig-boxplot-loan-int-rate-vs-loan-grade ---
credit_risk_clean |>
    na.omit() |>
    ggplot(aes(x = loan_grade, y = loan_int_rate)) +
    geom_boxplot(
        outlier.color = "red",
        outlier.size = 1,
        width = 0.5,
        show.legend = FALSE,
        fill = "deepskyblue"
    ) +
    labs(
        x = "Risco de inadimplência",
        y = "Taxa de juros"
    ) +
    theme_minimal(base_size = 11)

# --- fig-hist-loan-int-rate ---
credit_risk_clean |>
    na.omit() |>
    ggplot(aes(x = loan_int_rate)) +
    geom_histogram(bins = 12, fill = "steelblue", color = "white", alpha = 0.8) +
    theme_minimal(base_size = 11) +
    labs(
        x = "Taxa de juros",
        y = "Frequência"
    )

##############################################################################
# Comprometimento da renda
##############################################################################

# --- fig-boxplot-loan-percent-income ---
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

# --- fig-hist-loan-percent-income ---
credit_risk_clean |>
    na.omit() |>
    ggplot(aes(x = loan_percent_income)) +
    geom_histogram(bins = 12, fill = "steelblue", color = "white", alpha = 0.8) +
    theme_minimal(base_size = 11) +
    labs(
        x = "Comprometimento da renda",
        y = "Frequência"
    )

##############################################################################
# Histórico de crédito
##############################################################################

# --- fig-boxplot-cb-person-cred-hist-length ---
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

# --- fig-hist-cb-person-cred-hist-length ---
credit_risk_clean |>
    na.omit() |>
    ggplot(aes(x = cb_person_cred_hist_length)) +
    geom_histogram(bins = 12, fill = "steelblue", color = "white", alpha = 0.8) +
    theme_minimal(base_size = 11) +
    labs(
        x = "Histórico de crédito (anos)",
        y = "Frequência"
    )

##############################################################################
# Tipo de moradia
##############################################################################

# --- fig-hbar-person-home-ownership ---
ggplot(credit_risk_clean, aes(
  y = fct_rev(fct_infreq(person_home_ownership)),
  fill = loan_status
)) +
  geom_bar(
    aes(x = after_stat(count / tapply(count, y, sum)[y])),
    position = position_dodge(width = 0.7, reverse = TRUE),
    width = 0.7
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Adimplente" = "#2b8cbe", "Inadimplente" = "#e34a33")) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  geom_text(
    aes(
      x = after_stat(count / tapply(count, y, sum)[y]),
      label = scales::percent(after_stat(count / tapply(count, y, sum)[y]), accuracy = 0.1)
    ),
    stat = "count",
    position = position_dodge(width = 0.7, reverse = TRUE),
    hjust = -0.15,
    size = 3.5
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1.05), # Vai até 100% com margem para o texto
    breaks = seq(0, 1, 0.25)
  ) +
  labs(
    title = "Situação dos empréstimos por tipo de moradia",
    subtitle = "Proporção percentual dentro de cada categoria",
    x = NULL, # Como o subtítulo já explica e há rótulos nas barras, o título do eixo X fica opcional
    y = "Tipo de moradia",
    fill = "Situação"
  )


# --- tbl-dblentry-person-home-owner-ship ---
# 1. Calcula as proporções em relação ao total de cada coluna
tbl_contigency <- credit_risk_clean |>
  tabyl(person_home_ownership, loan_status) |>
  adorn_totals("col", name = "Total") |>
  mutate(
    pct_adimp = Adimplente / sum(Adimplente),
    pct_inadimp = Inadimplente / sum(Inadimplente),
    pct_total = Total / sum(Total)
  )

# 2. Constrói a tabela gt
tbl_contigency |>
  gt(rowname_col = "person_home_ownership") |>
  tab_stubhead("Tipo de moradia") |>
  tab_spanner(
    columns = c("Adimplente", "Inadimplente", "Total"),
    label = "Situação do empréstimo"
  ) |>
  fmt_integer(
    columns = c("Adimplente", "Inadimplente", "Total"),
    sep_mark = "."
  ) |>
  # Formata as porcentagens com zero à esquerda (ex: 09,1% e 00,3%)
  fmt(
    columns = starts_with("pct_"),
    fns = function(x) {
      gsub("\\.", ",", sprintf("%04.1f%%", x * 100))
    }
  ) |>
  cols_merge(
    columns = c(Adimplente, pct_adimp),
    pattern = "{1} ({2})"
  ) |>
  cols_merge(
    columns = c(Inadimplente, pct_inadimp),
    pattern = "{1} ({2})"
  ) |>
  cols_merge(
    columns = c(Total, pct_total),
    pattern = "{1} ({2})"
  ) |>
  grand_summary_rows(
    columns = c("Adimplente", "Inadimplente", "Total"),
    fns = list(Total = ~ sum(., na.rm = TRUE)),
    fmt = ~ fmt_integer(., sep_mark = ".")
  ) |>
  tab_source_note(
    source_note = md("**Fonte**: base de dados.")
  ) |>
  cols_align(align = "right", columns = everything()) |>
  cols_align(align = "left", columns = person_home_ownership) |>
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_stubhead()
  ) |>
  tab_style(
    style = cell_text(align = "left"),
    locations = list(cells_source_notes(), cells_footnotes())
  ) |>
  # Garante números tabulares (mesma largura para todos os dígitos de 0 a 9)
  tab_options(
    table.width = "100%",
    column_labels.font.weight = "bold",
    column_labels.padding = "10px",
    source_notes.font.size = px(12),
    footnotes.font.size = px(12),
  ) |>
  opt_css(
    css = "
      .gt_table td, .gt_table th {
        font-variant-numeric: tabular-nums;
      }
    "
  )

##############################################################################
# Finalidade do empréstimo
##############################################################################

# --- fig-hbar-loan-intent ---
ggplot(credit_risk_clean, aes(
  y = fct_rev(fct_infreq(loan_intent)),
  fill = loan_status
)) +
  geom_bar(
    aes(x = after_stat(count / tapply(count, y, sum)[y])),
    position = position_dodge(width = 0.7, reverse = TRUE),
    width = 0.7
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Adimplente" = "#2b8cbe", "Inadimplente" = "#e34a33")) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  geom_text(
    aes(
      x = after_stat(count / tapply(count, y, sum)[y]),
      label = scales::percent(after_stat(count / tapply(count, y, sum)[y]), accuracy = 0.1)
    ),
    stat = "count",
    position = position_dodge(width = 0.7, reverse = TRUE),
    hjust = -0.15,
    size = 3.5
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1.05), # Vai até 100% com margem para o texto
    breaks = seq(0, 1, 0.25)
  ) +
  labs(
    title = "Situação dos empréstimos por finalidade",
    subtitle = "Proporção percentual dentro de cada categoria",
    x = NULL, # Como o subtítulo já explica e há rótulos nas barras, o título do eixo X fica opcional
    y = "Finalidade",
    fill = "Situação"
  )

# --- tbl-dblentry-loan-intent ---
# 1. Calcula as proporções em relação ao total de cada coluna
tbl_contigency <- credit_risk_clean |>
  tabyl(loan_intent, loan_status) |>
  adorn_totals("col", name = "Total") |>
  mutate(
    pct_adimp = Adimplente / sum(Adimplente),
    pct_inadimp = Inadimplente / sum(Inadimplente),
    pct_total = Total / sum(Total)
  )

# 2. Constrói a tabela gt
tbl_contigency |>
  gt(rowname_col = "loan_intent") |>
  tab_stubhead("Finalidade") |>
  tab_spanner(
    columns = c("Adimplente", "Inadimplente", "Total"),
    label = "Situação do empréstimo"
  ) |>
  fmt_integer(
    columns = c("Adimplente", "Inadimplente", "Total"),
    sep_mark = "."
  ) |>
  # Formata as porcentagens com zero à esquerda (ex: 09,1% e 00,3%)
  fmt(
    columns = starts_with("pct_"),
    fns = function(x) {
      gsub("\\.", ",", sprintf("%04.1f%%", x * 100))
    }
  ) |>
  cols_merge(
    columns = c(Adimplente, pct_adimp),
    pattern = "{1} ({2})"
  ) |>
  cols_merge(
    columns = c(Inadimplente, pct_inadimp),
    pattern = "{1} ({2})"
  ) |>
  cols_merge(
    columns = c(Total, pct_total),
    pattern = "{1} ({2})"
  ) |>
  grand_summary_rows(
    columns = c("Adimplente", "Inadimplente", "Total"),
    fns = list(Total = ~ sum(., na.rm = TRUE)),
    fmt = ~ fmt_integer(., sep_mark = ".")
  ) |>
  tab_source_note(
    source_note = md("**Fonte**: base de dados.")
  ) |>
  cols_align(align = "right", columns = everything()) |>
  cols_align(align = "left", columns = loan_intent) |>
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_stubhead()
  ) |>
  tab_style(
    style = cell_text(align = "left"),
    locations = list(cells_source_notes(), cells_footnotes())
  ) |>
  # Garante números tabulares (mesma largura para todos os dígitos de 0 a 9)
  tab_options(
    table.width = "100%",
    column_labels.font.weight = "bold",
    column_labels.padding = "10px",
    source_notes.font.size = px(12),
    footnotes.font.size = px(12),
  ) |>
  opt_css(
    css = "
      .gt_table td, .gt_table th {
        font-variant-numeric: tabular-nums;
      }
    "
  )

##############################################################################
# Risco de inadimplência
##############################################################################

# --- fig-hbar-loan-grade ---
ggplot(credit_risk_clean, aes(
  y = fct_rev(fct_infreq(loan_grade)),
  fill = loan_status
)) +
  geom_bar(
    aes(x = after_stat(count / tapply(count, y, sum)[y])),
    position = position_dodge(width = 0.7, reverse = TRUE),
    width = 0.7
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Adimplente" = "#2b8cbe", "Inadimplente" = "#e34a33")) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  geom_text(
    aes(
      x = after_stat(count / tapply(count, y, sum)[y]),
      label = scales::percent(after_stat(count / tapply(count, y, sum)[y]), accuracy = 0.1)
    ),
    stat = "count",
    position = position_dodge(width = 0.7, reverse = TRUE),
    hjust = -0.15,
    size = 3.5
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1.05), # Vai até 100% com margem para o texto
    breaks = seq(0, 1, 0.25)
  ) +
  labs(
    title = "Situação dos empréstimos por risco de inadimplência",
    subtitle = "Proporção percentual dentro de cada categoria",
    x = NULL, # Como o subtítulo já explica e há rótulos nas barras, o título do eixo X fica opcional
    y = "Risco",
    fill = "Situação"
  )

# --- tbl-dblentry-loan-grade ---
# 1. Calcula as proporções em relação ao total de cada coluna
tbl_contigency <- credit_risk_clean |>
  tabyl(loan_grade, loan_status) |>
  adorn_totals("col", name = "Total") |>
  mutate(
    pct_adimp = Adimplente / sum(Adimplente),
    pct_inadimp = Inadimplente / sum(Inadimplente),
    pct_total = Total / sum(Total)
  )

# 2. Constrói a tabela gt
tbl_contigency |>
  gt(rowname_col = "loan_grade") |>
  tab_stubhead("Risco") |>
  tab_spanner(
    columns = c("Adimplente", "Inadimplente", "Total"),
    label = "Situação do empréstimo"
  ) |>
  fmt_integer(
    columns = c("Adimplente", "Inadimplente", "Total"),
    sep_mark = "."
  ) |>
  # Formata as porcentagens com zero à esquerda (ex: 09,1% e 00,3%)
  fmt(
    columns = starts_with("pct_"),
    fns = function(x) {
      gsub("\\.", ",", sprintf("%04.1f%%", x * 100))
    }
  ) |>
  cols_merge(
    columns = c(Adimplente, pct_adimp),
    pattern = "{1} ({2})"
  ) |>
  cols_merge(
    columns = c(Inadimplente, pct_inadimp),
    pattern = "{1} ({2})"
  ) |>
  cols_merge(
    columns = c(Total, pct_total),
    pattern = "{1} ({2})"
  ) |>
  grand_summary_rows(
    columns = c("Adimplente", "Inadimplente", "Total"),
    fns = list(Total = ~ sum(., na.rm = TRUE)),
    fmt = ~ fmt_integer(., sep_mark = ".")
  ) |>
  tab_source_note(
    source_note = md("**Fonte**: base de dados.")
  ) |>
  cols_align(align = "right", columns = everything()) |>
  cols_align(align = "left", columns = loan_grade) |>
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_stubhead()
  ) |>
  tab_style(
    style = cell_text(align = "left"),
    locations = list(cells_source_notes(), cells_footnotes())
  ) |>
  # Garante números tabulares (mesma largura para todos os dígitos de 0 a 9)
  tab_options(
    table.width = "100%",
    column_labels.font.weight = "bold",
    column_labels.padding = "10px",
    source_notes.font.size = px(12),
    footnotes.font.size = px(12),
  ) |>
  opt_css(
    css = "
      .gt_table td, .gt_table th {
        font-variant-numeric: tabular-nums;
      }
    "
  )


##############################################################################
# Registro de inadimplência anterior
##############################################################################

# --- cb-person-default-on-file ---
count_cb_person_default_on_file <- credit_risk_clean |>
    count(cb_person_default_on_file) |>
    mutate(perc = n * 100 / sum(n))

count_cb_person_default_on_file_yes <- count_cb_person_default_on_file |>
    filter(cb_person_default_on_file == "Sim")

count_cb_person_default_on_file_no <- count_cb_person_default_on_file |>
    filter(cb_person_default_on_file == "Não")

# --- tbl-dblentry-cb-person-default-on-file ---
tbl_contigency <- credit_risk_clean |>
    mutate(cb_person_default_on_file = fct_rev(cb_person_default_on_file)) |>
    tabyl(cb_person_default_on_file, loan_status) |>
    adorn_totals("col", name = "Total")

tbl_contigency |>
    gt(rowname_col = "cb_person_default_on_file") |>
    tab_stubhead("Registro") |>
    tab_spanner(
        columns = c("Adimplente", "Inadimplente", "Total"),
        label = "Situação do empréstimo"
    ) |>
    tab_source_note(
        source_note = md("**Fonte**: base de dados.")
    ) |>
    cols_align(align = "right", columns = everything()) |>
    cols_align(align = "left", columns = cb_person_default_on_file) |>
    tab_style(
        style = cell_text(align = "left"),
        locations = cells_stubhead()
    ) |>
    tab_style(
        style = cell_text(align = "left"),
        locations = list(cells_source_notes(), cells_footnotes())
    ) |>
    tab_options(
        table.width = "100%",
        column_labels.font.weight = "bold",
        column_labels.padding = "10px",
        source_notes.font.size = px(12),
        footnotes.font.size = px(12)
    ) |>
    fmt_integer(
        columns = c("Adimplente", "Inadimplente", "Total"),
        sep_mark = "."
    ) |>
    grand_summary_rows(
        columns = -cb_person_default_on_file,
        fns = list(Total = ~ sum(., na.rm = TRUE)),
        fmt = ~ fmt_integer(., sep_mark = ".")
    )

##############################################################################
# Situação do empréstimo
##############################################################################

# --- loan-status ---
count_loan_status <- credit_risk_clean |>
    count(loan_status) |>
    mutate(perc = n * 100 / sum(n))

count_loan_status_default <- count_loan_status |>
    filter(loan_status == "Inadimplente")

count_loan_status_paid <- count_loan_status |>
    filter(loan_status == "Adimplente")

##############################################################################
# Variáveis quantitativas
##############################################################################

# --- fig-corr-matrix ---
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

# --- fig-dispersion-loan-amnt-vs-loan-percent-income ---
ggplot(credit_risk_clean, aes(x = loan_amnt, y = loan_percent_income)) +
  geom_point(alpha = 0.5) +
    geom_hex(bins = 60) +
    scale_fill_viridis_c(trans = "log10") +
    theme_minimal() +
    labs(
        x = "Valor do empréstimo",
        y = "Comprometimento da renda",
        fill = "Nº de empréstimos"
    )

##############################################################################
# Variáveis categóricas
##############################################################################

# --- fig-associacao-categoricas ---
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
# Modelagem
##############################################################################

# --- training-definitions ---
# Split.
set.seed(2026)
split <- initial_split(credit_risk_clean, prop = 0.7, strata = loan_status)
train_data <- training(split)
test_data <- testing(split)

# Repeated 10-fold CV.
set.seed(2026)
folds <- vfold_cv(train_data, v = 10, repeats = 5, strata = loan_status)

# Metrics used throughout.
risk_metrics <- metric_set(
  roc_auc,
  brier_class,
  mn_log_loss,
  bal_accuracy, # Acurácia balanceada
  recall,       # Recall (Sensibilidade)
  precision     # Precisão
)

##############################################################################
# Seleção de variáveis
##############################################################################

# --- tbl-gvif-detection ---
# Variable selection with GVIF and CV.
# Base model for GVIF verification.
glm_check_vif <- glm(loan_status ~ . - person_income + log(person_income),
  family = binomial,
  data = train_data
) |>
    vif() |>
    as.data.frame()

glm_check_vif <- glm_check_vif |>
    mutate(variables = coalesce(nomes_legiveis[rownames(glm_check_vif)], "Renda pessoal"))

colnames(glm_check_vif) <- c("GVIF", "DF", "SGVIF", "variables")

# Problems detected here.
glm_check_vif |>
    gt(rowname_col = "variables") |>
    tab_stubhead("Variáveis") |>
    cols_label(
        DF = "Graus de liberdade",
        SGVIF = html("GVIF<sup>1/(2&times;Df)</sup>")
    ) |>
    tab_footnote(
        footnote = md("**Nota:** a variável de renda pessoal foi transformada para a escala logarítmica."),
        locations = cells_stub(rows = contains(nomes_legiveis["person_income"], ignore.case = TRUE)) # Locate the line automatically.
    ) |>
    tab_source_note(
        source_note = md("**Fonte**: Elaboração própria.")
    ) |>
    cols_align(align = "right", columns = everything()) |>
    cols_align(align = "left", columns = variables) |>
    tab_style(
        style = cell_text(align = "left"),
        locations = cells_stubhead()
    ) |>
    tab_style(
        style = cell_text(align = "left"),
        locations = list(cells_source_notes(), cells_footnotes())
    ) |>
    tab_options(
        table.width = "100%",
        column_labels.font.weight = "bold",
        column_labels.padding = "10px",
        source_notes.font.size = px(12),
        footnotes.font.size = px(12),
    ) |>
    fmt_number(
        columns = c(GVIF, SGVIF),
        decimals = 3,
        dec_mark = ",",
        sep_mark = "."
    )

# --- tbl-variable-selection ---
# Remove loan_amnt and adjust two models, one without loan_int_rate and one without loan_grade
# and check which of the two has the best performance.

# Model, recip, workflow.
logit_mod <- logistic_reg() |>
  set_engine("glm")

rec_exclude_int_rate <- recipe(loan_status ~ ., data = train_data) |>
  step_novel(all_nominal_predictors()) |>
  step_other(all_nominal_predictors(), threshold = 0.02) |>
  step_log(person_income) |>
  step_rm(loan_amnt, loan_int_rate) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors())

wf_exclude_int_rate <-
  workflow() |>
  add_model(logit_mod) |>
  add_recipe(rec_exclude_int_rate)

exclude_int_rate_rs <-
  wf_exclude_int_rate |>
  fit_resamples(
    folds,
    metrics = risk_metrics,
    control = control_resamples(event_level = "second")
  )

# Logit model without grade.
rec_exclude_grade <- recipe(loan_status ~ ., data = train_data) |>
  step_novel(all_nominal_predictors()) |>
  step_other(all_nominal_predictors(), threshold = 0.02) |>
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
    collect_metrics(exclude_int_rate_rs) |> mutate(model = "Risco de inadimplência"),
    collect_metrics(exclude_grade_rs)    |> mutate(model = "Taxa de juros")
) |>
    mutate(
        .metric = recode_values(
            .metric,
            "brier_class" ~ "Escore de Brier",
            "mn_log_loss" ~ "Log-Loss",
            "roc_auc" ~ "AUC-ROC",
            "bal_accuracy" ~ "Acurácia balanceada",
            "recall" ~ "Sensibilidade",
            "precision" ~ "Precisão",
            default = .metric
        )
    ) |>
    select(model, .metric, mean, std_err) |>
    gt(groupname_col = "model") |>
    cols_label(
        .metric = "Métrica",
        mean = "Média",
        std_err = "Erro padrão"
    ) |>
    tab_source_note(
        source_note = md("**Fonte**: Elaboração própria.")
    ) |>
    cols_align(align = "right", columns = everything()) |>
    cols_align(align = "left", columns = model) |>
    cols_align(align = "center", columns = .metric) |>
    tab_style(
        style = cell_text(align = "left"),
        locations = cells_stubhead()
    ) |>
    tab_style(
        style = cell_text(align = "left"),
        locations = list(cells_source_notes(), cells_footnotes())
    ) |>
    tab_options(
        table.width = "100%",
        column_labels.font.weight = "bold",
        column_labels.padding = "10px",
        source_notes.font.size = px(12),
        footnotes.font.size = px(12),
    ) |>
    fmt_number(
        columns = c(mean, std_err),
        decimals = 4,
        dec_mark = ",",
        sep_mark = "."
    )

# --- tbl-gvif-detection-clean ---
# Variable selection with GVIF and CV.
# Base model for GVIF verification.
glm_check_vif <- glm(
    loan_status ~ .
        - person_income + log(person_income) - loan_amnt - loan_int_rate,
    family = binomial,
    data = train_data
) |>
    vif() |>
    as.data.frame()

glm_check_vif <- glm_check_vif |>
    mutate(
        variables = coalesce(nomes_legiveis[rownames(glm_check_vif)], "Renda pessoal")
    )

colnames(glm_check_vif) <- c("GVIF", "DF", "SGVIF", "variables")

glm_check_vif |>
    gt(rowname_col = "variables") |>
    tab_stubhead("Variáveis") |>
    cols_label(
        DF = "Graus de liberdade",
        SGVIF = html("GVIF<sup>1/(2&times;Df)</sup>")
    ) |>
    tab_footnote(
        footnote = md("**Nota:** a variável de renda pessoal foi transformada para a escala logarítmica."),
        locations = cells_stub(rows = contains(nomes_legiveis["person_income"], ignore.case = TRUE)) # Locate the line automatically.
    ) |>
    tab_source_note(
        source_note = md("**Fonte**: Elaboração própria.")
    ) |>
    cols_align(align = "right", columns = everything()) |>
    cols_align(align = "left", columns = variables) |>
    tab_style(
        style = cell_text(align = "left"),
        locations = cells_stubhead()
    ) |>
    tab_style(
        style = cell_text(align = "left"),
        locations = list(cells_source_notes(), cells_footnotes())
    ) |>
    tab_options(
        table.width = "100%",
        column_labels.font.weight = "bold",
        column_labels.padding = "10px",
        source_notes.font.size = px(12),
        footnotes.font.size = px(12),
    ) |>
    fmt_number(
        columns = c(GVIF, SGVIF),
        decimals = 3,
        dec_mark = ",",
        sep_mark = "."
    )

##############################################################################
# Comparação e seleção de modelos
##############################################################################

# --- cv-logit-model ---
# Model, recipe, workflow.
logit_mod <-
    logistic_reg() |>
    set_engine("glm")

logit_rec <-
    recipe(loan_status ~ ., data = train_data) |>
    # 1. Handle high-cardinality nominal variables first
    step_novel(all_nominal_predictors()) |>
    step_other(all_nominal_predictors(), threshold = 0.02) |>
    step_dummy(all_nominal_predictors()) |>
    # 2. Perform variable removals and math transformations
    step_rm(loan_amnt, loan_int_rate) |>
    step_log(person_income) |>
    # 3. Clean up any columns that ended up with zero variance
    step_zv(all_predictors())

logit_wf <-
    workflow() |>
    add_model(logit_mod) |>
    add_recipe(logit_rec)

# Resample, metrics and predictions.
logit_fit_rs <-
    logit_wf |>
    fit_resamples(
        folds,
        metrics = risk_metrics,
        control = control_resamples(save_pred = TRUE, event_level = "second")
    )

logit_fit_rs_metrics <-
    collect_metrics(logit_fit_rs) |>
    mutate(model = "Regressão Logística")

logit_fit_rs_preds <-
    collect_predictions(logit_fit_rs) |>
    mutate(model = "Regressão Logística")

# --- cv-probit-model ---
# Model, recipe, workflow.
probit_mod <-
    logistic_reg() |>
    set_engine("glm", family = binomial(link = "probit"))

probit_rec <- logit_rec

probit_wf <-
    workflow() |>
    add_model(probit_mod) |>
    add_recipe(probit_rec)

# Resample, metrics and predictions.
probit_fit_rs <-
    probit_wf |>
    fit_resamples(
        folds,
        metrics = risk_metrics,
        control = control_resamples(save_pred = TRUE, event_level = "second")
    )

probit_fit_rs_metrics <-
    collect_metrics(probit_fit_rs) |>
    mutate(model = "Regressão Probit")

probit_fit_rs_preds <-
    collect_predictions(probit_fit_rs) |>
    mutate(model = "Regressão Probit")

# --- cv-cloglog-model ---
# Model, recipe, workflow.
cloglog_mod <-
    logistic_reg() |>
    set_engine("glm", family = binomial(link = "cloglog"))

cloglog_rec <- logit_rec

cloglog_wf <-
    workflow() |>
    add_model(cloglog_mod) |>
    add_recipe(cloglog_rec)

# Resample, metrics and predictions.
cloglog_fit_rs <-
    cloglog_wf |>
    fit_resamples(
        folds,
        metrics = risk_metrics,
        control = control_resamples(save_pred = TRUE, event_level = "second")
    )

cloglog_fit_rs_metrics <-
    collect_metrics(cloglog_fit_rs) |>
    mutate(model = "Regressão Complemento Log-Log")

cloglog_fit_rs_preds <-
    collect_predictions(cloglog_fit_rs) |>
    mutate(model = "Regressão Complemento Log-Log")

# 1. Função auxiliar para calcular as métricas nas 9 partes de treino (analysis set)
calc_train_cv_metrics <- function(wf, folds, model_name) {
  # Identifica o nome da coluna da classe positiva (2º nível do fator)
  pos_class <- levels(train_data$loan_status)[2]
  prob_col <- paste0(".pred_", pos_class)

  map_dfr(folds$splits, function(split) {
    train_fold <- analysis(split)
    fit_fold <- fit(wf, data = train_fold)

    preds <- bind_cols(
      train_fold |> select(loan_status),
      predict(fit_fold, new_data = train_fold, type = "prob"),
      predict(fit_fold, new_data = train_fold, type = "class")
    )

    # Calcula exatamente o mesmo risk_metrics com event_level = "second"
    risk_metrics(
      preds,
      truth = loan_status,
      estimate = .pred_class,
      !!sym(prob_col),
      event_level = "second"
    )
  }) |>
    group_by(.metric) |>
    summarise(
      mean_train = mean(.estimate),
      std_err_train = sd(.estimate) / sqrt(n()),
      .groups = "drop"
    ) |>
    mutate(model = model_name)
}

# 2. Calcula as métricas de treino (9 partes) para cada um dos 3 modelos
logit_train_metrics <- calc_train_cv_metrics(logit_wf, folds, "Modelo Logístico")
probit_train_metrics <- calc_train_cv_metrics(probit_wf, folds, "Modelo Probit")
cloglog_train_metrics <- calc_train_cv_metrics(cloglog_wf, folds, "Modelo Complemento Log-Log")

train_cv_all <- bind_rows(
  logit_train_metrics,
  probit_train_metrics,
  cloglog_train_metrics
)

# 3. Junta com as métricas de validação (10ª parte) e gera a tabela gt atualizada
# --- tbl-cv-metrics ---
bind_rows(
  logit_fit_rs_metrics |> mutate(model = "Modelo Logístico"),
  probit_fit_rs_metrics |> mutate(model = "Modelo Probit"),
  cloglog_fit_rs_metrics |> mutate(model = "Modelo Complemento Log-Log")
) |>
  left_join(train_cv_all, by = c("model", ".metric")) |>
  mutate(
    .metric = recode_values(
      .metric,
      "brier_class" ~ "Escore de Brier",
      "mn_log_loss" ~ "Log-Loss",
      "roc_auc" ~ "AUC-ROC",
      "bal_accuracy" ~ "Acurácia balanceada",
      "recall" ~ "Sensibilidade",
      "precision" ~ "Precisão",
      default = .metric
    )
  ) |>
  select(model, .metric, mean_train, mean, std_err) |>
  gt(groupname_col = "model") |>
  cols_label(
    .metric = "Métrica",
    mean_train = "Média (Treino)",
    mean = "Média (Validação)",
    std_err = "Erro padrão (Validação)"
  ) |>
  tab_source_note(
    source_note = md("**Fonte**: Elaboração própria.")
  ) |>
  cols_align(align = "right", columns = everything()) |>
  cols_align(align = "left", columns = model) |>
  cols_align(align = "center", columns = .metric) |>
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_stubhead()
  ) |>
  tab_style(
    style = cell_text(align = "left"),
    locations = list(cells_source_notes(), cells_footnotes())
  ) |>
  tab_options(
    table.width = "100%",
    column_labels.font.weight = "bold",
    column_labels.padding = "10px",
    source_notes.font.size = px(12),
    footnotes.font.size = px(12)
  ) |>
  fmt_number(
    columns = c(mean_train, mean, std_err),
    decimals = 4,
    dec_mark = ",",
    sep_mark = "."
  )


# --- fig-roc-curve-cv-models ---
bind_rows(
    logit_fit_rs_preds |>
        roc_curve(truth = loan_status, .pred_Inadimplente, event_level = "second") |>
        mutate(model = "Regressão Logística"),
    probit_fit_rs_preds |>
        roc_curve(truth = loan_status, .pred_Inadimplente, event_level = "second") |>
        mutate(model = "Regressão Probit"),
    cloglog_fit_rs_preds |>
        roc_curve(truth = loan_status, .pred_Inadimplente, event_level = "second") |>
        mutate(model = "Regressão Complemento Log-Log"),
) |>
    ggplot(aes(x = 1 - specificity, y = sensitivity, col = model)) +
    geom_path(lwd = 1.2, alpha = 0.5) +
    geom_abline(lty = 3) +
    coord_equal() +
    scale_color_viridis_d(option = "plasma", end = .6) +
    labs(
        color = "Modelo",
        x = "1 - Especificidade",
        y = "Sensibilidade"
    )

##############################################################################
# Ajuste final
##############################################################################

# --- logit-last-fit ---
# Final fit on training data, evaluated on test data.
logit_last_fit <-
    logit_wf |>
    last_fit(
        split,
        metrics = risk_metrics,
        control = control_last_fit(event_level = "second")
    )

# --- tbl-logit-last-fit ---
logit_fit_terms <- c(
    nomes_legiveis,
    "(Intercept)" = "(Intercepto)",
    "person_home_ownership_Hipoteca" = "Moradia financiada",
    "person_home_ownership_Própria" = "Moradia própria",
    "person_home_ownership_other" = "Outros tipos de moradia",
    "loan_intent_Saúde" = "Finalidade de saúde",
    "loan_intent_Empreendimento" = "Finalidade de empreendimento",
    "loan_intent_Uso.pessoal" = "Finalidade de uso pessoal",
    "loan_intent_Consolidação.de.dívidas" = "Finalidade de consolidação de dívidas",
    "loan_intent_Reforma.imobiliária" = "Finalidade de reforma imobiliária",
    "loan_grade_B" = "Risco de inadimplência B",
    "loan_grade_C" = "Risco de inadimplência C",
    "loan_grade_D" = "Risco de inadimplência D",
    "loan_grade_E" = "Risco de inadimplência E",
    "loan_grade_other" = "Outros riscos de inadimplência",
    "cb_person_default_on_file_Sim" = "Inadimplência anterior (Sim)"
)

logit_last_fit |>
    extract_fit_parsnip() |>
    tidy(exponentiate = FALSE, conf.int = TRUE) |>
    select(term, estimate, conf.low, conf.high, p.value) |>
    mutate(across(-c(term, p.value), \(x) round(x, 4))) |>
    mutate(p.value = label_scientific(digits = 4, decimal.mark = ",")(p.value)) |>
    mutate(term = logit_fit_terms[term]) |>
    gt(rowname_col = "term") |>
    tab_stubhead("Termos") |>
    cols_label(
        estimate = "Estimativa",
        conf.low = "Limite inferior",
        conf.high = "Limite superior",
        p.value = "P-valor"
    ) |>
    tab_footnote(
        footnote = "Variável transformada na escala logarítmica.",
        locations = cells_stub(rows = "Renda pessoal")
    ) |>
    tab_source_note(
        source_note = md("**Fonte**: Elaboração própria.")
    ) |>
    cols_align(align = "right", columns = everything()) |>
    cols_align(align = "left", columns = term) |>
    tab_style(
        style = cell_text(align = "left"),
        locations = cells_stubhead()
    ) |>
    tab_style(
        style = cell_text(align = "left"),
        locations = list(cells_source_notes(), cells_footnotes())
    ) |>
    tab_options(
        table.width = "100%",
        column_labels.font.weight = "bold",
        column_labels.padding = "10px",
        source_notes.font.size = px(12),
        footnotes.font.size = px(12)
    ) |>
    fmt_number(
        columns = c(estimate, conf.low, conf.high),
        decimals = 4,
        dec_mark = ",",
        sep_mark = "."
    )


# --- tbl-logit-last-fit-exponentiated ---
logit_fit_scales <- c(
  "person_age" = "1 ano",
  "person_income" = "1 unidade (log)",
  "person_emp_length" = "1 ano",
  "loan_percent_income" = "0,01 (1 ponto percentual)",
  "cb_person_cred_hist_length" = "1 ano",
  "person_home_ownership_Hipoteca" = "Ref.: Alugada",
  "person_home_ownership_Própria" = "Ref.: Alugada",
  "person_home_ownership_other" = "Ref.: Alugada",
  "loan_intent_Saúde" = "Ref.: Educação",
  "loan_intent_Empreendimento" = "Ref.: Educação",
  "loan_intent_Uso.pessoal" = "Ref.: Educação",
  "loan_intent_Consolidação.de.dívidas" = "Ref.: Educação",
  "loan_intent_Reforma.imobiliária" = "Ref.: Educação",
  "loan_grade_B" = "Ref.: Risco A",
  "loan_grade_C" = "Ref.: Risco A",
  "loan_grade_D" = "Ref.: Risco A",
  "loan_grade_E" = "Ref.: Risco A",
  "loan_grade_other" = "Ref.: Risco A",
  "cb_person_default_on_file_Sim" = "Ref.: Não"
)

logit_last_fit |>
  extract_fit_parsnip() |>
  tidy(exponentiate = TRUE) |>
  slice(-1) |>
  select(term, estimate) |>
  mutate(
    estimate = if_else(term == "loan_percent_income", estimate ^ 0.01, estimate),
    inv_estimate = if_else(estimate < 1, 1 / estimate, NA_real_),
    escala = logit_fit_scales[term],
    term = logit_fit_terms[term]
  ) |>
  select(term, escala, estimate, inv_estimate) |>
  gt(rowname_col = "term") |>
  tab_stubhead("Termos") |>
  cols_label(
    escala = "Escala / Referência",
    estimate = md("$e^\\beta$"),
    inv_estimate = md("$1/e^{\\beta}$ para $0 < e^{\\beta} < 1$")
  ) |>
  tab_footnote(
    footnote = "Variável transformada na escala logarítmica.",
    locations = cells_stub(rows = "Renda pessoal")
  ) |>
  tab_source_note(
    source_note = md("**Fonte**: Elaboração própria.")
  ) |>
  cols_align(align = "right", columns = c(estimate, inv_estimate)) |>
  cols_align(align = "left", columns = c(term, escala)) |>
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_stubhead()
  ) |>
  tab_style(
    style = cell_text(align = "left"),
    locations = list(cells_source_notes(), cells_footnotes())
  ) |>
  tab_options(
    table.width = "100%",
    column_labels.font.weight = "bold",
    column_labels.padding = "10px",
    source_notes.font.size = px(12),
    footnotes.font.size = px(12)
  ) |>
  fmt_number(
    columns = c(estimate, inv_estimate),
    decimals = 4,
    dec_mark = ",",
    sep_mark = "."
  ) |>
  sub_missing(
    columns = inv_estimate,
    missing_text = "—"
  )

# --- tbl-logit-last-fit-metrics ---
collect_metrics(logit_last_fit) |>
    mutate(
        .metric = recode_values(
            .metric,
            "brier_class" ~ "Escore de Brier",
            "mn_log_loss" ~ "Log-Loss",
            "roc_auc" ~ "AUC-ROC",
            "bal_accuracy" ~ "Acurácia balanceada",
            "recall" ~ "Sensibilidade",
            "precision" ~ "Precisão",
            default = .metric
        )
    ) |>
    select(.metric, .estimate) |>
    gt(rowname_col = ".metric") |>
    tab_stubhead("Métrica") |>
    cols_label(.estimate = "Estimativa") |>
    tab_source_note(
        source_note = md("**Fonte**: Elaboração própria.")
    ) |>
    cols_align(align = "right", columns = everything()) |>
    cols_align(align = "left", columns = .metric) |>
    tab_style(
        style = cell_text(align = "left"),
        locations = cells_stubhead()
    ) |>
    tab_style(
        style = cell_text(align = "left"),
        locations = list(cells_source_notes(), cells_footnotes())
    ) |>
    tab_options(
        table.width = "100%",
        column_labels.font.weight = "bold",
        column_labels.padding = "10px",
        source_notes.font.size = px(12),
        footnotes.font.size = px(12),
    ) |>
    fmt_number(
        columns = .estimate,
        decimals = 4,
        dec_mark = ",",
        sep_mark = "."
    )

# --- fig-roc-curve-logit-last-fit ---
collect_predictions(logit_last_fit) |>
    roc_curve(truth = loan_status, .pred_Inadimplente, event_level = "second") |>
    ggplot(aes(x = 1 - specificity, y = sensitivity)) +
    geom_path(lwd = 1.2, color = "#4B0082") +
    geom_abline(lty = 3) +
    coord_equal() +
    scale_color_viridis_d(option = "plasma", end = .6) +
    labs(
        x = "1 - Especificidade",
        y = "Sensibilidade"
    )

# --- tbl-confusion-matrix-logit-last-fit ---
logit_test_preds <- collect_predictions(logit_last_fit) |>
    mutate(
        .pred_class = factor(
            if_else(.pred_Inadimplente >= 0.5, "Inadimplente", "Adimplente"),
            levels = c("Adimplente", "Inadimplente")
        )
    )

logit_conf_mat <- logit_test_preds |>
    tabyl(.pred_class, loan_status) |>
    adorn_totals("col", name = "Total")

logit_acc <- accuracy(logit_test_preds, truth = loan_status, estimate = .pred_class, event_level = "second")$.estimate
logit_sens <- sens(logit_test_preds, truth = loan_status, estimate = .pred_class, event_level = "second")$.estimate
logit_spec <- spec(logit_test_preds, truth = loan_status, estimate = .pred_class, event_level = "second")$.estimate
logit_prec <- precision(logit_test_preds, truth = loan_status, estimate = .pred_class, event_level = "second")$.estimate

logit_conf_mat |>
    gt(rowname_col = ".pred_class") |>
    tab_stubhead("Predição") |>
    tab_spanner(
        columns = c("Adimplente", "Inadimplente", "Total"),
        label = "Situação observada"
    ) |>
    tab_footnote(
        footnote = md("**Nota:** classificações obtidas adotando o ponto de corte padrão de 0,50 para a probabilidade predita de inadimplência."),
        locations = cells_stubhead()
    ) |>
    tab_source_note(
        source_note = md("**Fonte**: Elaboração própria.")
    ) |>
    cols_align(align = "right", columns = everything()) |>
    cols_align(align = "left", columns = .pred_class) |>
    tab_style(
        style = cell_text(align = "left"),
        locations = cells_stubhead()
    ) |>
    tab_style(
        style = cell_text(align = "left"),
        locations = list(cells_source_notes(), cells_footnotes())
    ) |>
    tab_options(
        table.width = "100%",
        column_labels.font.weight = "bold",
        column_labels.padding = "10px",
        source_notes.font.size = px(12),
        footnotes.font.size = px(12)
    ) |>
    fmt_integer(
        columns = c("Adimplente", "Inadimplente", "Total"),
        sep_mark = "."
    ) |>
    grand_summary_rows(
        columns = -.pred_class,
        fns = list(Total = ~ sum(., na.rm = TRUE)),
        fmt = ~ fmt_integer(., sep_mark = ".")
    )

#
# Bootstrap para AUC
#

# 1. Extrai as predições do conjunto de teste do last_fit
preds_test <- collect_predictions(logit_last_fit)

# 2. Identifica automaticamente os nomes da coluna de desfecho e da probabilidade do 2º nível
outcome_col <- names(preds_test)[5] # Geralmente a 5ª coluna é o desfecho real
event_prob_col <- grep("^\\.pred_", names(preds_test), value = TRUE)[2] # 2º nível (.pred_Sim, .pred_1, etc.)

set.seed(123)
boot_samples <- bootstraps(
  preds_test,
  times = 1000,
  strata = all_of("loan_status"),
  apparent = TRUE # Necessário caso queira usar int_bca()
) |>
  mutate(
    estimates = map(
      splits,
      \(s) tibble(
        term = "roc_auc",
        estimate = roc_auc_vec(
          truth = analysis(s)[["loan_status"]],
          estimate = analysis(s)[[".pred_Inadimplente"]],
          event_level = "second"
        )
      )
    )
  )

# Intervalo de confiança percentil de 95%
int_pctl(boot_samples, estimates, alpha = 0.05)

nrow(preds_test)
# E para ver o tamanho por classe:
table(preds_test$loan_status)
