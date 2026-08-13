# =============================================================================
# plots.R  (MIS Dashboard)
# -----------------------------------------------------------------------------
# Motor de gráficos agnóstico del cuestionario: recibe un dataframe YA
# FILTRADO (una fila por país) + la especificación de la pregunta, y arma la
# barra "país vs. promedio del grupo". Dos wrappers arriba lo conectan a:
#   - MIS_QUESTIONS  (allMIS_data_forLAC, filtrado por módulo)
#   - CAP_QUESTIONS  (indcap_data, sin dimensión de módulo)
# =============================================================================

library(ggplot2)
library(dplyr)

theme_mis <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title      = element_text(face = "bold", size = base_size + 1, hjust = 0,
                                     margin = margin(b = 4), lineheight = 1.05),
      plot.subtitle   = element_text(size = base_size - 2, hjust = 0, color = "grey30",
                                     margin = margin(b = 8)),
      axis.title.x    = element_text(face = "bold", size = base_size - 2),
      axis.text.y     = element_text(color = "black", size = base_size - 2, lineheight = 0.9),
      axis.text.x     = element_text(color = "black", size = base_size - 2),
      legend.position = "bottom",
      legend.title    = element_blank(),
      legend.text     = element_text(size = base_size - 3),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      plot.caption    = element_text(size = base_size - 4, color = "grey45", hjust = 0),
      plot.margin     = margin(t = 10, r = 16, b = 8, l = 8)
    )
}

WB_BLUE <- "#002245"
CAP_TXT <- "Own elaboration based on the LAC Government Analytics Survey"

default_palette <- function(levels) {
  n <- length(levels)
  pal <- colorRampPalette(c("#2E8B57", "#8DC26F", "#D4813A", "#8B2020", "#BDBDBD"))(n)
  setNames(pal, levels)
}

# =============================================================================
# Motor genérico — SINGLE (opción única)
# `d` ya viene filtrado a UNA fila por país (un módulo MIS, o Capabilities
# entero, que no tiene módulo).
# =============================================================================

plot_single_generic <- function(d, q, country_name, title_txt, group_label, scope = "compare") {
  pal <- q$palette %||% default_palette(q$levels)

  row_c <- d |> filter(country == country_name)
  val_c <- if (nrow(row_c)) as.character(factor(row_c[[q$cols[1]]][1], levels = q$levels)) else NA

  if (scope == "country") {
    df <- tibble::tibble(category = factor(q$levels, levels = q$levels)) |>
      mutate(value = if_else(category == val_c, 1, 0))
    p <- ggplot(df, aes(category, value, fill = category)) +
      geom_col(width = 0.7) +
      scale_fill_manual(values = pal, drop = FALSE, guide = "none") +
      labs(title = title_txt,
           subtitle = paste0(country_name, " \u2014 selected response highlighted"),
           x = NULL, y = NULL, caption = CAP_TXT) +
      coord_flip() + theme_mis() +
      theme(axis.text.x = element_blank())
    return(p)
  }

  peers <- d |>
    filter(!is.na(.data[[q$cols[1]]])) |>
    mutate(.cat = factor(.data[[q$cols[1]]], levels = q$levels)) |>
    filter(!is.na(.cat)) |>
    group_by(country) |>
    mutate(w = 1 / n()) |>
    ungroup()

  grp <- peers |>
    group_by(.cat) |>
    summarise(n = sum(w), .groups = "drop") |>
    mutate(pct = 100 * n / sum(n)) |>
    rename(category = .cat)

  df <- tibble::tibble(category = factor(q$levels, levels = q$levels)) |>
    left_join(grp, by = "category") |>
    mutate(pct = tidyr::replace_na(pct, 0),
           is_country = category == val_c,
           # Rombo desplazado hacia adentro de la barra para no chocar con el % de afuera
           diamond_x = pmax(pct - 4, pct * 0.55))

  p <- ggplot(df, aes(category, pct, fill = category)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = if_else(pct > 0, paste0(round(pct), "%"), "")),
              hjust = -0.15, size = 3.2) +
    geom_point(data = df |> filter(is_country), aes(category, diamond_x),
               shape = 23, size = 3.2, fill = "white", color = WB_BLUE, stroke = 1.3) +
    scale_fill_manual(values = pal, drop = FALSE, guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.18)), labels = NULL) +
    labs(title = title_txt,
         subtitle = paste0(country_name, " (\u25c6) vs. average \u2014 ", group_label),
         x = NULL, y = NULL, caption = CAP_TXT) +
    coord_flip() + theme_mis()
  p
}

# =============================================================================
# Motor genérico — MULTI (select_multiple)
# =============================================================================

plot_multi_generic <- function(d, q, country_name, title_txt, group_label, scope = "compare") {
  cols <- q$cols[q$cols %in% names(d)]
  opts <- q$options[cols]
  to_flag <- function(x) as.numeric(!is.na(x) & x != "" & x != "0")

  if (scope == "country") {
    row_c <- d |> filter(country == country_name)
    if (!nrow(row_c)) return(NULL)
    vals <- vapply(cols, function(cc) to_flag(row_c[[cc]])[1], numeric(1))
    df <- tibble::tibble(option = unname(opts[cols]), value = vals) |>
      mutate(option = factor(option, levels = rev(unname(opts[cols]))))
    p <- ggplot(df, aes(option, value)) +
      geom_col(width = 0.6, fill = WB_BLUE) +
      labs(title = title_txt, subtitle = country_name, x = NULL, y = NULL, caption = CAP_TXT) +
      coord_flip() + theme_mis() + theme(axis.text.x = element_blank())
    return(p)
  }

  peers <- d |> mutate(across(all_of(cols), to_flag))
  grp <- peers |>
    summarise(across(all_of(cols), ~ 100 * mean(.x, na.rm = TRUE))) |>
    tidyr::pivot_longer(everything(), names_to = "col", values_to = "pct") |>
    mutate(option = unname(opts[col]))

  row_c <- d |> filter(country == country_name)
  vals_c <- if (nrow(row_c)) vapply(cols, function(cc) to_flag(row_c[[cc]])[1], numeric(1)) else NULL

  df <- grp |>
    mutate(option = factor(option, levels = rev(unname(opts[cols]))),
           is_country = if (!is.null(vals_c)) vals_c[col] == 1 else FALSE,
           # Rombo desplazado hacia adentro de la barra para no chocar con el % de afuera
           diamond_x = pmax(pct - 4, pct * 0.55))

  p <- ggplot(df, aes(option, pct)) +
    geom_col(width = 0.6, fill = "#8DC26F") +
    geom_text(aes(label = paste0(round(pct), "%")), hjust = -0.15, size = 3.2) +
    geom_point(data = df |> filter(is_country), aes(option, diamond_x),
               shape = 23, size = 3.2, fill = "white", color = WB_BLUE, stroke = 1.3) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2)), labels = NULL) +
    labs(title = title_txt,
         subtitle = paste0(country_name, " (\u25c6) vs. average \u2014 ", group_label),
         x = NULL, y = NULL, caption = CAP_TXT) +
    coord_flip() + theme_mis()
  p
}

# =============================================================================
# Wrapper 1 — MIS (filtra por módulo, reemplaza {module} en el título)
# =============================================================================

plot_question <- function(data_main, qid, country_name, mis_name, scope = "compare") {
  q <- MIS_QUESTIONS[[qid]]
  if (is.null(q)) return(NULL)
  d <- data_main |> filter(mis == mis_name)
  title_txt   <- mis_title(q$title, mis_name)
  group_label <- mis_short_name(mis_name)
  if (q$type == "single") plot_single_generic(d, q, country_name, title_txt, group_label, scope)
  else if (q$type == "multi") plot_multi_generic(d, q, country_name, title_txt, group_label, scope)
  else NULL
}

# =============================================================================
# Wrapper 2 — Capabilities (sin módulo, título ya es texto final)
# =============================================================================

plot_cap_question <- function(data_cap, qid, country_name, scope = "compare") {
  q <- CAP_QUESTIONS[[qid]]
  if (is.null(q)) return(NULL)
  plot_single_generic(data_cap, q, country_name, q$title, "LAC (Capabilities questionnaire)", scope)
}
