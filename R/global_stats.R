# =============================================================================
# global_stats.R  (MIS Dashboard)
# -----------------------------------------------------------------------------
# Estadísticas agregadas por MÓDULO (todos los países LAC que respondieron ese
# módulo). Se usan en el tab "Overview" del Shiny y en la sección global del
# brief (build_brief.R).
# =============================================================================

library(dplyr)
library(purrr)

# % de países (dentro del módulo) que respondieron "Yes"/una categoría dada
# en una pregunta single, por módulo.
gs_single_by_module <- function(data_main, qid, level_of_interest = "Yes") {
  q <- MIS_QUESTIONS[[qid]]
  col <- q$cols[1]
  data_main |>
    filter(!is.na(.data[[col]])) |>
    group_by(mis) |>
    summarise(
      n_countries = n_distinct(country),
      pct = 100 * mean(.data[[col]] == level_of_interest, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(pct))
}

# Tabla resumen "salud general" de cada módulo: % con MIS en pie (q8),
# % digitalizado (q9 = Fully/Partially), % con controles de calidad (q24), etc.
gs_module_overview <- function(data_main) {
  purrr::map_dfr(MIS_TYPES, function(m) {
    d <- data_main |> filter(mis == m)
    tibble::tibble(
      mis           = m,
      n_countries   = n_distinct(d$country),
      pct_in_place  = 100 * mean(d$q8 == "Yes", na.rm = TRUE),
      pct_digitized = 100 * mean(d$q9 %in% c("Fully digitized", "Partially digitized"), na.rm = TRUE),
      pct_dedicated_unit = 100 * mean(d$q20 %in%
        c("Centralized unit/team (e.g., a single data analytics unit at the federal level supports several entities)",
          "Decentralized unit/team (e.g., data analytics units within entity supporting exclusively that entity)"),
        na.rm = TRUE),
      pct_quality_controls = 100 * mean(d$q24 == "Yes", na.rm = TRUE)
    )
  })
}

# Ranking de países dentro de un módulo dado, según un "score" simple:
# # de preguntas Yes/afirmativas entre un set de preguntas binarias clave.
gs_country_score <- function(data_main, mis_name,
                              qids = c("q8","q9","q15","q19","q20","q21","q22","q24","q25","q26","q28")) {
  d <- data_main |> filter(mis == mis_name)
  binary_yes <- c("Yes", "Fully digitized",
                   "Centralized unit/team (e.g., a single data analytics unit at the federal level supports several entities)",
                   "Decentralized unit/team (e.g., data analytics units within entity supporting exclusively that entity)")
  d |>
    rowwise() |>
    mutate(score = sum(c_across(all_of(qids)) %in% binary_yes, na.rm = TRUE)) |>
    ungroup() |>
    select(country, score) |>
    arrange(desc(score))
}
