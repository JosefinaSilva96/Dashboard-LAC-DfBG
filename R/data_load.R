# =============================================================================
# data_load.R  (MIS Dashboard — LAC Government Analytics Survey)
# -----------------------------------------------------------------------------
# A diferencia del dashboard DfBG (3 bases separadas: agency/managers/systems),
# acá tenemos UNA sola base "long" por país x módulo:
#
#   allMIS_data_forLAC.dta : 85 filas = (país, MIS) únicos que respondieron.
#       q1 = país (33 posibles, LAC)
#       q2 = tipo de MIS: Human Resources / Procurement / Public Financial /
#            Tax / Education / Health
#       q8..q30 = mismo cuestionario para los 6 tipos de MIS (verificado
#            contra los 6 .docx: idéntica numeración, solo cambia el nombre
#            del sistema en el texto de la pregunta, ej. "the Health MIS"
#            vs "the Education MIS")
#
#   indcap_data.dta : 16 filas — cuestionario de "Capacidades individuales"
#       (unidad de análisis distinta, probablemente 1 fila por país o por
#       encuestado). Se carga aparte; todavía no está wireado al resto del
#       dashboard — ver nota al final de este archivo.
#
# Esto simplifica bastante respecto a DfBG: no hay que pegar 3 bases, y el
# "scope = compare" del dashboard original (país vs. income group) acá pasa a
# ser (país x módulo) vs. promedio LAC para ese mismo módulo.
# =============================================================================

library(dplyr)
library(haven)
library(janitor)

DATA_PATH <- file.path("data")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

# --- Carga principal -----------------------------------------------------

# haven::as_factor() decodifica los value labels de Stata (igual que el
# "build_dfbg_database" hacía para DfBG) para que el resto del pipeline
# trabaje siempre con etiquetas legibles, nunca con códigos 1/2/900/998.
decode_labelled <- function(df) {
  df |> mutate(across(where(haven::is.labelled), ~ haven::as_factor(.x) |> as.character()))
}

load_mis <- function(path = DATA_PATH) {

  f_main   <- file.path(path, "allMIS_data_forLAC.dta")
  f_indcap <- file.path(path, "indcap_data.dta")

  if (!file.exists(f_main)) stop("No encuentro ", f_main)

  main <- haven::read_dta(f_main) |>
    decode_labelled() |>
    janitor::clean_names()

  # El .dta ya trae una columna calculada llamada "mis" (no confundir con
  # nuestro q2, que es el TIPO de módulo MIS). La renombramos para no pisarla.
  if ("mis" %in% names(main)) main <- main |> rename(mis_flag_raw = mis)

  main <- main |>
    rename(country = q1, mis = q2) |>
    filter(!is.na(country), !is.na(mis))

  indcap <- tibble::tibble()
  if (file.exists(f_indcap)) {
    indcap <- haven::read_dta(f_indcap) |>
      decode_labelled() |>
      janitor::clean_names()
    if ("q1" %in% names(indcap)) indcap <- indcap |> rename(country = q1)
  }

  ig <- load_income_groups(path)
  if (nrow(ig) > 0 && "iso3c" %in% names(main)) {
    main <- main |> left_join(ig, by = "iso3c")
  } else if (!"income_group" %in% names(main)) {
    main$income_group <- NA_character_
  }

  list(main = main, indcap = indcap, income_groups = ig)
}

# --- Income groups (igual patrón que DfBG: opcional, cae a NA si no está) --

load_income_groups <- function(path = DATA_PATH) {
  class_file <- list.files(path, pattern = "^CLASS.*\\.xlsx$", full.names = TRUE)
  if (length(class_file) >= 1) {
    cls <- readxl::read_excel(class_file[1]) |> janitor::clean_names()
    code_col   <- intersect(c("code", "iso3c", "country_code"), names(cls))[1]
    income_col <- intersect(c("income_group", "income_group_1", "group"), names(cls))[1]
    if (!is.na(code_col) && !is.na(income_col)) {
      return(
        cls |>
          transmute(iso3c = toupper(.data[[code_col]]), income_group = .data[[income_col]]) |>
          filter(!is.na(iso3c), !is.na(income_group)) |>
          distinct()
      )
    }
  }
  message("CLASS_*.xlsx no encontrado: income groups quedan NA (no es obligatorio).")
  tibble::tibble(iso3c = character(), income_group = character())
}

# --- Listas auxiliares para los selectores del Shiny ------------------------

MIS_TYPES <- c("Human Resources MIS", "Procurement MIS", "Public Financial MIS",
                "Tax MIS", "Education MIS", "Health MIS")

country_choices <- function(data) {
  data$main |> filter(!is.na(country)) |> distinct(country) |> arrange(country) |> pull(country)
}

# Módulos MIS respondidos por un país dado
modules_for_country <- function(data, country_name) {
  data$main |> filter(country == country_name) |> distinct(mis) |> pull(mis)
}

# Encabezado país x módulo, análogo a country_header() de DfBG
country_module_header <- function(data, country_name, mis_name) {
  row <- data$main |> filter(country == country_name, mis == mis_name)
  list(
    country = country_name,
    mis     = mis_name,
    has_mis = if (nrow(row)) (row$q8[1] %||% NA) else NA,   # "8. Is there a <MIS> in place?"
    n_rows  = nrow(row)
  )
}

# =============================================================================
# NOTA sobre indcap_data.dta ("Capabilities/Capacidades")
# -----------------------------------------------------------------------------
# Solo 16 filas, 21 columnas (q1, q4, q7-q10 con _comments/_attachments/_dummy).
# No pude confirmar todavía si la unidad de análisis es país o encuestado, ni
# el texto completo de q4/q7-q10 (el docx de Capabilities no trae la hoja de
# choices, igual que le pasaba a module_metadata.R original con Agency
# q13/q19/q21). Lo dejo cargado y disponible en data$indcap para que lo
# revisemos juntas antes de wirearlo a module_metadata.R / question_dictionary.R
# como un 7mo módulo.
# =============================================================================
