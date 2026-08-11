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

# El archivo .dta ya viene en UTF-8 para la enorme mayoría del texto (Stata
# moderno guarda strings en UTF-8), PERO unas pocas celdas tienen bytes
# inválidos como UTF-8 (probablemente texto pegado desde Word/Excel con una
# codificación distinta), y esas rompían janitor::clean_names()/sub() más
# adelante. El fix anterior reinterpretaba TODO el texto como Latin-1, lo
# cual corrompía el 99% que ya estaba bien (efecto "doble encoding": ó -> Ã³).
# Este fix es quirúrgico: usa validUTF8() para tocar SOLO las celdas
# realmente inválidas, dejando intacto todo lo que ya era UTF-8 correcto.
fix_latin1_encoding <- function(x) {
  if (is.character(x)) {
    bad <- !is.na(x) & !validUTF8(x)
    if (any(bad)) {
      Encoding(x[bad]) <- "latin1"
      x[bad] <- enc2utf8(x[bad])
    }
    return(x)
  }
  if (haven::is.labelled(x)) {
    labs <- attr(x, "labels")
    if (!is.null(labs) && !is.null(names(labs))) {
      nm  <- names(labs)
      bad <- !is.na(nm) & !validUTF8(nm)
      if (any(bad)) {
        Encoding(nm[bad]) <- "latin1"
        nm[bad] <- enc2utf8(nm[bad])
        names(labs) <- nm
        attr(x, "labels") <- labs
      }
    }
  }
  x
}

fix_encoding_df <- function(df) df |> mutate(across(everything(), fix_latin1_encoding))

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
    fix_encoding_df() |>
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
      fix_encoding_df() |>
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
  c(data$main$country, data$indcap$country) |>
    unique() |> (\(x) x[!is.na(x)])() |> sort()
}

# Módulos MIS respondidos por un país dado
modules_for_country <- function(data, country_name) {
  data$main |> filter(country == country_name) |> distinct(mis) |> pull(mis)
}

n_modules_for_country <- function(data, country_name) length(modules_for_country(data, country_name))

has_capabilities <- function(data, country_name) {
  nrow(data$indcap) > 0 && country_name %in% data$indcap$country
}

country_income_group <- function(data, country_name) {
  ig <- data$main |> filter(country == country_name) |> pull(income_group)
  ig <- ig[!is.na(ig)]
  if (length(ig)) ig[1] else NA_character_
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
# RESUELTO: es el análogo al cuestionario "Agency" de DfBG — 16 filas, una
# por país, SIN dimensión de módulo. 4 preguntas (q7-q10, todas Yes/No/DK/PNR)
# sobre capacidades analíticas del gobierno (carrera profesional dedicada,
# evaluaciones de habilidades, capacitación, iniciativas de fortalecimiento).
# Diccionario en CAP_QUESTIONS (question_dictionary.R), gráficos vía
# plot_cap_question() (plots.R).
# =============================================================================
