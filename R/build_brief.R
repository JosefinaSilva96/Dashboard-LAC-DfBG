# =============================================================================
# build_brief.R  (MIS Dashboard)
# -----------------------------------------------------------------------------
# Versión inicial, más liviana que el build_brief.R de DfBG (que arma un .docx
# completo con officer, portada, disclaimer, etc.). Acá dejamos:
#
#   1. indicator_table()   : tabla país x módulo con la respuesta de cada
#                             pregunta del diccionario + el promedio LAC.
#   2. build_narrative()    : narrativa por reglas (sin LLM) a partir de esa
#                             tabla — SIEMPRE funciona, con o sin API key.
#   3. llm_narrative_section(): la misma idea pero llamando a Claude vía
#                             llm_narrative.R (call_claude), con fallback
#                             automático a build_narrative() si no hay key o
#                             falla la llamada.
#
# Para el .docx con portada/formato tipo DfBG, el siguiente paso natural es
# portar add_cover_page/add_disclaimer_page/generate_brief() del original
# adaptando country_value()/ig_modal() a MIS (quedan los hooks marcados TODO).
# =============================================================================

library(dplyr)
library(purrr)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a)) b else a

# --- Valor de una pregunta para un país x módulo (single) -------------------
mis_country_value <- function(data_main, qid, country_name, mis_name) {
  q <- MIS_QUESTIONS[[qid]]
  if (is.null(q) || q$type != "single") return(NA_character_)
  row <- data_main |> filter(country == country_name, mis == mis_name)
  if (!nrow(row)) return(NA_character_)
  as.character(row[[q$cols[1]]][1])
}

# --- Promedio LAC (moda) para esa misma pregunta y módulo -------------------
mis_lac_modal <- function(data_main, qid, mis_name) {
  q <- MIS_QUESTIONS[[qid]]
  if (is.null(q) || q$type != "single") return(NA_character_)
  vals <- data_main |> filter(mis == mis_name) |> pull(.data[[q$cols[1]]])
  vals <- vals[!is.na(vals)]
  if (!length(vals)) return(NA_character_)
  names(sort(table(vals), decreasing = TRUE))[1]
}

# --- Tabla de indicadores país x módulo --------------------------------------
indicator_table <- function(data_main, country_name, mis_name,
                             qids = mis_questions_ordered()) {
  purrr::map_dfr(qids, function(qid) {
    q <- MIS_QUESTIONS[[qid]]
    if (is.null(q) || q$type != "single") return(NULL)
    tibble::tibble(
      qid          = qid,
      question     = mis_title(q$title, mis_name),
      country_value = mis_country_value(data_main, qid, country_name, mis_name) %||% "No data",
      lac_modal     = mis_lac_modal(data_main, qid, mis_name) %||% "No data"
    )
  })
}

# --- Narrativa por reglas (sin LLM) ------------------------------------------
build_narrative <- function(data_main, country_name, mis_name) {
  v <- function(qid) mis_country_value(data_main, qid, country_name, mis_name)
  short_mis <- mis_short_name(mis_name)

  has_mis <- v("q8")
  if (is.na(has_mis) || has_mis == "No") {
    return(sprintf("%s does not currently have a %s in place, according to the survey response.",
                    country_name, mis_name))
  }

  bits <- c(
    sprintf("%s has a %s in place.", country_name, mis_name),
    sprintf("It is %s.", tolower(v("q9") %||% "not classified in terms of digitization")),
    if (!is.na(v("q20")) && grepl("no dedicated unit", v("q20"), ignore.case = TRUE))
      "There is no dedicated unit or team producing data analytics products."
    else if (!is.na(v("q20")))
      "There is a unit or team responsible for producing data analytics products.",
    if (!is.na(v("q24")) && v("q24") == "Yes")
      "Systematic data quality controls are in place."
    else "Systematic data quality controls do not appear to be in place.",
    if (!is.na(v("q25")) && v("q25") == "Yes")
      "Access to the underlying data is governed by a formal, documented protocol."
    else "There is no formal, documented protocol regulating data access."
  )
  paste(bits[!is.na(bits) & bits != ""], collapse = " ")
}

# --- Narrativa vía LLM (con fallback a build_narrative) ----------------------
# Requiere llm_narrative.R cargado (call_claude, have_api_key)
llm_narrative_section <- function(data_main, country_name, mis_name,
                                   qids = mis_questions_ordered()) {
  fallback <- build_narrative(data_main, country_name, mis_name)
  if (!exists("have_api_key") || !have_api_key()) return(fallback)

  tbl <- indicator_table(data_main, country_name, mis_name, qids)
  facts <- paste(sprintf("- %s: %s (%s countries answering this MIS in LAC most commonly answer: %s)",
                          tbl$question, tbl$country_value, mis_short_name(mis_name), tbl$lac_modal),
                 collapse = "\n")

  system_prompt <- paste(
    "You are a development economist writing a short, analytical brief section",
    "for a World Bank governance report on public-sector Management Information",
    "Systems (MIS) in Latin America and the Caribbean. Write 1-2 concise",
    "paragraphs, factual and neutral in tone, no bullet points."
  )
  user_prompt <- sprintf(
    "Country: %s\nMIS module: %s\n\nSurvey facts (country value vs. LAC most common answer):\n%s\n\nWrite the narrative.",
    country_name, mis_name, facts
  )

  out <- tryCatch(call_claude(system_prompt, user_prompt), error = function(e) NULL)
  if (is.null(out) || !nzchar(trimws(out))) fallback else out
}

# TODO (siguiente iteración): portar generate_brief()/add_cover_page()/
# add_disclaimer_page() de build_brief.R original (usa `officer`) para
# exportar esto como .docx con formato, gráficos embebidos (usa plot_question())
# y tabla de indicadores formateada — la estructura ya está lista para eso,
# solo falta la capa de maquetación.
