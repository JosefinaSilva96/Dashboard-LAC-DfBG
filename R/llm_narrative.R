# =============================================================================
# llm_narrative.R
# -----------------------------------------------------------------------------
# Genera narrativa analítica por país y por sección llamando a la API de Claude.
#
# La API key se lee de la variable de entorno ANTHROPIC_API_KEY. Para configurarla:
#   - Linux/Mac: export ANTHROPIC_API_KEY="sk-ant-..."
#   - Windows (PowerShell): setx ANTHROPIC_API_KEY "sk-ant-..."
#   - O en R, una vez:  Sys.setenv(ANTHROPIC_API_KEY = "sk-ant-...")
#   - O en .Renviron del proyecto:  ANTHROPIC_API_KEY=sk-ant-...
#
# Estrategia: una llamada por sección del brief. Cada llamada recibe un resumen
# estructurado de los datos del país (valores + benchmarks del income group) y
# devuelve 1-3 párrafos de análisis al estilo del brief de Austria.
#
# Si no hay API key o la llamada falla, se cae con gracia a la narrativa por
# reglas (build_narrative en build_brief.R), de modo que el brief SIEMPRE se
# genera, con o sin internet/key.
# =============================================================================

# Requiere: httr2, jsonlite (instalá con install.packages(c("httr2","jsonlite")))

ANTHROPIC_MODEL <- "claude-sonnet-5"   # ajustá si querés otro modelo
ANTHROPIC_URL   <- "https://api.anthropic.com/v1/messages"

# --- ¿hay key disponible? ----------------------------------------------------
have_api_key <- function() {
  key <- Sys.getenv("ANTHROPIC_API_KEY", "")
  nzchar(key)
}

# --- llamada de bajo nivel a la API ------------------------------------------
# Devuelve el texto de la respuesta, o NULL si falla.
call_claude <- function(system_prompt, user_prompt, max_tokens = 900) {
  key <- Sys.getenv("ANTHROPIC_API_KEY", "")
  if (!nzchar(key)) return(NULL)

  if (!requireNamespace("httr2", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    message("Claude API error: the 'httr2' and/or 'jsonlite' packages are not ",
            "installed. Run install.packages(c('httr2','jsonlite')) and restart R.")
    return(NULL)
  }

  # OJO: Claude Sonnet 5 (y en general la generación 4.6+) devuelve 400 si se
  # manda temperature/top_p/top_k con CUALQUIER valor, incluso el que sería
  # el default — hay que omitir el campo directamente, no ponerlo en 0/0.4.
  body <- list(
    model = ANTHROPIC_MODEL,
    max_tokens = max_tokens,
    system = system_prompt,
    messages = list(list(role = "user", content = user_prompt))
  )

  out <- tryCatch({
    resp <- httr2::request(ANTHROPIC_URL) |>
      httr2::req_headers(
        "x-api-key" = key,
        "anthropic-version" = "2023-06-01",
        "content-type" = "application/json"
      ) |>
      httr2::req_body_json(body) |>
      httr2::req_timeout(60) |>
      httr2::req_perform()

    parsed <- httr2::resp_body_json(resp)
    # content es una lista de bloques; tomamos los de tipo "text"
    txt <- vapply(parsed$content,
                  function(b) if (identical(b$type, "text")) b$text else "",
                  character(1))
    paste(txt[nzchar(txt)], collapse = "\n\n")
  }, httr2_http = function(e) {
    # Errores HTTP (4xx/5xx): el mensaje de httr2 solo trae el código.
    # El detalle real (qué campo rechazó la API) viene en el cuerpo JSON.
    detail <- tryCatch(httr2::resp_body_string(e$resp), error = function(e2) NULL)
    if (is.null(detail)) detail <- "(no body)"
    message("Claude API error (", conditionMessage(e), "): ", detail)
    NULL
  }, error = function(e) {
    message("Claude API error: ", conditionMessage(e))
    NULL
  })

  if (is.null(out) || !nzchar(trimws(out))) return(NULL)
  trimws(out)
}

# =============================================================================
# Traducción de respuestas abiertas al inglés
# -----------------------------------------------------------------------------
# Las respuestas de texto libre llegan en el idioma en que las escribió cada
# encuestado (inglés, español, francés...). Para que el dashboard se vea
# consistente en inglés, las traducimos on-the-fly con la misma API de Claude
# que arma la narrativa del brief. Se cachean las traducciones en memoria (por
# sesión de R) para no volver a traducir el mismo texto cada vez que se
# renderiza la pestaña o se cambia de pregunta.
#
# Si no hay ANTHROPIC_API_KEY configurada, o la llamada falla, se devuelve el
# texto original sin traducir (el dashboard nunca se rompe por esto).
# =============================================================================

.translation_cache <- new.env(parent = emptyenv())

TRANSLATE_SYSTEM_PROMPT <- paste(
  "You are a professional translator working for the World Bank.",
  "You will receive a numbered list of short survey responses, which may be",
  "in English, Spanish, French, or other languages.",
  "Translate every response into clear, natural English.",
  "If a response is already in English, return it unchanged.",
  "Keep product, tool, and platform names as they are (e.g. ChatGPT, Copilot, Diia).",
  "Return ONLY the numbered list of translations, one per line, in the exact",
  "same order and numbering as the input, with no extra commentary, no quotes,",
  "and no explanations.",
  sep = "\n"
)

translate_to_english <- function(text_vec) {
  if (length(text_vec) == 0) return(text_vec)

  # 1) Diccionario estático (data/translations_en.csv) — sin API, sin key,
  #    siempre disponible una vez que esté armado el CSV.
  static <- static_translate(text_vec)
  out <- ifelse(is.na(static), text_vec, static)

  # 2) Lo que no esté en el diccionario estático, intentamos traducirlo con
  #    la API (si hay ANTHROPIC_API_KEY). Si no hay key o falla, queda tal
  #    cual estaba (en el idioma original).
  todo_api <- which(is.na(static))
  if (length(todo_api) == 0 || !have_api_key()) return(out)

  sub_vec  <- text_vec[todo_api]
  keys     <- as.character(sub_vec)
  cached <- vapply(keys, function(k) {
    if (exists(k, envir = .translation_cache, inherits = FALSE)) {
      get(k, envir = .translation_cache, inherits = FALSE)
    } else {
      NA_character_
    }
  }, character(1), USE.NAMES = FALSE)

  todo <- which(is.na(cached))
  if (length(todo) > 0) {
    prompt <- paste(sprintf("%d. %s", seq_along(todo), sub_vec[todo]), collapse = "\n")
    resp <- tryCatch(
      call_claude(TRANSLATE_SYSTEM_PROMPT, prompt, max_tokens = 1200),
      error = function(e) NULL
    )
    translated <- sub_vec[todo]  # fallback: si algo falla, dejamos el original
    if (!is.null(resp)) {
      lines  <- strsplit(resp, "\n")[[1]]
      lines  <- trimws(lines[nzchar(trimws(lines))])
      parsed <- sub("^\\s*\\d+\\.\\s*", "", lines)
      if (length(parsed) == length(todo)) translated <- parsed
    }
    for (j in seq_along(todo)) {
      assign(keys[todo[j]], translated[j], envir = .translation_cache)
    }
    cached[todo] <- translated
  }
  out[todo_api] <- unname(cached)
  out
}

# =============================================================================
# Construcción del contexto de datos por sección
# -----------------------------------------------------------------------------
# Para cada sección, arma un bloque de texto compacto con los valores del país
# y el benchmark del income group, que se le pasa a Claude como evidencia.
# =============================================================================

# Resume las single de una sección como "Indicator: country value (ig benchmark)"
section_facts <- function(data, qids, country_name) {
  lines <- vapply(qids, function(qid) {
    q <- AGENCY_QUESTIONS[[qid]]
    if (is.null(q) || q$type != "single") return("")
    cv <- country_value(data, q, country_name)
    ig <- ig_modal(data, q, country_name)
    if (is.na(cv) && is.na(ig)) return("")
    sprintf("- %s: %s (income-group: %s)", q$short,
            ifelse(is.na(cv), "n/a", cv), ifelse(is.na(ig), "n/a", ig))
  }, character(1))
  paste(lines[nzchar(lines)], collapse = "\n")
}

# Resume las multi de una sección: opciones seleccionadas por el país
section_multi_facts <- function(data, qids, country_name) {
  out <- c()
  for (qid in qids) {
    q <- AGENCY_QUESTIONS[[qid]]
    if (is.null(q) || q$type != "multi") next
    ag <- data$agency
    cols <- resolve_cols(q, ag)
    if (length(cols) == 0) next
    opt_lab <- q$options %||% setNames(cols, cols)
    row <- dplyr::filter(ag, country == country_name)
    if (nrow(row) == 0) next
    sel <- c()
    for (cc in cols) {
      v <- row[[cc]][1]
      vl <- tolower(trimws(as.character(v)))
      if (!is.na(v) && vl != "" && !vl %in% c("0","no","none","not sure","na")) {
        sel <- c(sel, opt_lab[[cc]] %||% cc)
      }
    }
    if (length(sel)) out <- c(out, sprintf("- %s: %s", q$short,
                                           paste(sel, collapse = "; ")))
  }
  paste(out, collapse = "\n")
}

# Texto abierto de una sección
section_text_facts <- function(data, qids, country_name) {
  out <- c()
  for (qid in qids) {
    q <- AGENCY_QUESTIONS[[qid]]
    if (is.null(q) || q$type != "text") next
    vals <- make_text(data, q, country_name)$Response
    if (length(vals)) out <- c(out, sprintf("- %s: %s", q$short,
                                            paste(vals, collapse = "; ")))
  }
  paste(out, collapse = "\n")
}

# =============================================================================
# Narrativa de una sección vía LLM (con fallback)
# =============================================================================

SYSTEM_PROMPT <- paste(
  "You are a World Bank governance analyst writing a concise, analytical country brief",
  "on AI readiness in the public sector, based on the AI and Data for Better Governance (DfBG) Survey.",
  "Write in the measured, evidence-based register of a World Bank policy note.",
  "Rules:",
  "- 1 to 3 short paragraphs per section. No headers, no bullet points, no preamble.",
  "- Always interpret the country's position RELATIVE to its income-group peers, citing the benchmark percentages provided.",
  "- Identify tensions, paradoxes, or notable contrasts where the data supports them (e.g. strong governance but weak adoption).",
  "- Be specific and grounded ONLY in the data provided. Do not invent figures or facts.",
  "- Refer to the country by name. Do not use first person.",
  sep = "\n"
)

# Genera la narrativa de una sección. Devuelve texto (LLM) o NULL si no hay key/falla.
llm_section_narrative <- function(data, country_name, section_title, qids,
                                   extra_context = "") {
  facts_single <- section_facts(data, qids, country_name)
  facts_multi  <- section_multi_facts(data, qids, country_name)
  facts_text   <- section_text_facts(data, qids, country_name)
  ig <- data$agency$income_group[data$agency$country == country_name][1]
  ig_lbl <- if (is.na(ig)) "its peers" else ig

  facts <- paste(c(
    if (nzchar(facts_single)) facts_single,
    if (nzchar(facts_multi))  facts_multi,
    if (nzchar(facts_text))   facts_text,
    if (nzchar(extra_context)) extra_context
  ), collapse = "\n")

  if (!nzchar(trimws(facts))) return(NULL)

  user_prompt <- paste0(
    "Country: ", country_name, "\n",
    "Income group: ", ig_lbl, "\n",
    "Section: ", section_title, "\n\n",
    "Survey evidence for this section (country value, with income-group benchmark in parentheses):\n",
    facts, "\n\n",
    "Write the analysis for the \"", section_title, "\" section of ", country_name,
    "'s country brief."
  )

  call_claude(SYSTEM_PROMPT, user_prompt)
}
