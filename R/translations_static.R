# =============================================================================
# translations_static.R
# -----------------------------------------------------------------------------
# Traducciones al inglés PRE-ARMADAS para las respuestas de texto libre de la
# encuesta. Esto evita depender de la API de Claude (y de tener una
# ANTHROPIC_API_KEY configurada) para mostrar el dashboard en inglés.
#
# data/translations_en.csv tiene dos columnas:
#   original_text, english_text
#
# Se arma UNA VEZ:
#   1. Correr extract_unique_text_responses.R -> genera unique_text_responses.csv
#      con todas las respuestas de texto únicas de allMIS_data_forLAC.
#   2. Traducir esa lista (a mano, con Claude, con quien sea).
#   3. Guardar el resultado como data/translations_en.csv (mismas filas, en el
#      mismo orden, agregando la columna english_text).
#
# Si data/translations_en.csv no existe todavía, el dashboard sigue
# funcionando normal: simplemente no traduce nada por esta vía (translate_
# to_english() en llm_narrative.R cae a la API si hay key, o al texto
# original si no hay ninguna de las dos cosas).
# =============================================================================

.static_translations <- local({
  path <- file.path("data", "translations_en.csv")
  env  <- new.env(parent = emptyenv())

  if (file.exists(path)) {
    tbl <- tryCatch(
      utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8"),
      error = function(e) {
        message("translations_static.R: no se pudo leer ", path, ": ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(tbl) && all(c("original_text", "english_text") %in% names(tbl))) {
      for (i in seq_len(nrow(tbl))) {
        key <- trimws(as.character(tbl$original_text[i]))
        if (nzchar(key)) assign(key, as.character(tbl$english_text[i]), envir = env)
      }
      message("translations_static.R: ", nrow(tbl), " traducciones cargadas desde ", path)
    }
  }
  env
})

# Busca cada texto en el diccionario estático. Devuelve NA para los que no
# están (para que translate_to_english() decida si los manda a la API o los
# deja en el idioma original).
static_translate <- function(text_vec) {
  if (length(text_vec) == 0) return(character(0))
  vapply(text_vec, function(t) {
    key <- trimws(t)
    if (exists(key, envir = .static_translations, inherits = FALSE)) {
      get(key, envir = .static_translations, inherits = FALSE)
    } else {
      NA_character_
    }
  }, character(1), USE.NAMES = FALSE)
}
