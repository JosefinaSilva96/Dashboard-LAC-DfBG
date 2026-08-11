# =============================================================================
# extract_unique_text_responses.R
# -----------------------------------------------------------------------------
# Paso 1 del pipeline de traducción (ver translations_static.R). Extrae TODAS
# las respuestas de texto libre únicas (columnas *_comments) de
# allMIS_data_forLAC.dta e indcap_data.dta, y las guarda en
# data/unique_text_responses.csv (una columna: original_text).
#
# Correr UNA vez (o cuando entren respuestas nuevas a la encuesta):
#   source("extract_unique_text_responses.R")
#
# Después: source("batch_translate_responses.R")  (necesita ANTHROPIC_API_KEY)
# =============================================================================

source("R/data_load.R")

DATA <- load_mis()

comment_cols_main   <- grep("_comments$", names(DATA$main), value = TRUE)
comment_cols_indcap <- grep("_comments$", names(DATA$indcap), value = TRUE)

texts <- c(
  unlist(lapply(comment_cols_main, function(cc) DATA$main[[cc]])),
  unlist(lapply(comment_cols_indcap, function(cc) DATA$indcap[[cc]]))
)

texts <- trimws(as.character(texts))
texts <- texts[!is.na(texts) & nzchar(texts) & tolower(texts) != "na"]
uniq  <- sort(unique(texts))

message("Encontradas ", length(uniq), " respuestas de texto únicas.")

out <- data.frame(original_text = uniq, stringsAsFactors = FALSE)

# OJO Windows: write.csv(..., fileEncoding="UTF-8") con strings que YA están
# marcados como UTF-8 internamente puede terminar escribiendo un doble
# encoding (ó -> Ã³). El patrón seguro es abrir la conexión ya en UTF-8 y
# escribir sobre esa conexión, sin pasarle fileEncoding a write.csv.
con <- file(file.path("data", "unique_text_responses.csv"), open = "w", encoding = "UTF-8")
write.csv(out, con, row.names = FALSE)
close(con)

message("Guardado en data/unique_text_responses.csv")
