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

source("data_load.R")

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
write.csv(out, file.path("data", "unique_text_responses.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

message("Guardado en data/unique_text_responses.csv")
