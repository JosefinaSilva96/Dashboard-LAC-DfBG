# =============================================================================
# batch_translate_responses.R
# -----------------------------------------------------------------------------
# Paso 2 del pipeline de traducción. Lee data/unique_text_responses.csv
# (generado por extract_unique_text_responses.R) y traduce cada respuesta al
# inglés llamando a la API de Claude, en tandas de a POCOS textos por llamada
# (para que las respuestas largas no se corten). Guarda el resultado en
# data/translations_en.csv, que translations_static.R lee automáticamente al
# arrancar la app (sin necesidad de API key en producción, una vez armado).
#
# REQUIERE: ANTHROPIC_API_KEY seteada (ver instrucciones en llm_narrative.R)
#
# Correr UNA vez:
#   Sys.setenv(ANTHROPIC_API_KEY = "sk-ant-...")
#   source("batch_translate_responses.R")
#
# Es incremental: si ya existe data/translations_en.csv, NO vuelve a traducir
# lo que ya está ahí (solo lo que falte) — podés cortar y re-correr sin miedo
# a duplicar trabajo o gastar de más.
# =============================================================================

source("llm_narrative.R")   # call_claude(), have_api_key()

if (!have_api_key()) stop(
  "No hay ANTHROPIC_API_KEY seteada. Corré primero:\n",
  '  Sys.setenv(ANTHROPIC_API_KEY = "sk-ant-...")'
)

CHUNK_SIZE <- 8   # textos por llamada a la API (bajo, porque hay respuestas muy largas)

src_path <- file.path("data", "unique_text_responses.csv")
if (!file.exists(src_path)) stop(
  "No existe ", src_path, ". Corré primero source('extract_unique_text_responses.R')."
)

src <- utils::read.csv(src_path, stringsAsFactors = FALSE, encoding = "UTF-8")
all_texts <- trimws(src$original_text)

out_path <- file.path("data", "translations_en.csv")
done <- data.frame(original_text = character(), english_text = character(),
                    stringsAsFactors = FALSE)
if (file.exists(out_path)) {
  done <- utils::read.csv(out_path, stringsAsFactors = FALSE, encoding = "UTF-8")
  message(nrow(done), " traducciones ya existentes en ", out_path, " (no se repiten).")
}

todo <- setdiff(all_texts, done$original_text)
message(length(todo), " textos por traducir (de ", length(all_texts), " totales).")

TRANSLATE_SYSTEM_PROMPT_BATCH <- paste(
  "You are a professional translator working for the World Bank, translating",
  "open-ended responses from a government survey (Latin America and the Caribbean)",
  "into English. Responses may be in Spanish, Portuguese, or already in English.",
  "",
  "You will receive several items, each delimited by a line '----ITEM n----'",
  "followed by the original text (which may itself span multiple lines/paragraphs).",
  "",
  "For EACH item, respond with a line '----ITEM n----' followed by the English",
  "translation of that item's full text (preserve paragraph breaks with blank",
  "lines if the original has them). Keep URLs, institution names, acronyms, and",
  "system/platform names (e.g. SICOES, SIAFI, Tableau) UNCHANGED. Do not summarize",
  "or shorten — translate the full text faithfully. If a text is already in",
  "English, return it unchanged. Return ONLY the items in the exact same order",
  "and numbering as the input, no extra commentary before or after.",
  sep = "\n"
)

parse_batch_response <- function(resp, n) {
  # Divide por las marcas "----ITEM n----" y devuelve un vector de largo n
  # (o NULL si el parseo no da justo n piezas -> hacemos fallback afuera).
  parts <- strsplit(resp, "----ITEM \\d+----")[[1]]
  parts <- parts[-1]  # el primer elemento es lo que queda antes del primer marcador (vacío)
  parts <- trimws(parts)
  if (length(parts) != n) return(NULL)
  parts
}

results <- list()
n_chunks <- ceiling(length(todo) / CHUNK_SIZE)

for (i in seq_len(n_chunks)) {
  idx <- ((i - 1) * CHUNK_SIZE + 1):min(i * CHUNK_SIZE, length(todo))
  chunk <- todo[idx]

  prompt <- paste(
    sprintf("----ITEM %d----\n%s", seq_along(chunk), chunk),
    collapse = "\n\n"
  )

  message("Tanda ", i, "/", n_chunks, " (", length(chunk), " textos)...")

  resp <- tryCatch(
    call_claude(TRANSLATE_SYSTEM_PROMPT_BATCH, prompt, max_tokens = 8000),
    error = function(e) { message("  error: ", conditionMessage(e)); NULL }
  )

  translated <- NULL
  if (!is.null(resp)) translated <- parse_batch_response(resp, length(chunk))

  if (is.null(translated)) {
    message("  ! parseo falló o la API no respondió — se reintenta en tandas de a 1")
    translated <- character(length(chunk))
    for (j in seq_along(chunk)) {
      r1 <- tryCatch(
        call_claude(TRANSLATE_SYSTEM_PROMPT_BATCH,
                     sprintf("----ITEM 1----\n%s", chunk[j]), max_tokens = 2000),
        error = function(e) NULL
      )
      p1 <- if (!is.null(r1)) parse_batch_response(r1, 1) else NULL
      translated[j] <- if (!is.null(p1)) p1[1] else chunk[j]  # último fallback: texto original
      Sys.sleep(0.3)
    }
  }

  results[[i]] <- data.frame(original_text = chunk, english_text = translated,
                              stringsAsFactors = FALSE)

  # Guardado incremental por si se corta a mitad de camino
  partial <- rbind(done, do.call(rbind, results))
  write.csv(partial, out_path, row.names = FALSE, fileEncoding = "UTF-8")

  Sys.sleep(0.5)
}

message("Listo. ", nrow(partial), " traducciones guardadas en ", out_path)
