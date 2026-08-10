# =============================================================================
# question_dictionary.R  (MIS Dashboard)
# -----------------------------------------------------------------------------
# Mismo patrón que el dashboard DfBG: cada pregunta se describe UNA vez (texto,
# tipo, columnas, niveles, colores) y tanto el gráfico Shiny como el brief se
# generan a partir de esta especificación (ver plots.R / build_brief.R).
#
# Diferencia clave con DfBG: acá el mismo diccionario aplica a los 6 módulos
# MIS (HR, Procurement, PFM, Tax, Education, Health) — verificado contra los
# 6 .docx originales, que comparten idéntica numeración q8-q30. `title` usa
# "{module}" como placeholder, reemplazado en tiempo de gráfico por el nombre
# corto del módulo (ver mis_title() abajo).
#
# Cobertura: quedan afuera q10, q17, q18, q27 (listas de "tipos de entidad" /
# "categorías de datos") porque el .docx exportado no trae la hoja de choices
# con las opciones — mismo tipo de brecha que module_metadata.R ya documentaba
# para DfBG. Se pueden agregar apenas tengamos esas opciones.
# =============================================================================

library(dplyr)

# --- Paletas -----------------------------------------------------------------

PAL_YESNO <- c("Yes" = "#66c2a4", "No" = "#fb6a4a",
               "Don\u2019t know" = "#BDBDBD", "Prefer not to respond" = "#8C8C8C")

PAL_DIGITIZED <- c(
  "Fully digitized"      = "#2E8B57",
  "Partially digitized"  = "#D4813A",
  "No"                   = "#8B2020",
  "Don\u2019t know"       = "#BDBDBD",
  "Prefer not to respond" = "#8C8C8C"
)

PAL_UNIT <- c(
  "Centralized unit/team (e.g., a single data analytics unit at the federal level supports several entities)" = "#2E8B57",
  "Decentralized unit/team (e.g., data analytics units within entity supporting exclusively that entity)"      = "#8DC26F",
  "There is no dedicated unit/team (e.g., data analytics produced on an individual basis)"                     = "#8B2020",
  "Don\u2019t know" = "#BDBDBD", "Prefer not to respond" = "#8C8C8C"
)

PAL_DRIVER <- c(
  "Strategic: continuous analytical products in response to a data-driven analytical strategy"      = "#2E8B57",
  "Bureaucratic: regular analytical products in response to administrative laws or regulations"      = "#8DC26F",
  "Ad-hoc: sporadic analytical products developed in response to ad-hoc requests"                     = "#D4813A",
  "Other (please specify)" = "#BDBDBD",
  "Don\u2019t know" = "#BDBDBD", "Prefer not to respond" = "#8C8C8C"
)

recode_keep <- function(x, map) {
  x <- as.character(x)
  out <- unname(map[x])
  ifelse(is.na(out), x, out)
}

# Reemplaza "{module}" por el nombre corto del módulo (ej. "Health MIS" -> "Health")
mis_short_name <- function(mis_name) sub(" MIS$", "", mis_name)
mis_title <- function(template, mis_name) gsub("\\{module\\}", mis_short_name(mis_name), template)

# =============================================================================
# MIS_QUESTIONS — diccionario compartido por los 6 módulos
# =============================================================================

MIS_QUESTIONS <- list(

  q8 = list(
    id = "q8", section = "Existence & Digitization",
    title = "Is there a {module} in place in the government?",
    short = "{module} in place", type = "single", cols = "q8",
    levels = c("Yes", "No"), palette = PAL_YESNO, in_brief = TRUE
  ),

  q9 = list(
    id = "q9", section = "Existence & Digitization",
    title = "Is the {module} digitized?",
    short = "{module} digitization", type = "single", cols = "q9",
    levels = c("Fully digitized", "Partially digitized", "No",
               "Don\u2019t know", "Prefer not to respond"),
    palette = PAL_DIGITIZED, in_brief = TRUE
  ),

  q11 = list(
    id = "q11", section = "Use of Data Analytics",
    title = "How often do entities use data analytics products from the {module}?",
    short = "Frequency of use", type = "single", cols = "q11",
    levels = c("All or nearly all the time",
               "More than 50% of the time, but not all the time",
               "Between 25% and 50% of the time",
               "Up to 25% of the time",
               "No entity uses data analytics products to inform decisions",
               "Don\u2019t know", "Prefer not to respond"),
    palette = NULL, in_brief = TRUE
  ),

  q12 = list(
    id = "q12", section = "Use of Data Analytics",
    title = "What agents of government use data analytics products from the {module}?",
    short = "Agents using analytics", type = "multi",
    cols = c("q12_1","q12_2","q12_3","q12_700","q12_4","q12_900","q12_998"),
    options = c(
      q12_1 = "Non-managerial staff", q12_2 = "Managerial staff",
      q12_3 = "Politicians", q12_700 = "Other",
      q12_4 = "No agent uses data analytics products to inform decisions",
      q12_900 = "Don\u2019t know", q12_998 = "Prefer not to respond"
    ), in_brief = TRUE
  ),

  q13 = list(
    id = "q13", section = "Use of Data Analytics",
    title = "Main uses of data analytics products on the {module}",
    short = "Main uses", type = "multi",
    cols = c("q13_1","q13_2","q13_3","q13_4","q13_5","q13_700","q13_6","q13_900","q13_998"),
    options = c(
      q13_1 = "Monitoring", q13_2 = "Accountability",
      q13_3 = "Transparency towards citizens", q13_4 = "Policy evaluation",
      q13_5 = "Policy design", q13_700 = "Other",
      q13_6 = "Data analytics products are not used",
      q13_900 = "Don\u2019t know", q13_998 = "Prefer not to respond"
    ), in_brief = TRUE
  ),

  q14 = list(
    id = "q14", section = "Institutional Arrangements",
    title = "Main driver for developing data analytics products on the {module}",
    short = "Main driver", type = "single", cols = "q14",
    levels = c("Strategic: continuous analytical products in response to a data-driven analytical strategy",
               "Bureaucratic: regular analytical products in response to administrative laws or regulations",
               "Ad-hoc: sporadic analytical products developed in response to ad-hoc requests",
               "Other (please specify)", "Don\u2019t know", "Prefer not to respond"),
    palette = PAL_DRIVER, in_brief = TRUE
  ),

  q15 = list(
    id = "q15", section = "Institutional Arrangements",
    title = "Are there formal communication channels to disseminate data analytics products from the {module}?",
    short = "Formal comm. channels", type = "single", cols = "q15",
    levels = c("Yes", "No", "Don\u2019t know", "Prefer not to respond"),
    palette = PAL_YESNO, in_brief = TRUE
  ),

  q16 = list(
    id = "q16", section = "Institutional Arrangements",
    title = "Type of data analytics produced on the {module}",
    short = "Analytics type", type = "multi",
    cols = c("q16_1","q16_2","q16_3","q16_900","q16_998"),
    options = c(
      q16_1 = "Descriptive analytics", q16_2 = "Diagnostic analytics",
      q16_3 = "Predictive analytics", q16_900 = "Don\u2019t know",
      q16_998 = "Prefer not to respond"
    ), in_brief = TRUE
  ),

  q19 = list(
    id = "q19", section = "Institutional Arrangements",
    title = "Are data analytics products on the {module} regularly refined and updated?",
    short = "Regularly refined", type = "single", cols = "q19",
    levels = c("Yes", "No", "Don\u2019t know", "Prefer not to respond"),
    palette = PAL_YESNO, in_brief = TRUE
  ),

  q20 = list(
    id = "q20", section = "Institutional Arrangements",
    title = "Is there a dedicated unit/team producing data analytics products on the {module}?",
    short = "Dedicated unit", type = "single", cols = "q20",
    levels = c("Centralized unit/team (e.g., a single data analytics unit at the federal level supports several entities)",
               "Decentralized unit/team (e.g., data analytics units within entity supporting exclusively that entity)",
               "There is no dedicated unit/team (e.g., data analytics produced on an individual basis)",
               "Don\u2019t know", "Prefer not to respond"),
    palette = PAL_UNIT, in_brief = TRUE
  ),

  q21 = list(
    id = "q21", section = "Funding & Collaboration",
    title = "Are there internal funding opportunities to support analytical projects on the {module}?",
    short = "Internal funding", type = "single", cols = "q21",
    levels = c("Yes", "No", "Don\u2019t know", "Prefer not to respond"),
    palette = PAL_YESNO, in_brief = TRUE
  ),

  q22 = list(
    id = "q22", section = "Funding & Collaboration",
    title = "Is there a strategy to collaborate on {module} data analytics with academics/non-profits/multilaterals?",
    short = "External collaboration", type = "single", cols = "q22",
    levels = c("Yes", "No", "Don\u2019t know", "Prefer not to respond"),
    palette = PAL_YESNO, in_brief = TRUE
  ),

  q23 = list(
    id = "q23", section = "Funding & Collaboration",
    title = "Main drivers for collaborating on {module} data analytics externally",
    short = "Collaboration drivers", type = "multi",
    cols = c("q23_1","q23_2","q23_3","q23_4","q23_700","q23_6","q23_900","q23_998"),
    options = c(
      q23_1 = "Technical assistance", q23_2 = "Data access",
      q23_3 = "Innovations in data analytics", q23_4 = "Financial support",
      q23_700 = "Other",
      q23_6 = "There is no strategy to collaborate on data analytics with academics, etc.",
      q23_900 = "Don\u2019t know", q23_998 = "Prefer not to respond"
    ), in_brief = TRUE
  ),

  q24 = list(
    id = "q24", section = "Data Governance & Quality",
    title = "Are there systematic data quality controls in place for the {module}?",
    short = "Quality controls", type = "single", cols = "q24",
    levels = c("Yes", "No", "Don\u2019t know", "Prefer not to respond"),
    palette = PAL_YESNO, in_brief = TRUE
  ),

  q25 = list(
    id = "q25", section = "Data Governance & Quality",
    title = "Is there a formal protocol regulating access to {module} data?",
    short = "Access protocol", type = "single", cols = "q25",
    levels = c("Yes", "No", "Don\u2019t know", "Prefer not to respond"),
    palette = PAL_YESNO, in_brief = TRUE
  ),

  q26 = list(
    id = "q26", section = "Data Governance & Quality",
    title = "Is there an engagement metric measuring how often {module} data is accessed?",
    short = "Engagement metric", type = "single", cols = "q26",
    levels = c("Yes", "No", "Don\u2019t know", "Prefer not to respond"),
    palette = PAL_YESNO, in_brief = TRUE
  ),

  q28 = list(
    id = "q28", section = "Data Governance & Quality",
    title = "Is there a data inventory listing all available data on the {module}?",
    short = "Data inventory", type = "single", cols = "q28",
    levels = c("Yes", "No", "Don\u2019t know", "Prefer not to respond"),
    palette = PAL_YESNO, in_brief = TRUE
  )
)

# Preguntas de texto libre / listado que van directo al brief como cita, no como gráfico
MIS_TEXT_QUESTIONS <- c("q29")  # 29. Lista de contribuyentes al cuestionario

# --- Helper: arma el vector de preguntas en orden para un módulo dado -------
mis_questions_ordered <- function() {
  c("q8","q9","q11","q12","q13","q14","q15","q16","q19","q20",
    "q21","q22","q23","q24","q25","q26","q28")
}

# =============================================================================
# CAP_QUESTIONS — cuestionario "Capabilities/Capacidades" (indcap_data.dta)
# -----------------------------------------------------------------------------
# A diferencia de MIS_QUESTIONS, este cuestionario NO tiene dimensión de
# módulo: una fila por país (16 países respondieron). Es el análogo al
# cuestionario "Agency" del dashboard DfBG original: preguntas de nivel
# institucional/país sobre capacidades analíticas de la administración
# pública, no de un sistema MIS particular.
# =============================================================================

CAP_QUESTIONS <- list(
  q7 = list(
    id = "q7", section = "Capabilities",
    title = "Is there a dedicated career track in the government specifically designed for data analytics?",
    short = "Dedicated career track", type = "single", cols = "q7",
    levels = c("Yes", "No", "Don\u2019t know", "Prefer not to respond"),
    palette = PAL_YESNO, in_brief = TRUE
  ),
  q8 = list(
    id = "q8", section = "Capabilities",
    title = "Are there assessments in the government (e.g., exams, focus groups, surveys) of the analytical skills of public servants?",
    short = "Skills assessments", type = "single", cols = "q8",
    levels = c("Yes", "No", "Don\u2019t know", "Prefer not to respond"),
    palette = PAL_YESNO, in_brief = TRUE
  ),
  q9 = list(
    id = "q9", section = "Capabilities",
    title = "Is there training in the government that supports data analytics skills among public servants?",
    short = "Training available", type = "single", cols = "q9",
    levels = c("Yes", "No", "Don\u2019t know", "Prefer not to respond"),
    palette = PAL_YESNO, in_brief = TRUE
  ),
  q10 = list(
    id = "q10", section = "Capabilities",
    title = "Are there initiatives in the government to strengthen the capacity of public servants in using data analytics products for decision-making?",
    short = "Capacity-building initiatives", type = "single", cols = "q10",
    levels = c("Yes", "No", "Don\u2019t know", "Prefer not to respond"),
    palette = PAL_YESNO, in_brief = TRUE
  )
)

cap_questions_ordered <- function() c("q7","q8","q9","q10")

# Comentarios de texto libre asociados a cada pregunta de Capabilities
CAP_COMMENT_COLS <- c(q7 = "q7_comments", q8 = "q8_comments",
                       q9 = "q9_comments", q10 = "q10_comments")
