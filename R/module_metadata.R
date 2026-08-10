# =============================================================================
# module_metadata.R  (MIS Dashboard)
# -----------------------------------------------------------------------------
# Texto EXACTO de cada pregunta, tal cual viene del cuestionario (verificado
# contra Health__Salud__Questionnaire_ENG.docx; idéntico en los otros 5 .docx
# salvo el nombre del sistema, que queda como placeholder "{module}").
#
# Cobertura: igual que en DfBG, quedan afuera las preguntas cuyas OPCIONES de
# respuesta no vienen en el .docx exportado (falta la hoja "choices" del
# XLSForm): q10 (tipos de entidad que usan analytics), q17/q18 (categorías de
# datos disponibles/usadas), q27 (tipos de entidad que acceden a datos), y la
# pregunta abierta q29 (lista de contribuyentes). Se agregan apenas tengamos
# esas opciones — avisame si las tenés en algún otro archivo (el XLSForm
# original .xlsx, por ejemplo).
# =============================================================================

MIS_SECTIONS <- list(
  list(section = "Existence & Digitization",
       qids = c("q8", "q9")),
  list(section = "Use of Data Analytics",
       qids = c("q11", "q12", "q13")),
  list(section = "Institutional Arrangements",
       qids = c("q14", "q15", "q16", "q19", "q20")),
  list(section = "Funding & Collaboration",
       qids = c("q21", "q22", "q23")),
  list(section = "Data Governance & Quality",
       qids = c("q24", "q25", "q26", "q28"))
)

# {module} se reemplaza por mis_short_name(mis) al momento de graficar/exportar
MIS_QTEXT <- c(
  q8  = "Is there a {module} in place in the government?",
  q9  = "Is the {module} digitized?",
  q11 = "How often, approximately, does (do) this (these) entity (entities) use data analytics products on the {module} to inform decisions?",
  q12 = "What agents of government, if any, use data analytics products on the {module} to inform decisions?",
  q13 = "What are the main uses of data analytics products on the {module}?",
  q14 = "What is the main driver for the development of data analytics products on the {module}?",
  q15 = "Are there formal communication channels to disseminate data analytics products on the {module} within the government?",
  q16 = "What best describes the type of data analytics on the {module}?",
  q19 = "Are data analytics products on the {module} regularly refined and updated to answer the demands of decision-makers?",
  q20 = "Is there a dedicated unit/team responsible for producing data analytics products on the {module}?",
  q21 = "Are there internal funding opportunities (for example, calls for proposals) to support analytical projects within the government on the {module}?",
  q22 = "Is there a strategy to collaborate on data analytics on the {module} with academics, non-profits, foundations, or multilateral organizations?",
  q23 = "What are the main drivers for collaborating on data analytics on the {module} with academics, non-profits, foundations, or multilateral organizations?",
  q24 = "Are there data systematic quality controls (e.g., data cleaning, coverage, harmonization) in place for the {module}?",
  q25 = "Is there a formal and well-documented protocol that regulates access to the {module} data within the government?",
  q26 = "Is there an engagement metric to measure how often data on the {module} is being accessed by government entities?",
  q28 = "Is there a data inventory that lists all available data on the {module}?"
)

# Preguntas cuyas OPCIONES no están wireadas a MIS_QUESTIONS (falta la hoja
# choices del XLSForm) pero cuyo TEXTO sí tenemos, para labels en el tab
# "Text responses" (comentarios libres existen igual aunque no grafiquemos
# la pregunta en sí).
MIS_QTEXT_EXTRA <- c(
  q10 = "What type of government entities, if any, use data analytics products on the {module} to inform decisions?",
  q17 = "Which of the following data element categories are available on the {module}?",
  q18 = "Which of the following data element categories are used for producing data analytics products on the {module}?",
  q27 = "What type of government entities, if any, access data on the {module}?",
  q29 = "Please provide a comprehensive list of the individuals who contributed to answering the modules on the {module}, including name, position and institution in government, and email"
)
MIS_QTEXT_ALL <- c(MIS_QTEXT, MIS_QTEXT_EXTRA)
