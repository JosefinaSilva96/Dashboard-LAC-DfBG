# =============================================================================
# app.R  (MIS Dashboard — LAC Government Analytics Survey)
# -----------------------------------------------------------------------------
# Layout calcado del dashboard DfBG original: sidebar (país, income group,
# contadores de cuestionarios respondidos, botón de descarga de brief,
# selector de cuestionario) + panel principal con tabs (About / How to use /
# Explore charts / Text responses).
#
# "Cuestionario" acá cubre 7 opciones: los 6 módulos MIS (Human Resources,
# Procurement, Public Financial, Tax, Education, Health) + Capabilities
# (indcap_data, sin dimensión de módulo — análogo a "Agency" en DfBG).
#
# Correr con:  shiny::runApp()   (desde esta carpeta, con data/ poblada)
# =============================================================================

library(shiny)
library(bslib)
library(dplyr)

#source("data_load.R")
#source("question_dictionary.R")
#source("module_metadata.R")
#source("global_stats.R")
#source("plots.R")
#source("build_brief.R")
#source("translations_static.R")
#if (file.exists("llm_narrative.R")) source("llm_narrative.R")

DATA <- load_mis()
QUESTIONNAIRE_CHOICES <- c(MIS_TYPES, "Capabilities")

# Helper: si el .md no existe todavía, no rompe la app
includeMarkdown_safe <- function(path) {
  if (file.exists(path)) shiny::includeMarkdown(path)
  else div(class = "text-muted", em(paste("(", path, "no encontrado todav\u00eda \u2014 falta redactar este texto)")))
}

# =============================================================================
# UI
# =============================================================================

ui <- page_fillable(
  title = "AI & Data for Better Governance \u2014 MIS Dashboard",
  theme = bs_theme(bootswatch = "flatly", primary = "#002245"),

  div(class = "d-flex align-items-center", style = "padding: 8px 4px 4px 4px;",
      h3("AI & Data for Better Governance \u2014 MIS Dashboard", style = "color:#002245; font-weight:600;")
  ),

  layout_sidebar(
    sidebar = sidebar(
      width = 300,

      h5("Economy"),
      selectInput("country", NULL, choices = country_choices(DATA), selected = country_choices(DATA)[1]),

      uiOutput("income_badge"),
      uiOutput("count_badges"),

      br(),
      downloadButton("download_brief", "Download economy brief (.docx)",
                      class = "btn-secondary w-100"),

      hr(),
      h5("Questionnaire"),
      radioButtons("questionnaire", NULL, choices = QUESTIONNAIRE_CHOICES,
                   selected = QUESTIONNAIRE_CHOICES[1]),

      conditionalPanel(
        "input.questionnaire != 'Capabilities'",
        radioButtons("scope", "View", choices = c("Country only" = "country",
                                                    "vs. LAC average" = "compare"),
                     selected = "compare")
      )
    ),

    navset_tab(
      nav_panel("About the survey", includeMarkdown_safe("about_survey.md")),
      nav_panel("How to use this dashboard", includeMarkdown_safe("how_to_use.md")),
      nav_panel("Explore charts", uiOutput("explore_charts")),
      nav_panel("Text responses", uiOutput("text_responses"))
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  is_cap <- reactive(input$questionnaire == "Capabilities")

  # --- Sidebar: badges --------------------------------------------------

  output$income_badge <- renderUI({
    req(input$country)
    ig <- country_income_group(DATA, input$country)
    if (is.na(ig)) return(NULL)
    div(class = "badge bg-primary mb-2", style = "font-size: 0.85rem; white-space: normal;",
        paste("Income group:", ig))
  })

  output$count_badges <- renderUI({
    req(input$country)
    n_mis <- n_modules_for_country(DATA, input$country)
    has_cap <- has_capabilities(DATA, input$country)
    div(
      span(class = "badge bg-dark me-1 mb-1", paste("MIS modules:", n_mis)),
      span(class = "badge bg-info text-dark mb-1", paste("Capabilities:", if (has_cap) "Yes" else "No"))
    )
  })

  # --- Gráficos: un reactive() por pregunta (MIS y Capabilities), + un
  # renderPlot() y un downloadHandler() de PNG fijos por pregunta. Así el
  # gráfico se calcula una sola vez y se reusa tanto para mostrarlo en
  # pantalla como para el botón de descarga. -------------------------------

  mis_qids <- mis_questions_ordered()
  cap_qids <- cap_questions_ordered()

  plot_reactives_mis <- setNames(lapply(mis_qids, function(qid) {
    reactive({
      req(input$country, input$questionnaire)
      if (is_cap()) return(NULL)
      mods <- modules_for_country(DATA, input$country)
      if (!(input$questionnaire %in% mods)) return(NULL)
      tryCatch(
        plot_question(DATA$main, qid, input$country, input$questionnaire, scope = input$scope %||% "compare"),
        error = function(e) NULL
      )
    })
  }), mis_qids)

  plot_reactives_cap <- setNames(lapply(cap_qids, function(qid) {
    reactive({
      req(input$country)
      if (!is_cap()) return(NULL)
      if (!has_capabilities(DATA, input$country)) return(NULL)
      tryCatch(
        plot_cap_question(DATA$indcap, qid, input$country, scope = input$scope %||% "compare"),
        error = function(e) NULL
      )
    })
  }), cap_qids)

  png_filename <- function(qid, prefix) {
    function() {
      country_slug <- gsub("[^A-Za-z0-9]+", "_", input$country %||% "country")
      paste0(country_slug, "_", prefix, "_q", sub("^q", "", qid), ".png")
    }
  }

  for (qid in mis_qids) {
    local({
      qid_local <- qid
      output[[paste0("mis_plot_", qid_local)]] <- renderPlot({
        req(plot_reactives_mis[[qid_local]]())
      })
      output[[paste0("mis_dl_", qid_local)]] <- downloadHandler(
        filename = png_filename(qid_local, "MIS"),
        content = function(file) {
          p <- plot_reactives_mis[[qid_local]]()
          req(p)
          ggplot2::ggsave(file, plot = p, width = 8, height = 4.5, dpi = 150, bg = "white")
        }
      )
    })
  }

  for (qid in cap_qids) {
    local({
      qid_local <- qid
      output[[paste0("cap_plot_", qid_local)]] <- renderPlot({
        req(plot_reactives_cap[[qid_local]]())
      })
      output[[paste0("cap_dl_", qid_local)]] <- downloadHandler(
        filename = png_filename(qid_local, "Capabilities"),
        content = function(file) {
          p <- plot_reactives_cap[[qid_local]]()
          req(p)
          ggplot2::ggsave(file, plot = p, width = 8, height = 4.5, dpi = 150, bg = "white")
        }
      )
    })
  }

  # --- Explore charts: arma el layout, referenciando los outputs fijos ----

  plot_block <- function(plot_id, dl_id) {
    div(class = "mb-2",
        plotOutput(plot_id, height = 300),
        downloadButton(dl_id, "Download PNG", class = "btn-sm btn-outline-secondary mt-1 mb-3"),
        hr()
    )
  }

  output$explore_charts <- renderUI({
    req(input$country, input$questionnaire)

    if (is_cap()) {
      if (!has_capabilities(DATA, input$country)) {
        return(div(class = "text-muted mt-3",
                    em(paste(input$country, "did not answer the Capabilities questionnaire."))))
      }
      return(tagList(lapply(cap_qids, function(qid) {
        plot_block(paste0("cap_plot_", qid), paste0("cap_dl_", qid))
      })))
    }

    mis_name <- input$questionnaire
    mods <- modules_for_country(DATA, input$country)
    if (!(mis_name %in% mods)) {
      return(div(class = "text-muted mt-3",
                  em(paste(input$country, "did not answer the", mis_name, "questionnaire."))))
    }

    tagList(lapply(MIS_SECTIONS, function(sec) {
      qids <- sec$qids[sec$qids %in% names(MIS_QUESTIONS)]
      tagList(
        h5(sec$section, style = "margin-top: 12px; color:#002245;"),
        lapply(qids, function(qid) {
          plot_block(paste0("mis_plot_", qid), paste0("mis_dl_", qid))
        })
      )
    }))
  })

  # --- Text responses (comentarios libres) ---------------------------------

  output$text_responses <- renderUI({
    req(input$country, input$questionnaire)

    if (is_cap()) {
      row <- DATA$indcap |> filter(country == input$country)
      if (!nrow(row)) return(div(class = "text-muted", em("No responses for this country.")))
      blocks <- lapply(cap_questions_ordered(), function(qid) {
        col <- CAP_COMMENT_COLS[[qid]]
        txt <- if (col %in% names(row)) row[[col]][1] else NA
        if (is.na(txt) || !nzchar(trimws(txt %||% ""))) return(NULL)
        txt_en <- if (exists("translate_to_english")) translate_to_english(txt) else txt
        qnum <- sub("^q", "", qid)
        label <- paste0(qnum, ". ", CAP_QUESTIONS[[qid]]$title)
        tagList(strong(label), p(txt_en), hr())
      })
      return(tagList(blocks))
    }

    row <- DATA$main |> filter(country == input$country, mis == input$questionnaire)
    if (!nrow(row)) return(div(class = "text-muted", em("No responses for this country/module.")))
    comment_cols <- grep("_comments$", names(row), value = TRUE)
    blocks <- lapply(comment_cols, function(cc) {
      txt <- row[[cc]][1]
      if (is.na(txt) || !nzchar(trimws(txt %||% ""))) return(NULL)
      txt_en <- if (exists("translate_to_english")) translate_to_english(txt) else txt
      qid <- sub("_comments$", "", cc)
      qnum <- sub("^q", "", qid)
      qtext <- MIS_QTEXT_ALL[[qid]]
      label <- if (!is.null(qtext)) paste0(qnum, ". ", mis_title(qtext, input$questionnaire))
               else cc  # fallback si aparece una columna de comentarios que no mapeamos
      tagList(strong(label), p(txt_en), hr())
    })
    if (!any(!vapply(blocks, is.null, logical(1)))) {
      return(div(class = "text-muted", em("No free-text comments for this country/module.")))
    }
    tagList(blocks)
  })

  # --- Download brief -------------------------------------------------------

  output$download_brief <- downloadHandler(
    filename = function() paste0(gsub(" ", "_", input$country), "_brief.docx"),
    content = function(file) {
      if (is_cap()) {
        tbl <- purrr::map_dfr(cap_questions_ordered(), function(qid) {
          q <- CAP_QUESTIONS[[qid]]
          row <- DATA$indcap |> filter(country == input$country)
          tibble::tibble(question = q$title,
                          answer = if (nrow(row)) as.character(row[[q$cols[1]]][1]) else "No data")
        })
        narrative <- paste("Capabilities questionnaire responses for", input$country, "on data analytics capacity-building.")
      } else {
        tbl <- indicator_table(DATA$main, input$country, input$questionnaire) |>
          select(question, country_value)
        narrative <- llm_narrative_section(DATA$main, input$country, input$questionnaire)
      }

      doc <- officer::read_docx()
      doc <- officer::body_add_par(doc, paste(input$country, "\u2014", input$questionnaire), style = "heading 1")
      doc <- officer::body_add_par(doc, narrative, style = "Normal")
      doc <- officer::body_add_par(doc, "", style = "Normal")
      doc <- officer::body_add_table(doc, tbl, style = "table_template")
      print(doc, target = file)
    }
  )
}

shinyApp(ui, server)
