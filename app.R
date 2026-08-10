# =============================================================================
# app.R  (MIS Dashboard — LAC Government Analytics Survey)
# -----------------------------------------------------------------------------
# Selector de país + módulo MIS -> gráficos por sección (país vs. promedio
# LAC) + tab de overview global + tab de brief narrativo.
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

ui <- page_navbar(
  title = "LAC Government Analytics Survey — MIS Dashboard",
  theme = bs_theme(bootswatch = "flatly", primary = "#002245"),

  nav_panel(
    "Country x Module",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("country", "Country", choices = country_choices(DATA), selected = "Chile"),
        uiOutput("module_selector"),
        radioButtons("scope", "View", choices = c("Country only" = "country",
                                                    "vs. LAC average" = "compare"),
                     selected = "compare"),
        hr(),
        p(class = "text-muted small",
          "Comparisons are against the average of all LAC countries that answered the same MIS module ",
          "(weighted 1/n by country).")
      ),
      navset_tab(
        nav_panel("Existence & Digitization", uiOutput("plots_existence")),
        nav_panel("Use of Data Analytics",     uiOutput("plots_use")),
        nav_panel("Institutional Arrangements",uiOutput("plots_institutional")),
        nav_panel("Funding & Collaboration",   uiOutput("plots_funding")),
        nav_panel("Data Governance & Quality", uiOutput("plots_governance")),
        nav_panel("Brief (narrative)", 
                   h4(textOutput("brief_title")),
                   tableOutput("brief_table"),
                   hr(),
                   verbatimTextOutput("brief_narrative"))
      )
    )
  ),

  nav_panel(
    "Overview (all countries)",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("ov_module", "MIS module", choices = MIS_TYPES, selected = "Health MIS")
      ),
      h4("Module health across LAC"),
      tableOutput("overview_table"),
      hr(),
      h4(textOutput("ranking_title")),
      tableOutput("ranking_table")
    )
  )
)

server <- function(input, output, session) {

  output$module_selector <- renderUI({
    req(input$country)
    mods <- modules_for_country(DATA, input$country)
    if (!length(mods)) mods <- MIS_TYPES
    selectInput("mis", "MIS module", choices = mods, selected = mods[1])
  })

  section_plots <- function(qids) {
    renderUI({
      req(input$country, input$mis)
      plots <- lapply(qids, function(qid) {
        if (is.null(MIS_QUESTIONS[[qid]])) return(NULL)
        p <- tryCatch(
          plot_question(DATA$main, qid, input$country, input$mis, scope = input$scope),
          error = function(e) NULL
        )
        if (is.null(p)) return(NULL)
        tagList(renderPlot(p, height = 320), hr())
      })
      tagList(plots)
    })
  }

  output$plots_existence      <- section_plots(c("q8", "q9"))
  output$plots_use            <- section_plots(c("q11", "q12", "q13"))
  output$plots_institutional  <- section_plots(c("q14", "q15", "q16", "q19", "q20"))
  output$plots_funding        <- section_plots(c("q21", "q22", "q23"))
  output$plots_governance     <- section_plots(c("q24", "q25", "q26", "q28"))

  output$brief_title <- renderText({
    req(input$country, input$mis)
    paste(input$country, "\u2014", input$mis)
  })

  output$brief_table <- renderTable({
    req(input$country, input$mis)
    indicator_table(DATA$main, input$country, input$mis) |>
      rename(Question = question, `Country answer` = country_value, `LAC most common` = lac_modal) |>
      select(Question, `Country answer`, `LAC most common`)
  })

  output$brief_narrative <- renderText({
    req(input$country, input$mis)
    llm_narrative_section(DATA$main, input$country, input$mis)
  })

  output$overview_table <- renderTable({
    gs_module_overview(DATA$main) |>
      rename(Module = mis, `# countries` = n_countries,
             `% MIS in place` = pct_in_place, `% digitized` = pct_digitized,
             `% dedicated unit` = pct_dedicated_unit, `% quality controls` = pct_quality_controls)
  }, digits = 0)

  output$ranking_title <- renderText(paste("Country ranking \u2014", input$ov_module))

  output$ranking_table <- renderTable({
    req(input$ov_module)
    gs_country_score(DATA$main, input$ov_module) |> rename(Country = country, Score = score)
  })
}

shinyApp(ui, server)
