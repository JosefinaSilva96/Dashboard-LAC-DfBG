# Data for Better Governance Government Analytics Ecosystems in Latin America Dashboard

Interactive R Shiny dashboard for exploring results from the **LAC Government Analytics Survey**, covering six Management Information Systems (MIS) modules plus a Capabilities questionnaire, for countries in Latin America and the Caribbean.

---

## 1. What this dashboard does

For a selected country, the dashboard shows:

- **Coverage badges**: income group, number of MIS modules answered, and whether the country completed the Capabilities questionnaire.
- **Charts** for every survey question, either showing the country's own responses or comparing them against the average of all countries that answered that module (with the country's selected category marked by a blue diamond ◆).
- **Free-text responses**: open-ended comments countries provided for each question, translated into English when needed.
- **PNG downloads** for every individual chart.
- Two informational tabs (**About the survey** and **How to use this dashboard**) loaded from Markdown files.

---

## 2. Repository structure

```
.
├── app.R                  # Main Shiny app: UI + server
├── plots.R                # Chart engine (ggplot2) — bar charts with country vs. average
├── about_survey.md         # Content for the "About the survey" tab
├── how_to_use.md            # Content for the "How to use this dashboard" tab
├── data/                    # Survey data (not tracked here — see section 4)
│
│   # The files below are referenced by app.R but not included in this
│   # handoff — they hold the data-loading and dictionary logic and need
│   # to exist in the project root for the app to run:
├── data_load.R              # load_mis(), country_choices(), income group / module helpers
├── question_dictionary.R    # MIS_QUESTIONS, CAP_QUESTIONS, MIS_QTEXT_ALL, question ordering
├── module_metadata.R        # MIS_TYPES, MIS_SECTIONS, mis_title(), mis_short_name()
├── global_stats.R           # n_modules_for_country(), has_capabilities(), modules_for_country()
├── translations_static.R    # translate_to_english() and static translation dictionaries
└── llm_narrative.R          # (optional) LLM-generated narrative text — only sourced if present
```

> `build_brief.R` is no longer needed — the "Download economy brief (.docx)" feature was removed from the dashboard.

---

## 3. How to run it

From the project's root folder (with `data/` populated and all the source files above present):

```r
shiny::runApp()
```

### Required R packages

```r
install.packages(c(
  "shiny", "bslib", "dplyr", "tidyr", "tibble", "ggplot2", "purrr", "officer"
))
```

`officer` is only needed if `build_brief.R` / docx generation is reintroduced; it is not required for the current version of the app.

---

## 4. Data

The app expects a `DATA` object (built by `load_mis()`), with at least:

- `DATA$main` — one row per country × MIS module, with response columns (`q...`) and matching `..._comments` free-text columns.
- `DATA$indcap` — one row per country for the Capabilities questionnaire, with response and comment columns referenced in `CAP_QUESTIONS` / `CAP_COMMENT_COLS`.

Both are expected to already be cleaned/labeled — `app.R` and `plots.R` do not perform any data cleaning themselves, only filtering, aggregation, and plotting.

---

## 5. Layout & navigation

**Sidebar**
- **Economy**: country selector.
- **Income group / MIS modules / Capabilities** badges.
- **Questionnaire**: choice of the 7 modules (6 MIS + Capabilities).
- **View**: "Country only" vs. "vs. LAC average" (MIS modules only; not shown for Capabilities).

**Main panel (tabs)**
1. **About the survey** — background on the survey, sourced from `about_survey.md`.
2. **How to use this dashboard** — usage instructions, sourced from `how_to_use.md`.
3. **Explore charts** — one chart per question, grouped by section for MIS modules; flat list for Capabilities. Each chart has a "Download PNG" button.
4. **Text responses** — free-text comments per question, translated to English when a translation function is available.

---

## 6. Chart design (`plots.R`)

- Charts are horizontal bar charts (`coord_flip()`), one bar per response category.
- In "vs. LAC average" view, bar length = % of countries (that answered that module) choosing each category; a blue diamond (◆) marks the category selected by the chosen country, positioned slightly inside the bar so it doesn't overlap the "%" label drawn outside it.
- `plot_question()` is the wrapper for MIS modules (filters by module, substitutes `{module}` in question titles).
- `plot_cap_question()` is the wrapper for the Capabilities questionnaire (no module dimension).
- Colors: World Bank navy (`#002245`) for diamonds and titles; a green-to-red palette (`default_palette()`) for response categories by default, overridable per question via `q$palette`.

---

## 7. Styling

- Theme: `bslib::bs_theme(bootswatch = "flatly", primary = "#002245")`.
- Tab titles are forced to `#002245` (World Bank navy) via a custom CSS block in `app.R`, to match the dashboard's main title (Flatly's default tab color is otherwise a teal/green).

---

## 8. Questions or issues?

**AI and Data for Better Governance team**
dataforbettergovernance@worldbank.org

**Josefina Silva Fuentealba**
jsilvafuentealba@worldbank.org
