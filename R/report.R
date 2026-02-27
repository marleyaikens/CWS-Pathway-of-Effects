#' Create html report of diagram
#'
#' Generates an HTML report showing the pathway diagram with selected
#' mitigations and project information.
#'
#' @param vc Character. Valued component to include in report.
#' @param a Character vector. Activities to include in report.
#' @param m Character vector. Mitigation IDs to include. Defaults to `NULL`.
#' @param m_df Data frame. Mitigation metadata. Defaults to [read_mitigations()]
#' @param notes Character. User notes to include in report. Defaults to `NULL`.
#' @param project_name Character. Project name for report header. Defaults to
#'   "Project Placeholder".
#' @param project_status Character. Project status for report header. Defaults
#'   to "Status Placeholder".
#' @param lang Character. Language code ("en" or "fr").
#' @param path Character. Output file path. Defaults to
#'   `paste0("report_", Sys.Date(), ".html")`.
#'
#' @returns Character. Path to created HTML report file.
#'
#' @export
#' @examplesIf have_data()
#' v <- "Terrestrial and Semi-Aquatic SAR"
#' a <- c("Shoreline / Bank stabilization", "Water extraction")
#' m <- c("terrestrial_and_semi-aquatic_sar_relocate",
#'        "terrestrial_and_semi-aquatic_sar_create_habitat")
#' m_df <- read_mitigations()
#' create_report(v, a, m, m_df, lang = "en")
#' create_report(v, a, m, m_df, lang = "fr")
#'
#' # No Mitigations
#' v <- "Marine Birds"
#' a <- "Aircraft overflights / runway activity"
#' create_report(v, a, lang = "en")
#' create_report(v, a, lang = "fr")
#'
#' # Cleanup
#' file.remove(paste0("report_", Sys.Date(), ".html"))

create_report <- function(
  vc,
  a,
  m = NULL,
  m_df = read_mitigations(),
  notes = NULL,
  project_name = "Project Placeholder",
  project_status = "Status Placeholder",
  lang = lang,
  path = paste0("report_", Sys.Date(), ".html")
) {
  if (!is.null(m_df)) {
    m_df <- unique(m_df)
    m_df <- lapply(m_df, \(col) {
      col[is.na(col)] <- ""
      col
    })
  }

  # Write to temp dir, copy back to Shiny download handler file
  template <- file.path(tempdir(), "report_template.qmd")
  report_out <- file.path(tempdir(), "report.html")

  file.copy(
    data_location("translations.xlsx"),
    file.path(tempdir(), "translations.xlsx")
  )
  file.copy(
    system.file("extdata", "report_template.qmd", package = "poe"),
    template
  )

  quarto::quarto_render(
    template,
    output_file = "report.html",
    execute_params = list(
      vc = vc,
      a = a,
      m = m,
      m_all = m_df,
      notes = notes,
      project_name = project_name,
      project_status = project_status,
      lang = lang
    )
  )
  file.copy(report_out, path, overwrite = TRUE)
  file.remove(report_out)

  path
}

#' Create list of tables for reports
#'
#' Creates list of reactable tables from data frame, split by first column and
#' with translated labels.
#'
#' @param tbl Data frame. Table data to format.
#' @param cols Named character vector. Columns to keep in table. Names become
#'   column headers.
#' @param lang Character. Language code ("en" or "fr") for translations.
#'
#' @returns Named list of reactable tables, one per unique value in first
#'   column.
#'
#' @export
report_tables <- function(tbl, cols, lang) {
  tbl <- tbl[, cols] |>
    unique() |> # Don't remove duplicated untranslated labels
    lapply(translate_text, lang = lang) |>
    as.data.frame()

  names(tbl) <- names(cols)

  tbl <- split(tbl, tbl[[1]])

  r <- lapply(
    tbl,
    \(x) {
      reactable::reactable(
        x[, -1],
        defaultColDef = reactable::colDef(header = \(h) {
          translate_text(h, lang = lang)
        }),
        searchable = TRUE,
        filterable = TRUE,
        language = reactable::reactableLang(
          searchPlaceholder = translate_text("Search", lang = lang),
          pageNext = translate_text("Next", lang = lang),
          pagePrevious = translate_text("Previous", lang = lang),
          # Hard code this translation as the unicode is difficult
          pageInfo = ifelse(
            lang == "en",
            "{rowStart}\u2013{rowEnd} of {rows} rows",
            "{rowStart}\u2013{rowEnd} sur {rows} lignes"
          )
        )
      )
    }
  )

  r
}

#' Print a list of reactable tables in report
#'
#' Prints reactable tables with headers for use in Quarto reports. This
#' function is used internally by the report template and is not intended to be
#' called directly by the user.
#'
#' @param tbls Named list. List of reactable tables created by
#'   [report_tables()].
#'
#' @returns NULL (invisibly). Prints tables to output.
#'
#' @export
report_tables_print <- function(tbls) {
  for (i in seq_along(tbls)) {
    cat("###", names(tbls)[i], "\n\n")
    cat("\n\n```{=html}\n\n")

    shiny::tagList(tbls[[i]]) |> print()

    cat("\n\n```\n\n")
  }
}
