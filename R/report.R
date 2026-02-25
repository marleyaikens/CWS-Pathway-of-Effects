#' Create html report of diagram
#'
#' @returns
#'
#' @export
#' @examples
#' v <- "Terrestrial and Semi-Aquatic SAR"
#' a <- c("Shoreline / Bank stabilization", "Water extraction")
#' m <- c("terrestrial_and_semi-aquatic_sar_relocate", "terrestrial_and_semi-aquatic_sar_create_habitat")
#' m_df <- read_mitigations()
#' create_report(v, a, m, m_df, lang = "en")
#' create_report(v, a, m, m_df, lang = "fr")
#'
#' # No Mitigations
#' create_report(
#'   "Marine Birds",
#'   "Aircraft overflights / runway activity",
#'   lang = "en"
#' )
#'
#' # Mitigations
#' create_report(
#'   vc = "Marine Birds",
#'   a = "Aircraft overflights / runway activity",
#'   m = "Mitigation",
#'   m_df = data.frame(
#'     start_node = NA,
#'     end_node = NA,
#'     edge = 67,
#'     m_id = "vc_mitigation",
#'     short = "Mitigation",
#'     long = "Test"
#'   ),
#'   lang = "en"
#' )

create_report <- function(
  vc,
  a,
  m = NULL,
  m_df = NULL,
  notes = NULL,
  lang = lang,
  path = paste0("report_", Sys.Date(), ".html")
) {
  if (!is.null(m) && is.null(m_df)) {
    m_df <- readxl::read_excel("data/mitigations.xlsx")
  }

  if (!is.null(m_df)) {
    m_df <- unique(m_df)
    m_df <- lapply(m_df, \(col) {
      col[is.na(col)] <- ""
      col
    })
  }

  # Check and update dictionary
  dictionary_update()

  # Write to temp dir, copy back to Shiny download handler file
  #template <- file.path(tempdir(), "report_template.qmd")
  #file.copy("report_template.qmd", template)

  quarto::quarto_render(
    "report_template.qmd",
    output_file = "report.html",
    execute_params = list(
      vc = vc,
      a = a,
      m = m,
      m_all = m_df,
      notes = notes,
      lang = lang
    )
  )
  file.copy("report.html", path, overwrite = TRUE)
  file.remove("report.html")

  path
}

#' Create list of tables for reports
#'
#' @param tbl Data frame.
#' @param cols Named character vector. Columns to keep in table, names become
#'   pretty column names.
#' @param lang Character. 'en' or 'fr' language to translate values to.
#'
#' @returns List of reactable tables
#'
#' @export
#' @examples
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
      reactable(
        x[, -1],
        defaultColDef = colDef(header = \(h) translate_text(h, lang = lang)),
        searchable = TRUE,
        filterable = TRUE,
        language = reactableLang(
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
#' @param tbls List of reactable tables created by `report_tables()`
#'
#' @returns
#'
#' @export
#' @examples
report_tables_print <- function(tbls) {
  for (i in seq_along(r)) {
    cat("###", names(r)[i], "\n\n")
    cat("\n\n```{=html}\n\n")

    shiny::tagList(r[[i]]) |> print()

    cat("\n\n```\n\n")
  }
}
