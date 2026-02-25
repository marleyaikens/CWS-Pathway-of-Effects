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
