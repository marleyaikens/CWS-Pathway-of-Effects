#' Create html report of diagram
#'
#' @returns
#'
#' @export
#' @examples
#' pathways <- jsonlite::read_json(
#'  path = "data/poe.json",
#'  simplifyVector = FALSE
#' )
#'
#' mitigations <- readxl::read_excel("data/mitigations.xlsx")
#' ref <- readRDS("data/act2Pres.rds") |>
#'   as.data.frame()
#'
#'
#' create_report(
#'   "Terrestrial and Semi-Aquatic SAR",
#'   c(
#'     "Shoreline / Bank stabilization",
#'     "Land and Vegetation clearing and vegetation maintenance"
#'   ),
#'   c("Relocate", "Create Habitat"),
#'   m_df = mitigations,
#'   lang = "en"
#' )
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
#'     short_en = "Mitigation",
#'     long_en = "Test"
#'   ),
#'   lang = "en"
#' )

create_report <- function(
  vc,
  a,
  m = NULL,
  m_df = NULL,
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
      lang = lang
    )
  )
  file.copy("report.html", path)
  file.remove("report.html")

  path
}
