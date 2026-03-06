#' Update translations dictionary
#'
#' Updates the translations Excel file by extracting new English text from all
#' data sources and adding them to the dictionary with placeholder French
#' translations.
#'
#' @returns NULL (invisibly). Saves updated translations.xlsx file.
#'
#' @export

dictionary_update <- function() {
  current <- read_translations()

  labs <- read_sheets("ui_labels.xlsx")
  names(labs)[names(labs) == "label"] <- "english"
  components <- read_components() |>
    extract_labels("components etc.")
  sectors <- read_sectors() |>
    extract_labels("sectors")

  pathways <- read_pathways() |>
    extract_labels("pathways", keep = "label")
  mitigations <- read_mitigations() |>
    extract_labels("mitigations", keep = c("short", "long"))

  new <- rbind(labs, components, sectors, pathways, mitigations)
  new <- new[!is.na(new$english), ] |>
    unique()
  new <- new[!new$english %in% current$english, ]
  new$french <- "FR"

  current <- rbind(current, new)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  openxlsx::writeData(wb, "Sheet1", current)
  openxlsx::setColWidths(
    wb,
    sheet = "Sheet1",
    cols = 1:3,
    widths = c(50, 50, 10)
  )

  openxlsx::saveWorkbook(
    wb,
    file.path(system.file("extdata", package = "poe"), "translations.xlsx"),
    overwrite = TRUE
  )
}

#' Extract labels for translation
#'
#' Extracts unique text values from specified columns of a data frame for the
#' translation dictionary.
#'
#' @param df Data frame. Source data to extract labels from.
#' @param note Character. Note to add indicating source of labels.
#' @param keep Character vector. Column names to extract labels from. Defaults
#'   to `NULL` which uses all columns.
#'
#' @returns Data frame with columns:
#' * `english` - Extracted text values
#' * `note` - Source note
#'
#' @noRd

extract_labels <- function(df, note, keep = NULL) {
  if (is.null(keep)) {
    keep <- names(df)
  }
  labs <- df[keep] |> unlist() |> unique()
  data.frame(english = labs, note = note)
}
