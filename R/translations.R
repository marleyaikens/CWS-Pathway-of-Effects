dictionary_update <- function() {
  current <- read_sheets("translations.xlsx")

  labs <- read_sheets("ui_labels.xlsx")
  names(labs)[names(labs) == "label"] <- "english"
  components <- read_components() |>
    extract_labels("components etc.")
  sectors <- read_activities() |>
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

extract_labels <- function(df, note, keep = NULL) {
  if (is.null(keep)) {
    keep <- names(df)
  }
  labs <- df[keep] |> unlist() |> unique()
  data.frame(english = labs, note = note)
}
