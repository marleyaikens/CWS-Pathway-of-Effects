# Collect all terms for translation and for coding
# NOTE: This should only be run ONCE! Otherwise it will overwrite any existing translations

ui <- c(
  "Pathways of Effect",

  # Tabs
  "Interactive View",
  "Flowchart View",
  "Orthogonal View",

  # Inputs
  "Build Pathways",
  "Valued Component", # And legend
  "Select all Activities/Components that apply:",
  "Apply",
  "Mitigation Measures",

  # Legends
  "Stressors/pressures associated with Activities/Components selected by user",
  "Potential Contravention of Legislation and/or Regulations",

  # Other
  "Value Component",
  "Select Language"
)

labels <- jsonlite::read_json("data/poe.json") |>
  lapply(function(vc) {
    data.frame(
      valued_component = vc[["name"]],
      node_id = sapply(vc[["nodes"]], getElement, "id"),
      label = sapply(vc[["nodes"]], getElement, "label") |> trimws()
    )
  })
labels <- do.call("rbind", labels) |>
  dplyr::filter(label != "")

# Contains inputs not included in the diagrams, so no ID...
crosswalk <- readRDS("data/act2Pres.rds") |>
  unlist(use.names = FALSE)

english <- c(ui, labels$label, crosswalk) |>
  unique()

# Write English/French translation table
data.table::data.table(
  english,
  french = sprintf("french-%03d", seq_along(english))
) |>
  data.table::fwrite("data/en-fr-table.csv")

# Write node id table for references
write.csv(labels, "data/node_ids.csv", row.names = FALSE)
