ui <- c(
  "Pathways of Effect",
  "Inputs",
  "Value Component", 
  "Select all Activities/Components that apply:",
  "Apply",
  "Interactive View",
  "Flowchart View",
  "Orthogonal View",
  "Valued Component", 
  "Stressors/pressures associated with Activities/Components selected by user",
  "Potential Contravention of Legislation and/or Regulations",
  "Select Language"
)
labels <- jsonlite::read_json("app/poe.json") |> 
  lapply(function(vc) {
    sapply(vc[["nodes"]], getElement, "label") |> trimws()
  }) |> 
  unlist(use.names = FALSE)
labels <- labels[labels != ""]
crosswalk <- readRDS("app/act2Pres.rds") |> 
  unlist(use.names = FALSE)
english <- c(ui, labels, crosswalk) |> 
  unique()
data.table::data.table(
  english,
  french = sprintf("french-%03d", seq_along(english))
) |> 
  data.table::fwrite("app/en-fr-table.csv")
