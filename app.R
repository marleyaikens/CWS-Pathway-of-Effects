# Load Packages --------------------------------------------------------
library(shiny)

# Load functions -------------------------------------------------------
list.files("R", full.names = TRUE) |> lapply(source)

# Load Data ------------------------------------------------------------

# Activities to Stressors/Pressures Crosswalk
act2Pres <- readRDS("data/act2Pres.rds") |>
  as.data.frame()

# Parsed Visio Diagrams Data
pathways <- jsonlite::read_json(
  path = "data/poe.json",
  simplifyVector = FALSE
)

# Translation Table
enFr <- data.table::fread("data/en-fr-table.csv")

# Legends -------------------------------------------------------------

# Legend text labels and colors
legText <- c(
  "Valued Component",
  "Stressors/pressures associated with Activities/Components selected by user",
  "Potential Contravention of Legislation and/or Regulations"
)
legColors <- c("#88bde9", "#fee599", "#f06c6c")
legSize <- 40
# headers/labels that need to be translated later
htmlLabels <- list(
  "valuedComponent-label" = "Valued Component",
  apply = "Apply",
  dbTitle = "Pathways of Effect",
  cardTitle1 = "Inputs",
  cardTitle2 = "Interactive View",
  cardTitle3 = "Flowchart View",
  cardTitle4 = "Orthogonal View",
  leg1Text1 = legText[1],
  leg1Text2 = legText[2],
  leg1Text3 = legText[3],
  leg2Text1 = legText[1],
  leg2Text2 = legText[2],
  leg2Text3 = legText[3],
  leg3Text1 = legText[1],
  leg3Text2 = legText[2],
  leg3Text3 = legText[3],
  "activities-label" = "Select all Activities/Components that apply:"
)

# App structure -------------------------------------------------------
ui <- bslib::page_fillable(
  title = "CWS",
  theme = bslib::bs_theme(version = 5, bootswatch = "materia"),
  # main application, add modules below in future to expand
  poeUI(
    id = "poe",
    act2Pres = act2Pres,
    htmlLabels = htmlLabels,
    legColors = legColors,
    legText = legText,
    legSize = legSize
  )
)

server <- function(input, output, session) {
  # main application, add modules in future to expand
  poeServer(
    id = "poe",
    act2Pres = act2Pres,
    pathways = pathways,
    htmlLabels = htmlLabels
  )
}

# Run app -------------------------------------------------------------
shinyApp(ui, server)
