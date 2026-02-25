# Load Data ------------------------------------------------------------

poe_app <- function() {
  # Activities to Stressors/Pressures Crosswalk
  # act2Pres <- readRDS("data/act2Pres.rds") |>
  #   as.data.frame()

  # Check and update dictionary
  dictionary_update()

  act2Pres <- read_components()

  # Mitigations - Ensuring unique ids
  mitigations <- read_mitigations()

  # Parsed Visio Diagrams Data
  pathways <- read_pathways()

  # Translations -----------------------------------------------
  dict <- read_sheets("translations.xlsx")
  options("poe.dict" = dict)

  # Legends -------------------------------------------------------------

  # Legend text labels and colors
  legText <- c(
    "Valued Component",
    "Stressors/pressures associated with Activities/Components selected by user",
    "Potential Contravention of Legislation and/or Regulations"
  )
  legColors <- c("#88bde9", "#fee599", "#f06c6c")
  legSize <- 40

  # Headers/labels that need to be translated later
  # - The name refers to the element id, hence the two 'applies'

  htmlLabels <- list(
    # App title
    dbTitle = "Pathways of Effect",

    # Accordion titles
    "valuedComponent" = "Valued Component",
    "mitigationMeasures" = "Mitigation Measures",

    # Buttons
    applyActivities = "Apply",
    applyMitigations = "Apply",

    # Cards
    cardTitle1 = "Build Pathways",
    cardTitle2 = "Interactive View",
    cardTitle3 = "Flowchart View",
    cardTitle4 = "Orthogonal View",

    # Inputs
    "activities-label" = "Select all Activities/Components that apply:",
    "mitigations-label" = "Select all Mitigations that apply:",

    # Legends
    leg1Text1 = legText[1],
    leg1Text2 = legText[2],
    leg1Text3 = legText[3],
    leg2Text1 = legText[1],
    leg2Text2 = legText[2],
    leg2Text3 = legText[3],
    leg3Text1 = legText[1],
    leg3Text2 = legText[2],
    leg3Text3 = legText[3]
  )

  # App structure -------------------------------------------------------
  ui <- #page_fillable(
    #title = "CWS",
    #theme = bs_theme(version = 5, bootswatch = "materia"),
    # main application, add modules below in future to expand
    poeUI(
      id = "poe",
      act2Pres = act2Pres,
      htmlLabels = htmlLabels,
      legColors = legColors,
      legText = legText,
      legSize = legSize
    )

  server <- function(input, output, session) {
    # main application, add modules in future to expand
    poeServer(
      id = "poe",
      act2Pres = act2Pres,
      mitigations = mitigations,
      pathways = pathways,
      htmlLabels = htmlLabels
    )
  }

  # Run app -------------------------------------------------------------
  shinyApp(ui, server)
}
