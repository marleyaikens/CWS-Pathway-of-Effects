# Load Data ------------------------------------------------------------

#' Launch the Pathways of Effect Shiny App
#'
#' Launches the app. Note that the App data must be in the current working
#' directory for the app to work.
#'
#' @export
#' @examplesIf interactive()
#' poe_app()

poe_app <- function() {
  check_components()
  check_pathways()
  check_activities()
  check_stressors()
  check_mitigations()

  # Activities to Stressors/Pressures
  act2Pres <- read_components()

  # Mitigations - Ensuring unique ids
  mitigations <- read_mitigations()

  # Parsed Visio Diagrams Data
  pathways <- read_pathways()

  # Activities by sector
  activities <- read_sectors()

  # Translations -----------------------------------------------
  dict <- read_translations()
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
    appTitle = "Pathways of Effect",

    # Accordion titles
    "valuedComponentLabel" = "Valued Component",
    "mitigationMeasures" = "Mitigation Measures",
    "addMitigations" = "Add Mitigations",
    "removeMitigations" = "Remove Created Mitigations",

    # UI only Inputs
    "sectorLabel" = "Sector",
    "toggleMitigationsLabel" = "Add Custom Mitigations",
    "report" = "Report",
    "reportLabel" = "Download report",
    "reportNameLabel" = "Project name",
    "reportStatusLabel" = "Project status",
    "addMitigationNameLabel" = "Name",
    "addMitigationDescriptionLabel" = "Description",
    "createMitigationLabel" = "Create mitigation",

    # Sidebars Tabs
    "sidebarMitigationsLabel" = "Custom Mitigations",
    "sidebarPathwaysLabel" = "Build Pathways",
    "tabInteractive" = "Interactive View",
    "tabFlowchart" = "Flowchart View",
    "tabOrthogonal" = "Orthogonal View",

    # Inputs
    "activitiesLabel" = "Select all Activities/Components that apply:",
    "mitigationsLabel" = "Select all Mitigations that apply:",

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
      activities = activities,
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
      activities = activities,
      htmlLabels = htmlLabels
    )
  }

  # Run app -------------------------------------------------------------
  shinyApp(ui, server, options = list(host = "0.0.0.0", port = 8080), )
}
