###########################################################################
#                                                                         #   
# App Data                                                                #
#                                                                         #
###########################################################################
# Load functions
list.files("../R", full.names = TRUE) |> lapply(source)


# Module ID
modId <- "poe"
# Activities to Stressors/Pressures Crosswalk
act2Pres <- readRDS("../data/act2Pres.rds") |> 
  as.data.frame()
# Parsed Visio Diagrams Data
pathways <- jsonlite::read_json(path = "../data/poe.json", simplifyVector = FALSE)
# Translation Table 
enFr <- data.table::fread("../data/en-fr-table.csv")
# legend text labels and colors
legText <- c("Valued Component",
  "Stressors/pressures associated with Activities/Components selected by user",
  "Potential Contravention of Legislation and/or Regulations")
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
