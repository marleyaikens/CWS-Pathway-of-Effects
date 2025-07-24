library(shiny)
bslib::page_fillable(
  title = "CWS",
  theme = bslib::bs_theme(version = 5, bootswatch = "materia"),
  # main application, add modules below in future to expand
  poeUI(id = modId, act2Pres = act2Pres, htmlLabels = htmlLabels)
)
