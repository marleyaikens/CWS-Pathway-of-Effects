library(shiny)
function(input, output, session) {
  # main application, add modules in future to expand
  poeServer(id = modId, act2Pres = act2Pres, pathways = pathways, 
    htmlLabels = htmlLabels)
}
