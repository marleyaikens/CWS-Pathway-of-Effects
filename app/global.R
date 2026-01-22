###########################################################################
#                                                                         #   
# App Data                                                                #
#                                                                         #
###########################################################################
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
###########################################################################
#                                                                         #   
# Toolkit                                                                 #
#                                                                         #
###########################################################################
# Converts JSON output from Visio into R visual compatible nodes and 
# edges dataframes
# 
# @param pathway - JSON of single valued component pathway
prep_visnetwork <- function(pathway) {
  nodes <- pathway |> getElement("nodes") |> 
    data.table::rbindlist(fill = TRUE) |> 
    suppressWarnings()
  data.table::setnames(nodes, old = c("fillcolor", "color"), 
    new = c("color.background", "color.border"))
  nodes[["shape"]] <- "box"
  nodes[["x"]] <- (nodes[["x"]] - min(nodes[["x"]])) / 
    diff(range(nodes[["x"]]))
  nodes[["y"]] <- (nodes[["y"]] - min(nodes[["y"]])) / 
    diff(range(nodes[["y"]]))
  nodes[["y"]] <- -nodes[["y"]]
  nodes[["label"]] <- trimws(nodes[["label"]])
  nodes[["color.border"]][is.na(nodes[["color.border"]])] <- "#0b0b0b"
  nodes[["color.background"]][is.na(nodes[["color.background"]])] <- "#ffffff"
  nodes[["level"]] <- round(nodes[["y"]], 1)
  edges <- pathway |> getElement("edges") |> data.table::rbindlist(fill = TRUE)
  # only keep nodes that have edges
  nodes <- nodes[nodes[["id"]] %in% unique(c(edges[["from"]], edges[["to"]])), ]
  list(nodes = nodes, edges = edges)
}
# Recursive function: 
# find parent branches 
# if there are child branches, append edges and call same function
# else stop and return compiled results filtering nodes as well
#
# @param ids - node IDs to search for
# @param tree - full network/tree to search through
# @param pruned - pruned branches to append to 
prune_branches <- function(ids, tree, pruned = NULL) {
  branch <- tree$edges[tree$edges$from %in% ids, ]
  if (nrow(branch) > 0) {
    # continue branch search
    prune_branches(ids = unique(branch$to), tree = tree, 
      pruned = data.table::rbindlist(list(pruned, branch))
    )
  } else {
    # Ensure uniqueness of nodes/edges spec, otherwise will not work
    list(
      nodes = tree$nodes[tree$nodes$id %in% pruned[ ,unique(c(from, to))], ],
      edges = unique(pruned)
    )
  }
}
# Wrapper for making the POE legend with ids for translations
#
# @param id - id for legend
# @param colors - legend square colors
# @param labels - legend text
# @param size - size of squares
make_poe_legend <- function(id, colors, labels, size) {
  # id is semi hard-coded so the text can be found and translated
  legendItems <- Map(function(id, color, label, size) {
    make_legend_item(size = size, color = color, text = label, id = id)
  }, id = sprintf("%sText%d", id, seq_along(colors)), color = colors,
    label = labels, size = size) 
  make_legend(legendItems)
}
# Wrapper for making the POE legend with ids for translations
#
# @param ... - list of items from make_legend_item
make_legend <- function(...) {
  shiny::tags$div(
    # id = "tree-legend",
    style = "height: 50px; width: 100%;",
    class = "d-flex align-items-center justify-content-around",
    ...
  )
}
# Creates a legend item 
# @param color - color of square
# @param text - text label
# @param ... - named arguments to pass to text div
make_legend_item <- function(size, color, text, ...) {
  shiny::tags$div(
    class = "d-flex align-items-center",
    shiny::tags$div(
      class = "me-2 rounded",
      style = 
        sprintf("height: %1$spx; width: %1$spx; background-color: %2$s",
          size, color)
      ),
    shiny::tags$div(text, class = "h6 m-0 p-0", ...)
  )
}
# Converts visNetwork data format to DOT language
#
# @param visNet - visNetwork data object
convert_to_dot <- function(visNet) {
  visNet[["nodes"]][["id"]] <- as.integer(visNet[["nodes"]][["id"]])
  visNet[["nodes"]][["color"]] <- visNet[["nodes"]][["color.border"]]
  visNet[["nodes"]][["fillcolor"]] <- visNet[["nodes"]][["color.background"]]
  visNet[["nodes"]][["fontcolor"]] <- "black"
  visNet[["edges"]][["from"]] <- as.integer(visNet[["edges"]][["from"]])
  visNet[["edges"]][["to"]] <- as.integer(visNet[["edges"]][["to"]])
  visNet[["edges"]][["id"]] <- NULL
  visNet
}
# Converts visNetwork data format to mermaid js graph specification
#
# @param visNet - visNetwork data object
convert_mermaid_flowchart <- function(visNet) {
  nodeSpec <- sprintf("\tid%s(\"%s\")", visNet[["nodes"]][["id"]],
    gsub(pattern = "\\n", replacement = "<br>", x = visNet$nodes$label)
  ) |>
    paste(collapse = "\n")
  edgeSpec <- sprintf("\tid%s --> id%s", visNet$edges$from, visNet$edges$to) |>
    paste(collapse = "\n")
  styleSpec <- sprintf("style id%s fill:%s,stroke:%s", 
    visNet$nodes$id, visNet$nodes$color.background, 
    visNet$nodes$color.border) |>
    paste(collapse = "\n")
  sprintf("graph TB\n%s\n%s\n%s", nodeSpec, edgeSpec, styleSpec)
}
# Adds zoom functionality to an htmlwidget
#
# @param x - htmlwidget
# @param id - id of widget
add_zoom <- function(x, id) {
  htmlwidgets::onRender(x = x,
    jsCode = sprintf(
      "function() {
          svgPanZoom('#%s > svg')
      }", id)
  )
}
# Translates text from english to french 
# 
# @param x - english text to translate
# @param lang - one of "en" or "fr"
# @param translations - dataframe with columns "en" and "fr" for translations
translate_text <- function(x, lang = "en", translations = enFr) {
  if (lang == "en") {
    x
  } else if (lang == "fr") {
    french <- enFr[["fr"]][match(x, enFr[["en"]])]
    if (!is.null(names(x))) {
      french <- stats::setNames(french, names(x))
    }
    french
  } else {
    stop("Only English and French are supported.")
  }
}
###########################################################################
#                                                                         #   
# Module                                                                  #
#                                                                         #
###########################################################################
# User Interface module for pathways of effects
#
# @param id - module id
# @param act2Pres - activity to pressures lookup table
# @param htmlLabels - named vector with element IDs as names & labels as values
poeUI <- function(id, act2Pres, htmlLabels) {
  ns <- shiny::NS(id)
  js <- "
  $( document ).ready(function() {
    Shiny.addCustomMessageHandler('toggleDisable', function(toggle) {
      document.getElementById(toggle[0]).disabled = toggle[1];
    });
  });
  Shiny.addCustomMessageHandler('translateLabels', function(labels) {
    for (let key in labels) {
      try {
        document.getElementById(key).innerHTML = labels[key]
      } catch(error) {
        console.log(error);
      }
    }
    //console.log(labels);
  });
  "
  css <- "
  g > .node {
    text-align: center;
  }
  "
  list(
    tags$style(HTML(css)),
    shiny::tags$script(shiny::HTML(js)),
    shiny::includeScript("svg-pan-zoom.min.js"),
    shiny::tags$div(
      class = "card bslib-card card-header d-flex 
    justify-content-between align-items-center flex-row",
      shiny::tags$div("Pathways of Effect", id = ns("dbTitle"),
        style="font-size: 2rem; font-weight: bold;"),
      shiny::radioButtons(inputId = ns("lang"), label = "", 
        choices = c("English" = "en", "French" = "fr"), inline = TRUE)
    ),
    bslib::layout_columns(
      col_widths = c(3, 9),
      class = "p-1",
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(shiny::tags$div("Inputs", id = ns("cardTitle1"))),
        bslib::card_body(
          shiny::selectInput(
            inputId = ns("valuedComponent"), 
            label = htmlLabels[["valuedComponent-label"]], 
            choices = unique(act2Pres$valued_component), selected = "", 
            multiple = FALSE, selectize = TRUE, width = "100%"),
          shiny::uiOutput(outputId = ns("activitiesUi")),
          shiny::actionButton(ns("apply"), htmlLabels[["apply"]], 
            class = "btn-primary")
        )
      ),
      bslib::navset_card_tab(
        full_screen = TRUE,
        bslib::nav_panel(
          title = shiny::tags$div("Interactive View", id = ns("cardTitle2")),
          visNetwork::visNetworkOutput(ns("pathwayInteractive"), width = "100%", 
            height = "100%"),
          make_poe_legend(id = ns("leg1"), colors = legColors, labels = legText,
            size = legSize)
        ),
        bslib::nav_panel(
          title = shiny::tags$div("Flowchart View", id = ns("cardTitle3")),
          DiagrammeR::DiagrammeROutput(ns("pathwayFlowchart"), width = "100%", 
            height = "100%"),
          make_poe_legend(id = ns("leg2"), colors = legColors, labels = legText,
            size = legSize)
        ),
        bslib::nav_panel(
          title = shiny::tags$div("Orthogonal View", id = ns("cardTitle4")),
          DiagrammeR::grVizOutput(ns("pathwayOrthogonal"), width = "100%", 
            height = "100%"),
          make_poe_legend(id = ns("leg3"), colors = legColors, labels = legText,
            size = legSize)
        )
      )
    )
  )
}
# Server module for pathways of effects
#
# @param id - module id
# @param act2Pres - activity to pressures lookup table
# @param pathways - JSON of all pathways
# @param htmlLabels - named vector with element IDs as names & labels as values
poeServer <- function(id, act2Pres, pathways, htmlLabels) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    htmlLabels <- stats::setNames(htmlLabels, ns(names(htmlLabels)))
    ###########################################################################
    #                                                                         #   
    # Data                                                                    #
    #                                                                         #
    ###########################################################################
    # start with no pathway
    pathway <- shiny::reactiveVal(NULL)
    # prune selected value component pathway by selected activities
    shiny::observeEvent(input$apply, {
      tree <- pathways[[input$valuedComponent]] |> 
        prep_visnetwork()
      stressors <- act2Pres[["stressors"]][
        act2Pres[["valued_component"]] %in% input$valuedComponent &
          act2Pres[["activities"]] %in% input$activities] |> 
        unique()
      cleanLabels <- gsub(pattern = "\\n", replacement = " ", 
        x = tree$nodes$label)
      ids <- tree$nodes[cleanLabels %in% stressors, id]
      # start with top branch which is the "Valued Component"
      topNode <- tree[["nodes"]][["id"]][cleanLabels %in% input$valuedComponent]
      topBranch <- tree[["edges"]][tree[["edges"]][["from"]] %in% topNode &
          tree[["edges"]][["to"]] %in% ids, ]
      pruned <- prune_branches(ids = ids, tree = tree, pruned = topBranch)
      pruned$nodes$label <- pruned$nodes$label |> 
        translate_text(lang = input$lang) |> 
        stringr::str_wrap(width = 15)
      # update pathway
      pathway(pruned)
    })
    ###########################################################################
    #                                                                         #   
    # User Interactions                                                       #
    #                                                                         #
    ###########################################################################
    output$activitiesUi <- shiny::renderUI({
      choices <- act2Pres[["activities"]][
        act2Pres[["valued_component"]] %in% input$valuedComponent] |> 
        unique()
      shiny::checkboxGroupInput(
        inputId = ns("activities"), 
        label = htmlLabels[[ns("activities-label")]] |> 
          translate_text(lang = input$lang), 
        choices = stats::setNames(choices, 
          choices |> translate_text(lang = input$lang)
          ),
        width = "100%"
      ) |> 
        shiny::tagAppendAttributes(style = "
          display: flex;
          justify-content: space-between;
          ", 
          .cssSelector = "label") |> 
        shiny::tagAppendAttributes(style = "
          width: 1em;
          margin-right: 1em;
          ", 
          .cssSelector = "input") |> 
        shiny::tagAppendAttributes(style = "
          width: calc(100% - 1em);
          ", .cssSelector = "span")
    })
    # translations
    shiny::observeEvent(input$lang, {
      shiny::updateSelectInput(
        session = session, 
        inputId = "valuedComponent", 
        choices = stats::setNames(
          unique(act2Pres$valued_component),
          unique(act2Pres$valued_component) |> 
          translate_text(lang = input$lang)
        )
      )
      session$sendCustomMessage("translateLabels", htmlLabels |>
          translate_text(lang = input$lang) |>
          # convert to list for conversion to JSON object
          lapply(identity)
      )
    })
    # used to disable apply button if activities are not selected
    shiny::observeEvent(input$activities, {
      btnId <- "apply"
      if (length(input$activities) > 0) {
        session$sendCustomMessage("toggleDisable", list(ns(btnId), FALSE))
      } else {
        session$sendCustomMessage("toggleDisable", list(ns(btnId), TRUE))
        pathway(NULL)
      }
    }, ignoreNULL = FALSE)
    ###########################################################################
    #                                                                         #   
    # Diagrams                                                                #
    #                                                                         #
    ###########################################################################
    # Interactive View
    # visNetwork
    output$pathwayInteractive <- visNetwork::renderVisNetwork({
      shiny::req(pathway())
      visNetwork::visNetwork(nodes = pathway()[["nodes"]],
        edges = pathway()[["edges"]]) |>
        visNetwork::visNodes(font = list(size = 10)) |>
        visNetwork::visEdges(arrows = "to") |>
        visNetwork::visHierarchicalLayout(levelSeparation = 800) |> 
        visNetwork::visOptions(
          highlightNearest = list(
            enabled = TRUE, 
            degree = list(from=1000, to=1000),
            algorithm = "hierarchical",
            labelOnly = FALSE
            )
          ) 
    })
    # Flowchart View
    # mermaid
    output$pathwayFlowchart <- DiagrammeR::renderDiagrammeR({
      shiny::req(pathway())
      flowchart <- convert_mermaid_flowchart(data.table::copy(pathway()))
      flowchart |> 
        DiagrammeR::DiagrammeR() |> 
        add_zoom(id = ns("pathwayFlowchart"))
    })
    # Orthogonal View
    # DiagrammR
    output$pathwayOrthogonal <- DiagrammeR::renderGrViz({
      shiny::req(pathway())
      dot <- convert_to_dot(visNet = data.table::copy(pathway()))
      DiagrammeR::create_graph(nodes_df = dot[["nodes"]], 
        edges_df = dot[["edges"]]) |>
        DiagrammeR::add_global_graph_attrs(
          attr = "splines", value = "ortho", attr_type = "graph") |>
        DiagrammeR::add_global_graph_attrs(
          attr = "overlap", value = FALSE, attr_type = "graph") |>
        DiagrammeR::add_global_graph_attrs(
          attr = "style", value = "rounded, filled", attr_type = "node") |> 
        DiagrammeR::render_graph(layout = "tree", output = "graph") |> 
        add_zoom(id = ns("pathwayOrthogonal"))
    })
  })
}
