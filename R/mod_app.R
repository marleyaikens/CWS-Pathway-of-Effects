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
poeUI <- function(id, act2Pres, htmlLabels, legColors, legText, legSize) {
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
    shiny::includeScript("www/svg-pan-zoom.min.js"),
    shiny::tags$div(
      class = "card bslib-card card-header d-flex 
    justify-content-between align-items-center flex-row",
      shiny::tags$div(
        "Pathways of Effect",
        id = ns("dbTitle"),
        style = "font-size: 2rem; font-weight: bold;"
      ),
      shiny::radioButtons(
        inputId = ns("lang"),
        label = "",
        choices = c("English" = "en", "French" = "fr"),
        inline = TRUE
      )
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
            choices = unique(act2Pres$valued_component),
            selected = "",
            multiple = FALSE,
            selectize = TRUE,
            width = "100%"
          ),
          shiny::uiOutput(outputId = ns("activitiesUi")),
          shiny::actionButton(
            ns("apply"),
            htmlLabels[["apply"]],
            class = "btn-primary"
          )
        )
      ),
      bslib::navset_card_tab(
        full_screen = TRUE,
        bslib::nav_panel(
          title = shiny::tags$div("Interactive View", id = ns("cardTitle2")),
          visNetwork::visNetworkOutput(
            ns("pathwayInteractive"),
            width = "100%",
            height = "100%"
          ),
          make_poe_legend(
            id = ns("leg1"),
            colors = legColors,
            labels = legText,
            size = legSize
          )
        ),
        bslib::nav_panel(
          title = shiny::tags$div("Flowchart View", id = ns("cardTitle3")),
          DiagrammeR::DiagrammeROutput(
            ns("pathwayFlowchart"),
            width = "100%",
            height = "100%"
          ),
          make_poe_legend(
            id = ns("leg2"),
            colors = legColors,
            labels = legText,
            size = legSize
          )
        ),
        bslib::nav_panel(
          title = shiny::tags$div("Orthogonal View", id = ns("cardTitle4")),
          DiagrammeR::grVizOutput(
            ns("pathwayOrthogonal"),
            width = "100%",
            height = "100%"
          ),
          make_poe_legend(
            id = ns("leg3"),
            colors = legColors,
            labels = legText,
            size = legSize
          )
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
        act2Pres[["valued_component"]] %in%
          input$valuedComponent &
          act2Pres[["activities"]] %in% input$activities
      ] |>
        unique()
      cleanLabels <- gsub(
        pattern = "\\n",
        replacement = " ",
        x = tree$nodes$label
      )
      ids <- tree$nodes[cleanLabels %in% stressors, id]
      # start with top branch which is the "Valued Component"
      topNode <- tree[["nodes"]][["id"]][cleanLabels %in% input$valuedComponent]
      topBranch <- tree[["edges"]][
        tree[["edges"]][["from"]] %in%
          topNode &
          tree[["edges"]][["to"]] %in% ids,
      ]
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
        act2Pres[["valued_component"]] %in% input$valuedComponent
      ] |>
        unique()
      shiny::checkboxGroupInput(
        inputId = ns("activities"),
        label = htmlLabels[[ns("activities-label")]] |>
          translate_text(lang = input$lang),
        choices = stats::setNames(
          choices,
          choices |> translate_text(lang = input$lang)
        ),
        width = "100%"
      ) |>
        shiny::tagAppendAttributes(
          style = "
          display: flex;
          justify-content: space-between;
          ",
          .cssSelector = "label"
        ) |>
        shiny::tagAppendAttributes(
          style = "
          width: 1em;
          margin-right: 1em;
          ",
          .cssSelector = "input"
        ) |>
        shiny::tagAppendAttributes(
          style = "
          width: calc(100% - 1em);
          ",
          .cssSelector = "span"
        )
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
      session$sendCustomMessage(
        "translateLabels",
        htmlLabels |>
          translate_text(lang = input$lang) |>
          # convert to list for conversion to JSON object
          lapply(identity)
      )
    })
    # used to disable apply button if activities are not selected
    shiny::observeEvent(
      input$activities,
      {
        btnId <- "apply"
        if (length(input$activities) > 0) {
          session$sendCustomMessage("toggleDisable", list(ns(btnId), FALSE))
        } else {
          session$sendCustomMessage("toggleDisable", list(ns(btnId), TRUE))
          pathway(NULL)
        }
      },
      ignoreNULL = FALSE
    )
    ###########################################################################
    #                                                                         #
    # Diagrams                                                                #
    #                                                                         #
    ###########################################################################
    # Interactive View
    # visNetwork
    output$pathwayInteractive <- visNetwork::renderVisNetwork({
      shiny::req(pathway())
      visNetwork::visNetwork(
        nodes = pathway()[["nodes"]],
        edges = pathway()[["edges"]]
      ) |>
        visNetwork::visNodes(font = list(size = 10)) |>
        visNetwork::visEdges(arrows = "to") |>
        visNetwork::visHierarchicalLayout(levelSeparation = 800) |>
        visNetwork::visOptions(
          highlightNearest = list(
            enabled = TRUE,
            degree = list(from = 1000, to = 1000),
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
      DiagrammeR::create_graph(
        nodes_df = dot[["nodes"]],
        edges_df = dot[["edges"]]
      ) |>
        DiagrammeR::add_global_graph_attrs(
          attr = "splines",
          value = "ortho",
          attr_type = "graph"
        ) |>
        DiagrammeR::add_global_graph_attrs(
          attr = "overlap",
          value = FALSE,
          attr_type = "graph"
        ) |>
        DiagrammeR::add_global_graph_attrs(
          attr = "style",
          value = "rounded, filled",
          attr_type = "node"
        ) |>
        DiagrammeR::render_graph(layout = "tree", output = "graph") |>
        add_zoom(id = ns("pathwayOrthogonal"))
    })
  })
}
