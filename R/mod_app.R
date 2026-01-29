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
poeUI <- function(
  id,
  act2Pres,
  htmlLabels,
  legColors,
  legText,
  legSize
) {
  # Setup -----------------------------------------------------------------
  ns <- NS(id)
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
  div.checkbox label {
    display: flex; 
    justify-content: space-between;
  }
  input[type='checkbox'] {
    width: 1em;
    margin-right: 1em;
  }
  div.checkbox span {
    width: calc(100% - 1em);
  }"
  list(
    tags$style(HTML(css)),
    tags$script(HTML(js)),
    includeScript("www/svg-pan-zoom.min.js"),
    tags$div(
      class = "card bslib-card card-header d-flex 
    justify-content-between align-items-center flex-row",
      tags$div(
        "Pathways of Effect",
        id = ns("dbTitle"),
        style = "font-size: 2rem; font-weight: bold;"
      ),
      radioButtons(
        inputId = ns("lang"),
        label = "",
        choices = c("English" = "en", "French" = "fr"),
        inline = TRUE
      )
    ),
    layout_columns(
      col_widths = c(3, 9),
      class = "p-1",

      # Inputs ----------------------------------------------------
      card(
        full_screen = TRUE,
        card_header(tags$div("Inputs", id = ns("cardTitle1"))),

        accordion(
          open = htmlLabels[["valuedComponent"]],

          accordion_panel(
            span(
              id = ns("valuedComponent"),
              htmlLabels[["valuedComponent"]]
            ),
            value = "a1", # By default is the title, but doesn't like span(...)

            # Valued Components ----------------------------------------
            selectInput(
              inputId = ns("valuedComponent"),
              label = NULL,
              choices = unique(act2Pres$valued_component),
              selected = "",
              multiple = FALSE,
              selectize = TRUE,
              width = "100%"
            ),
            # Activities ---------------------------------------------
            uiOutput(outputId = ns("activitiesUi")),
            actionButton(
              ns("applyActivities"),
              htmlLabels[["applyActivities"]],
              class = "btn-primary"
            )
          ),
          accordion_panel(
            span(
              id = ns("mitigationMeasures"),
              htmlLabels[["mitigationMeasures"]]
            ),
            value = "a2",
            # Mitigation Measures ----------------------------------------
            uiOutput(ns("mitigationsUi")),
            actionButton(
              ns("applyMitigations"),
              htmlLabels[["applyMitigations"]],
              class = "btn-primary"
            )
          )
        )
      ),
      navset_card_tab(
        full_screen = TRUE,
        nav_panel(
          title = tags$div("Interactive View", id = ns("cardTitle2")),
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
        nav_panel(
          title = tags$div("Flowchart View", id = ns("cardTitle3")),
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
        nav_panel(
          title = tags$div("Orthogonal View", id = ns("cardTitle4")),
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
poeServer <- function(id, act2Pres, mitigations, pathways, htmlLabels) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    htmlLabels <- stats::setNames(htmlLabels, ns(names(htmlLabels)))

    ###########################################################################
    #                                                                         #
    # Data --------------------------------------------------------------------
    #                                                                         #
    ###########################################################################

    # start with no pathway
    pathway <- reactiveVal(NULL)

    # prune selected value component pathway by selected activities
    observe({
      p <- prep_pathways(
        pathways,
        act2Pres,
        vc = input$valuedComponent,
        a = input$activities,
        lang = input$lang
      )
      # update pathway
      pathway(p)
    }) |>
      bindEvent(input$applyActivities, input$lang)

    ###########################################################################
    #                                                                         #
    # User Interactions -------------------------------------------------------
    #                                                                         #
    ###########################################################################

    ## Activities -------------------------------------
    choices_activities <- reactive({
      choices <- act2Pres[["activities"]][
        act2Pres[["valued_component"]] %in% input$valuedComponent
      ] |>
        unique()

      choices
    })

    output$activitiesUi <- renderUI({
      choices <- choices_activities()
      checkboxGroupInput(
        inputId = ns("activities"),
        label = htmlLabels[[ns("activities-label")]] |>
          translate_text(isolate(input$lang)),
        choices = named_choices(choices, lang = isolate(input$lang)),
        width = "100%"
      )
    })

    ## Mitigations ------------------------------------------------------
    choices_mitigations <- reactive({
      req(pathway())

      # Only show mitigations with start/end or edge on the diagram
      nodes <- pathway()$nodes$id
      edges <- pathway()$edges$id
      choices <- mitigations$short_en[
        (mitigations$start_node %in% nodes & mitigations$end_node %in% nodes) |
          mitigations$edge %in% edges
      ]

      choices
    })

    output$mitigationsUi <- renderUI({
      choices <- choices_mitigations()

      # TODO: Translate Validates
      validate(need(
        length(choices) > 0,
        "No mitigation measures apply to these activities"
      ))

      validate(need(
        length(choices) == length(unique(choices)),
        "There are duplicate short names in Mitigations. These should be unique."
      ))

      checkboxGroupInput(
        inputId = ns("mitigations"),
        label = htmlLabels[[ns("mitigations-label")]] |>
          translate_text(isolate(input$lang)),
        choices = named_choices(choices, lang = isolate(input$lang)),
        width = "100%"
      )
    })

    # Translations ------------------------------------------------------
    observe({
      # Update UI Inputs
      updateSelectInput(
        session = session,
        inputId = "valuedComponent",
        choices = named_choices(
          unique(act2Pres$valued_component),
          lang = input$lang
        ),
        selected = input$valuedComponent
      )
      updateCheckboxGroupInput(
        session = session,
        inputId = "activities",
        choices = named_choices(choices_activities(), lang = input$lang),
        selected = input$activities
      )
      updateCheckboxGroupInput(
        session = session,
        inputId = "mitigations",
        choices = named_choices(choices_mitigations(), lang = input$lang),
        selected = input$mitigations
      )

      # Update other elements with JS

      session$sendCustomMessage(
        "translateLabels",
        htmlLabels |>
          translate_text(input$lang) |>
          # convert to list for conversion to JSON object
          lapply(identity)
      )
    }) |>
      bindEvent(input$lang, ignoreInit = TRUE)

    # used to disable apply button if activities are not selected
    observeEvent(
      input$activities,
      {
        btnId <- "applyActivities"
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
    # Diagrams --------------------------------------------------------------
    #                                                                         #
    ###########################################################################

    # Interactive View
    ## visNetwork ------------------------------------------------------------
    output$pathwayInteractive <- visNetwork::renderVisNetwork({
      req(pathway())
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
    ## mermaid ----------------------------------------------------------------
    output$pathwayFlowchart <- DiagrammeR::renderDiagrammeR({
      req(pathway())
      flowchart <- convert_mermaid_flowchart(data.table::copy(pathway()))
      flowchart |>
        DiagrammeR::DiagrammeR() |>
        add_zoom(id = ns("pathwayFlowchart"))
    })

    # Orthogonal View
    ## DiagrammR --------------------------------------------------------------
    output$pathwayOrthogonal <- DiagrammeR::renderGrViz({
      req(pathway())
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
