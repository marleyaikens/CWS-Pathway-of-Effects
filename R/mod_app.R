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
  }
  .mermaid-disabled > rect {
    fill: #e3e3e3 !important;
  }
  .mermaid-disabled .label {
    color: #d5d5d5 !important;
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
          id = ns("ui"),
          open = "a_vc",

          accordion_panel(
            span(
              id = ns("valuedComponent"),
              htmlLabels[["valuedComponent"]]
            ),
            value = "a_vc", # By default is the title, not when that is html (e.g., span(...))

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
            uiOutput(outputId = ns("activitiesUi"))
          ),
          accordion_panel(
            span(
              id = ns("mitigationMeasures"),
              htmlLabels[["mitigationMeasures"]]
            ),
            value = "a_m",
            # Mitigation Measures ----------------------------------------
            uiOutput(ns("mitigationsUi"))
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

    # TODO: Remove Developer testing at end --------------------------------
    observe({
      req(input$valuedComponent)
      updateCheckboxGroupInput(
        session,
        "activities",
        selected = "Land and Vegetation clearing and vegetation maintenance"
      )
      accordion_panel_close("ui", values = "a_vc")
      accordion_panel_open("ui", values = "a_m")
    })

    ###########################################################################
    #                                                                         #
    # Pathways --------------------------------------------------------------------
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
      bindEvent(
        input$activities,
        input$lang
      )

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

      o <- tryCatch(check_mitigations(mitigations), error = \(r) {
        list(FALSE, r$message)
      })

      validate(need(
        o[[1]],
        paste0(
          "Problems with the mitigations file, contact the CWS team and report the following:\n\n",
          o[[2]]
        )
      ))

      # Only show mitigations with start/end or edge on the diagram
      nodes <- pathway()$nodes$id
      edges <- pathway()$edges$id
      choices <- mitigations$short_en[
        (mitigations$start_node %in% nodes & mitigations$end_node %in% nodes) |
          mitigations$edge %in% edges
      ] |>
        unique()

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

    ###########################################################################
    #                                                                         #
    # Diagrams --------------------------------------------------------------
    #                                                                         #
    ###########################################################################

    ## Interactive View - visNetwork ----------------------------------------
    output$pathwayInteractive <- visNetwork::renderVisNetwork({
      make_visnetwork(pathway())
    })

    ## Flowchart View - mermaid ------------------------------------------
    output$pathwayFlowchart <- DiagrammeR::renderDiagrammeR({
      #make_flowchart(pathway(), ns("pathwayFlowchart"))
      make_flowchart(vis_mitigations()) |>
        add_zoom(id = ns("pathwayFlowchart"))
    })

    ## Orthogonal View - DiagrammR ----------------------------------------
    output$pathwayOrthogonal <- DiagrammeR::renderGrViz({
      make_orthogonal(pathway()) |>
        add_zoom(id = ns("pathwayOrthogonal"))
    })

    ## Add mitigations -------------------------------------
    vis_mitigations <- reactive({
      add_mitigation(
        pathway(),
        mitigations,
        input$mitigations,
        lang = input$lang
      )
    })

    observe({
      visNetwork::visNetworkProxy(ns("pathwayInteractive")) |>
        visNetwork::visUpdateEdges(edges = vis_mitigations()$edges) |>
        visNetwork::visUpdateNodes(nodes = vis_mitigations()$nodes)
    })
  })
}
