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
  list(
    #tags$style(HTML(css)),
    tags$script(HTML(js)),
    includeScript("www/svg-pan-zoom.min.js"),
    # tags$div(
    #   class = "card bslib-card card-header d-flex
    # justify-content-between align-items-center flex-row",
    #   tags$div(
    #     "Pathways of Effect",
    #     id = ns("dbTitle"),
    #     style = "font-size: 2rem; font-weight: bold;"
    #   ),
    # ),
    page_navbar(
      title = "Pathways of Effect",
      theme = poe_theme(),
      header = shinyjs::useShinyjs(),
      # Inputs -------------------------------------------
      sidebar = sidebar(
        width = "25%",
        title = tags$header(
          "Build Pathways",
          class = "sidebar-title",
          id = ns("cardTitle1")
        ),

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
            uiOutput(ns("mitigationsUi")),
            input_switch(ns("toggleMitigations"), "Add Custom Mitigations")
          ),
          downloadButton(
            ns("report"),
            "Download report",
            class = "btn-primary"
          ),
        )
      ),

      nav_panel(
        title = tags$div("Interactive View", id = ns("cardTitle2")),
        # Custom Mitigations Sidebar -------------------------------------
        layout_sidebar(
          sidebar = sidebar(
            class = "pad-left",
            width = 350,
            id = ns("sidebarMitigations"),
            position = "right",
            open = FALSE,
            title = "Custom Mitigations",
            accordion(
              open = TRUE,
              multiple = TRUE,
              accordion_panel(
                title = "Add Mitigations",
                textOutput(ns("currentEdge"), inline = TRUE),
                textInput(
                  ns("addMitigationName"),
                  "Name",
                  placeholder = "Short name"
                ),
                textInput(
                  ns("addMitigationDesc"),
                  "Description",
                  placeholder = "Longer descriptive title"
                ),
                actionButton(ns("createMitigation"), "Create mitigation")
              ),
              accordion_panel(
                title = "Remove Created Mitigations",
                uiOutput(ns("removeMitigationsUi"))
              )
            )
          ),
          # Diagrams ----------------------------------------
          card(
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
          )
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
      ),
      nav_spacer(),
      nav_item(radioButtons(
        inputId = ns("lang"),
        label = "",
        choices = c("English" = "en", "French" = "fr"),
        inline = TRUE
      ))
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
    # Setup ----------------------------------------------------------------
    ns <- session$ns
    htmlLabels <- stats::setNames(htmlLabels, ns(names(htmlLabels)))
    customMitigations <- reactiveVal(data.frame())

    # TODO: Remove Developer Testing at end --------------------------------
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

      m <- mitigations_all()

      # Only show mitigations with start/end or edge on the diagram
      nodes <- pathway()$nodes$id
      edges <- pathway()$edges$id
      choices <- m$short_en[
        (m$start_node %in% nodes & m$end_node %in% nodes) |
          m$edge %in% edges
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
        selected = isolate(input$mitigations),
        width = "100%"
      )
    })

    mitigations_all <- reactive({
      rbind(mitigations, customMitigations())
    })

    # Adding Mitigations ------------------------------------------------

    # Toggle Custom Mitigation sidebar
    observe({
      req(!is.null(input$toggleMitigations))

      bslib::sidebar_toggle(
        id = "sidebarMitigations",
        open = input$toggleMitigations
      )

      # Completely hide side bar if not adding custom mitigations
      shinyjs::toggleCssClass(
        selector = paste0("#", ns("sidebarMitigations"), " + button"),
        condition = !input$toggleMitigations,
        class = "poe-hidden"
      )
    })

    output$currentEdge <- renderText({
      shinyjs::toggleState(
        "addMitigationName",
        condition = length(input$currentEdge) == 1
      )
      shinyjs::toggleState(
        "addMitigationDesc",
        condition = length(input$currentEdge) == 1
      )

      validate(need(
        !is.null(input$currentEdge),
        "Click on an edge to create a mitigation"
      ))

      validate(need(
        input$currentEdge != "tryagain",
        "Clear your selection (click on the background) and try clicking on an edge (not an activity) to create a mitigation"
      ))

      if (length(input$currentEdge) > 1) {
        shinyjs::runjs(
          paste0(
            "Shiny.onInputChange('",
            ns("currentEdge"),
            "', 'tryagain');"
          )
        )
      }

      req(length(input$currentEdge) == 1)

      paste0(
        "Currently selected edge:",
        input$currentEdge
      )
    })

    # Process and update customMitigations() data frame
    observe({
      input$currentEdge

      nm <- paste0(input$addMitigationName, " (custom)")

      m <- data.frame(
        start_node = NA_real_,
        end_node = NA_real_,
        edge = input$currentEdge,
        short_en = nm,
        long_en = input$addMitigationDesc,
        short_fr = nm,
        long_fr = input$addMitigationDesc
      )

      m <- rbind(customMitigations(), m)
      customMitigations(m)
    }) |>
      bindEvent(input$createMitigation)

    output$removeMitigationsUi <- renderUI({
      # TODO: Programatically add buttons/observersfor removing custom mitigations

      btns <- lapply(customMitigations()$short_en, \(m) {
        tagList(
          span(
            actionButton(
              ns(paste0("remove-miti-", m)),
              label = NULL,
              icon = icon("x"),
              class = "btn-sm"
            ),
            stringr::str_remove(m, " (custom)")
          )
        )
      })
      tagList(!!!btns)
    })

    observe({
      req(nrow(customMitigations()) > 0)
      lapply(customMitigations()$short_en, \(m) {
        observe({
          cm <- customMitigations()
          cm <- cm[cm$short_en != m, ]
          customMitigations(cm)
        }) |>
          bindEvent(input[[paste0("remove-miti-", m)]], ignoreInit = TRUE)
      })
    }) |>
      bindEvent(customMitigations())

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
      # selectEdge example: https://github.com/datastorm-open/visNetwork/issues/385
      make_visnetwork(pathway()) |>
        # fmt:skip
        visNetwork::visEvents(
        selectEdge = paste0(
          "function(data) {
            Shiny.onInputChange('", ns("currentEdge"), "', data.edges);
           }"
        )
      )
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
        mitigations_all(),
        input$mitigations,
        lang = input$lang
      )
    })

    observe({
      visNetwork::visNetworkProxy(ns("pathwayInteractive")) |>
        visNetwork::visUpdateEdges(edges = vis_mitigations()$edges) |>
        visNetwork::visUpdateNodes(nodes = vis_mitigations()$nodes)
    })

    # Download Report --------------------------------------------------------
    output$report <- downloadHandler(
      filename = paste0("report_", Sys.Date(), ".html"),
      content = \(file) {
        withProgress(message = 'Creating report...', {
          create_report(
            vc = input$valuedComponent,
            a = input$activities,
            m = input$mitigations,
            m_df = mitigations_all(),
            lang = input$lang,
            path = file
          )
        })
      }
    )
  })
}
