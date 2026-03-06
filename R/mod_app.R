#' User Interface module for pathways of effects
#'
#' Creates the Shiny UI for the Pathways of Effects Tool.
#'
#' @param id Character. Shiny Module ID.
#' @param act2Pres Data frame. Components and activity to stressors lookup table.
#' @param activities Data frame. Activities organized by sector.
#' @param htmlLabels Named list. Element IDs as names and labels as values for
#'   translation.
#' @param legColors Character vector. Legend square colors.
#' @param legText Character vector. Legend text labels.
#' @param legSize Numeric. Size of legend squares in pixels.
#'
#' @returns Shiny module UI.
#'
#' @noRd

poeUI <- function(
  id,
  act2Pres,
  activities,
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
    includeScript(system.file("www", "svg-pan-zoom.min.js", package = "poe")),
    # tags$div(
    #   class = "card bslib-card card-header d-flex
    # justify-content-between align-items-center flex-row",
    #   tags$div(
    #     "Pathways of Effects",
    #     id = ns("dbTitle"),
    #     style = "font-size: 2rem; font-weight: bold;"
    #   ),
    # ),
    page_navbar(
      title = span(id = ns("uiTitle"), htmlLabels[["uiTitle"]]),
      theme = poe_theme(),
      header = shinyjs::useShinyjs(),
      # Inputs -------------------------------------------
      sidebar = sidebar(
        width = "25%",
        title = tags$header(
          htmlLabels[["sidebarPathwaysLabel"]],
          class = "sidebar-title",
          id = ns("sidebarPathwaysLabel")
        ),

        accordion(
          id = ns("ui"),
          open = c("a_vc", "a_m", "a_r"),

          accordion_panel(
            title = span(
              id = ns("valuedComponentLabel"),
              htmlLabels[["valuedComponentLabel"]]
            ),
            value = "a_vc", # By default is the title, not when that is html (e.g., span(...))

            # Valued Components ----------------------------------------
            selectInput(
              inputId = ns("valuedComponent"),
              label = NULL,
              choices = c(" " = "", unique(act2Pres$valued_component)),
              selected = ""
            ),
            # Sector ------------------------------------------------
            uiOutput(outputId = ns("sectorsUi")),
            # Activities ---------------------------------------------
            uiOutput(outputId = ns("activitiesUi"))
          ),
          accordion_panel(
            title = span(
              id = ns("mitigationMeasures"),
              htmlLabels[["mitigationMeasures"]]
            ),
            value = "a_m",
            # Mitigation Measures ----------------------------------------
            uiOutput(ns("mitigationsUi")),
            input_switch(
              id = ns("toggleMitigations"),
              label = span(
                id = ns("toggleMitigationsLabel"),
                htmlLabels[["toggleMitigationsLabel"]]
              )
            )
          ),
          # Report -------------------------------------
          accordion_panel(
            title = span(id = ns("report"), htmlLabels[["report"]]),
            value = "a_r",
            bslib::layout_column_wrap(
              textInput(
                ns("reportName"),
                label = span(
                  id = ns("reportNameLabel"),
                  htmlLabels[["reportNameLabel"]]
                )
              ),
              textInput(
                ns("reportStatus"),
                label = span(
                  id = ns("reportStatusLabel"),
                  htmlLabels[["reportStatusLabel"]]
                )
              )
            ),
            textAreaInput(
              ns("reportNotes"),
              label = NULL,
              rows = 3,
              placeholder = "Notes..."
            ),
            downloadButton(
              ns("report"),
              label = span(id = ns("reportLabel"), htmlLabels[["reportLabel"]])
            ),
            div(
              style = "margin-top: 5px;",
              tags$em(
                "Errors in the download can arise if the VPN drops the connection. If this happens, try again. If the error persists, refresh the page."
              )
            )
          ),
        )
      ),

      nav_panel(
        title = tags$div("Interactive View", id = ns("tabInteractive")),
        # Custom Mitigations Sidebar -------------------------------------
        layout_sidebar(
          sidebar = sidebar(
            id = ns("sidebarMitigations"), # Required for toggling
            class = "pad-left",
            width = 350,
            position = "right",
            open = FALSE,
            title = tags$header(
              htmlLabels[["sidebarMitigationsLabel"]],
              class = "sidebar-title",
              id = ns("sidebarMitigationsLabel")
            ),
            accordion(
              open = TRUE,
              multiple = TRUE,
              accordion_panel(
                title = span(
                  id = ns("addMitigations"),
                  htmlLabels[["addMitigations"]]
                ),
                value = "",
                textOutput(ns("currentEdge"), inline = TRUE),
                textInput(
                  ns("addMitigationName"),
                  label = span(
                    id = ns("addMitigationNameLabel"),
                    htmlLabels[["addMitigationNameLabel"]]
                  )
                ),
                textInput(
                  ns("addMitigationDesc"),
                  label = span(
                    id = ns("addMitigationDescriptionLabel"),
                    htmlLabels[["addMitigationDescriptionLabel"]]
                  )
                ),
                actionButton(
                  ns("createMitigation"),
                  label = span(
                    id = ns("createMitigationLabel"),
                    htmlLabels[["createMitigationLabel"]]
                  )
                )
              ),
              accordion_panel(
                title = span(
                  id = ns("removeMitigations"),
                  htmlLabels[["removeMitigations"]]
                ),
                value = "",
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
      # ON HOLD (logic for the flowcharts and orthoganal views are on hold)
      # nav_panel(
      #   title = tags$div("Flowchart View", id = ns("tabFlowchart")),
      #   DiagrammeR::DiagrammeROutput(
      #     ns("pathwayFlowchart"),
      #     width = "100%",
      #     height = "100%"
      #   ),
      #   make_poe_legend(
      #     id = ns("leg2"),
      #     colors = legColors,
      #     labels = legText,
      #     size = legSize
      #   )
      # ),
      # nav_panel(
      #   title = tags$div("Orthogonal View", id = ns("tabOrthogonal")),
      #   DiagrammeR::grVizOutput(
      #     ns("pathwayOrthogonal"),
      #     width = "100%",
      #     height = "100%"
      #   ),
      #   make_poe_legend(
      #     id = ns("leg3"),
      #     colors = legColors,
      #     labels = legText,
      #     size = legSize
      #   )
      # ),
      nav_spacer(),
      nav_item(radioButtons(
        inputId = ns("lang"),
        label = "",
        choices = c("English" = "en", "Fran\u00e7ais" = "fr"),
        inline = TRUE
      ))
    )
  )
}

#' Server module for pathways of effects
#'
#' Creates the Shiny server logic for the Pathways of Effects tool.
#'
#' @param id Character. Shiny Module ID.
#' @param act2Pres Data frame. Components and activity to stressors lookup table.
#' @param mitigations Data frame. Mitigation data.
#' @param pathways Data frame. Pathway diagrams.
#' @param activities Data frame. Activities organized by sector.
#' @param htmlLabels Named list. Element IDs as names and labels as values for
#'   translation.
#'
#' @returns Shiny module Server.
#'
#' @noRd

poeServer <- function(
  id,
  act2Pres,
  mitigations,
  pathways,
  activities,
  htmlLabels
) {
  moduleServer(id, function(input, output, session) {
    # Setup ----------------------------------------------------------------
    ns <- session$ns
    htmlLabels <- stats::setNames(htmlLabels, ns(names(htmlLabels)))
    customMitigations <- reactiveVal(data.frame())

    ###########################################################################
    #                                                                         #
    # Pathways --------------------------------------------------------------------
    #                                                                         #
    ###########################################################################

    # start with no pathway
    pathway <- reactiveVal(NULL)

    # prune selected value component pathway by selected activities
    observe({
      req(input$activities)
      p <- prep_pathways(
        pathways,
        act2Pres,
        vc = input$valuedComponent,
        a = input$activities,
        lang = input$lang
      )

      # update pathway
      pathway(p)
    })

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
        label = htmlLabels[[ns("activitiesLabel")]] |>
          translate_text(isolate(input$lang)),
        choices = named_choices(choices, lang = isolate(input$lang)),
        selected = activities$activities[activities$sector == input$sector],
        width = "100%"
      )
    })

    ## Sectors ------------------------------------------
    output$sectorsUi <- renderUI({
      selectInput(
        inputId = ns("sector"),
        label = NULL,
        choices = c(
          stats::setNames("", translate_text("Sector", input$lang)),
          unique(activities$sector[!is.na(activities$sector)])
        ),
        selected = translate_text("Sector", input$lang)
      )
    })

    ## Mitigations ------------------------------------------------------
    choices_mitigations <- reactive({
      validate(
        need(
          !is.null(input$activities) && length(input$activities) > 0,
          translate_text(
            "Activities must be selected before mitigation measures",
            input$lang
          )
        ),
        errorClass = "space"
      )
      req(pathway())

      o <- tryCatch(check_mitigations(mitigations), error = \(r) {
        list(FALSE, r$message)
      })

      validate(need(
        o[[1]],
        paste0(
          translate_text(
            "Problems with the mitigations file, contact the CWS team and report the following",
            input$lang
          ),
          ":\n\n",
          o[[2]]
        )
      ))

      # Only show mitigations with start/end or edge on the diagram
      nodes <- pathway()$nodes$id
      edges <- pathway()$edges$id

      m <- mitigations_all()
      m <- m[m$valued_component == input$valuedComponent, ]
      m <- m[
        (m$start_node %in% nodes & m$end_node %in% nodes) |
          m$edge_id %in% edges,
        c("m_id", "short")
      ] |>
        unique()

      stats::setNames(m$m_id, m$short)
    })

    output$mitigationsUi <- renderUI({
      choices <- choices_mitigations()

      validate(need(
        length(choices) > 0,
        translate_text(
          "No mitigation measures apply to these activities",
          input$lang
        )
      ))

      validate(need(
        length(choices) == length(unique(choices)),
        translate_text(
          "There are duplicate short names in Mitigations. These should be unique.",
          input$lang
        )
      ))

      checkboxGroupInput(
        inputId = ns("mitigations"),
        label = htmlLabels[[ns("mitigationsLabel")]] |>
          translate_text(isolate(input$lang)),
        choices = named_choices(
          choices,
          name = names(choices),
          lang = isolate(input$lang)
        ),
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

      if (!input$toggleMitigations) {
        bslib::toggle_sidebar(
          id = "sidebarMitigations",
          open = input$toggleMitigations
        )
      }

      # Completely hide side bar if not adding custom mitigations

      shinyjs::toggleCssClass(
        selector = paste0("#", ns("sidebarMitigations"), " + button"),
        condition = !input$toggleMitigations,
        class = "poe-hidden"
      )

      if (input$toggleMitigations) {
        shinyjs::delay(
          100,
          bslib::toggle_sidebar(
            id = "sidebarMitigations",
            open = input$toggleMitigations
          )
        )
      }
    }) |>
      bindEvent(input$toggleMitigations)

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
        translate_text("Click on an edge to create a mitigation", input$lang)
      ))

      validate(need(
        input$currentEdge != "tryagain",
        translate_text(
          "Clear your selection (click on the background) and try clicking on an edge (not an activity) to create a mitigation",
          input$lang
        )
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
        translate_text("Currently selected edge", input$lang),
        ": ",
        input$currentEdge
      )
    })

    # Process and update customMitigations() data frame
    observe({
      req(input$currentEdge)

      nm <- paste0(input$addMitigationName, " *")
      m <- data.frame(
        valued_component = input$valuedComponent,
        start_node = NA_real_,
        end_node = NA_real_,
        m_id = paste0(
          simple_name(input$valuedComponent),
          "_",
          simple_name(nm)
        ),
        short = nm,
        long = input$addMitigationDesc,
        edge_id = input$currentEdge
      )

      m <- rbind(customMitigations(), m) |> unique()
      customMitigations(m)
    }) |>
      bindEvent(input$createMitigation)

    output$removeMitigationsUi <- renderUI({
      btns <- lapply(unique(customMitigations()$m_id), \(m) {
        # Mitigations with the same name, but different edge, treated as single
        # mitigation with mutliple paths

        m <- customMitigations()[
          customMitigations()$m_id == m,
          c("valued_component", "short", "m_id")
        ] |>
          unique()

        tagList(
          span(
            actionButton(
              ns(paste0("remove-miti-", m$m_id)),
              label = NULL,
              icon = icon("x"),
              class = "btn-sm"
            ),
            paste0(
              stringr::str_remove(
                m$short,
                " \\*"
              ),
              " (",
              m$valued_component,
              ")"
            )
          )
        )
      })
      tagList(!!!btns)
    })

    observe({
      req(nrow(customMitigations()) > 0)
      lapply(customMitigations()$m_id, \(m) {
        observe({
          cm <- customMitigations()
          cm <- cm[cm$m_id != m, ]
          customMitigations(cm)
        }) |>
          bindEvent(input[[paste0("remove-miti-", m)]], ignoreInit = TRUE)
      })
    }) |>
      bindEvent(customMitigations())

    # Translations ------------------------------------------------------
    observe({
      # Update non-dynamic UI elements with JS
      session$sendCustomMessage(
        "translateLabels",
        htmlLabels |>
          translate_text(input$lang) |>
          # convert to list for conversion to JSON object
          lapply(identity)
      )

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

      updateSelectInput(
        session = session,
        inputId = "sector",
        choices = named_choices(
          unique(activities$sector[!is.na(activities$sector)]),
          lang = input$lang
        ),
        selected = input$sector
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
        choices = named_choices(
          choices_mitigations(),
          name = names(choices_mitigations()),
          lang = input$lang
        ),
        selected = input$mitigations
      )
      # NOTE: Nothing should follow mitigations here because at the start they
      #  validate(need) activities, so the logic won't proceed until activities
      #  are selected
    }) |>
      bindEvent(input$lang, ignoreInit = TRUE)

    ###########################################################################
    #                                                                         #
    # Diagrams --------------------------------------------------------------
    #                                                                         #
    ###########################################################################

    ## Interactive View - visNetwork ----------------------------------------
    vis_ready <- reactiveVal(FALSE)

    output$pathwayInteractive <- visNetwork::renderVisNetwork({
      req(pathway())

      # Track readiness of vis
      vis_ready(FALSE)

      # selectEdge example: https://github.com/datastorm-open/visNetwork/issues/385
      make_visnetwork(pathway()) |>
        # fmt:skip
        visNetwork::visEvents(
          selectEdge = paste0(
            "function(data) {
              Shiny.onInputChange('", ns("currentEdge"), "', data.edges);
            }"
          ),
          # Ping input$vis_stable when ready
          afterDrawing = paste0(
            "function() {
              Shiny.onInputChange('", ns("vis_stable"), "', Math.random());
            }"
          )
        )
    })

    observe({
      vis_ready(TRUE)
    }) |>
      bindEvent(input$vis_stable, ignoreInit = TRUE)

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
      req(vis_ready())
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
    }) |>
      bindEvent(vis_mitigations(), ignoreInit = TRUE)

    # Download Report --------------------------------------------------------
    output$report <- downloadHandler(
      filename = paste0("report_", Sys.Date(), ".html"),
      content = \(file) {
        withProgress(
          message = 'Creating report...',
          detail = "This may take a minute",
          {
            create_report(
              vc = input$valuedComponent,
              a = input$activities,
              m = input$mitigations,
              m_df = mitigations_all(),
              notes = input$reportNotes,
              project_name = input$reportName,
              project_status = input$reportStatus,
              lang = input$lang,
              path = file
            )
          }
        )
      }
    )
  })
}
