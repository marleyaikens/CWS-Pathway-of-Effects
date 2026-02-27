#' Check mitigations file for problems
#'
#' Validates that all mitigation edges exist in the pathway data.
#'
#' @param mitigations Data frame. Mitigations data from [read_mitigations()].
#' @param pathways Data frame. Pathways data from [read_pathways()].
#'
#' @returns List with validation status (invisibly). Stops with error if
#'   problems are found.
#'
#' @export
#' @examplesIf have_data()
#' check_mitigations()

check_mitigations <- function(
  mitigations = read_mitigations(),
  pathways = read_pathways()
) {
  edges <- get_edges(pathways)
  edges$edge_id = edges$id
  mitigations <- mitigations[, c(
    "valued_component",
    "start_node",
    "end_node",
    "short",
    "edge_id"
  )]

  # Check that the edge to be mitigated does exist
  edges$exists <- TRUE
  m_check <- merge(
    mitigations,
    edges[, c("valued_component", "edge_id", "exists")],
    all.x = TRUE
  )

  m_check$exists[is.na(m_check$exists)] <- FALSE

  if (!all(m_check$exists)) {
    stop(
      "Problems found!\n",
      "Some mitigations are for non-existant edges:\n",
      paste0(
        utils::capture.output(as.data.frame(m_check[!m_check$exists, ])),
        collapse = "\n"
      ),
      "\n\n"
    )
  }

  invisible(list(TRUE, "")) # For capturing the message in the Shiny app
}

#' Get node color palette
#'
#' Returns a named vector of colors for different node types in the pathway
#' diagram.
#'
#' @returns Named character vector. Node type names with corresponding hex
#'   colors.
#'
#' @noRd

node_colours <- function() {
  c(
    "component" = "#88bde9", # Valued Component
    "stressor" = "#fee599", # Stressors/Pressures
    "effect" = "#f2f2f2", # Effect
    "contravention" = "#f06c6c", # Potential Contravention of Legislation and/or Regulations
    "habitat" = "#9dbb61", # Habitat/Wetland Availability and Distribution
    "quality" = "#f59d56", # Fitness, Reproduction, and Mortality / Wetland Quality and Function
    "population" = "#7e649e"
  )
}


#' Create PoE Shiny app theme
#'
#' Creates a Bootstrap theme with custom styling for the Pathways of Effect
#' Shiny app.
#'
#' @returns bslib theme object.
#'
#' @noRd

poe_theme <- function() {
  bs_theme(
    version = 5,
    #bootswatch = "materia",
    "success" = "#5a8e51ff",
    "card-border-radius" = "0",
    "btn-padding-y-sm" = "0.10rem",
    "btn-padding-x-sm" = "0.25rem",
    "btn-font-size-sm" = "0.75rem"
  ) |>
    bs_add_rules(
      "
      /* Custom rules */
      g > .node {
        text-align: center;
      }
      .shiny-output-error-space {
        margin-bottom: 20px;
      }
      /* Reduce padding in sidebars */
      .sidebar-content {
        padding-top: 20px !important;
      } 
      .sidebar-title {
        margin-bottom: 0 !important;
      }
      /* Completely disable the side bar if no mitigations */
      .poe-hidden {
        display:none !important;
      }
      .pad-left  .sidebar-title {
        padding-left: 15%;
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
      }
     /* Remove gaps between cards and page */
        .main.bslib-gap-spacing {
          padding: 0.5px !important;
        }
    "
    )
}

#' Check pathways file for problems
#'
#' Placeholder function to validate that valued components have paths to all
#' listed stressors. Not yet implemented.
#'
#' @returns NULL.
#'
#' @noRd

check_pathways <- function() {
  # Expect that a Valued Component has a path to each stressor listed
}

#' Check if package data files are available
#'
#' Tests whether the required data files (mitigations, activities, components,
#' pathways, translations) can be successfully read.
#'
#' @returns Logical. `TRUE` if all data files can be read, `FALSE` otherwise.
#'
#' @export

have_data <- function() {
  m <- try(read_mitigations(), silent = TRUE)
  a <- try(read_activities(), silent = TRUE)
  vc <- try(read_components(), silent = TRUE)
  p <- try(read_pathways(), silent = TRUE)
  t <- try(read_translations(), silent = TRUE)

  !inherits(m, "try-error") &&
    !inherits(a, "try-error") &&
    !inherits(vc, "try-error") &&
    !inherits(p, "try-error")
}

#' Skip test if no data available
#'
#' Testthat helper to skip tests when required data files are not available.
#'
#' @returns Invisible `NULL` if data is available, otherwise skips test.
#'
#' @noRd

skip_if_no_data <- function() {
  if (!have_data()) {
    testthat::skip("No data")
  } else {
    invisible()
  }
}


#' Convert to simple name format
#'
#' Converts text to lowercase and replaces spaces with underscores for use as
#' IDs or keys.
#'
#' @param x Character. Text to convert.
#'
#' @returns Character. Simplified text.
#'
#' @noRd

simple_name <- function(x) {
  x |> tolower() |> stringr::str_replace_all(" ", "_")
}
