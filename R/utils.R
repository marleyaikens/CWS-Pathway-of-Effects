#' Check mitigations file for problems
#'
#' @param mitigations
#' @param pathways
#'
#' @returns
#'
#' @export
#' @examples
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

  if (any(!m_check$exists)) {
    stop(
      "Problems found!\n",
      "Some mitigations are for non-existant edges:\n",
      paste0(
        capture.output(as.data.frame(m_check[!m_check$exists, ])),
        collapse = "\n"
      ),
      "\n\n"
    )
  }

  invisible(list(TRUE, "")) # For capturing the message in the Shiny app
}

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

check_pathways <- function() {
  # Expect that a Valued Component has a path to each stressor listed
}

is_ready <- function(r) {
  tryCatch(!is.null(r()), error = \(e) FALSE)
}

simple_name <- function(x) {
  x |> tolower() |> stringr::str_replace_all(" ", "_")
}
