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

  invisible(list(TRUE, "")) # For capturing the message in the Shiny UI
}

#' Check pathways file for problems
#'
#' Placeholder function to validate that valued components have paths to all
#' listed stressors. Not yet implemented.
#'
#' @returns NULL.
#'
#' @noRd

check_pathways <- function(pathways = read_pathways()) {
  # Downstream nodes exist
  p <- split(pathways, pathways$valued_component)
  e <- get_edges(pathways)
  e <- split(e, e$valued_component)

  nodes_good <- sapply(e, \(x) {
    ids <- p[[x$valued_component[1]]]$node_id
    all(x$from %in% ids & x$to %in% ids)
  })

  msg <- ""
  if (!all(nodes_good)) {
    w <- names(nodes_good)[!nodes_good]
    msg <- c(
      msg,
      "Some 'lead_to' include node ids which don't exist in that valued component: ",
      paste0(w, collapse = ", ")
    )
  }

  if (msg != "") {
    stop(msg, call. = FALSE)
  }
}

check_activities <- function(
  components = read_components(),
  sectors = read_sectors()
) {
  a_c <- unique(components$activities)
  a_s <- unique(sectors$activities)

  # All sector activities must be in components (but not vice versa)
  if (!all(a_s %in% a_c)) {
    stop(
      "Some activities in sectors.xlsx are not in components.xlsx:",
      paste0(a_s[!a_s %in% a_c], collapse = ", "),
      call. = FALSE
    )
  }
}

check_stressors <- function(
  pathways = read_pathways(),
  components = read_components()
) {
  p <- split(
    pathways[, c("valued_component", "label", "type")],
    pathways$valued_component
  )
  c <- split(
    components[, c("valued_component", "stressors")],
    components$valued_component
  )

  s_good <- lapply(names(p), \(v) {
    s_p <- p[[v]]$label[p[[v]]$type == "stressor"] |> unique()
    s_c <- c[[v]]$stressors |> unique()

    c(all(s_p %in% s_c), all(s_c %in% s_p))
  })

  # Tests
  #s_good[[1]][2] <- FALSE
  #s_good[[4]][1] <- FALSE

  if (!all(unlist(s_good))) {
    p1 <- names(p)[sapply(s_good, \(x) !x[1])]
    p2 <- names(p)[sapply(s_good, \(x) !x[2])]
    msg <- ""
    if (length(p1) > 0) {
      msg <- c(
        msg,
        "Some stressors in pathways.xlsx not in components.xlsx: ",
        paste0(p1, collapse = ", "),
        "\n"
      )
    }
    if (length(p2) > 0) {
      msg <- c(
        msg,
        "Some stressors in components.xlsx not in pathways.xlsx: ",
        paste0(p2, collapse = ", "),
        "\n"
      )
    }
    stop(msg, call. = FALSE)
  }
}

check_components <- function(
  pathways = read_pathways(),
  components = read_components(),
  mitigations = check_mitigations()
) {
  v_p <- pathways$valued_component |> unique()
  v_c <- components$valued_component |> unique()
  v_m <- mitigations$valued_component |> unique()

  # For testing
  #v_p <- "other"

  msg <- ""
  if (!all(v_p %in% v_c)) {
    msg <- c(
      msg,
      "Some components in pathways.xlsx not in components.xlsx: ",
      paste0(v_p[!v_p %in% v_c], collapse = ", "),
      "\n"
    )
  }
  if (!all(v_c %in% v_p)) {
    msg <- c(
      msg,
      "Some components in components.xlsx not in pathways.xlsx: ",
      paste0(v_c[!v_c %in% v_p], collapse = ", "),
      "\n"
    )
  }
  if (!all(v_m %in% c(v_p, v_c))) {
    msg <- c(
      msg,
      "Some components in mitigations.xlsx not in pathways.xlsx or components.xlsx: ",
      paste0(v_m[!v_m %in% c(v_c, v_p)], collapse = ", "),
      "\n"
    )
  }

  if (length(msg) > 1) {
    stop(msg, call. = FALSE)
  }
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

mitigated_colours <- function() {
  c(
    edge = "#e3e3e3",
    background = "#e3e3e3",
    border = "#d5d5d5",
    text = "#646464" # "#d5d5d5"  # Older, lighter version
  )
}

#' Create PoE Shiny UI theme
#'
#' Creates a Bootstrap theme with custom styling for the Pathways of Effects
#' Shiny UI.
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
  a <- try(read_sectors(), silent = TRUE)
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
