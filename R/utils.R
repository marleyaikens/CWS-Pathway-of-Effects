#' Check mitigations file for problems
#'
#' @param mitigations
#' @param pathways
#'
#' @returns
#'
#' @export
#' @examples
#' mitigations <- readxl::read_excel("data/mitigations.xlsx")
#'
#' check_mitigations(mitigations)
#'
#' m <- data.frame(start_node = c(1, 1), end_node = c(99, 99), edge = c(NA, 5))
#' check_mitigations(m)
#'

check_mitigations <- function(mitigations, pathways = "data/poe.json") {
  pathways <- jsonlite::read_json(pathways, simplifyVector = FALSE)

  edges <- lapply(pathways, \(vc) {
    data.frame(
      valued_component = vc[["name"]],
      id = sapply(vc[["edges"]], getElement, "id"),
      from = sapply(vc[["edges"]], getElement, "from"),
      to = sapply(vc[["edges"]], \(x) {
        to <- getElement(x, "to")
        if (is.null(to)) {
          to <- NA
        }
        to
      })
    )
  })

  edges <- do.call("rbind", edges)
  mitigations <- mitigations[, c(
    "start_node",
    "end_node",
    "edge",
    "short_en",
    "short_fr"
  )]

  # Check that edges and to/from nodes are in agreement if both are provided
  edge_agrees <- sapply(seq_len(nrow(mitigations)), \(m) {
    m <- mitigations[m, ]
    if (!is.na(m$start_node) & !is.na(m$end_node) & !is.na(m$edge)) {
      agrees <- identical(
        edges$id[edges$from == m$start_node & edges$to == m$end_node],
        as.character(m$edge)
      )
    } else {
      agrees <- TRUE
    }
    agrees
  })

  # Check that the edge to be mitigated does exist
  edge_exists <- sapply(seq_len(nrow(mitigations)), \(m) {
    m <- mitigations[m, ]
    if (!is.na(m$start_node) & !is.na(m$end_node)) {
      in_edges <- edges[edges$from == m$start_node & edges$to == m$end_node, ]
      in_edges <- nrow(in_edges) > 0
    } else if (!is.na(m$edge)) {
      in_edges <- m$edge %in% edges$id
    } else {
      in_edges <- FALSE
    }

    in_edges
  })

  msg <- c()
  if (any(!edge_exists)) {
    msg <- c(
      msg,
      "Some mitigations are for non-existant edges:\n",
      paste0(
        capture.output(as.data.frame(mitigations[!edge_exists, ])),
        collapse = "\n"
      ),
      "\n\n"
    )
  }

  if (any(!edge_agrees)) {
    msg <- c(
      msg,
      "Some mitigations have both to/from and edge ids which do not agree:\n",
      paste0(
        capture.output(as.data.frame(mitigations[!edge_exists, ])),
        collapse = "\n"
      )
    )
  }

  if (length(msg) > 0) {
    stop("Problems found!\n", msg)
  }
  invisible(list(TRUE, ""))
}


poe_theme <- function() {
  bs_theme(
    version = 5,
    #bootswatch = "materia",
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
      /* Reduce padding in sidebars */
      .sidebar-content {
        padding-top: 10px !important;
      } 
      .sidebar-title {
        margin-bottom: 0 !important;
      }
      /* Completely disable the side bar if no mitigations */
      /*aside#poe-sidebarMitigations.[hidden] + button {
        display: none !important;
      }*/
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
          padding: 0 !important;
        }
    "
    )
}
