#' Read pathway data
#'
#' Reads pathway Excel file containing nodes, labels, types, and levels for
#' each valued component. Used to create pathways diagrams for different
#' valued components and different stressors.
#'
#' @param file_name Character. Name of Excel file to read. Defaults to
#'   "pathways.xlsx".
#' @param dir Character. Directory containing the file. Defaults to `NULL`
#'   which uses package data or looks for the file in the current working
#'   directory.
#'
#' @returns Data frame with columns:
#' * `valued_component` - Valued component name
#' * `label` - Node label text
#' * `level` - Hierarchical level for network layout
#' * `type` - Node type (e.g., "pressures", "activities")
#' * `node_id` - Node identifier
#' * `leads_to` - Comma-separated list of child node IDs
#' * `sort` - Optional sort order for within a level
#'
#' @export
#' @examplesIf have_data()
#' read_pathways()

read_pathways <- function(file_name = "pathways.xlsx", dir = NULL) {
  read_sheets(file_name, dir)
}

#' Read activities data
#'
#' Reads activities Excel file containing activity names organized by sector.
#' Used to create Sector drop down to pre-select activities.
#'
#' @param file_name Character. Name of Excel file to read. Defaults to
#'   "sectors.xlsx".
#' @param dir Character. Directory containing the file. Defaults to `NULL`
#'   which uses package data or looks for the file in the current working
#'   directory.
#'
#' @returns Data frame with columns:
#' * `activities` - Activity name
#' * `sector` - Sector name
#'
#' @export
#' @examplesIf have_data()
#' read_sectors()

read_sectors <- function(file_name = "sectors.xlsx", dir = NULL) {
  read_sheets(file_name, dir)
}

#' Read components data
#'
#' Reads components Excel file containing the mapping among valued components,
#' activities, and stressors. Used to create the stressors checkbox selections
#' and to select which nodes are included in the diagram.
#'
#' @param file_name Character. Name of Excel file to read. Defaults to
#'   "components.xlsx".
#' @param dir Character. Directory containing the file. Defaults to `NULL`
#'   which uses package data or looks for the file in the current working
#'   directory.
#'
#' @returns Data frame with columns:
#' * `valued_component` - Valued component name
#' * `activities` - Activity name
#' * `stressors` - Stressor name
#'
#' @export
#' @examplesIf have_data()
#' read_components()

read_components <- function(file_name = "components.xlsx", dir = NULL) {
  read_sheets(file_name, dir)
}

#' Read mitigations data
#'
#' Reads mitigations Excel file containing mitigation descriptions and the
#' edges they apply to. Creates edge IDs and mitigation IDs. Used to create
#' mitigations checkboxes and to apply mitigations to the diagrams.
#'
#' @param file_name Character. Name of Excel file to read. Defaults to
#'   "mitigations.xlsx".
#' @param dir Character. Directory containing the file. Defaults to `NULL`
#'   which uses package data or looks for the file in the current working
#'   directory.
#'
#' @returns Data frame with columns:
#' * `valued_component` - Valued component name
#' * `start_node` - Starting node ID for mitigation edge
#' * `end_node` - Ending node ID for mitigation edge
#' * `short` - Short mitigation description
#' * `long` - Long mitigation description
#' * `edge_id` - Edge identifier (created from start and end nodes)
#' * `m_id` - Mitigation identifier (created from VC and short description)
#'
#' @export
#' @examplesIf have_data()
#' read_mitigations()

read_mitigations <- function(file_name = "mitigations.xlsx", dir = NULL) {
  m <- read_sheets(file_name, dir)
  m <- m[!(is.na(m$start_node) | is.na(m$end_node)), ]
  m$edge_id <- paste0(m$start_node, "-", m$end_node)
  m$m_id <- paste0(
    simple_name(m$valued_component),
    "_",
    simple_name(m$short)
  )
  m
}

#' Read translations data
#'
#' Reads translations Excel file containing English to French translations for
#' all UI elements, labels, and content in the app.
#'
#' @param file_name Character. Name of Excel file to read. Defaults to
#'   "translations.xlsx".
#' @param dir Character. Directory containing the file. Defaults to `NULL`
#'   which uses package data or looks for the file in the current working
#'   directory.
#'
#' @returns Data frame with columns:
#' * `english` - English text
#' * `french` - French translation
#' * `note` - Optional note about the translation source
#'
#' @export
#' @examplesIf have_data()
#' read_translations()

read_translations <- function(file_name = "translations.xlsx", dir = NULL) {
  read_sheets(file_name, dir)
}

#' Read and combine Excel sheets
#'
#' Internal function to read all sheets from an Excel file and combine them
#' into a single data frame. Excludes sheets named "notes".
#'
#' @param file_name Character. Name of Excel file to read.
#' @param dir Character. Directory containing the file. Defaults to `NULL`
#'   which uses package data or looks for the file in the current working
#'   directory.
#'
#' @returns Data frame. Combined data from all sheets.
#'
#' @noRd

read_sheets <- function(file_name, dir = NULL) {
  path <- data_location(file_name, dir)

  sheets <- readxl::excel_sheets(path)
  sheets <- sheets[tolower(sheets) != "notes"]
  s <- lapply(sheets, \(x) readxl::read_excel(path, sheet = x))
  do.call("rbind", s)
}

data_location <- function(file_name, dir = NULL) {
  if (is.null(dir)) {
    path <- system.file("extdata", file_name, package = "poe")
  } else {
    path <- file.path(dir, file_name)
  }

  if (!file.exists(path)) {
    dir <- "."
    cat(file_name, " looked for in ", normalizePath(dir))
    path <- file.path(dir, file_name)
    if (!file.exists(path)) {
      stop(
        "Cannot find ",
        file_name,
        " in ",
        normalizePath(dir),
        "\nEnsure that App Data files are in the working directory.",
        call. = FALSE
      )
    }
  }
  path
}


#' Prepare pruned pathways from spreadsheet
#'
#' Based on full pathway diagrams, prune according to selections of valued
#' component and activity to show only relevant pathways and stressors.
#'
#' @param pathways Data frame. Pathway diagrams from [read_pathways()].
#' @param ref Data frame. Component reference data from [read_components()]
#'   including valued components, activities, and stressors.
#' @param vc Character. Valued component to select pathways for.
#' @param a Character. Activity to select pathways for.
#' @param lang Character. Language for display, "en" (English) or "fr" (French).
#'   Defaults to "en".
#'
#' @returns Named list with:
#' * `nodes` - Data frame of nodes in the pruned pathway
#' * `edges` - Data frame of edges in the pruned pathway
#'
#' @export
#' @examplesIf have_data()
#' prep_pathways(
#'   read_pathways(),
#'   read_components(),
#'   vc = "Terrestrial and Semi-Aquatic SAR",
#'   a = "Shoreline / Bank stabilization"
#' )

prep_pathways <- function(pathways, ref, vc, a, lang = "en") {
  shiny::req(ref, vc, a, lang)

  p <- pathways[pathways$valued_component == vc, ]
  tree <- prep_visnetwork(p)

  stressors <- ref$stressors[
    ref$valued_component %in% vc & ref$activities %in% a
  ] |>
    unique()

  ids <- tree$nodes$id[tree$nodes$label %in% stressors]
  # start with top branch which is the "Valued Component"
  topNode <- tree[["nodes"]][["id"]][tree$nodes$label %in% vc]
  topBranch <- tree[["edges"]][
    tree[["edges"]][["from"]] %in%
      topNode &
      tree[["edges"]][["to"]] %in% ids,
  ]

  pruned <- prune_branches(ids = ids, tree = tree, pruned = topBranch)
  pruned$nodes$label <- pruned$nodes$label |>
    translate_text(lang) |>
    stringr::str_wrap(width = 15)

  pruned
}

#' Prepare pathway data for visNetwork
#'
#' Converts pathway data into the format required by visNetwork, including
#' nodes and edges with proper formatting, colors, and layout information.
#'
#' @param pathway Data frame. Pathway data for a single valued component,
#'   typically a subset of output from [read_pathways()].
#'
#' @returns Named list with:
#' * `nodes` - Data frame of formatted nodes for visNetwork
#' * `edges` - Data frame of edges connecting nodes
#'
#' @noRd
#' @examplesIf have_data()
#' p <- read_pathways()
#' p <- p[p$valued_component == "Marine Birds", ]
#' prep_visnetwork(p)

prep_visnetwork <- function(pathway) {
  nodes <- pathway
  nodes$color.background <- node_colours()[nodes$type]
  nodes$color <- NA
  names(nodes)[names(nodes) == "node_id"] <- "id"

  edges <- get_edges(pathway)

  # Node placement
  nodes[["level"]] <- (nodes[["level"]] - min(nodes[["level"]])) /
    diff(range(nodes[["level"]]))
  nodes[["level"]] <- -nodes[["level"]]
  nodes[["level"]] <- round(nodes[["level"]], 1)
  nodes[["level"]] <- as.numeric(as.factor(nodes[["level"]]))

  nodes[["title"]] <- nodes[["node_id"]]

  # Node formatting
  nodes[["shape"]] <- "box"
  nodes[["label"]] <- trimws(nodes[["label"]])
  nodes[["font.color"]] <- "#000000"

  nodes[["color.border"]] <- "#0b0b0b"
  nodes[["color.background"]][is.na(nodes[["color.background"]])] <- "#ffffff"

  # Edge formatting
  edges[["color"]] <- "#000000"
  edges[["label"]] <- " " # NOTE: this must be " " (not "", not NA, see CODE-DESIGN.md)
  edges[["title"]] <- edges[["id"]]

  # only keep nodes that have edges
  nodes <- nodes[nodes[["id"]] %in% unique(c(edges[["from"]], edges[["to"]])), ]

  # Tweak sorting
  if (!all(is.na(nodes[["sort"]]))) {
    sort <- nodes[["sort"]][!is.na(nodes[["x"]])]
    id <- nodes[["id"]][!is.na(nodes[["x"]])]
    sort <- paste0(sort, "_", id)
    change <- stats::setNames(sort, paste0("^", id, "$"))
    nodes[["id"]] <- stringr::str_replace_all(nodes[["id"]], change)
    edges[["from"]] <- stringr::str_replace_all(edges[["from"]], change)
    edges[["to"]] <- stringr::str_replace_all(edges[["to"]], change)
  }

  list(nodes = nodes, edges = edges)
}

#' Extract edges from pathways file
#'
#' @param pathways Data frame. Pathways including 'valued_component', 'node_id',
#'  and 'leads_to'. Converted to data frame of edges.
#'
#' @returns Data frame with columns:
#' * `from` - Starting node ID
#' * `to` - Ending node ID
#' * `id` - Edge identifier
#' * `valued_component` - Valued component name
#'
#' @noRd
#' @examplesIf have_data()
#' get_edges(read_pathways())

get_edges <- function(pathways) {
  edges <- pathways[, c("valued_component", "node_id", "leads_to")]
  edges <- split(pathways, pathways$valued_component)
  edges <- lapply(edges, \(e) {
    e$leads_to <- stringr::str_split(e$leads_to, ", ?")
    e <- data.frame(
      from = rep(e$node_id, lengths(e$leads_to)),
      to = unlist(e$leads_to, use.names = FALSE)
    )
    e <- e[!(is.na(e$from) | is.na(e$to)), ] # Omit final nodes
    e$id = paste0(e$from, "-", e$to)
    e
  })
  edges <- do.call("rbind", edges)

  # Add valued_component back in from rownames
  edges$valued_component = stringr::str_remove(row.names(edges), "\\.\\d+$")

  edges
}

#' Recursively prune pathway branches
#'
#' Recursive function to find and retain only the branches that lead to
#' specified node IDs, removing all other branches.
#'
#' @param ids Character vector. Node IDs to search for.
#' @param tree Named list. Full network/tree to search through with `nodes` and
#'   `edges` data frames.
#' @param pruned Data frame. Pruned edges to append to. Defaults to `NULL`.
#'
#' @returns Named list with:
#' * `nodes` - Data frame of filtered nodes
#' * `edges` - Data frame of filtered edges
#'
#' @noRd
#' @examplesIf have_data()
#' p <- read_pathways()
#' p <- p[p$valued_component == "Marine Birds", ]
#' tree <- prep_visnetwork(p)
#' ids <- tree$nodes$id[1:10]
#' topBranch <- tree$edges
#' topBranch <- topBranch[topBranch$from == tree$nodes$id[1] & topBranch$to %in% ids,]
#' pruned <- prune_branches(ids = ids, tree = tree, pruned = topBranch)

prune_branches <- function(ids, tree, pruned = NULL) {
  branch <- tree$edges[tree$edges$from %in% ids, ]
  if (nrow(branch) > 0) {
    # continue branch search
    prune_branches(
      ids = unique(branch$to),
      tree = tree,
      pruned = rbind(pruned, branch)
    )
  } else {
    # Ensure uniqueness of nodes/edges spec, otherwise will not work
    list(
      nodes = tree$nodes[
        tree$nodes$id %in% unlist(unique(pruned[, c("from", "to")])),
      ],
      edges = unique(pruned)
    )
  }
}

#' Create visnetwork interactive figure
#'
#' @param pathway List. Current set of diagram pathways for a single component.
#'
#' @returns visnetwork diagram
#'
#' @export
#' @examplesIf have_data()
#' pathway <- prep_pathways(
#'   read_pathways(),
#'   read_components(),
#'   vc = "Terrestrial and Semi-Aquatic SAR",
#'   a = "Shoreline / Bank stabilization"
#' )
#'
#' make_visnetwork(pathway)

make_visnetwork <- function(pathway) {
  shiny::req(pathway)

  visNetwork::visNetwork(
    nodes = pathway[["nodes"]],
    edges = pathway[["edges"]]
  ) |>
    visNetwork::visNodes(font = list(size = 10)) |>
    visNetwork::visEdges(arrows = "to") |>
    visNetwork::visHierarchicalLayout() |>
    visNetwork::visOptions(
      highlightNearest = list(
        enabled = TRUE,
        degree = list(from = 1000, to = 1000),
        algorithm = "hierarchical",
        labelOnly = FALSE
      )
    ) |>
    visNetwork::visPhysics(enabled = FALSE)
}


#' [ON HOLD] Create Diagrammr mermaid flowchart
#'
#' @param pathway List. Current set of diagram pathways for a single component.
#'
#' @returns DiagrammeR htmlwidget.
#'
#' @export

make_flowchart <- function(pathway) {
  shiny::req(pathway)
  # ON HOLD
  #pathway <- data.table::copy(pathway)
  flowchart <- convert_mermaid_flowchart(pathway)
  flowchart |>
    DiagrammeR::DiagrammeR()
}

#' [ON HOLD] Create orthogonal graph
#'
#' Creates an orthogonal graph layout using DiagrammeR with orthogonal edge
#' routing.
#'
#' @param pathway Named list. Pathway with `nodes` and `edges` data frames.
#'
#' @returns DiagrammeR graph object.
#'
#' @noRd

make_orthogonal <- function(pathway) {
  shiny::req(pathway)
  # ON HOLD
  #pathway <- data.table::copy(pathway)
  dot <- convert_to_dot(visNet = pathway)
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
    DiagrammeR::render_graph(layout = "tree", output = "graph")
}

#' Create pathway of effects legend
#'
#' Creates an HTML legend for the pathway diagram with color-coded squares and
#' labels. IDs are structured to enable translation.
#'
#' @param id Character. Base ID for legend elements.
#' @param colors Character vector. Legend square colors.
#' @param labels Character vector. Legend text labels.
#' @param size Numeric. Size of legend squares in pixels.
#'
#' @returns HTML div element containing legend.
#'
#' @noRd

make_poe_legend <- function(id, colors, labels, size) {
  # id is semi hard-coded so the text can be found and translated
  legendItems <- Map(
    function(id, color, label, size) {
      make_legend_item(size = size, color = color, text = label, id = id)
    },
    id = sprintf("%sText%d", id, seq_along(colors)),
    color = colors,
    label = labels,
    size = size
  )
  make_legend(legendItems)
}
#' Create legend container
#'
#' Creates an HTML div container for legend items.
#'
#' @param ... Legend items created by [make_legend_item()].
#'
#' @returns HTML div element.
#'
#' @noRd

make_legend <- function(...) {
  tags$div(
    # id = "tree-legend",
    style = "height: 50px; width: 100%;",
    class = "d-flex align-items-center justify-content-around",
    ...
  )
}
#' Create individual legend item
#'
#' Creates an HTML element for a single legend item with colored square and
#' text label.
#'
#' @param size Numeric. Size of square in pixels.
#' @param color Character. Color of square.
#' @param text Character. Text label.
#' @param ... Named arguments to pass to text div.
#'
#' @returns HTML div element.
#'
#' @noRd

make_legend_item <- function(size, color, text, ...) {
  tags$div(
    class = "d-flex align-items-center",
    tags$div(
      class = "me-2 rounded",
      style = sprintf(
        "height: %1$spx; width: %1$spx; background-color: %2$s",
        size,
        color
      )
    ),
    tags$div(text, class = "h6 m-0 p-0", ...)
  )
}
#' [ON HOLD] Convert visNetwork format to DOT language
#'
#' Converts visNetwork data object to DOT language format for use with
#' DiagrammeR.
#'
#' @param visNet Named list. visNetwork data object with `nodes` and `edges`
#'   data frames.
#'
#' @returns Named list with formatted `nodes` and `edges` for DOT.
#'
#' @noRd

convert_to_dot <- function(visNet) {
  # ON HOLD
  #dot <- visNet[["nodes"]]
  visNet[["nodes"]][["id"]] <- as.integer(visNet[["nodes"]][["id"]])
  visNet[["nodes"]][["color"]] <- visNet[["nodes"]][["color.border"]]
  visNet[["nodes"]][["fillcolor"]] <- visNet[["nodes"]][["color.background"]]
  visNet[["nodes"]][["fontcolor"]] <- "black"
  visNet[["nodes"]][["title"]] <- NULL
  visNet[["edges"]][["from"]] <- as.integer(visNet[["edges"]][["from"]])
  visNet[["edges"]][["to"]] <- as.integer(visNet[["edges"]][["to"]])
  visNet[["edges"]][["id"]] <- NULL
  visNet[["edges"]][["disabled"]] <- NULL
  visNet[["edges"]][["label"]] <- NULL

  visNet[["edges"]][["title"]] <- NULL
}

#' [ON HOLD] Convert visNetwork format to Mermaid.js specification
#'
#' Converts visNetwork data object to Mermaid.js graph specification string.
#'
#' @param visNet Named list. visNetwork data object with `nodes` and `edges`
#'   data frames.
#'
#' @returns Character. Mermaid.js graph specification.
#'
#' @noRd

convert_mermaid_flowchart <- function(visNet) {
  # ON HOLD
  #visNet$nodes <- as.data.table(visNet$nodes)
  #visNet$edges <- as.data.table(visNet$edges)
  # nodeSpec <- sprintf(
  #   "\tid%s(\"%s\")",
  #   visNet[["nodes"]][["id"]],
  #   gsub(pattern = "\\n", replacement = "<br>", x = visNet$nodes$label)
  # ) |>
  #   paste(collapse = "\n")
  # normal <- visNet$edges[color == "#000000"]
  # disabled <- visNet$edges[color != "#000000" & label == " "]
  # labeled <- visNet$edges[label != " "]
  # e1 <- sprintf("\tid%s --> id%s", normal$from, normal$to) |>
  #   stats::setNames(normal$id)
  # e2 <- sprintf("\tid%s --> id%s", disabled$from, disabled$to) |>
  #   stats::setNames(disabled$id)
  # e3 <- sprintf(
  #   "\tid%s -. \"%s\" .-> id%s",
  #   labeled$from,
  #   labeled$label,
  #   labeled$to
  # ) |>
  #   stats::setNames(labeled$id)
  # # Combine and put back in order
  # edgeSpec <- c(e1, e2, e3)[visNet$edges$id] |>
  #   paste(collapse = "\n")
  # linkStyle <- sprintf(
  #   "linkStyle %s stroke:#e3e3e3,fill:none;",
  #   match(c(names(e2), names(e3)), visNet$edges$id) - 1
  # ) |>
  #   paste(collapse = "\n")
  # normal <- visNet$nodes[visNet$nodes$font.color == "#000000"]
  # disabled <- visNet$nodes[visNet$nodes$font.color != "#000000"]
  # s1 <- sprintf(
  #   "style id%s fill:%s,stroke:%s",
  #   normal$id,
  #   normal$color.background,
  #   normal$color.border
  # ) |>
  #   stats::setNames(normal$id)
  # s2 <- sprintf(
  #   "class id%s mermaid-disabled",
  #   disabled$id
  # ) |>
  #   stats::setNames(disabled$id)
  # # Combine and put back in order
  # styleSpec <- c(s1, s2)[visNet$nodes$id] |>
  #   paste(collapse = "\n")
  # sprintf("graph TB\n%s\n%s\n%s\n%s", nodeSpec, edgeSpec, styleSpec, linkStyle)
}

#' Add zoom functionality to htmlwidget
#'
#' Adds SVG pan and zoom functionality to an htmlwidget using svg-pan-zoom.js.
#'
#' @param x htmlwidget. Widget to add zoom to.
#' @param id Character. ID of widget element.
#'
#' @returns Modified htmlwidget with zoom functionality.
#'
#' @noRd

add_zoom <- function(x, id) {
  htmlwidgets::onRender(
    x = x,
    jsCode = sprintf(
      "function() {
          svgPanZoom('#%s > svg')
      }",
      id
    )
  )
}

#' Translates text from english to french
#'
#' @param x Character. English text to translate
#' @param lang Character. Language for display, "en" (English) or "fr" (French).
#' @param dict Data frame. With columns "en" and "fr" for translations.
#'
#' @returns Character. Translated text (or original if lang is "en").
#'
#' @export
#'
#' @examples
#' translate_text("Increase in Edge Habitat", "fr")

translate_text <- function(
  x,
  lang = "en",
  dict = read_translations()
) {
  if (!lang %in% c("en", "fr")) {
    stop(
      "Language must be one of 'en' (English) or 'fr' (French)",
      call. = FALSE
    )
  }

  dict <- dict %||% getOption("poe.dict")

  if (is.null(dict)) {
    stop("No dictionary found", call. = FALSE)
  }

  if (lang == "en") {
    x
  } else if (lang == "fr") {
    french <- dict[["french"]][match(x, dict[["english"]])]
    if (!is.null(names(x))) {
      french <- stats::setNames(french, names(x))
    }
    french
  }
}

#' Add mitigations to pathways
#'
#' Applies selected mitigations to a pathway diagram by visually disabling edges
#' and nodes downstream of the mitigation points. Only nodes that are not
#' maintained by alternate pathways are disabled.
#'
#' @param pathway Named list. Pathway with `nodes` and `edges` data frames from
#'   [prep_pathways()].
#' @param mitigations Data frame. Mitigation metadata from [read_mitigations()].
#' @param m Character vector. Mitigation IDs to apply (refer to `m_id` column
#'   in `mitigations`).
#' @param lang Character. Language for display, "en" (English) or "fr" (French).
#'   Defaults to "en".
#'
#' @returns Named list with:
#' * `edges` - Data frame of edges with mitigations applied
#' * `nodes` - Data frame of nodes with downstream nodes disabled
#'
#' @export
#' @examplesIf have_data()
#' p <- prep_pathways(
#'   read_pathways(),
#'   read_components(),
#'   vc = "Terrestrial and Semi-Aquatic SAR",
#'   a = "Shoreline / Bank stabilization"
#' )
#' p1 <- add_mitigation(p, read_mitigations(), "Selective work")
#'
#' make_visnetwork(p)
#' make_visnetwork(p1)

add_mitigation <- function(pathway, mitigations, m, lang = "en") {
  shiny::req(pathway)

  e <- pathway[["edges"]]
  n <- pathway[["nodes"]]

  m <- mitigations[mitigations$m_id %in% m, ]

  m$label <- translate_text(m$short, lang = lang)
  label <- "label"

  # Nodes & Edges - Mitigation point
  m_start <- c()
  for (i in seq_len(nrow(m))) {
    if (is.na(m$edge_id[i])) {
      # By start/end nodes - Get ii to change edge label
      ii <- which(e$from == m$start_node[i] & e$to == m$end_node[i])
    } else {
      # By edge id
      ii <- which(e$id == m$edge_id[i])
    }
    # First node mitigated (i.e. 'to' node of mitigated edge)
    m_start <- c(m_start, e$to[ii])
    # Add mitigated edge labels
    e$label[ii] <- stringr::str_wrap(m[[label]][i], width = 10)
  }

  m_start <- unique(m_start) # In-case multiple mitigations point to same node

  # Nodes - Get those on remaining active pathways
  # (i.e. those not travelling through mitigations/disabled edges)
  na <- get_children(
    # Start with highest level node that is not directly mitigated
    node_starts = n$id[!n$id %in% m_start][1],
    # All non-mitigated pathways
    edges = e[e$label == " ", ]
  )

  # Nodes - Get those downstream of mitigation
  # - downstream of mitigations AND not maintained by other pathways
  nd <- get_children(m_start, e)

  # Nodes - To be disabled - Downstream of mitigation & without alternate paths
  n_disabled <- n$id %in% nd[!nd %in% na]

  # Edges to be disabled - the ones mitigated and all those leaving disabled nodes
  e_disabled <- e$label != " " | e$from %in% n$id[n_disabled]

  # Visually disable edges
  e$color[e_disabled] <- "#e3e3e3"

  # Visually disable nodes
  n$color.background[n_disabled] <- "#e3e3e3"
  n$color.border[n_disabled] <- "#d5d5d5"
  n$font.color[n_disabled] <- "#d5d5d5"

  list("edges" = e, "nodes" = n)
}

#' Get all child nodes downstream of starting nodes
#'
#' Traverses the network graph to find all nodes downstream of the specified
#' starting nodes.
#'
#' @param node_starts Character vector. Starting node IDs.
#' @param edges Data frame. Edge data with `from` and `to` columns.
#'
#' @returns Character vector. All node IDs downstream of starting nodes.
#'
#' @noRd

get_children <- function(node_starts, edges) {
  children <- c() # All nodes travelled through

  for (n in node_starts) {
    nodes <- n # Nodes to travel from
    while (length(nodes) > 0) {
      children <- unique(c(children, nodes)) # Update
      nodes <- edges$to[edges$from %in% nodes] # Follow path down
    }
  }
  children
}

#' Create named choices with translations
#'
#' Creates a named vector for use in Shiny input choices, translating names
#' based on language.
#'
#' @param value Character vector. Values for choices.
#' @param name Character vector. Names for choices. Defaults to `value`.
#' @param lang Character. Language code ("en" or "fr").
#' @param dict Data frame. Translation dictionary. Defaults to reading
#'   translations.xlsx.
#'
#' @returns Named character vector with translated names.
#'
#' @noRd

named_choices <- function(
  value,
  name = value,
  lang = "en",
  dict = read_translations()
) {
  dict <- dict %||% getOption("poe.dict")
  stats::setNames(
    value,
    translate_text(name, isolate(lang), dict)
  )
}
