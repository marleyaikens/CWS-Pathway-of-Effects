read_pathways <- function(file_name = "pathways.xlsx", dir = NULL) {
  read_sheets(file_name, dir)
}

read_components <- function(file_name = "components.xlsx", dir = NULL) {
  read_sheets(file_name, dir)
}

read_mitigations <- function(file_name = "mitigations.xlsx", dir = NULL) {
  m <- read_sheets(file_name, dir)
  m <- m[!(is.na(m$start_node) | is.na(m$end_node)), ]
  m$edge_id <- paste0(m$start_node, "-", m$end_node)
  m$m_id <- paste0(
    simple_name(m$valued_component),
    "_",
    simple_name(m$short_en)
  )
  m
}

read_sheets <- function(file_name, dir = NULL) {
  if (is.null(dir)) {
    path <- system.file("extdata", file_name, package = "poe")
  } else {
    path <- file.path(dir, file_name)
  }
  sheets <- readxl::excel_sheets(path)
  sheets <- sheets[tolower(sheets) != "notes"]
  s <- lapply(sheets, \(x) readxl::read_excel(path, sheet = x))
  do.call("rbind", s)
}


#' Prepare pruned pathways from spreadsheet
#'
#' Based on full pathway diagrams, prune according to selections
#'
#' @param pathways Data frame. Pathway diagrams loaded from `extdata/pathways`
#' @param ref Data.frame. References data from `data/act2Pres.rds` including
#'   Valued Components, Activities and Stressors.
#' @param vc Character. Valued Component to select pathways for
#' @param a Character. Activity to select pathways for
#' @param lang Character. Language for display, "en" (English) or "fr" (French).
#'
#' @returns
#'
#' @export
#' @examples
#' ref <- readRDS("inst/extdata/act2Pres.rds") |>  as.data.frame()
#'
#' prep_pathways(
#'   read_pathways(),
#'   ref,
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

# Converts JSON output from Visio into R visual compatible nodes and
# edges dataframes
#
# @param pathway - JSON of single valued component pathway
prep_visnetwork <- function(pathway) {
  nodes <- pathway
  nodes$color.background <- node_colours()[nodes$type]
  nodes$color <- NA
  names(nodes)[names(nodes) == "node_id"] <- "id"

  edges <- get_edges(pathway)

  # Node placement
  nodes[["x"]] <- (nodes[["x"]] - min(nodes[["x"]])) /
    diff(range(nodes[["x"]]))
  nodes[["y"]] <- (nodes[["y"]] - min(nodes[["y"]])) /
    diff(range(nodes[["y"]]))
  nodes[["y"]] <- -nodes[["y"]]
  #nodes[["level"]] <- ceiling(nodes[["y"]] * 10) / 10
  nodes[["level"]] <- round(nodes[["y"]], 1)
  nodes[["level"]] <- as.numeric(as.factor(nodes[["level"]]))
  nodes[["y"]] <- NULL
  nodes[["x"]] <- NULL
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

  list(nodes = nodes, edges = edges)
}

#' Extract edges from pathways file
#'
#' @param pathways Data frame. Pathways including 'valued_component', 'node_id',
#'  and 'leads_to'. Converted to data frame of edges.
#'
#' @returns
#'
#' @noRd
#' @examples
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

# Recursive function:
# find parent branches
# if there are child branches, append edges and call same function
# else stop and return compiled results filtering nodes as well
#
# @param ids - node IDs to search for
# @param tree - full network/tree to search through
# @param pruned - pruned branches to append to
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
#' @examples
#' ref <- read_components()
#' pathway <- prep_pathways(
#'   read_pathways(),
#'   ref,
#'   vc = "Terrestrial and Semi-Aquatic SAR",
#'   a = "Shoreline / Bank stabilization"
#' )
#'
#' make_visnetwork(pathway)

make_visnetwork <- function(pathway, width = NULL, height = NULL) {
  shiny::req(pathway)

  visNetwork::visNetwork(
    nodes = pathway[["nodes"]],
    edges = pathway[["edges"]]
  ) |>
    visNetwork::visNodes(font = list(size = 10)) |>
    visNetwork::visEdges(
      arrows = "to" #,
      # smooth = list(
      #   type = "horizontal",
      #   roundness = 0.05,
      #   forceDirection = "vertical"
      # )
    ) |>
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
  # visNetwork::visPhysics(
  #   hierarchicalRepulsion = list(
  #     damping = 1,
  #     springConstant = 0,
  #     springLength = 0
  #   )
  # )
}


#' Create Diagrammr mermaid flowchart
#'
#' @param pathway List. Current set of diagram pathways for a single component.
#'
#' @returns
#'
#' @export
#' @examples
#' pathway <- prep_pathways(
#'   read_pathways(),
#'   read_components(),
#'   vc = "Terrestrial and Semi-Aquatic SAR",
#'   a = "Shoreline / Bank stabilization"
#' )
#'
#' make_flowchart(pathway)

make_flowchart <- function(pathway) {
  shiny::req(pathway)

  flowchart <- convert_mermaid_flowchart(data.table::copy(pathway))
  flowchart |>
    DiagrammeR::DiagrammeR()
}

make_orthogonal <- function(pathway) {
  shiny::req(pathway)

  dot <- convert_to_dot(visNet = data.table::copy(pathway))
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

# Wrapper for making the POE legend with ids for translations
#
# @param id - id for legend
# @param colors - legend square colors
# @param labels - legend text
# @param size - size of squares
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
# Wrapper for making the POE legend with ids for translations
#
# @param ... - list of items from make_legend_item
make_legend <- function(...) {
  tags$div(
    # id = "tree-legend",
    style = "height: 50px; width: 100%;",
    class = "d-flex align-items-center justify-content-around",
    ...
  )
}
# Creates a legend item
# @param color - color of square
# @param text - text label
# @param ... - named arguments to pass to text div
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
# Converts visNetwork data format to DOT language
#
# @param visNet - visNetwork data object
convert_to_dot <- function(visNet) {
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
# Converts visNetwork data format to mermaid js graph specification
#
# @param visNet - visNetwork data object
convert_mermaid_flowchart <- function(visNet) {
  visNet$nodes <- as.data.table(visNet$nodes)
  visNet$edges <- as.data.table(visNet$edges)
  nodeSpec <- sprintf(
    "\tid%s(\"%s\")",
    visNet[["nodes"]][["id"]],
    gsub(pattern = "\\n", replacement = "<br>", x = visNet$nodes$label)
  ) |>
    paste(collapse = "\n")

  normal <- visNet$edges[color == "#000000"]
  disabled <- visNet$edges[color != "#000000" & label == " "]
  labeled <- visNet$edges[label != " "]
  e1 <- sprintf("\tid%s --> id%s", normal$from, normal$to) |>
    stats::setNames(normal$id)
  e2 <- sprintf("\tid%s --> id%s", disabled$from, disabled$to) |>
    stats::setNames(disabled$id)
  e3 <- sprintf(
    "\tid%s -. \"%s\" .-> id%s",
    labeled$from,
    labeled$label,
    labeled$to
  ) |>
    stats::setNames(labeled$id)

  # Combine and put back in order
  edgeSpec <- c(e1, e2, e3)[visNet$edges$id] |>
    paste(collapse = "\n")

  linkStyle <- sprintf(
    "linkStyle %s stroke:#e3e3e3,fill:none;",
    match(c(names(e2), names(e3)), visNet$edges$id) - 1
  ) |>
    paste(collapse = "\n")

  normal <- visNet$nodes[visNet$nodes$font.color == "#000000"]
  disabled <- visNet$nodes[visNet$nodes$font.color != "#000000"]

  s1 <- sprintf(
    "style id%s fill:%s,stroke:%s",
    normal$id,
    normal$color.background,
    normal$color.border
  ) |>
    stats::setNames(normal$id)

  s2 <- sprintf(
    "class id%s mermaid-disabled",
    disabled$id
  ) |>
    stats::setNames(disabled$id)

  # Combine and put back in order
  styleSpec <- c(s1, s2)[visNet$nodes$id] |>
    paste(collapse = "\n")

  sprintf("graph TB\n%s\n%s\n%s\n%s", nodeSpec, edgeSpec, styleSpec, linkStyle)
}
# Adds zoom functionality to an htmlwidget
#
# @param x - htmlwidget
# @param id - id of widget
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
#' @param translations Data.frame. with columns "en" and "fr" for translations
#'
#' @returns
#'
#' @examples
#' enFr <- read.csv("data/en-fr-table.csv")
#' translate_text("Increase in Edge Habitat", "fr", dict = enFr)

translate_text <- function(x, lang = "en", dict = NULL) {
  if (!lang %in% c("en", "fr")) {
    stop(
      "Language must be one of 'en' (English) or 'fr' (French)",
      call. = FALSE
    )
  }

  dict <- dict %||% getOption("poe.dict")

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
#' Disables nodes and edges downstream of a mitigation (disabled edge) which are
#' not maintained by alternate pathways.
#'
#' @param pathway List. Current set of diagram pathways for a single component.
#' @param mitigations Data frame. Master list of mitigation metadata.
#' @param m Character vector. Currently selected mitigations (refer to
#' `short_en` in `mitigations`) to add to the pathway.
#' @param lang Character. Language for display, "en" (English) or "fr" (French).
#'
#' @returns
#'
#' @export
#' @examples
#' p <- prep_pathways(
#'   read_pathways(),
#'   read_components(),
#'   vc = "Terrestrial and Semi-Aquatic SAR",
#'   a = "Shoreline / Bank stabilization"
#' )
#'
#' make_visnetwork(p)
#'
#' p1 <- add_mitigation(p, read_mitigations(), "Selective work")
#' make_visnetwork(p1)
#' make_flowchart(p1)

add_mitigation <- function(pathway, mitigations, m, lang = "en") {
  shiny::req(pathway)

  e <- pathway[["edges"]]
  n <- pathway[["nodes"]]

  m <- mitigations[mitigations$m_id %in% m, ]

  label <- ifelse(lang == "en", "short_en", "short_fr")

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

named_choices <- function(
  value,
  name = value,
  lang = "en",
  dict = NULL
) {
  dict <- dict %||% getOption("poe.dict")
  stats::setNames(
    value,
    translate_text(name, isolate(lang), dict)
  )
}
