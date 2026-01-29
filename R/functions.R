###########################################################################
#                                                                         #
# Toolkit                                                                 #
#                                                                         #
###########################################################################

#' Prepare pruned pathways
#'
#' Based on full pathway diagrams, prune according to selections
#'
#' @param pathways List. Pathway diagrams loaded from `data/poe.json`
#' @param ref Data.frame. References data from `data/act2Pres.rds` including
#'   Valued Components, Activities and Stressors.
#' @param vc Character. Valued Component to select pathways for
#' @param a Character. Activity to select pathways for
#' @param lang Character. Language ("english" or "french") in which to display
#' pathways.
#'
#' @returns
#'
#' @export
#' @examples
#' pathways <- jsonlite::read_json(
#'  path = "data/poe.json",
#'  simplifyVector = FALSE
#' )
#'
#' ref <- readRDS("data/act2Pres.rds") |>
#'   as.data.frame()
#'
#' prep_pathways(
#'   pathways,
#'   ref,
#'   vc = "Terrestrial and Semi-Aquatic SAR",
#'   a = "Shoreline / Bank stabilization"
#' )

prep_pathways <- function(pathways, ref, vc, a, lang = "english") {
  req(ref, vc, a, lang)

  tree <- pathways[[vc]] |>
    prep_visnetwork()

  stressors <- ref$stressors[
    ref$valued_component %in% vc & ref$activities %in% a
  ] |>
    unique()

  cleanLabels <- gsub(
    pattern = "\\n",
    replacement = " ",
    x = tree$nodes$label
  )
  ids <- tree$nodes[cleanLabels %in% stressors, id]
  # start with top branch which is the "Valued Component"
  topNode <- tree[["nodes"]][["id"]][cleanLabels %in% vc]
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
  nodes <- pathway |>
    getElement("nodes") |>
    data.table::rbindlist(fill = TRUE) |>
    suppressWarnings()
  data.table::setnames(
    nodes,
    old = c("fillcolor", "color"),
    new = c("color.background", "color.border")
  )
  nodes[["shape"]] <- "box"
  nodes[["x"]] <- (nodes[["x"]] - min(nodes[["x"]])) /
    diff(range(nodes[["x"]]))
  nodes[["y"]] <- (nodes[["y"]] - min(nodes[["y"]])) /
    diff(range(nodes[["y"]]))
  nodes[["y"]] <- -nodes[["y"]]
  nodes[["label"]] <- trimws(nodes[["label"]])
  nodes[["color.border"]][is.na(nodes[["color.border"]])] <- "#0b0b0b"
  nodes[["color.background"]][is.na(nodes[["color.background"]])] <- "#ffffff"
  nodes[["level"]] <- round(nodes[["y"]], 1)
  edges <- pathway |> getElement("edges") |> data.table::rbindlist(fill = TRUE)
  # only keep nodes that have edges
  nodes <- nodes[nodes[["id"]] %in% unique(c(edges[["from"]], edges[["to"]])), ]
  list(nodes = nodes, edges = edges)
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
      pruned = data.table::rbindlist(list(pruned, branch))
    )
  } else {
    # Ensure uniqueness of nodes/edges spec, otherwise will not work
    list(
      nodes = tree$nodes[tree$nodes$id %in% pruned[, unique(c(from, to))], ],
      edges = unique(pruned)
    )
  }
}

make_visnetwork <- function(pathway) {
  req(pathway)

  visNetwork::visNetwork(
    nodes = pathway[["nodes"]],
    edges = pathway[["edges"]]
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
}

make_flowchart <- function(id, pathway) {
  req(pathway)
  flowchart <- convert_mermaid_flowchart(data.table::copy(pathway))
  flowchart |>
    DiagrammeR::DiagrammeR() |>
    add_zoom(id = id)
}

make_orthogonal <- function(id, pathway) {
  req(pathway)

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
    DiagrammeR::render_graph(layout = "tree", output = "graph") |>
    add_zoom(id = id)
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
  visNet[["nodes"]][["id"]] <- as.integer(visNet[["nodes"]][["id"]])
  visNet[["nodes"]][["color"]] <- visNet[["nodes"]][["color.border"]]
  visNet[["nodes"]][["fillcolor"]] <- visNet[["nodes"]][["color.background"]]
  visNet[["nodes"]][["fontcolor"]] <- "black"
  visNet[["edges"]][["from"]] <- as.integer(visNet[["edges"]][["from"]])
  visNet[["edges"]][["to"]] <- as.integer(visNet[["edges"]][["to"]])
  visNet[["edges"]][["id"]] <- NULL
  visNet
}
# Converts visNetwork data format to mermaid js graph specification
#
# @param visNet - visNetwork data object
convert_mermaid_flowchart <- function(visNet) {
  nodeSpec <- sprintf(
    "\tid%s(\"%s\")",
    visNet[["nodes"]][["id"]],
    gsub(pattern = "\\n", replacement = "<br>", x = visNet$nodes$label)
  ) |>
    paste(collapse = "\n")
  edgeSpec <- sprintf("\tid%s --> id%s", visNet$edges$from, visNet$edges$to) |>
    paste(collapse = "\n")
  styleSpec <- sprintf(
    "style id%s fill:%s,stroke:%s",
    visNet$nodes$id,
    visNet$nodes$color.background,
    visNet$nodes$color.border
  ) |>
    paste(collapse = "\n")
  sprintf("graph TB\n%s\n%s\n%s", nodeSpec, edgeSpec, styleSpec)
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
#' @param lang Character. One of "en" or "fr"
#' @param translations Data.frame. with columns "en" and "fr" for translations
#'
#' @returns
#'
#' @examples
#' enFr <- read.csv("data/en-fr-table.csv")
#' translate_text("Increase in Edge Habitat", "fr", dict = enFr)

translate_text <- function(x, lang = "en", dict = NULL) {
  dict <- dict %||% getOption("poe.dict")

  if (lang == "en") {
    x
  } else if (lang == "fr") {
    french <- dict[["french"]][match(x, dict[["english"]])]
    if (!is.null(names(x))) {
      french <- stats::setNames(french, names(x))
    }
    french
  } else {
    stop("Only English and French are supported.")
  }
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
