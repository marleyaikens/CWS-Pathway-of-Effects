###########################################################################
#                                                                         #
# Toolkit                                                                 #
#                                                                         #
###########################################################################
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
