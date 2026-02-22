pathway <- prep_pathways(
  read_pathways(),
  read_components(),
  vc = "Terrestrial and Semi-Aquatic SAR",
  a = c("Shoreline / Bank stabilization", "Drilling, Pile Driving and Fracking")
)

#make_visnetwork(pathway)
# n <- pathway$nodes

# n$child_n <- NA
# for (i in seq_len(nrow(n))) {
#   n$child_n[i] <- length(get_children(n$id[i], pathway$edges))
# }
# n$child_n <- rev(as.numeric(as.factor(n$child_n)))

# n$to <- NA
# for (i in seq_len(nrow(n))) {
#   if (!is.na(n$leads_to[i])) {
#     to <- stringr::str_split_1(n$leads_to[i], ", ?")
#     to <- to[to %in% n$id]
#     n$to[i] <- list(to)
#   } else {
#     n$to[i] <- NA
#   }
# }

# n$from <- NA
# n$from_n <- NA
# for (i in seq_len(nrow(n))) {
#   id <- n$id[i]
#   from <- c()
#   for (t in seq_len(nrow(n))) {
#     parent <- id %in% n$to[[t]]
#     if (parent) from <- c(from, n$id[t])
#   }
#   n$from[i] <- list(from)
#   n$from_n[i] <- length(from)
# }
# n$level <- as.numeric(as.factor(n$from_n)) + 2

# n$level <- n$child_n
# n$level[n$type == "component"] <- 1
# n$level[n$type == "stressor"] <- 2
# lvl_max <- max(n$child_n[
#   !n$type %in% c("component", "stressor", "habitat", "quality", "population")
# ])
# n$level[n$type %in% c("habitat", "quality")] <- lvl_max + 1
# n$level[n$type == "population"] <- lvl_max + 2

# pathway$nodes <- n

# pathway$nodes$level <- NULL
# pathway$nodes$fixed <- FALSE
# pathway$nodes$fixed[1] <- TRUE

visNetwork::visNetwork(
  nodes = pathway[["nodes"]],
  edges = pathway[["edges"]]
) |>
  visNetwork::visNodes(font = list(size = 10)) |>
  #dynamic, , discrete, straightCross, horizontal,
  # continuous
  visNetwork::visEdges(
    arrows = "to",
    smooth = list(
      type = "horizontal",
      roundness = 0.05,
      forceDirection = "vertical"
    )
  ) |>
  #visNetwork::visHierarchicalLayout(sortMethod = "directed", levelSeparation = 100) |>
  visNetwork::visHierarchicalLayout(levelSeparation = 800) |>
  visNetwork::visOptions(
    highlightNearest = list(
      enabled = TRUE,
      degree = list(from = 1000, to = 1000),
      algorithm = "hierarchical",
      labelOnly = FALSE
    )
  ) |>
  visNetwork::visPhysics(enabled = FALSE)


# - --------------

# From Custom File
ref <- read_components()
pathway <- prep_pathways(
  read_pathways(),
  ref,
  vc = "Terrestrial and Semi-Aquatic SAR",
  a = "Shoreline / Bank stabilization"
)

make_visnetwork(pathway)
