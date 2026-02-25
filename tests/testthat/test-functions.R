# Test read functions --------------------------------------------------------

test_that("read_pathways() reads pathway data", {
  expect_silent(pathways <- read_pathways())

  expect_s3_class(pathways, "data.frame")
  expect_true(all(c("node_id", "label", "type", "level") %in% names(pathways)))
  expect_gt(nrow(pathways), 0)
})

test_that("read_components() reads component data", {
  expect_silent(components <- read_components())

  expect_s3_class(components, "data.frame")
  expect_gt(nrow(components), 0)
})

test_that("read_mitigations() reads mitigation data", {
  expect_silent(mitigations <- read_mitigations())

  expect_s3_class(mitigations, "data.frame")
  expect_true("edge_id" %in% names(mitigations))
  expect_true("m_id" %in% names(mitigations))
  expect_gt(nrow(mitigations), 0)
})

# Test prep functions --------------------------------------------------------

test_that("prep_pathways()", {
  pathways <- read_pathways()
  components <- read_components()

  pathway <- expect_silent(
    prep_pathways(
      pathways,
      components,
      vc = "Terrestrial and Semi-Aquatic SAR",
      a = "Shoreline / Bank stabilization"
    )
  )

  expect_type(pathway, "list")
  expect_named(pathway, c("nodes", "edges"), ignore.order = TRUE)
  expect_s3_class(pathway$nodes, "data.frame")
  expect_s3_class(pathway$edges, "data.frame")
  expect_gt(nrow(pathway$nodes), 0)
  expect_gt(nrow(pathway$edges), 0)
})

test_that("prep_visnetwork()", {
  pathways <- read_pathways()
  pathway <- pathways[
    pathways$valued_component == "Terrestrial and Semi-Aquatic SAR",
  ]

  expect_silent(result <- prep_visnetwork(pathway))

  expect_type(result, "list")
  expect_named(result, c("nodes", "edges"), ignore.order = TRUE)
  expect_s3_class(result$nodes, "data.frame")
  expect_s3_class(result$edges, "data.frame")
  expect_true("id" %in% names(result$nodes))
  expect_true(all(c("from", "to") %in% names(result$edges)))
})


test_that("prep_pathways() translated", {
  pathways <- read_pathways()
  components <- read_components()

  pathway <- expect_silent(
    prep_pathways(
      pathways,
      components,
      vc = "Terrestrial and Semi-Aquatic SAR",
      a = "Shoreline / Bank stabilization",
      lang = "fr"
    )
  )
})


# Test make functions --------------------------------------------------------

test_that("make_visnetwork() creates visNetwork object", {
  pathways <- read_pathways()
  components <- read_components()

  pathway <- prep_pathways(
    pathways,
    components,
    vc = "Terrestrial and Semi-Aquatic SAR",
    a = "Shoreline / Bank stabilization"
  )

  result <- make_visnetwork(pathway)

  expect_s3_class(result, "visNetwork")
  expect_s3_class(result, "htmlwidget")
})

test_that("make_flowchart() creates DiagrammeR object", {
  pathways <- read_pathways()
  components <- read_components()

  pathway <- prep_pathways(
    pathways,
    components,
    vc = "Terrestrial and Semi-Aquatic SAR",
    a = "Shoreline / Bank stabilization"
  )

  expect_silent(result <- make_flowchart(pathway))

  expect_s3_class(result, "DiagrammeR")
  expect_s3_class(result, "htmlwidget")
})

test_that("make_orthogonal() creates graph object", {
  pathways <- read_pathways()
  components <- read_components()

  pathway <- prep_pathways(
    pathways,
    components,
    vc = "Terrestrial and Semi-Aquatic SAR",
    a = "Shoreline / Bank stabilization"
  )

  expect_silent(result <- make_orthogonal(pathway))

  expect_s3_class(result, "grViz")
  expect_s3_class(result, "htmlwidget")
})

# Test helper functions ------------------------------------------------------

test_that("get_edges() extracts edges from pathways", {
  pathways <- read_pathways()

  expect_silent(edges <- get_edges(pathways))

  expect_s3_class(edges, "data.frame")
  expect_named(
    edges,
    c("from", "to", "id", "valued_component"),
    ignore.order = TRUE
  )
  expect_gt(nrow(edges), 0)
})

test_that("add_mitigation() adds mitigations to pathway", {
  pathways <- read_pathways()
  components <- read_components()
  mitigations <- read_mitigations()

  pathway <- prep_pathways(
    pathways,
    components,
    vc = "Terrestrial and Semi-Aquatic SAR",
    a = "Shoreline / Bank stabilization"
  )

  m_id <- mitigations$m_id[1]

  expect_silent(result <- add_mitigation(pathway, mitigations, m_id))

  expect_type(result, "list")
  expect_named(result, c("edges", "nodes"), ignore.order = TRUE)
  expect_s3_class(result$nodes, "data.frame")
  expect_s3_class(result$edges, "data.frame")

  expect_silent(make_visnetwork(result))
})

test_that("translate_text() translates to French", {
  dict <- data.frame(
    english = c("Hello", "World"),
    french = c("Bonjour", "Monde")
  )

  result <- translate_text("Hello", lang = "fr", dict = dict)

  expect_equal(result, "Bonjour")
})

test_that("translate_text() returns English unchanged", {
  dict <- data.frame(
    english = c("Hello", "World"),
    french = c("Bonjour", "Monde")
  )

  result <- translate_text("Hello", lang = "en", dict = dict)

  expect_equal(result, "Hello")
})
