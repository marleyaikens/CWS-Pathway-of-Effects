# Formatting the original data into Excel files which can be customized in future

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(writexl)


# Components > Activities > Stressors -----------------------------------------
# - Clarify which Valued Components are associated with which activities and
#   consequently stressors
# - Used to create dropdown menus
# - TODO: Add 'group' for drop down menu groupings?

a <- readRDS(system.file("extdata", "act2Pres.rds", package = "poe")) |>
  as_tibble() |>
  select("valued_component", "activities", "stressors") |>
  nest_by(valued_component, .keep = TRUE)

a <- setNames(a$data, a$valued_component)


write_xlsx(a, system.file("extdata", "components.xlsx", package = "poe"))

# Diagrams --------------------------------------------------------------------
# Parsed Visio Diagrams Data created by python script
p <- jsonlite::read_json(
  path = system.file("extdata", "poe.json", package = "poe"),
  simplifyVector = FALSE
)
n <- map(p, \(x) {
  map(x[["nodes"]], \(n) {
    n <- map(n, \(nn) if (is.null(nn)) NA else nn)
    as_tibble(n)
  }) |>
    list_rbind()
}) |>
  list_rbind(names_to = "valued_component") |>
  rename("node_id" = "id") |>
  mutate(
    label = str_squish(label),
    type = recode_values(
      fillcolor,
      to = names(node_colours()),
      from = node_colours()
    )
  ) |>
  filter(label != "") |>
  select(-c("width", "height", "color", "linewidth")) # I don't think these are used in interactive

# select(n, vc, label, fillcolor, type) |>
#   filter(is.na(type))

e <- map(p, \(x) {
  map(x[["edges"]], \(e) {
    e <- map(e, \(ee) if (is.null(ee)) NA else ee)
    as_tibble(e)
  }) |>
    list_rbind() |>
    rename("edge_id" = "id")
}) |>
  list_rbind(names_to = "valued_component")


nn <- n |>
  left_join(e, by = c("valued_component", "node_id" = "from")) |>
  summarize(
    leads_to = paste0(sort(to), collapse = ", "),
    .by = c(-"to", -"edge_id")
  ) |>
  select(
    "valued_component",
    "label",
    "x",
    "y",
    "type",
    "node_id",
    "leads_to"
  ) |>
  nest_by(valued_component, .keep = TRUE)

nn <- setNames(nn$data, nn$valued_component)


writexl::write_xlsx(
  nn,
  system.file("extdata", "pathways.xlsx", package = "poe")
)
