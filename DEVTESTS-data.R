# Understanding the data

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(writexl)

act2Pres <- readRDS("data/act2Pres.rds") |>
  as_tibble()

# Parsed Visio Diagrams Data
p <- jsonlite::read_json(
  path = "data/poe.json",
  simplifyVector = FALSE
)

act2Pres

n <- map(p, \(x) {
  map(x[["nodes"]], \(n) {
    n <- map(n, \(nn) if (is.null(nn)) NA else nn)
    as_tibble(n)
  }) |>
    list_rbind()
}) |>
  list_rbind(names_to = "vc") |>
  rename("node_id" = "id") |>
  mutate(
    label = str_squish(label),
    type = recode_values(
      fillcolor,
      "#88bde9" ~ "component", # Valued Component
      "#fee599" ~ "stressor", # Stressors/Pressures
      "#f2f2f2" ~ "effect", # Effect
      "#f06c6c" ~ "contravention", # Potential Contravention of Legislation and/or Regulations
      "#9dbb61" ~ "habitat", # Habitat/Wetland Availability and Distribution
      "#f59d56" ~ "quality", # Fitness, Reproduction, and Mortality / Wetland Quality and Function
      "#7e649e" ~ "population"
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
  list_rbind(names_to = "vc")


nn <- n |>
  left_join(e, by = c("vc", "node_id" = "from")) |>
  summarize(to = paste0(sort(to), collapse = ", "), .by = c(-"to", -"edge_id"))

writexl::write_xlsx(nn, "data/pathways_test.xlsx")
