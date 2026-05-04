## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  fig.path = "figures/visualisation-",
  collapse = TRUE,
  comment = "#>"
)

## ----message = FALSE, warning = FALSE-----------------------------------------
library(spatialrisk)
library(sf)
library(dplyr)

data(nl_gemeente)

## -----------------------------------------------------------------------------
set.seed(1)
municipality_values <- nl_gemeente |>
  st_drop_geometry() |>
  select(id, code, areaname) |>
  mutate(value = runif(n(), min = 0, max = 100))

## -----------------------------------------------------------------------------
map_data <- nl_gemeente |>
  left_join(municipality_values, by = c("id", "code", "areaname"))

## ----eval = requireNamespace("tmap", quietly = TRUE)--------------------------
choropleth(
  map_data,
  value = "value",
  id = "areaname",
  legend_title = "Value"
)

