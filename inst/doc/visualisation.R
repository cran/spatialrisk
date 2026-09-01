## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  fig.path = "figures/visualisation-",
  collapse = TRUE,
  comment = "#>"
)

## ----message = FALSE, warning = FALSE-----------------------------------------
library(spatialrisk)
library(sf)

point_exposures <- insurance[, c("lon", "lat", "amount")]

head(point_exposures)

## -----------------------------------------------------------------------------
nl_gemeente[, c("id", "code", "areaname")]

## ----message = FALSE----------------------------------------------------------
municipality_exposure <- summarise_points_by_polygon(
  polygons = nl_gemeente,
  points = point_exposures,
  value = "amount",
  fun = sum,
  outside = "ignore"
)

sf::st_drop_geometry(municipality_exposure)[
  1:6,
  c("areaname", "amount_sum")
]

## ----eval = requireNamespace("tmap", quietly = TRUE)--------------------------
choropleth(
  municipality_exposure,
  value = "amount_sum",
  id = "areaname",
  legend_title = "Total insured amount"
)

