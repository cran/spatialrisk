## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  fig.path = "figures/fixed-radius-concentration-",
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(spatialrisk)

portfolio <- Groningen
portfolio <- portfolio[, c("lon", "lat", "amount")]

head(portfolio)

## -----------------------------------------------------------------------------
local_points <- points_within_radius(
  portfolio,
  lon_center = 6.5549,
  lat_center = 53.1942,
  radius = 200
)

head(local_points)
nrow(local_points)
sum(local_points$amount)

## -----------------------------------------------------------------------------

targets <- portfolio[1:5, c("lon", "lat")]

target_sums <- radius_sum(
  targets = targets,
  reference = portfolio,
  value = "amount",
  radius = 200,
  progress = FALSE,
  result_col = "amount_200m"
)

target_sums

## -----------------------------------------------------------------------------

hotspot <- concentration_hotspot(
  portfolio,
  value = "amount",
  radius = 200,
  cell_size = 100,
  progress = FALSE, 
  top_n = 2
)

plot(hotspot)


## -----------------------------------------------------------------------------
head(hotspot$contributing_points[, c("id", "data_row", "lon", "lat", "amount", "amount_sum")])

## -----------------------------------------------------------------------------
hotspot_continuous <- concentration_hotspot(
  portfolio,
  value = "amount",
  radius = 200,
  cell_size = 100,
  progress = FALSE
)

hotspot_observed <- concentration_hotspot(
  portfolio,
  value = "amount",
  radius = 200,
  method = "observed",
  progress = FALSE
)

rbind(
  continuous = hotspot_continuous$hotspots,
  observed = hotspot_observed$hotspots
)


## ----hotspot-method-comparison, eval = requireNamespace("mapview", quietly = TRUE)----

plot(hotspot_continuous)
plot(hotspot_observed)


## ----message = FALSE----------------------------------------------------------
province_summary <- summarise_points_by_polygon(
  polygons = nl_provincie,
  points = insurance,
  value = "amount",
  fun = sum,
  outside = "ignore"
)

sf::st_drop_geometry(province_summary)[, c("areaname", "amount_sum")]

