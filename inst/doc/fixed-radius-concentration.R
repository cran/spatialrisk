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
hotspot <- concentration_hotspot(
  portfolio,
  value = "amount",
  radius = 200,
  cell_size = 100,
  progress = FALSE
)

hotspot

## ----hotspot-map, eval = requireNamespace("mapview", quietly = TRUE)----------
plot(hotspot)

## -----------------------------------------------------------------------------
hotspot$hotspots

head(hotspot$contributing_points[, c("id", "data_row", "lon", "lat",
                                     "amount", "amount_sum")])

## -----------------------------------------------------------------------------
head(hotspot$contributing_points)
nrow(hotspot$contributing_points)
sum(hotspot$contributing_points$amount)

## -----------------------------------------------------------------------------
known_centre_points <- points_within_radius(
  portfolio,
  lon_center = 6.5549,
  lat_center = 53.1942,
  radius = 200
)

head(known_centre_points)
nrow(known_centre_points)
sum(known_centre_points$amount)

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

## -----------------------------------------------------------------------------
hotspot_top2 <- concentration_hotspot(
  portfolio,
  value = "amount",
  radius = 200,
  cell_size = 100,
  progress = FALSE,
  n_hotspots = 2
)

hotspot_top2$hotspots

## ----hotspot-top2-map, eval = requireNamespace("mapview", quietly = TRUE)-----
plot(hotspot_top2)

## -----------------------------------------------------------------------------
model <- prepare_spatialrisk(portfolio, value = "amount", radius = 200,
                             cell_size = 100)
model <- select_candidates(model, progress = FALSE)
step_hotspot <- optimize_hotspot(model, n_hotspots = 2, progress = FALSE)

step_hotspot$hotspots

## -----------------------------------------------------------------------------
validation_portfolio <- portfolio[1:200, ]
validation_model <- prepare_spatialrisk(
  validation_portfolio,
  value = "amount",
  radius = 200,
  cell_size = 100
)

# Full geometric reference search
full <- optimize_hotspot(validation_model, progress = FALSE)

# Screened continuous search
screened <- validation_model |>
  select_candidates(progress = FALSE) |>
  optimize_hotspot(progress = FALSE)

full$hotspots
screened$hotspots

## ----workflow-plots, eval = requireNamespace("mapview", quietly = TRUE)-------
prepared <- prepare_spatialrisk(portfolio, value = "amount", radius = 200,
                                cell_size = 100)
plot(prepared)

selected <- select_candidates(prepared, progress = FALSE)
plot(selected)

## ----message = FALSE----------------------------------------------------------
province_summary <- summarise_points_by_polygon(
  polygons = nl_provincie,
  points = insurance,
  value = "amount",
  fun = sum,
  outside = "ignore"
)

sf::st_drop_geometry(province_summary)[, c("areaname", "amount_sum")]

