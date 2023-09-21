library(dplyr)


data <- readRDS("data/gps_by_segment_variables.rds") %>% ungroup()

# list of routes
routes <- distinct(data, shape_id, .keep_all = TRUE) %>%
  select(route_id, shape_id)
readr::write_rds(routes, "data/routes_shapes_list.rds")

# list of intervals
intervals <- sort(unique(data$interval))
readr::write_rds(intervals, "data/intervals.rds")


# stops ---------------------------------------------------------------------------------------
stops <- readRDS("data/stops_gtfs_routes.rds")
stops <- sf::st_as_sf(stops, coords = c("stop_lon", "stop_lat"), crs = 4326)
readr::write_rds(stops, "data/stops_gtfs_routes_sf.rds")



# segments ------------------------------------------------------------------------------------

segments <- readRDS("data/segments_gtfs_unique.rds") %>% st_transform(4326)
