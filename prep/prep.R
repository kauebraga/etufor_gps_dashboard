library(dplyr)
library(data.table)


# full data -----------------------------------------------------------------------------------
segments_variables <- readRDS("data/gps_by_segment_variables.rds")
# extrair sentido
segments_variables <- segments_variables %>% mutate(direction = stringr::str_sub(shape_id, -1, -1)) 
# save
readr::write_rds(segments_variables, "data/gps_by_segment_variables.rds")


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
stops <- stops %>% mutate(direction = stringr::str_sub(shape_id, -1, -1)) 
stops_unique <- distinct(stops, geometry, .keep_all = TRUE)
stops_unique <- select(stops_unique, stop_id)
readr::write_rds(stops, "data/stops_gtfs_routes_sf.rds")
readr::write_rds(stops_unique, "data/stops_gtfs_sf.rds")


stringi::stri_trans_general(stops$stop_name, id = "ASCII//TRANSLIT")

ui <- gtfstools::read_gtfs("data-raw/gtfs_20230519_mod.zip")

ui$stops$stop_name

# segments ------------------------------------------------------------------------------------

segments <- readRDS("data/segments_gtfs_unique.rds") %>% st_transform(4326)



# graph #2: by interval -----------------------------------------------------------------------


data_interval_all <- setDT(segments_variables)[, .(velocidade = weighted.mean(velocidade, w = n)), by = .(interval, segment_id)]
data_interval_all[, velocidade := round(velocidade, 1)]
setorder(data_interval_all, interval)

# save
readr::write_rds(data_interval_all, "data/graphs/graphs_interval_segments.rds")
