# identify routes based on shape id

library(gtfstools)
library(dplyr)

gtfs <- read_gtfs("data-raw/gtfs_20230519_mod.zip")


routes_id <- gtfs$trips %>% 
  distinct(route_id, shape_id)

# get names
routes <- routes_id %>%
  left_join(gtfs$routes %>% select(route_id, route_long_name)) %>%
  mutate(direction = ifelse(grepl("I$", shape_id), "I", "V")) %>%
  mutate(shape_id = stringr::str_remove(shape_id, "shape"))

readr::write_rds(routes, "data/routes_shapes.rds")
