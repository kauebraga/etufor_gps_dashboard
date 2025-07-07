library(osmdata)
library(dplyr)
library(gtfstools)


# donwload street data
streets <- opq(bbox = 'Fortaleza, Brazil') %>%
  add_osm_feature(key = 'highway') %>%
  osmdata_sf ()

streets_polygons <- streets$osm_lines %>%
  select(osm_id, name)

# save
readr::write_rds(streets_polygons, "data-raw/fortaleza_streets.rds")



# abrir trechos -------------------------------------------------------------------------------

# extrair do gtfs
gtfs <- read_gtfs("../pasfor/data/gtfs_202503_mod.zip")

# extrair trechos
gtfs_trechos_possiveis <- kauetools::extract_scheduled_stops(gtfs)

trechos_possiveis <- gtfs_trechos_possiveis %>%
  group_by(shape_id) %>%
  summarise(segments = paste0(stop_id, collapse = "-")) %>%
  distinct(segments)

# save
readr::write_rds(trechos_possiveis, "data/trechos_possiveis.rds")

# ha intersecao entre os segmentos possiveis?
# se eu pegar um segment selecionado, por exemplo...
id <- 5552

a <- subset(trechos_possiveis, grepl(pattern = paste0(id, "-"), x = segments))
a <- strsplit(a$segments, id)  
# take the second element, which is the one after the segment you just selected
b <- lapply(a, function(x) sub(pattern = "^-", replacement = "", x = x[[2]]))
# compor segmentos
c <- lapply(b, function(x) strsplit(x, "-")) 
d <- lapply(c, as.data.table)
e <- lapply(d, function(x) x[, uh := paste0(V1, "-", shift(V1, -1))])
f <- rbindlist(e)
g <- unique(f$uh)

  

