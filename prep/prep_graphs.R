library(dplyr)
library(data.table)

segments_variables <- setDT(readRDS("data/gps_by_segment_variables.rds"))
segments_variables[, month := "2023-03"]



# graph #1: by month --------------------------------------------------------------------------

data_month_all <- segments_variables[, .(velocidade = median(velocidade)), by = month]
data_month_all[, velocidade := round(velocidade, 1)]
setorder(data_month_all, month)

# to filter
data_month <- segments_variables[, .(velocidade = median(velocidade)), 
                                 by = .(month, route_id, interval)]
data_month[, velocidade := round(velocidade, 1)]

# save
readr::write_rds(data_month_all, "data/graphs/graphs_month_all.rds")
readr::write_rds(data_month, "data/graphs/graphs_month.rds")



# graph #2: by interval -----------------------------------------------------------------------


data_interval_all <- segments_variables[, .(velocidade = median(velocidade)), by = interval]
data_interval_all[, velocidade := round(velocidade, 1)]
setorder(data_interval_all, interval)

# to filter
data_interval <- segments_variables[, .(velocidade = median(velocidade)), 
                                 by = .(interval, route_id)]
data_interval[, velocidade := round(velocidade, 1)]

# save
readr::write_rds(data_interval_all, "data/graphs/graphs_interval_all.rds")
readr::write_rds(data_interval, "data/graphs/graphs_interval.rds")



# graph #3: by hourly volume ------------------------------------------------------------------

data_fluxo_all <- segments_variables[, .(velocidade = median(velocidade),
                                         fluxo_15min = sum(n)), by = .(segment_id, interval)]
data_fluxo_all[, fluxo_horario := fluxo_15min * 4]
data_fluxo_all[, fluxo_horario := cut(fluxo_horario, breaks = c(0, 10, 20, 30, 50, 75, 100, 125, 150, 175, 200, 
                                                                225, 250, 300,
                                                                Inf))]
data_fluxo_all_fim <- data_fluxo_all[, .(velocidade = median(velocidade)), by = .(fluxo_horario)]
setorder(data_fluxo_all_fim, fluxo_horario)

readr::write_rds(data_fluxo_all_fim, "data/graphs/graphs_fluxo_all.rds")


# to filter
data_fluxo <- segments_variables[, .(velocidade = median(velocidade)), by = .(segment_id, interval)]
data_fluxo_all[, fluxo_horario := fluxo_15min * 4]
data_fluxo_all[, fluxo_horario := cut(fluxo_horario, breaks = c(0, 10, 20, 30, 50, 75, 100, 125, 150, 175, 200, 
                                                                225, 250, 300,
                                                                Inf))]
data_fluxo_all <- data_fluxo_all[, .(velocidade = median(velocidade)), by = .(fluxo_horario)]