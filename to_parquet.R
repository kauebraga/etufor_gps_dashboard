library(arrow)
# Read .rds file into a data frame
df <- readRDS("data/gps_by_segment_variables.rds")
# Write to a Parquet file
write_parquet(df, "data/gps_by_segment_variables.parquet")
