
library("XML")
library("RCurl")
library("gtools")

osmosis_source <- "../R"

osmosis_files <- c("get.R", "source.R", "source-api.R", "source-osmosis.R",
                   "as.osm.R", "extract_data.R","extract_attr.R","extract_ref.R", "osm_parse.R")
osmosis_files <- file.path(osmosis_source, osmosis_files)

sapply(osmosis_files, source)


