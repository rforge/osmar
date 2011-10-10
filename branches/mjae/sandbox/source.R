
library("XML")
library("RCurl")

osmosis_source <- "../R"

osmosis_files <- c("get.R", "source.R", "source-api.R", "source-osmosis.R",
                   "as.osm.R", "extract_data.R", "osm_parse.R")
osmosis_files <- file.path(osmosis_source, osmosis_files)

sapply(osmosis_files, source)


