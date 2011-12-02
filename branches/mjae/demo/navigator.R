#' @demo Simple routing demo using the igraph-package.
#'
#' @details
#'   Simple routing using the igraph-package. In Munich, we want from
#'   the "Sendlinger Tor" to the north.
#'
#'   OSM planet dump is copied from
#'   http://www.dbs.ifi.lmu.de/cms/Project_PAROS.
#'
#'   Ensure that Osmosis is installed and in your PATH environment
#'   variable; see http://wiki.openstreetmap.org/wiki/Osmosis.

library("osmar")



### Download and extract data: #######################################

download.file("http://osmar.r-forge.r-project.org/muenchen.osm.gz",
              "muenchen.osm.gz")

system("gzip -d muenchen.osm.gz")



### Import subset based on bounding box: #############################

src <- osmsource_osmosis(file = "muenchen.osm",
                         osmosis = "osmosis")

muc_bbox <- center_bbox(11.575278, 48.137222, 3000, 3000)

muc <- get_osm(muc_bbox, src)
muc



### Reduce to highways: ##############################################

hways <- find(muc, way(tags(k == "highway")))
hways <- find_down(muc, way(hways))
hways_muc <- subset(muc, ids = hways)


## Plot complete data and highways on top:
plot(muc)
plot_ways(hways_muc, col = "red", add = TRUE)


## Plot street map only:
plot_nodes(muc, pch = 19, cex = 0.1, col = "lightgray")
plot_ways(hways_muc, add = TRUE)
plot_nodes(hways_muc, add = TRUE, pch = 19, cex = 0.6)



### Define route start and end nodes: ################################

hway_start <- local({
  id <- find(muc, node(tags(v == "Sendlinger Tor")))[1]
  find_nearest_node(muc, id, way(tags(k == "highway")))
})

hway_end <- local({
  id <- find(muc, node(attrs(lon > 11.59 & lat > 48.150)))[1]
  find_nearest_node(muc, id, way(tags(k == "highway")))
})



### Create street graph and compute shortest route: ##################

graph_muc <- as_igraph(hways_muc)

route <- get.shortest.paths(graph_muc,
                            from = as.character(hway_start),
                            to = as.character(hway_end))

route_nodes <- as.numeric(V(graph_muc)[route[[1]]]$name)
route_ids <- find_up(hways_muc, node(route_nodes))

route_muc <- subset(hways_muc, ids = route_ids)


## Add route to the plot:
plot_nodes(route_muc, add = TRUE, col = "red")
plot_ways(route_muc, add = TRUE, col = "blue", lwd = 2)

