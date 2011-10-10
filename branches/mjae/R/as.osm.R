as.osm <- function(xml,...){
  core<- xmlRoot(xml)
  osm_parsed<- osm_parse(core) 
  osm_data<- extract_data(osm_parsed)
#  osm_attr<- extract_attr(osm_parsed)
#  osm_ref<- extract_ref(osm_parsed)
#  
#  OSM(Node(osm_attr$nodeattr, osm_data$nodedata),
#      Way(osm_attr$wayattr, osm_data$waydata, osm_ref$wayref),
#      Relation(osm_attr$relationattr, osm_data$relationdata, osm_ref$relationref))
  osm_data 
}