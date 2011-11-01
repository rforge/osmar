as.osmar <- function(xml,...){
  core<- xmlRoot(xml)
  osm_parsed<- osm_parse(core) 
  osm_data<- extract_data(osm_parsed)
  osm_attr<- extract_attr(osm_parsed)
  osm_ref<- extract_ref(osm_parsed)
  
  OSM(list(osmar_classes(list(osm_attr$nodeattr, osm_data$nodedata), "NODE"),
      osmar_classes(list(osm_attr$wayattr, osm_data$waydata, osm_ref$wayref), "WAY"),
      osmar_classes(list(osm_attr$relationattr, osm_data$relationdata, osm_ref$relationref), "RELATION")))
}

