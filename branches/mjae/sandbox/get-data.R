###setwd("D:/R_packages/osmar/branches/mjae/sandbox")
###  ist nur damit ich das nicht immer neu eintippen muss ;)

#source("source.R")
library("osmar")



box <- bbox(11.579341, 48.15102, 11.582852, 48.1530)
box

nd <- node(373395)


### Access per API:

api <- osmsource_api()
api

xml1 <- get_osm(box, source = api)
xml1

summary(xml1)


xml2 <- get_osm(nd, api)


kaufstr <- get_osm(way(3810479))
kaufstr

summary(kaufstr)


### Access per osmosis:

osmosis <- osmsource_osmosis(file = "muenchen.osm",
                             osmosis = "Z://Temp//osmar//osmosis-0.39//bin//osmosis.bat")
osmosis

xml2 <- get_osm(box, source = osmosis)



#### sp spielerei

bb <- center_bbox(174.76778, -36.85056, 700,700)
src<- osmsource_api()
ua<- get_osm(bb, source=src)

a<-as_sp(ua,"points")
uasplines<- as_sp(ua, "lines")
uaspoly <- as_sp(ua, "polygons")
rel_bus<- subset(ua, ids=find_down(ua,relation(
                                      find(ua, relation(tags(v=="bus"))))
                                      ))
buslines<- as_sp(rel_bus, "lines")

nd<- get_osm(node(18961430), source=src)
sp_nd <- as_sp(nd, "points")
sp_nd <- as_sp(nd, "lines")
sp_ndp <- as_sp(nd, "polygons")

wy<- get_osm(way(3810479), source=src)
wyfull<- get_osm(way(3810479), source=src, full=TRUE)
wyspl <- as_sp(wy, "lines")
wyspp <- as_sp(wy, "points")
wysppol <- as_sp(wy, "polygons")
wyfullspl <- as_sp(wyfull, "lines")
wyfullspp <- as_sp(wyfull, "points")
wyfullsppol <- as_sp(wyfull, "polygons")

rl<- get_osm(relation(30023), source=src)
rlfull <- get_osm(relation(30023), source=src, full=TRUE)
rlspp <- as_sp(rl, "points")
rlspl <- as_sp(rl, "lines")
rlsppol <- as_sp(rl, "polygons")
rlfullspp <- as_sp(rlfull, "points")
rlfullspl <- as_sp(rlfull, "lines")
rlfullsppol <- as_sp(rl, "polygons")

routeBus <- find(ua, relation(tags(v=="bus")))
bus_osmar <- list("vector", length(routeBus))
for(i in 1:length(routeBus))
  bus_osmar[[i]] <- get_osm(relation(routeBus[i]), source=src, full=TRUE)
bus_osmarsp <- list("vector", length(routeBus))
for(i in 1:length(routeBus))
  bus_osmarsp[[i]] <- as_sp(bus_osmar[[i]], "lines")

for(i in 1:length(routeBus)){
  plot(bus_osmarsp[[i]], add=TRUE, axes=TRUE, col=i)
  plot(uasplines, add=TRUE, col=2)
  Sys.sleep(1)
}

buil_ids <- find(ua, way(tags(k=="building")))
buil_ids <- find_down(ua, way(buil_ids))
buildings <- subset(ua, ids=buil_ids)
build_spol<- as_sp(buildings, "polygons")
build_spl <- as_sp(buildings, "lines")



