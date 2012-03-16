#######################################################################################
####### Examples of the osmar-presentation at the Fossgis2012 in Dessau(Germany)#######
#######################################################################################

par(ask=TRUE)
readline("Press <Enter> to continue")

#### Download (or load) 4 km² (2*2 km) bounding box from dessau
## library(osmar)

#### downloading data
## src<- osmsource_api()
## bb<-center_bbox(12.232221,51.840577, 2000,2000)
## dessau<-get_osm(bb, source=src)
###

### loading data
data(fossgis2012)
dessau

#### structure of dessau with cut-out of examples
str(dessau,2,give.attr=FALSE)

dessau$nodes$attrs[1:6,]
dessau$ways$tags[1:4,]
dessau$ways$refs[1:4,]

#### summary of osmar-object
summary(dessau$nodes)
summary(dessau$ways)
summary(dessau$relations)
summary(dessau)$ways$keyval[1:6,] 

par(ask=TRUE)
readline("Press <Enter> to continue")


###### making of the benchmap   ############################

#### defining original bounding box and 
#### reducing the osmar object to this bounding box
bb<-center_bbox(12.232221,51.840577, 2000,2000)
red_id<-find(dessau, node(attrs(lon>=bb["left"] & lon<=bb["right"] & lat>=bb["bottom"] & lat<=bb["top"])))
red_ids<-find_up(dessau, node(red_id))
red_dessau<-subset(dessau, ids=red_ids)
red_dessau

par(ask=TRUE)
readline("Press <Enter> to continue")

## 1) find amenity=bench in nodes
## 2) subset of these nodes ->new osmar-object
## 3) find ways with maxspeed=30
## 4) find corresponding nodes
## 5) subset of these nodes/ways ->new osmar-object
bench_ids<- find(red_dessau, node(tags(k=="amenity" & v=="bench")))
bench_dessau<-subset(red_dessau, node_ids=bench_ids)
footway_ids<- find(red_dessau,way(tags(k=="maxspeed" & v=="30")))
footway_ids<- find_down(red_dessau, way(footway_ids))
footway_dessau<-subset(red_dessau,ids=footway_ids)

par(ask=TRUE)
readline("Press <Enter> to continue")

## stepwise plotting of benchmap
## green (ways with maxspeed=30), red (bench), blue (location of Fossgis2012)
plot_ways(red_dessau, col=gray(0.4))
title("benches in dessau")
plot_ways(footway_dessau, col="darkgreen", add=TRUE, lwd=2)
plot_nodes(bench_dessau, col=2, add=TRUE, pch=8)
points(12.228295,51.840113, col="blue", pch=8, cex=2)

par(ask=TRUE)
readline("Press <Enter> to continue")


##### data.frame expansion with osmar-package ##########################
##### bench example                           ##########################

## finding benches and saving in data.frame with lon and lat variables
bench_ids<- find(dessau, node(tags(k=="amenity" & v=="bench")))
bench_dessau<-subset(dessau, node_ids=bench_ids)
bench_coords<-data.frame(id=bench_ids, lon=bench_dessau$nodes$attrs$lon, lat=bench_dessau$nodes$attrs$lat)
bench_coords[1:10,]

## downloading 2500m² bounding box around every bench
##
## bench_list<-vector("list", nrow(bench_coords))
## for(i in 1:nrow(bench_coords)){
##  tmp_bb<-center_bbox(bench_coords[i,"lon"],bench_coords[i,"lat"], 50,50)
##  tmp_osm<-get_osm(tmp_bb, source=src)
##  bench_list[[i]] <- tmp_osm
## }

data(fossgis2012)
bench_list[1:5]

par(ask=TRUE)
readline("Press <Enter> to continue")

## building new data.frame with counts of street-types and buildings 
## for every data point

bench_data<-bench_coords
for(i in 1:nrow(bench_data)){
  bench_data$footway[i]<-summary(bench_list[[i]]$ways$tags$v)["footway"]
  bench_data$secondary[i]<-summary(bench_list[[i]]$ways$tags$v)["secondary"]
  bench_data$primary[i]<-summary(bench_list[[i]]$ways$tags$v)["primary"]
  bench_data$residential[i]<-summary(bench_list[[i]]$ways$tags$v)["residential"]
  bench_data$building[i]<-summary(bench_list[[i]]$ways$tags$k)["building"]
}
bench_data[is.na(bench_data)]<-0
head(bench_data)

