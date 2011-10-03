#' @include source.R
{}


get_osm <- function(x, source = osmsource_api(), ...) {
  get_osm_data(source, x, ...)
}



### Bounding box: ####################################################

bbox <- function(west_lon, south_lat, east_lon, north_lat) {
  structure(c(west_lon, south_lat, east_lon, north_lat), class = "bbox")
}


size <- function(x, ...) {
  UseMethod("size")
}

size.bbox <- function(x) {
  (x[1] - x[3]) * (x[2] - x[4])
}


center_bbox <- function(center_lon, center_lat, width, heigth) {
  stopifnot(center_lon <= 180 & center_lon >= -180)
  stopifnot(center_lat <= 90 & center_lat >= -90)

  width <- width / 2
  height <- height / 2

  a <- 6378137
  esq <- (2 - (1/298.257223563)) * (1/298.257223563)
  W <- sqrt(1 - esq * (sin(lat * pi/180))^2)
  M <- a * (1 - esq)/W^3
  mPerLatD <- 1/((pi/180) * M)
  top <- lat + mPerLatD * height
  bottom <- lat - mPerLatD * height
  N <- a/W
  mPerLonD <- 1/((pi/180) * N * cos(lat * pi/180))
  left <- lon - mPerLonD * width
  right <- lon + mPerLonD * width

  if (left < -180) 
    left <- left + 360
  if (right > 180) 
    right <- right - 360

  bbox(left, bottom, right, top)
}

