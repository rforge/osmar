bbox2coords <-
function(center, area){
    ##  center  = Vektor Länge 2 in der Form c(lon, lat) c(x,y)
    ##  area    = breite und länge der bbox c(breite, höhe)
  stopifnot(center[1]<=180 & center[1]>=-180)
  stopifnot(center[2]<=90 & center[2]>=-90)
  stopifnot(length(c(center,area))==4)
  lon<-center[1]
  lat<-center[2]
  width<- area[1]/2
  height<-area[2]/2
  equrad<-6378137
  esq<-(2-0.003352811)*0.003352811
  W<-sqrt(1- esq*(sin(lat*pi/180))^2)
  
  M<-equrad*(1-esq)/W^3
  mPerLatD<-1/ ((pi/180)*M)
  top<-lat+ mPerLatD*height              ## abstand zwischen 2 breitengraden hat 111km
  bottom<-lat- mPerLatD*height

  N<-equrad/W
  mPerLonD<- 1/ ((pi/180) * N * cos(lat*pi/180))
  left <- lon- mPerLonD*width
  right<- lon+ mPerLonD*width
  if(left< -180) left<-left+360
  if(right>180) right<-right-360
  ret<- c(left, bottom, right, top)
  names(ret)<- c("minlat", "minlon", "maxlat", "maxlon") 
  ret  
}

