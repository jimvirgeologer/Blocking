
library(rspat)

library(raster)

library(rgdal)

library(spatstat)

library(rspatial)


counties <- spat_data('counties')

yolo <- counties[counties$NAME == 'Yolo', ]
plot(counties, col='light gray', border='gray')
plot(yolo, add=TRUE, density=20, lwd=2, col='red')


rail <- spat_data("yolo-rail")
class(rail)
city <- spat_data("city")
crs(yolo, TRUE)
crs(rail, TRUE)
crs(city, TRUE)


TA <- "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=WGS84 +units=m"
countiesTA <- terra::project(counties, TA)
yoloTA <- terra::project(yolo, TA)
railTA <- terra::project(rail, TA)
cityTA <-terra:: project(city, TA)

davis <- centroids(cityTA)
i <- which(relate(davis, countiesTA, "intersects"))
countiesTA$NAME[i]



i <- intersect(cityTA, countiesTA)
i$area <- expanse(i)
as.data.frame(i)

plot(rail)
plot(city, add = TRUE)
plot(rail, add = TRUE)

plot(cityTA, col='blue')
plot(yoloTA, add=TRUE, border='red', lwd=3)
plot(railTA, add = TRUE)

davis_rail <- intersect(railTA, cityTA)


buf <- buffer(railTA, width=500)
rail_buf <- intersect(buf, cityTA)
plot(cityTA, col='light gray')
plot(rail_buf, add=TRUE, col='light blue', border='light blue')
plot(railTA, add=TRUE, lty=2, lwd=6)
plot(cityTA, add=TRUE)
plot(davis_rail, add=TRUE, col='red', lwd=6)
box()