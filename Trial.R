la_sfg <- st_point(c(-118.2615805, 34.1168926))
amsterdam_sfg <- st_point(c(4.8979755, 52.3745403))

rbind(c(-118.2615805, 34.1168926), c(4.8979755, 52.3745403))
multipoint_sfg <- st_multipoint(rbind(c(-118.2615805, 34.1168926), c(4.8979755, 52.3745403)))
multipoint_sfg
linestring_sfg


linestring_sfg <- st_linestring(rbind(c(-118.2615805, 34.1168926), c(4.8979755, 52.3745403)))
par(mar = c(0, 1, 2, 1),
    mfrow = c(1, 2))
plot(multipoint_sfg, main = "MULTIPOINT")
plot(linestring_sfg, main = "LINESTRING")

st_sfc(multipoint_sfg)


st_distance(points_sfc)
data <- data.frame(name = c("Los Angeles", "Amsterdam"),
                   language = c("English", "Dutch"),
                   weather = c("sunny", "rainy/cold"))
