library(readxl)
library(sf)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
setwd("~/current work/01_R_Projects/02_Blocking")

face_map <- read_excel("Face_Maps/515_SDNS_90S.xlsx")
face_map <- face_map %>% filter(!is.na(LOCATIONX),
                                !is.na(LOCATIONY))

face_map_plot<- st_as_sf(face_map , coords = c("LOCATIONX", "LOCATIONY"), crs = 3125)

face_map_2 <- read_excel("Face_Maps/515_SDN_80S.xlsx")
face_map_2 <- face_map_2 %>% filter(!is.na(LOCATIONX),
                                !is.na(LOCATIONY))
face_map_plot_2<- st_as_sf(face_map_2 , coords = c("LOCATIONX", "LOCATIONY"), crs = 3125)

############ INPUT SHAPEFILE ###############  



aray <- st_read(
  "Shapefiles/N_S_Positions.shp")

maria_tambok <- ggplot() + 
  geom_sf(data = aray, size = 1, color = "black", fill = "cyan1") + 
  ggtitle("maria tambok") + 
  coord_sf()


ggplotly(maria_tambok)

######## PLOTTING ##############
face_map_names <- st_centroid(face_map_plot) ############### creating centroids on the data
face_map_names <- cbind(face_map_names, st_coordinates(st_centroid(face_map_plot$geometry)))############### addng X and Y to points


position_lines <- st_centroid(aray) ############### creating centroids on the position lines
position_lines<- cbind(position_lines , st_coordinates(st_centroid(aray$geometry)))############### addng X and Y to pos lines


b<- ggplot() +
  geom_sf(data = aray) +
  geom_text(data = position_lines, aes(x = X, y = Y, label = POS_N_S))+
  geom_sf(data = face_map_plot_2) +
  geom_sf(data = face_map_plot, size = 4,shape = 23) +
  geom_text(data = face_map_names, aes(x = X, y = Y, label = HOLE.ID))
ggplotly(b)

ggplot(data = face_map_plot) +geom_sf()


b <- plot(aray[,'POS_N_S'])
plot((face_map_plot[,'LEVEL']),pch = 15, add = TRUE)
plotly

mark <- st_intersection(aray,face_map_plot_2)

