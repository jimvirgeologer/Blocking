library(readxl)
library(sf)
library(tidyverse)
library(dplyr)
library(ggplot2)

library(plotly)

############ INPUT FACE MAPPING SHEETS ###############  
setwd("~/current work/01_R_Projects/02_Blocking")

face_map <- read_excel("Face_Maps/515_SDNS_90S.xlsx")
face_map <- face_map %>% filter(!is.na(LOCATIONX),
                                !is.na(LOCATIONY))
face_map_2 <- read_excel("Face_Maps/515_SDN_80S.xlsx")
face_map_2 <- face_map_2 %>% filter(!is.na(LOCATIONX),
                                !is.na(LOCATIONY))

face_map <- rbind(face_map,face_map_2)
face_map_plot<- st_as_sf(face_map , coords = c("LOCATIONX", "LOCATIONY"), crs = 3125)
ggplot(data = face_map_plot) +geom_sf()
############ INPUT SHAPEFILE POSITION LINES ###############  

POS_LINES <- st_read(
  "Shapefiles/N_S_Positions.shp")
POS_LINES<- POS_LINES[,-c(1:2)]
POS_LINES_PLOT <- ggplot() + 
  geom_sf(data = POS_LINES, size = 0.1, color = "cyan") + 
  ggtitle("POS_LINES_PLOT") + 
  coord_sf()

ggplotly(POS_LINES_PLOT)

######## PLOTTING ##############
face_map_names <- st_centroid(face_map_plot) ############### creating centroids on the data
face_map_names <- cbind(face_map_names, st_coordinates(st_centroid(face_map_plot$geometry)))############### addng X and Y to points
# 
# 
# position_lines <- st_centroid(aray) ############### creating centroids on the position lines
# position_lines<- cbind(position_lines , st_coordinates(st_centroid(aray$geometry)))############### addng X and Y to pos lines
# 
# 
# b<- ggplot() +
#   geom_sf(data = aray) +
#   geom_text(data = position_lines, aes(x = X, y = Y, label = POS_N_S))+
#   geom_sf(data = face_map_plot_2) +
#   geom_sf(data = face_map_plot, size = 4,shape = 23) +
#   geom_text(data = face_map_names, aes(x = X, y = Y, label = HOLE.ID))
# ggplotly(b)
# 
# ggplot(data = face_map_plot) +geom_sf()
# 
# 
# b <- plot(aray[,'POS_N_S'])
# plot((face_map_plot[,'LEVEL']),pch = 15, add = TRUE)
# plotly

POS_FACE_MAP <- st_intersection(POS_LINES,face_map_plot)

POS_FACE_MAP_PLOT <- ggplot(data = POS_FACE_MAP, aes(color = AREA)) +
  geom_sf() +
 geom_text(data = face_map_names, aes(x = X, y = Y, label = HOLE.ID, colour = "blue"))
ggplotly(POS_FACE_MAP_PLOT)


############## Compositing per block ##############33

POS_FACE_MAP_AVERAGE <- POS_FACE_MAP %>%
  group_by(POS_N_S, AREA, LEVEL) %>% 
  summarize(LENGTH = mean(LENGTH)
            )



