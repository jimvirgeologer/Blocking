library(readxl)
library(sf)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(visdat)
############ INPUT FACE MAPPING SHEETS ###############  
# setwd("~/current work/01_R_Projects/02_Blocking/Blocking/Face_Maps")

setwd("~/current work/01_R_Projects/02_Blocking/Blocking")

file.list_gis <- list.files(path = './Face_Maps', pattern = '.xlsx', recursive = TRUE, full.names = TRUE)
file.list_gis <- file.list_gis[!grepl("~", file.list_gis)]

face_map_gis<- function(i) {
  x = read_xlsx(i,sheet = 1)
  colnames(x) <-
    c(
      "c1",
      "c2",
      "c3",
      "c4",
      "c5",
      "c6",
      "c7",
      "c8",
      "c9",
      "c10",
      "c11"
    )
  x <- x %>% transmute(
    HOLE_ID = as.character(c1),
    LOCATIONX = as.numeric(c2),
    LOCATIONY = as.numeric(c3),
    LOCATIONZ = as.numeric(c4),
    LENGTH = as.numeric(c5),
    LEVEL = as.numeric(c6),
    AREA = as.character(c7),
    ROCKCODE = as.character(c8),
    SAMP_BY = as.character(c9),
    DATE_SAMP = as.Date(c10),
    TENEMENT = as.character(c11)) %>%
    filter(!is.na(HOLE_ID))
} 


df_gis <- lapply(file.list_gis, face_map_gis) %>%
  bind_rows %>%
  as.data.frame()
df <- df %>% mutate(file = "MINE GEO")

########### joining table ###########
df_joined <- full_join(df,df_gis,by = c("SHEET" = "HOLE_ID"))
vis_miss(df_joined)

df_joined <- df_joined %>% filter(!is.na(LOCATIONX))

face_map_plot<- st_as_sf(df_joined , coords = c("LOCATIONX", "LOCATIONY"), crs = 3125)
ggplot(data = face_map_plot) + geom_sf()
############ INPUT SHAPEFILE POSITION LINES ###############  

POS_LINES <- st_read(
  "./Shapefiles/N_S_Positions.shp")
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

POS_FACE_MAP_PLOT <- ggplot(data = POS_FACE_MAP, aes(color = file, text = SHEET)) +
  geom_sf()
  
 # geom_text(data = face_map_names, aes(x = X, y = Y, label = SHEET, colour = "blue"))

ggplotly(POS_FACE_MAP_PLOT,tooltip = "text")


############## Compositing per block ##############33

POS_FACE_MAP_AVERAGE <- POS_FACE_MAP %>%
  group_by(POS_N_S, AREA, LEVEL) %>% 
  summarize(LENGTH = mean(LENGTH)
            )









#################### Acquiring Coordinates / Joining Mine Geology and GIS Data  #############

df_joined <- full_join(df,df_gis,by = c("SHEET" = "HOLE_ID"))
vis_miss(df_joined)



