library(sf)
barrios = st_read("barrios_cordoba.gpkg")
tabla = read.csv("clasificacion.csv", header = T, sep = ";")
library(dplyr)
tabla = rename(tabla, Nombre = nombre)
barrios = left_join(barrios, tabla, by = "Nombre")
barrios = st_transform(barrios, 4326)
barrios$categoria = ifelse(is.na(barrios$categoria)==T, "Sin clasificar", barrios$categoria)
barrios$categoria <- factor(barrios$categoria, levels = c("Alto", "Medio-alto","Medio", "Medio-bajo", "Bajo","Sin clasificar"))

library(osmdata)
bbox <- getbb("Municipio de Cordoba, Argentina", format_out = "sf_polygon")
vias_prim <- opq(bbox) %>% 
  add_osm_feature(key = "highway", value = "primary") %>% 
  osmdata_sf()
vias_prim = vias_prim$osm_lines

vias_sec <- opq(bbox) %>% 
  add_osm_feature(key = "highway", value = "secondary") %>% 
  osmdata_sf()
vias_sec = vias_sec$osm_lines

vias_autop <- opq(bbox) %>% 
  add_osm_feature(key = "highway", value = c("motorway","trunk")) %>% 
  osmdata_sf()
vias_autop = vias_autop$osm_lines

rio = opq(bbox) %>% 
  add_osm_feature(key = "waterway", value = "river") %>% 
  osmdata_sf()
rio = rio$osm_lines

calles = rbind(vias_prim[,c("geometry")], vias_sec[,c("geometry")])


library(tmap)
tmap_mode("plot")
pal = c("#2B83BA","#ABDDA4","#FFFFBF","#FDAE61","#E53E3E","#99A3A4")
mapa_pred <- tm_basemap(leaflet::providers$OpenStreetMap.Mapnik) +
  tm_shape(barrios) +
  tm_fill(col = c("categoria"), palette = pal) +
  tm_borders(col = "#85929E", lwd = 0.1, alpha = 0.2) +
  tm_layout(main.title= "Figura 1 - Clasificación de barrios según nivel socioeconómico.",
            main.title.position = c('left', 'center'), main.title.size = 1, main.title.fontfamily = "serif") +
  tm_legend(position = c("left", "bottom"),
            bg.color = "white",
            bg.alpha=1,
            width = .25, title.size = 1) +
  tm_shape(vias_autop) + tm_lines(col = "black",lwd = 3)+
  tm_shape(calles) + tm_lines(col = "black",lwd = 1)+
  tm_shape(rio) + tm_lines(col = "#3498DB",lwd = 2) +
  tm_compass(type = "4star", position=c("right", "top")) + tm_scale_bar()

tmap_save(mapa_pred, filename = "mapa_barrios.png")

