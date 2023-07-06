# library(tmap)
library(sf)
library(tidyverse)
bsas <- st_read("data/partidos.gpkg")
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)

regionales <- st_read("data/sitios_exp_regionales2.csv") |> 
  st_set_crs(4326) |> 
  mutate(lon = st_coordinates(geometry)[,1],
         lat = st_coordinates(geometry)[,2])
bsas %>% 
  ggplot() + 
  geom_sf(fill="white", alpha=.5, col="grey70") + 
  geom_point(data=regionales,  aes(x = lon, y = lat)) + 
  theme_void() + 
  ggrepel::geom_text_repel(
    data=regionales,
    aes(label = Localidad, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0, size=3) +  
  coord_sf(xlim = c(-65,-57))
ggsave(last_plot(), file="plots/mapa.png", dpi=300)

# id = 1:n(),
    # Regional_short = str_remove_all(Regional, "Regional ") |> 
    #   fct_recode(
    #     "Bahía Blanca" = "Bahia Blanca",
    #     "Cuenca del Salado" = "Cuenca del salado"
    #   ),
  #   ymod = ifelse(id %in% c(1, 4), -0.5, 0.5),
  #   just = ifelse(id == 12, "left", "right")
  # )
# mapa <- tm_shape(bsas) +
#   tm_polygons(col = NULL,border.col = "gray") +
#   tm_shape(regionales) +
#   tm_dots(size = 0.25, col = "blue") +
#   tm_text(text = "Localidad", size = 1, ymod = "ymod", just = "right")
# mapa
# tmap_save(mapa, "mapa.png")

