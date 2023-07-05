# ---
# Mapa de sitios
# ---
# 

library(tmap)
library(sf)
library(rio)
library(tidyverse)

bsas <- st_read("data/partidos.gpkg")

regionales <- st_read("data/sitios_exp_regionales.csv") |> 
  st_set_crs(4326) |> 
  mutate(
    id = 1:n(),
    Regional_short = str_remove_all(Regional, "Regional ") |> 
      fct_recode(
        "Bahía Blanca" = "Bahia Blanca",
        "Cuenca del Salado" = "Cuenca del salado"
      ),
    ymod = ifelse(id %in% c(1, 4), -0.5, 0.5),
    just = ifelse(id == 12, "left", "right")
  )

mapa <- tm_shape(bsas) +
  tm_polygons(col = NULL,border.col = "gray") +
  tm_shape(regionales) +
  tm_dots(size = 0.25, col = "blue") +
  tm_text(text = "Regional_short", size = 1, ymod = "ymod", just = "right")

tmap_save(mapa, "mapa.png")

