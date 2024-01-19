
# modelos disgitales de terreno (MDE 5m)
browseURL("https://www.ign.gob.ar/NuestrasActividades/Geodesia/ModeloDigitalElevaciones/Mapa")

# paletas de colores de {tidyterra}
browseURL("https://dieghernan.github.io/tidyterra/articles/palettes.html")

# paquetes ----------------------------------------------------------------

library(terra)
library(tidyterra)
library(sf)
library(glue)
library(showtext)
library(ggtext)
library(tidyverse)

# fuentes -----------------------------------------------------------------

c1 <- "#08306B"
c2 <- "#4292C6"
c3 <- "#F7FBFF"
c4 <- "#08519C"

# texto gral
font_add_google(name = "Ubuntu", family = "ubuntu")
# alturas
font_add_google(name = "Victor Mono", family = "victor", db_cache = FALSE)
# título
font_add_google(name = "Bevan", family = "bevan")

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
datos <- glue(
  "Datos: <span style='color:{c1};'>MDE-Ar</span>, ",
  "<span style='color:{c1};'>IGN</span>, ",
  "<span style='color:{c1};'>OSM</span>")
autor <- glue("<span style='color:{c1};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
icon_mastodon <- glue("<span style='font-family:fa-brands;'>&#xf4f6;</span>")
usuario <- glue("<span style='color:{c1};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{datos}<br>",
  "{autor} {sep} {icon_github} {icon_twitter} {icon_mastodon} {usuario}")

# datos -------------------------------------------------------------------

# archivos .img del MDE
# archivos_img <- list.files("mapa_dem/img/", pattern = "img", full.names = TRUE)

# leo como ráster
# lista_rast <- map(archivos_img, rast)

# genero una colección y combino los datos
# caba_col <- terra::sprc(lista_rast)
# caba <- terra::merge(caba_col)

# vector de las provincias de Argentina
pcias <- st_read("osm_extras/pcias_continental.gpkg") |> 
  st_transform(crs = 4326)

# Regiones de interés
bsas_sf <- filter(pcias, NAM == "Buenos Aires")
caba_sf <- filter(pcias, NAM == "Ciudad Autónoma de Buenos Aires")
amba_sf <- st_union(bsas_sf, caba_sf)

# convierto a vector {terra}
caba_vect <- vect(caba_sf)
bsas_vect <- vect(bsas_sf)
amba_vect <- vect(amba_sf)

# recorto MDE
# caba_crop <- terra::crop(caba, caba_vect, mask = TRUE)
# names(caba_crop) <- "elevacion"

bsas_crop <- st_crop(bsas_sf, caba_sf) |> vect()
bb <- st_bbox(caba_vect) |> st_as_sfc()
amba_crop <- st_crop(amba_sf, bb) |> vect()

# guardo MDE de CABA
# writeRaster(caba_crop, "mapa_dem/caba_crop.tif", overwrite = TRUE)
caba_crop <- rast("mapa_dem/caba_crop.tif")

# rutas/avenidas de OSM
# caminos <- st_read("osm/osm_gpkg/roads.gpkg")

# recorto a la extensión de la región de interés
# caminos_crop <- st_crop(caminos, bb)

# clases de camino principales
# caminos_clase <- c("primary", "secondary", "tertiary")

# caminos_crop_ppal <- caminos_crop |> 
#   filter(fclass %in% caminos_clase) |> 
#   st_geometry()

# guardo los caminos principales de toda la extensión de interés
# st_write(caminos_crop_ppal, "mapa_dem/caminos_crop_ppal.gpkg")
caminos_crop_ppal <- vect("mapa_dem/caminos_crop_ppal.gpkg")

# caminos de BSAS, por fuera de CABA
caminos_bsas <- terra::mask(caminos_crop_ppal, caba_vect, inverse = TRUE)

# figura ------------------------------------------------------------------

mi_title <- "Elevación de la Ciudad Autónoma de Buenos Aires"

# usar 'maxcell' incrementa la calidad de la imagen final
# el valor actual de 'maxcell' es el máximo posible, el número de celdas
# en el ráster, dado por la función size() de {terra}

# figura
g <- ggplot() +
  # agua
  geom_sf(data = bb, fill = c1, color = NA) +
  # AMBA
  geom_sf(data = amba_crop, fill = c4, color = NA) +
  # elevación
  geom_spatraster(data = caba_crop, alpha = 1, maxcell = terra::size(caba_crop)) +
  # caminos
  geom_sf(data = caminos_bsas, color = c3, linewidth = 1, alpha = .1) +
  scale_fill_hypso_c(
    palette = "wiki-schwarzwald-cont", na.value = NA, direction = 1,
    breaks = seq(-20, 120, 20), limits = c(-20, 120)) +
  coord_sf(expand = TRUE) +
  labs(fill = "Altura (m)", title = mi_title, caption = mi_caption) +
  guides(fill = guide_colorbar(ticks.colour = "black", ticks.linewidth = .2)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = c2, color = c1, linewidth = 3),
    plot.title.position = "plot",
    plot.title = element_text(
      size = 30, family = "bevan", color = c3, hjust = .5, 
      margin = margin(t = 10, b = -10)),
    plot.caption = element_markdown(
      color = c3, family = "ubuntu", size = 13, margin = margin(r = 5, b = 5),
      lineheight = unit(1.2, "line")),
    legend.justification = c(1, 1),
    legend.position = c(.95, .95),
    legend.key.height = unit(1.3, "cm"),
    legend.margin = margin(10, 10, 10, 10),
    legend.background = element_rect(fill = c2, color = NA),
    legend.title = element_text(
      color = c3, family = "ubuntu", size = 17, margin = margin(b = 5)),
    legend.text = element_text(
      color = c3, family = "victor", size = 12, hjust = 1, face = "bold")
  )

# guardo
ggsave(
  plot = g,
  filename = "mapa_dem/viz.png",
  width = 30,
  height = 35.37,
  units = "cm")

# abro
browseURL("mapa_dem/viz.png")
