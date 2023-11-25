
# tutorial
browseURL("https://r-graph-gallery.com/web-valued-dots-map-bertin.html")

# paquetes ----------------------------------------------------------------

library(tidyverse)
library(terra)
library(glue)
library(sf)
library(ggtext)
library(showtext)

# fuente ------------------------------------------------------------------
c1 <- "white"
c2 <- "black"
c3 <- "grey70"
c4 <- "grey95"

# texto gral
font_add_google(name = "Ubuntu", family = "ubuntu")
# letras
font_add_google(name = "IBM Plex Serif", family = "ibm", db_cache = FALSE)

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
autor <- glue("Autor: <span style='color:{c2};'>**Víctor Gauto**</span>")
datos <- glue("Global Human Settlement Layer")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
icon_mastodon <- glue("<span style='font-family:fa-brands;'>&#xf4f6;</span>")
usuario <- glue("<span style='color:{c2};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{autor} {sep} {datos} {sep} {icon_github} {icon_twitter} {icon_mastodon} ",
  "{usuario}")

# datos -------------------------------------------------------------------

# GHSL - Global Human Settlement Layer
browseURL("https://ghsl.jrc.ec.europa.eu/download.php?ds=pop")

# leo los ráster con los datos poblacionales
# son 13 para abarcar Argentina
archivos_raster <- list.files(
  path = "mapa_puntos_pob/", pattern = "tif", full.names = TRUE)

# cargo los ráster
lista_raster <- map(archivos_raster, rast)

# provincias de Argentina
# EPSG:5346
pcias_posgar <- st_read("osm_extras/pcias_continental.gpkg")

# unifico los polígonos
arg_posgar <- st_union(pcias_posgar)

# convierto al CRS de los ráster
arg <- arg_posgar |> 
  st_transform(crs = st_crs(lista_raster[[1]])) |> 
  st_as_sf()

# función que recorta los ráster a la región Argentina
f_union <- function(x) {
  h <- terra::crop(x, arg) |> 
    terra::mask(arg)
  
  print(names(x))
  
  return(h)
}

# lista que contiene todos los ráster recortados
# lleva TIEMPO!!!
# lista_recorte <- map(lista_raster, f_union)

# creo una colección a partir de los recortes
# coleccion <- sprc(lista_recorte)

# unifico
# lleva TIEMPO!!!
# arg_pob <- terra::merge(coleccion)

# guardo el ráster
# writeRaster(arg_pob, "mapa_puntos_pob/arg_pob.tif")
arg_pob <- rast("mapa_puntos_pob/arg_pob.tif")

# factor de reducción de los datos de población
# que la figura final tenga 200 filas
factor <- round(nrow(arg_pob)/200)

# agrego los puntos quedando 200 filas
pob_agg <- terra::aggregate(
  arg_pob, fact = factor, fun = "sum", na_rm = TRUE)

# remplazo NA
pob_agg[is.na(pob_agg)] <- 0

# re proyecto a POSGAR
pob_agg2 <- terra::project(pob_agg, "EPSG:5346")

# creo un tibble con las coordenas y la población
pob_tib <- as.data.frame(
  pob_agg2,
  xy = TRUE,
  na.rm = TRUE) |> as_tibble()

# convierto a sf y re proyecto a POSGAR
sf_pob <- pob_tib |> 
  st_as_sf(
    coords = c("x","y"), 
    crs= st_crs(arg_posgar)) |> 
  rename(pob = 1)

# recorto a la extensión de Argentina
sf_pob_crop <- st_crop(sf_pob, arg_posgar)
# st_write(sf_pob_crop, "mapa_puntos_pob/sf_pob_crop_100.gpkg", append = FALSE)

# aplico la máscara de Argentina a los puntos en QGIS (tarda mucho tiempo en R)
# ?????????????????????????
# sf_pob_mask <- st_intersection(sf_pob_crop, arg_posgar)
sf_pob_mask <- st_read("mapa_puntos_pob/sf_pob_mask_200.gpkg")

# tamaño de píxel y área
resol <- terra::res(pob_agg2)[1]
area <- round((resol/1000)^2, 0) # km2

# calculo la densidad de población y divido en rangos
sf_pob_mask_dens <- sf_pob_mask |> 
  mutate(hab = pob/area) |> 
  mutate(rango = case_when(
    hab >= 1000 ~ "Mayor a 1.000",
    hab >= 100 & hab < 1000 ~ "100 - 1.000",
    hab >= 10 & hab < 100 ~ "10 - 100",
    hab >= 1 & hab < 10 ~ "1 - 10",
    hab >= .1 & hab < 1 ~ "0,1 - 1",
    hab < .1 ~ "Menor a 0,1")) |> 
  mutate(rango = fct(
    rango, 
    levels = c(
      "Menor a 0,1","0,1 - 1", "1 - 10", "10 - 100", "100 - 1.000", 
      "Mayor a 1.000")))

# cantidad de rango, para scale_size_manual()
rangos <- unique(sf_pob_mask_dens$rango) |> length()

# extensión de Argentina
bb <- st_bbox(arg_posgar) |> 
  st_as_sfc() |> 
  st_as_sf()

# diferencia entre extensión y Argentina, para remover el borde externo
arg_dif <- st_difference(bb, arg_posgar)

# figura ------------------------------------------------------------------

# ubicación de la flecha y texto
flecha <- c(x = 4615305, y = 3903927, xend = 4407566, yend = 3921090)
flecha_label <- glue(
  "Cada círculo<br>abarca {area} km<sup>2</sup>")

# figura
g <- ggplot()+
  geom_sf(data = pcias_posgar, fill = c4, color = c2, linewidth = .5) +
  geom_sf(data = arg_dif, fill = c1, color = c1, linewidth = .6) +
  geom_sf(data = sf_pob_mask_dens, aes(size = rango, geometry = geom)) +
  annotate(
    geom = "curve", x = flecha["x"], y = flecha["y"], xend = flecha["xend"], 
    yend = flecha["yend"], arrow = arrow(angle = 10, type = "closed", length = unit(4, "mm")),
    curvature = -.2, ) +
  annotate(
    geom = "richtext", x = flecha["x"], y = flecha["y"], label = flecha_label,
    hjust = 0, vjust = 0, fill = c4, family = "ubuntu", size = 8, 
    label.color = NA, label.r = unit(0, "lines"), label.padding = unit(.5, "lines")) +
  scale_size_manual(
    values = seq(.2, 3.5, length.out = rangos)) +
  labs(
    title = "Densidad de Población\nArgentina",
    size = "Cantidad de<br>personas<br>por km<sup>2</sup>", caption = mi_caption) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = c1, color = c3, linewidth = 3),
    plot.margin = margin(0, 28.35, 0, 28.35),
    plot.title = element_text(
      hjust = .5, size = 50, family = "ibm", margin = margin(50, 0, 0, 0)),
    plot.title.position = "plot",
    plot.caption = element_markdown(
      family = "ubuntu", color = c3, size = 15, margin = margin(0, 10, 10, 0)),
    legend.position = c(.72, .3),
    legend.background = element_rect(fill = c4, color = NA),
    legend.margin = margin(10, 10, 10, 10),
    legend.title = element_markdown(size = 30, hjust = 0, family = "ubuntu"),
    legend.justification = c(0, 0),
    legend.text = element_markdown(size = 25, hjust = 0, family = "ubuntu"))

# guardo
ggsave(
  plot = g,
  filename = "mapa_puntos_pob/viz.png",
  width = 30,
  height = 67,
  units = "cm")

# abro
browseURL("mapa_puntos_pob/viz.png")
