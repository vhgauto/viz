
browseURL("https://dieghernan.github.io/202205_Unknown-pleasures-R/")

# paquetes ----------------------------------------------------------------

library(sf)
library(elevatr)
library(tidyverse)
library(terra)
library(ggridges)
library(glue)
library(ggtext)
library(showtext)

# fuentes -----------------------------------------------------------------

# colores de la bandera
c1 <- "white"
c2 <- "black"
c3 <- "grey50"
c4 <- "#74ACDF"
c5 <- "#F6B40E"
c6 <- "grey10"

# texto gral
font_add_google(name = "Ubuntu", family = "ubuntu")
# letras
font_add_google(name = "Libre Bodoni", family = "libre", db_cache = FALSE)

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
autor <- glue("<span style='color:{c1};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
icon_mastodon <- glue("<span style='font-family:fa-brands;'>&#xf4f6;</span>")
usuario <- glue("<span style='color:{c1};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{autor} {sep} {icon_github} {icon_twitter} {icon_mastodon} {usuario}")

# datos -------------------------------------------------------------------

# Argentina, POSGAR
arg <- st_read("mapa_topografico/arg_continental.gpkg") |> 
  st_transform(crs = 5346)

# elevación de Argentina
ele <- get_elev_raster(
  locations = arg,
  z = 7,
  clip = "location") |> 
  rast()

# renombro la banda a 'altura'
names(ele) <- "altura"

# que queden ~150 filas
factor <- round(nrow(ele)/150)

# hago una reducción de los datos aplicando el factor
ele_agr <- aggregate(ele, factor)

# elimino valores negativos y NA
ele_agr[ele_agr < 0] <- 0
ele_agr[is.na(ele_agr)] <- 0

# convierto a tibble, manteniendo las coordenadas
# divido los datos en tres, para luego colorear
ele_tbl <- as.data.frame(ele_agr, xy = TRUE, na.rm = FALSE) |> 
  as_tibble() |> 
  mutate(rango = cut_number(y, n = 3))

# para el exterior de Argentina conservo los valores nulos
exterior <- ele_agr == 0

# convierto a tibble, y elimino los valores internos de Argentina
exterior_tbl <- as.data.frame(exterior, xy = TRUE, na.rm = FALSE) |> 
  as_tibble() |> 
  filter(altura)

# las Islas Malvinas no se notan, así que las agrego
im <- st_read("extras/Islas_Malvinas.gpkg") |> 
  st_transform(crs = 5346)

# figura ------------------------------------------------------------------

# para agregar el Sol, obtengo el centroide de la región del medio y 
# creo un buffer
ele_mitad_sf <- ele_tbl |> 
  filter(rango == "(5.12e+06,6.35e+06]") |> 
  st_as_sf(coords = c("x", "y")) |> 
  st_set_crs(value = 5346)

# extensión de la región del medio
bb_mitad <- st_bbox(ele_mitad_sf) |> 
  st_as_sfc() |> 
  st_as_sf()

# Sol
sol_sf <- st_centroid(bb_mitad) |>
  st_buffer(dist = 200000)

# recorto el ráster reducido usando el sf del Sol
ele_agr_sol <- terra::mask(ele_agr, vect(sol_sf))

# convierto el ráster del Sol a tibble
sol_tbl <- as.data.frame(ele_agr_sol, xy = TRUE, na.rm = FALSE) |> 
  as_tibble() |> 
  drop_na()

# figura
g <- ggplot() +
  # puntos alrededor de Argentina
  geom_point(
    data = exterior_tbl, aes(x, y, color = rango), color = c6, size = 2) +
  # Argentina
  geom_ridgeline(
    data = ele_tbl, aes(x, y, group = y, height = altura, color = rango),
    min_height = .01, stat = "identity", scale = 30, show.legend = FALSE,
    fill = c2, size = 1) +
  # Sol
  geom_ridgeline(
    data = sol_tbl, aes(x, y, group = y, height = altura),
    min_height = .01, stat = "identity", scale = 30, show.legend = FALSE,
    fill = c2, size = 2, color = c5) +
  # Islas Malvinas
  geom_sf(data = im, fill = c2, color = c4, linewidth = .1) +
  scale_color_manual(values = c(c4, c1, c4)) +
  coord_sf(expand = FALSE, clip = "off") +
  labs(caption = mi_caption) +
  theme_void() +
  theme(
    plot.margin = margin(40, 0, 20, 0),
    plot.background = element_rect(fill = c2, color = NA),
    plot.caption = element_markdown(
      color = c3, family = "ubuntu", size = 20, margin = margin(30, 0, 0, 0)))

# guardo
ggsave(
  plot = g,
  filename = "mapa_ridges/viz.png",
  width = 35,
  height = 65,
  units = "cm")

# abro
browseURL("mapa_ridges/viz.png")
