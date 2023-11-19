
# paquetes ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(showtext)
library(glue)
library(ggtext)

# fuentes -----------------------------------------------------------------

# colores de la bandera
c1 <- "#74ACDF"
c2 <- "#F6B40E"
c3 <- "white"

# texto gral
font_add_google(name = "Ubuntu", family = "ubuntu")
# letras
font_add_google(name = "Libre Bodoni", family = "libre", db_cache = FALSE)

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
autor <- glue("Autor: <span style='color:{c3};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
icon_mastodon <- glue("<span style='font-family:fa-brands;'>&#xf4f6;</span>")
usuario <- glue("<span style='color:{c3};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{autor} {sep} {icon_github} {icon_twitter} {icon_mastodon} {usuario}")

# datos -------------------------------------------------------------------

# Argentina
arg <- st_read("mapa_topografico/arg_continental.gpkg") |> 
  st_transform(crs = 5346)

# extensión de Argentina
arg_ext <- st_bbox(arg) |> 
  st_as_sfc() |> 
  st_as_sf() |> 
  rename(geom = x)

# centroide de Argentina
arg_centro <- arg_ext |> 
  mutate(centro = st_centroid(geom)) |> 
  select(-geom) |> 
  st_drop_geometry() |> 
  st_as_sf()

# buffer cuadrado alrededor de Argentina
arg_buffer <- st_buffer(
  arg_centro, dist = 3e6, nQuadSegs = 1, endCapStyle = "SQUARE") |> 
  rename(geom = centro)

# grilla 3x3 de Argentina
arg_grid <- tibble(geom = rep(arg$geom, 9)) |> 
  mutate(pos = 1:9) |> 
  mutate(relleno = case_when(
    pos %in% c(1, 2, 3, 7, 8, 9) ~ c3,
    pos %in% c(4, 6) ~ c1,
    pos == 5 ~ c2)) |> 
  st_as_sf()

# grilla 3x3 recuadro de Argentina
arg_ext_grid <- tibble(geom = rep(arg_buffer$geom, 9)) |> 
  mutate(pos = 1:9) |> 
  mutate(fondo = case_when(
    pos %in% c(1, 2, 3, 7, 8, 9) ~ c1,
    pos %in% c(4, 5, 6) ~ c3)) |> 
  st_as_sf()

# grilla 3x3 de las letras de Argentina
arg_let <- arg_grid |> 
  mutate(centro = st_centroid(geom)) |> 
  select(-geom) |> 
  st_drop_geometry() |> 
  mutate(label = list_c(str_split("ARGENTINA", pattern = ""))) |> 
  mutate(col_letra = case_when(
    pos %in% c(1, 2, 3, 7, 8, 9) ~ c1,
    .default = c3)) |> 
  st_as_sf()
  
# figura ------------------------------------------------------------------

# autor
caption_tbl <- tibble(
  pos = 9,
  x = st_bbox(arg_buffer)$xmax,
  y = st_bbox(arg_buffer)$ymin,
  label = mi_caption)

# figura
g <- ggplot() +
  # recuadro de fondo
  geom_sf(data = arg_ext_grid, aes(fill = fondo), color = NA) +
  # Argentina
  geom_sf(data = arg_grid, aes(fill = relleno), color = NA) +
  # letras
  geom_sf_text(
    data = arg_let, aes(label = label, color = col_letra), size = 25,
    family = "libre", fontface = "bold") +
  # caption
  geom_richtext(
    data = caption_tbl, aes(x, y, label = label),color = c2, size = 5, 
    family = "ubuntu", fill = NA, label.color = NA, hjust = 1, vjust = 0) +
  # faceta
  facet_wrap(vars(pos), ncol = 3) +
  scale_color_identity() +
  scale_fill_identity() +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.caption = element_markdown(color = c2, size = 10, family = "ubuntu"),
    strip.text = element_blank(),
    panel.spacing = unit(-.1, "mm"))

# guardo
ggsave(
  plot = g,
  filename = "arg_3x3/arg.png",
  width = 40,
  height = 40,
  units = "cm")

# abro
browseURL("arg_3x3/arg.png")
