
# paquetes ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(glue)
library(showtext)
library(ggtext)

# fuentes -----------------------------------------------------------------

# RColorBrewer Blues
RColorBrewer::display.brewer.pal(n = 9, name = "Blues")
c1 <- "#08306B"
c2 <- "#08519C"
c3 <- "#C6DBEF"
c4 <- "#F7FBFF"

# texto
font_add_google(name = "Ubuntu", family = "ubuntu", db_cache = FALSE)

showtext_auto()
showtext_opts(dpi = 300)

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")

# caption

fuente <- glue("Datos: <span style='color:{c3};'>Instituto Geográfico Nacional</span>")
epsg <- glue("EPSG: <span style='color:{c3};'>5346</span>")
autor <- glue("Autor: <span style='color:{c3};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
usuario <- glue("<span style='color:{c3};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue("{fuente} {sep} {epsg} {sep} {autor} {sep} {icon_github} {icon_twitter} {usuario}")

# datos -------------------------------------------------------------------

browseURL("https://www.ign.gob.ar/NuestrasActividades/InformacionGeoespacial/CapasSIG")

# polígono de Argentina
p <- st_read("ign_red_vial/arg_continental.gpkg")
# ggplot(data = p) + geom_sf()

# leo los vectores de las redes viales
l_nacional <- st_read("ign_red_vial/LíneaRed vial nacional.json") |> 
  mutate(obj = "nacional") |> 
  select(obj, geometry)
l_provincial <- st_read("ign_red_vial/LíneaRed vial provincial.json") |> 
  mutate(obj = "provincial") |> 
  select(obj, geometry)
l_terciaria <- st_read("ign_red_vial/LíneaRed vial terciaria.json") |> 
  mutate(obj = "municipal") |> 
  select(obj, geometry)

# vector con el orden correcto de las redes viales
obj_orden <- c("nacional", "provincial", "municipal")

# unifico las redes viales
rv <- rbind(l_nacional, l_provincial, l_terciaria) |> 
  mutate(obj = factor(obj, levels = obj_orden))

# figura ------------------------------------------------------------------

# ubicación de las etiquetas, en 'sf'
coord_etq <- tibble(lon = -63, lat = -45) |> 
  st_as_sf(coords = c("lon", "lat")) |> 
  st_set_crs(value = st_crs(p))

# etiquetas
etq_tbl <- tibble(obj = unique(rv$obj)) |> 
  mutate(geometry = coord_etq$geometry) |> 
  mutate(label = glue("Red vial\n{toupper(obj)}")) |> 
  st_as_sf()

# figura
g <- ggplot() +
  # país
  geom_sf(data = p, color = NA, fill = c2, linewidth = .25) +
  # red vial
  geom_sf(
    data = rv, aes(linewidth = obj), 
    color = c3, show.legend = FALSE) +
  # etiqueta
  geom_sf_text(
    data = etq_tbl, aes(label = label), 
    color = c3, size = 13, hjust = 0, family = "ubuntu") +
  # transformo a Faja POSGAR 2007 faja 4
  coord_sf(clip = "off", crs = 5346) +
  # especifico los grosores de línea
  scale_linewidth_manual(values = c(.5, .25, .1)) +
  # faceta
  facet_wrap(~ obj, ncol = 3) +
  # epígrafe
  labs(caption = mi_caption) +
  # tema
  theme_void() +
  theme(
    plot.background = element_rect(fill = c1, color = c2, linewidth = 3),
    plot.margin = margin(0, 2.5, 0, 2.5),
    plot.title.position = "plot",
    plot.title = element_text(
      size = 50, color = c3, hjust = .5),
    plot.caption = element_markdown(
      hjust = .5, size = 15, color = c4, family = "ubuntu", 
      margin = margin(0, 0, 10, 0)),
    strip.text = element_blank()
  ); ggsave(
    plot = g,
    filename = "ign_red_vial/viz.png",
    width = 60,
    height = 44,
    units = "cm",
    dpi = 300
  ); browseURL("ign_red_vial/viz.png")
