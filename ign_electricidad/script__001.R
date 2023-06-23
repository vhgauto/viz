
# paquetes ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(glue)
library(showtext)
library(ggtext)

# fuentes -----------------------------------------------------------------

l1 <- "grey10"
l2 <- "grey15"
l3 <- "grey20"
l4 <- "grey25"
l5 <- "grey90"
p1 <- "#054544"
p2 <- "#175F5D"
p3 <- "#178F92"
p4 <- "#4FB6CA"
p5 <- "#EAF3FF"

# texto gral
font_add_google(name = "Ubuntu", family = "ubuntu", db_cache = TRUE)
# título
font_add(family = "voltage", regular = "fuentes/Voltage-Regular.otf")
# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue("Datos: <span style='color:{p4};'>Instituto Geográfico Nacional</span>")
autor <- glue("Autor: <span style='color:{p4};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
usuario <- glue("<span style='color:{p4};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue("{fuente} {sep} {autor} {sep} {icon_github} {icon_twitter} {usuario}")

# datos -------------------------------------------------------------------

browseURL("https://www.ign.gob.ar/NuestrasActividades/InformacionGeoespacial/CapasSIG")

# polígono de Argentina
p <- st_read("mapa_topografico/arg_continental.gpkg")

# el tendido eléctrico no está completo, hay espacios vacíos
# Industria y servicios, Línea, Línea de transmisión eléctrica
lin_elec <- st_read("ign_electricidad/lineas_de_energia_AT030.json")

# si utilizo geom_sf() se generan espacios entre las uniones de las líneas
# unifico las geometrías, convierto los 'sf' a data.frame p/luego usar 
# geom_path() y que las líneas sean todas contínuas
browseURL("https://stackoverflow.com/questions/51812311/fixing-gaps-in-osm-roads-when-plotting-with-ggplot2-geom-sf")

# acomodo los datos de las líneas eléctricas de ALTA TENSIÓN ACTIVAS
alta_tension <- lin_elec |> 
  # ALTA TENSIÓN
  filter(ten == 6) |> 
  # ACTIVAS
  filter(fun == 6) |> 
  # uno todas las geometrías
  st_union() |> 
  st_line_merge() |> 
  # obtengo las coordenadas
  st_coordinates() |> 
  # convierto a data.frame
  as.data.frame()

# plantas transformadoras
# Industria y servicios, Punto, Central eléctrica
planta_transf <- st_read("ign_electricidad/puntos_de_energia_AD030.json")

# centrales eléctricas activas
planta_transf_act <- planta_transf |> 
  # ACTIVAS
  filter(fun == 6)

# figura ------------------------------------------------------------------

g <- ggplot() +
  # Argentina
  geom_sf(data = p, fill = "black", color = NA) +
  # líneas eléctricas
  geom_path(
    data = alta_tension, aes(x = X, y = Y, group = L1),
    color = l1, linewidth = 7, lineend = "round") +
  geom_path(
    data = alta_tension, aes(x = X, y = Y, group = L1),
    color = l2, linewidth = 5, lineend = "round") +
  geom_path(
    data = alta_tension, aes(x = X, y = Y, group = L1),
    color = l3, linewidth = 3, lineend = "round") +
  geom_path(
    data = alta_tension, aes(x = X, y = Y, group = L1),
    color = l4, linewidth = 1, lineend = "round") +
  geom_path(
    data = alta_tension, aes(x = X, y = Y, group = L1),
    color = l5, linewidth = .2, lineend = "round") +
  # plantas transformadoras
  #   stroke = 1, show.legend = TRUE) +
  geom_sf(data = planta_transf_act, size = 5, color = p1) +
  geom_sf(data = planta_transf_act, size = 4, color = p2) +
  geom_sf(data = planta_transf_act , size = 3, color = p3) +
  geom_sf(data = planta_transf_act , size = 2, color = p4) +
  geom_sf(data = planta_transf_act , size = .1, color = p5) +
  # ejes
  coord_sf(crs = 4326) +
  labs(
    title = "RED ELECTRICA ARGENTINA",
    subtitle = "Las líneas representan la red de alta tensión y<br>
    los puntos las plantas transformadoras.",
    caption = mi_caption) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "grey5", color = p4, linewidth = 3),
    plot.margin = margin(15, 14, 15, 14),
    plot.title.position = "plot",
    plot.title = element_markdown(
      color = p4, size = 100, family = "voltage", hjust = .5),
    plot.subtitle = element_markdown(
      color = p5, size = 25, hjust = .5, family = "ubuntu"),
    plot.caption.position = "plot",
    plot.caption = element_markdown(
      color = p5, family = "ubuntu", size = 20, margin = margin(10, 0, 5, 0)))

# guardo
ggsave(
    plot = g,
    filename = "ign_electricidad/viz.png",
    width = 30,
    height = 69,
    units = "cm",
    dpi = 300)

# abro
browseURL("ign_electricidad/viz.png")
