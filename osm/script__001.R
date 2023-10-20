# paquetes ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(glue)
library(showtext)
library(ggtext)

# fuentes -----------------------------------------------------------------

# colores, Ernst, [MoMAColors]
c1 <- "#1B2958"
c2 <- "#131631"
c3 <- "#4F9293"
c4 <- "#7BB488"

# caption
font_add_google(name = "Ubuntu", family = "ubuntu")
# título
font_add_google(name = "Water Brush", family = "water", db_cache = FALSE)

showtext_auto()
showtext_opts(dpi = 300)

# íconos
font_add("fa-reg", "icon/Font Awesome 6 Free-Regular-400.otf")
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")
font_add("fa-solid", "icon/Font Awesome 6 Free-Solid-900.otf")

# caption
fuente <- glue(
  "Datos: <span style='color:{c3};'>OpenStreetMap, Geofabrik</span>")
autor <- glue("Autor: <span style='color:{c3};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
usuario <- glue("<span style='color:{c3};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{fuente} {sep} {autor} {sep} {icon_github} {icon_twitter} {usuario}")

# datos -------------------------------------------------------------------

# islas Malvinas, porque el archivo de OpenStreetMap NO TIENE LAS ISLAS MALVINAS
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
malvinas <- st_read("osm_extras/malvinas.gpkg") |> 
  st_transform(crs = 5346)

# ríos, MULTILINESTRING
linea_agua <- st_read("osm/osm_gpkg/waterways.gpkg") |> 
  st_make_valid() |> 
  st_transform(crs = 5346)

# colores, p/cada línea
colores <- as.character(MoMAColors::moma.colors(palette_name = "Connors"))

# agrego los colores y el largo de los MULTILINESTRING
# los colores se agregan aleatoriamente, así que defino una seed para que los
# colores sean siempre los mismos
set.seed(2024)
d <- linea_agua |> 
  # quito el Río de la Plata porque queda raro
  filter(!str_detect(name, "Río de la Plata")) %>%
  mutate(
    color = sample(colores, nrow(.), replace = TRUE), .before = 1) |> 
  mutate(largo = st_length(geom)) |> 
  mutate(largo = as.numeric(largo))

# figura ------------------------------------------------------------------

g <- ggplot() +
  geom_sf(data = malvinas, color = NA, fill = c1) +
  geom_sf(
    data = d, aes(color = color, linewidth = largo), lineend = "round", 
    alpha = .9, show.legend = FALSE) +
  annotate(
    geom = "text", x = 4.6e6, y = 5e6, label = "Ríos de\nArgentina",
    family = "water", size = 35, color = c4, hjust = 0) +
  scale_color_identity() +
  scale_linewidth_continuous(range = c(.5, 1)) +
  labs(caption = mi_caption) +
  theme_void() +
  theme(
    plot.margin = margin(0, 3.25, 0, 3.25),
    plot.background = element_rect(fill = c2, color = c3, linewidth = 3),
    plot.caption = element_markdown(
      family = "ubuntu", size = 12, color = c4, margin = margin(0, 20, 20, 0)))

# guardo
ggsave(
  plot = g,
  filename = "osm/viz.png",
  width = 30,
  height = 66,
  units = "cm")

# abro
browseURL("osm/viz.png")
