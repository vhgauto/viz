
# paquetes ----------------------------------------------------------------

library(sf)
library(patchwork)
library(ggpattern)
library(ggtext)
library(glue)
library(showtext)
library(tidyverse)

# fuente ------------------------------------------------------------------

# colores
c <- monochromeR::generate_palette(
  color = "#D485AA", 
  modification = "go_both_ways", 
  n_colours = 9
)

# fuente: Ubuntu
font_add(
  family = "ubuntu", 
  regular = "fuentes/Ubuntu-Regular.ttf",
  bold = "fuentes/Ubuntu-Bold.ttf",
  italic = "fuentes/Ubuntu-Italic.ttf")

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue(
  "Datos: <b style='color:{c[9]};'>IGN</b>, <b style='color:{c[9]};'>SMN</b>")
autor <- glue("<span style='color:{c[9]};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
icon_mastodon <- glue("<span style='font-family:fa-brands;'>&#xf4f6;</span>")
usuario <- glue("<span style='color:{c[9]};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{fuente} {sep} {autor} {sep} {icon_github} {icon_twitter} 
  {icon_mastodon} {usuario}")

# datos -------------------------------------------------------------------

# sitios de las estaciones meteorológicas
p <- st_read("estaciones_smn/smn_estaciones_meteorologicas.json") |> 
  st_transform(crs = 5346) |> 
  select(nombre)

# provincias de Argentina, continental
pcias_cont <- st_read("extras/pcias_continental.gpkg")

# departamentos de Argentina, continental
dptos_cont <- st_read("extras/dptos_continental.gpkg")

# departamentos de Argentina, Antártida
dptos_antart <- st_read("extras/dptos_antartida.gpkg")

# recorto los puntos de las estaciones a las regiones
p_cont <- st_crop(p, dptos_cont)
p_antart <- st_crop(p, dptos_antart)

# creo buffer alrededor de los puntos de las estaciones
# NO pueden ser puntos para el difuminado de los colores,
# tiene que ser un polígono
# como el mapa de Antártida es más pequeño, los polígonos tiene que ser más
# grandes
p_cont_buffer <- st_buffer(p_cont, dist = 12000)
p_antart_buffer <- st_buffer(p_antart, dist = 30000)

# figura ------------------------------------------------------------------

# logo SMN
smn <- "<img src='estaciones_smn/logo.png' width='200'></img>"

# subtítulo
mi_subtitle <- glue(
  "En **Argentina** hay {nrow(p)}<br>estaciones meteorológicas.<br>"
)

# coordenadas de la extensión de Argentina continental
# útil para ubicar el texto alrededor del mapa
bb <- st_bbox(pcias_cont)

# diseño de las figuras en el mapa final
diseño <- "
A#
AB
"

# figura Antártida
g_antart <- ggplot() +
  # departamentos
  geom_sf(data = dptos_antart, fill = c[3], color = c[5]) +
  # estaciones meteorológicas
  geom_sf_pattern(
    data = p_antart_buffer, color = NA, pattern = "gradient",
    pattern_orientation = "radial",
    pattern_fill = c[3], # centro
    pattern_fill2 = c[8], # exterior
    pattern_density = 1) +
  scale_fill_viridis_d(option = "turbo") +
  coord_sf(clip = "off", expand = TRUE) +
  theme_void() +
  theme(
    plot.background = element_rect(color = NA, linewidth = 2, fill = c[2])
  )

# figura Argentina continental
g_cont <- ggplot() +
  # departamentos
  geom_sf(data = dptos_cont, fill = c[2], color = c[5]) +
  # provincias
  geom_sf(data = pcias_cont, fill = NA, color = c[7], linewidth = .25) +
  # estaciones meteorológicas
  geom_sf_pattern(
    data = p_cont_buffer, color = NA, pattern = "gradient",
    pattern_orientation = "radial",
    pattern_fill = c[3], # centro
    pattern_fill2 = c[8], # exterior
    pattern_density = 1) +
  # subtítulo
  annotate(
    geom = "richtext", x = bb["xmax"], y = bb["ymax"], label = mi_subtitle, 
    fill = NA, label.color = NA, hjust = 0, size = 9, color = c[9], 
    family = "ubuntu", vjust = 1
  ) +
  # logo SMN
  annotate(
    geom = "richtext", x = bb["xmax"], y = 6e6, 
    label.padding = unit(rep(1, 4), "lines"),
    label.margin = unit(rep(4, 4), "lines"),
    label = smn, fill = NA, label.color = NA, hjust = 0, vjust = 0
  ) +
  scale_fill_viridis_d(option = "turbo") +
  coord_sf(clip = "off", expand = FALSE) +
  theme_void()

# figura
g <- g_cont + g_antart +
  plot_layout(widths = c(1, .6), design = diseño) +
  plot_annotation(
    caption = mi_caption,
    theme = theme(
      plot.margin = margin(t = 25, r = 25, b = 25, l = 25),
      plot.background = element_rect(
        fill = c[1], color = c[4], linewidth = 3),
      plot.caption = element_markdown(
        family = "ubuntu", size = 13, color = c[7])
    )
  )

# guardo
ggsave(
  plot = g,
  filename = "estaciones_smn/viz.png",
  width = 30,
  height = 40,
  units = "cm"
)

# abro
browseURL("estaciones_smn/viz.png")
