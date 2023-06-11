
# paquetes ----------------------------------------------------------------

library(tidyverse)
library(sf)
library(ggrepel)
library(patchwork)
library(showtext)
library(ggtext)
library(glue)

# fuente ------------------------------------------------------------------

# colores
MetBrewer::met.brewer(palette_name = "Moreau")
c1 <- "#421600"
c2 <- "#792503"
c3 <- "white"
c4 <- "#BC7524"
c5 <- "#8DADCA"
c6 <- "#527BAA"
c7 <- "#082844"

# años, eje vertical
font_add_google(name = "Bebas Neue", family = "bebas", db_cache = TRUE)
# números, eje horizontal
font_add_google(name = "Inconsolata", family = "inconsolata", db_cache = FALSE)
# resto del texto
font_add_google(name = "Ubuntu", family = "ubuntu", db_cache = FALSE)

showtext_auto()
showtext_opts(dpi = 300)

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue("Datos: <span style='color:{c3};'>**Instituto Geográfico Nacional**</span>")
autor <- glue("Autor: <span style='color:{c3};'>**Víctor Gauto**</span>")
proy <- glue("EPSG: <span style='color:{c3};'>**4326**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
usuario <- glue("<span style='color:{c3};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue("{fuente} {sep} {proy} {sep} {autor} {sep} {icon_github} {icon_twitter} {usuario}")

# datos -------------------------------------------------------------------

browseURL("https://www.ign.gob.ar/NuestrasActividades/InformacionGeoespacial/CapasSIG")
# Polígono, Área protegida

# vector de áreas protegidas de Argentina
ap <- st_read("ign_parques/area_protegida.json")

# vector de Argentina
p <- st_read("mapa_topografico/arg_continental.gpkg")

# selecciono sólo los parques nacionales
parq_nac <- ap |> 
  filter(gna == "Parque Nacional") |> 
  # unifico Iberá, que está dividido en 3 partes
  mutate(nam = if_else(str_detect(nam, "Iberá"), "Iberá", nam))

# arreglo los datos
parq_nac1 <- parq_nac |> 
  # agrupo por nombre del parque
  group_by(nam) |> 
  # fusiono las áreas, p/q Iberá sea una única geometría
  summarise(geometry = st_union(geometry)) |>
  # calculo las áreas (m^2)
  mutate(a = st_area(geometry)) |> 
  mutate(a = as.numeric(a)) |> 
  # convierto a km^2
  mutate(a = a*1e-6) |> 
  # recorto el nombre p/usar en el mapa
  mutate(nam_corto = str_wrap(nam, width = 15)) |> 
  # convierto los nombres (corto/extendido) a factores, según área
  mutate(nam = fct_reorder(nam, a)) |> 
  mutate(nam_corto = fct_reorder(nam_corto, a)) |> 
  # ordeno por nombre y agrego nro de fila, útil para la figura
  arrange(nam) |> 
  mutate(fila = row_number())

# figura ------------------------------------------------------------------

# __columna ---------------------------------------------------------------

# función que agrega un punto por unidad de mil (1000 -> 1.000)
f_punto <- scales::label_number(big.mark = ".", decimal.mark = ",")

# valores de áreas para el eje horizontal  
eje_x <- tibble(
  x = seq(0, 6000, 1000), y = 0, label = seq(0, 6000, 1000)) |> 
  mutate(label = f_punto(label))

# ubicación del título del eje x
tit_eje_x <- tibble(
  x = 3000, y = -.5, label = "Área, km<sup>2</sup>")

# líneas verticales
verticales <- tibble(
  x = seq(0, 6000, 1000),
  xend = x,
  y = .5,
  yend = 32.5)

# shape de los puntos, en figura de barras y mapa  
forma <- 23

# figura de columnas
g_col <- parq_nac1 |> 
  ggplot(aes(x = a, y = fila)) +
  # líneas verticales de fondo
  geom_segment(
    data = verticales, aes(x = x, xend = xend, y = y, yend = yend),
    color = c2, linewidth = .25) +
  # segmentos horizontales, áreas de los parques
  geom_segment(aes(x = 0, xend = a, yend = fila), color = c4, linewidth = 2) +
  # puntos, áreas de los parques, rombo
  geom_point(color = c5, fill = c7, size = 6, shape = forma) +
  # puntos, áreas de los parques, punto 
  geom_point(shape = 16, color = c5, size = 1) +
  # eje horizontal, valores de las áreas
  geom_text(data = eje_x,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE, size = 8, color = c5, family = "inconsolata") +
  # título del eje horizontal, área
  geom_richtext(
    data = tit_eje_x, aes(x = x, y = y, label = label),
    inherit.aes = FALSE, color = c5, size = 10, family = "ubuntu", fill = NA,
    label.color = NA, vjust = 1) +
  # nombre de los parques
  geom_text(
    aes(label = nam), nudge_x = 120, hjust = 0, color = c3, family = "ubuntu",
    size = 7) +
  # ejes
  scale_x_continuous(
    breaks = seq(0, 6000, 1000), limits = c(0, 6000), expand = c(0, 0),
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  scale_y_continuous(labels = 32:1, breaks = 1:32, limits = c(-1, 34)) +
  labs(y = NULL, x = "Área, km<sup>2</sup>") +
  coord_cartesian(clip = "off") +
  # tema
  theme_void() +
  theme(
    aspect.ratio = 2,
    plot.margin = margin(10, 70, 10, 10),
    axis.text.y = element_text(
      color = c5, family = "inconsolata", hjust = 0, size = 18, 
      margin = margin(0, 10, 0, 0)),
    axis.ticks.y = element_blank()
  )

# __sf --------------------------------------------------------------------

# centroide de los parques
cen <- parq_nac1 |> 
  st_centroid() |> 
  st_transform(crs = 4326) |> 
  st_geometry()

# mapa
g_sf <- ggplot() +
  # polígono Argentin
  geom_sf(data = p, fill = c2, color = c7, linewidth = .1) +
  # centroides de los parques, rombos
  geom_sf(data = cen, shape = forma, color = c5, fill = c7, size = 8) +
  # centroides de los parques, puntos
  geom_sf(data = cen, shape = 16, color = c5, size = 1) +
  # nombres de los parques
  geom_label_repel(
    data = parq_nac1, aes(label = nam_corto, geometry = geometry),
    stat = "sf_coordinates", size = 5, point.padding = 20, hjust = 0,
    family = "ubuntu", seed = 2023, fill = NA, color = c3,
    label.size = unit(0, "line"), label.padding = unit(.1, "line")) +
  # eje
  coord_sf(clip = "off") +
  # tema
  theme_void()

# diseño de la imagen compuesta
diseño <- "
  A#B
  A#B
"

# __figura compuesta ------------------------------------------------------

g_comp <- g_sf + g_col +
  plot_layout(design = diseño, widths = c(1, .1, 1)) +
  plot_annotation(
    title = glue("Parques Nacionales de <span style='color:{c6};'>Arg</span>ent<span style='color:{c6};'>ina</span>"),
    caption = mi_caption,
    theme = theme(
      plot.background = element_rect(
        fill = c1, color = c2, linewidth = 3),
      plot.margin = margin(10, 10, 10, 10),
      plot.title.position = "plot",
      plot.title = element_markdown(
        hjust = .5, size = 80, family = "bebas", color = "white"),
      plot.caption = element_markdown(hjust = .57, color = c5, size = 20)))

# guardo
ggsave(
  plot = g_comp,
  filename = "ign_parques/viz.png",
  width = 50,
  height = 50,
  units = "cm",
  dpi = 300)

# abro
browseURL("ign_parques/viz.png")
