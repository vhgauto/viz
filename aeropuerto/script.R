
# paquetes ----------------------------------------------------------------

library(terra)
library(tidyterra)
library(glue)
library(showtext)
library(ggtext)
library(tidyverse)

# fuente ------------------------------------------------------------------

set.seed(2024)
pp <- PrettyCols::prettycols(palette = "Pinks", n = 24, type = "continuous") |> 
  sample()

c1 <- "black"
c2 <- "white"
c3 <- "#FCAADE"
c4 <- "#860A4D"

# fuente: Ubuntu
font_add(
  family = "ubuntu", 
  regular = "fuentes/Ubuntu-Regular.ttf",
  bold = "fuentes/Ubuntu-Bold.ttf",
  italic = "fuentes/Ubuntu-Italic.ttf")

# monoespacio & íconos
font_add(
  family = "jet", 
  regular = "fuentes/JetBrainsMonoNLNerdFontMono-Regular.ttf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue(
  "<b>Datos:</b> <span style='color:{c1};'>IGN</span>")
autor <- glue("<span style='color:{c1};'>Víctor Gauto</span>")
icon_twitter <- glue("<span style='font-family:jet;'>&#xeb72;</span>")
icon_instagram <- glue("<span style='font-family:jet;'>&#xf16d;</span>")
icon_github <- glue("<span style='font-family:jet;'>&#xf09b;</span>")
icon_mastodon <- glue("<span style='font-family:jet;'>&#xf0ad1;</span>")
usuario <- glue("<span style='color:{c1};'>vhgauto</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{fuente} {sep} {autor} {sep} <b>{icon_github} {icon_twitter} ",
  "{icon_instagram} {icon_mastodon}</b> {usuario}")

# datos -------------------------------------------------------------------

# dptos <- vect("extras/dptos_continental.gpkg")

pcias <- vect("extras/pcias_continental.gpkg") |> 
  project("EPSG:5346")

dptos_pcias <- vect("extras/dptos_pcias_continental.gpkg")

aer <- vect("aeropuerto/puntos_de_transporte_aereo_GB005.json") |> 
  project("EPSG:5346")

# as.data.frame(aer) |> 
#   tibble() |> 
#   filter(fun == 6)

aer <- aer[aer$fun == 6]

# figura ------------------------------------------------------------------

# posición del subtítulo
x_sub <- ext(aer)$xmax
y_sub <- ext(aer)$ymax

mi_subtitle <- glue(
  "En <b style='color: {c1}'>Argentina</b> hay en",
  "funcionamiento <b style='color:{c4}'>{nrow(aer)}</b>",
  "aeropuertos.",
  .sep = "<br>"
)

# figura
g <- ggplot() +
  # departamentos
  geom_sf(
    data = dptos_pcias, aes(fill = provincia), color = c2, linewidth = .3,
    linetype = "22"
  ) +
  # provincias
  geom_sf(
    data = pcias, fill = NA, color = c1, linewidth = .4
  ) +
  # aeropuertos
  geom_sf(
    data = aer, fill = c2, color = c1, size = 7, alpha = .8, 
    stroke = 1, shape = 21) +
  # subtítulo
  annotate(
    geom = "richtext", x = x_sub*.84, y = y_sub*.66, label = mi_subtitle,
    fill = NA, label.color = NA, size = 12, color = c1, hjust = 0,
    family = "ubuntu"
  ) +
  scale_fill_manual(values = pp) +
  labs(caption = mi_caption) +
  theme_void() +
  theme(
    plot.margin = margin(r = 8, l = 8, t = 2),
    plot.background = element_rect(
      fill = c3, color = c4, linewidth = 3),
    plot.caption = element_markdown(
      family = "jet", color = c4, hjust = .5, size = 20,
      margin = margin(b = 10)),
    legend.position = "none"
  ); ggsave(
    plot = g,
    filename = "aeropuerto/viz.png",
    width = 30,
    height = 65,
    units = "cm"
  ); browseURL("aeropuerto/viz.png")

