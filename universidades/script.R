
# paquetes ----------------------------------------------------------------

library(terra)
library(glue)
library(tidyterra)
library(ggfx)
library(showtext)
library(ggtext)
library(tidyverse)

# fuentes -----------------------------------------------------------------

# colores
c1 <- "lightblue"
c2 <- "orange"
c3 <- "white"
c4 <- "grey5"
c5 <- "grey30"

# fuente: Ubuntu
font_add(
  family = "ubuntu", 
  regular = "fuentes/Ubuntu-Regular.ttf",
  bold = "fuentes/Ubuntu-Bold.ttf",
  italic = "fuentes/Ubuntu-Italic.ttf")

# fuente: Victor
font_add(
  family = "victor", 
  regular = "fuentes/VictorMono-ExtraLight.ttf",
  bold = "fuentes/VictorMono-VariableFont_wght.ttf",
  italic = "fuentes/VictorMono-ExtraLightItalic.ttf")

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue(
  "Datos: <b style='color:{c3};'>IGN</b>")
autor <- glue("<span style='color:{c3};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_instagram <- glue("<span style='font-family:fa-brands;'>&#xf16d;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
icon_mastodon <- glue("<span style='font-family:fa-brands;'>&#xf4f6;</span>")
usuario <- glue("<span style='color:{c3};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{fuente} {sep} {autor} {sep} {icon_github} {icon_twitter} {icon_instagram} ",
  "{icon_mastodon} {usuario}")

# datos -------------------------------------------------------------------

# sitios de educación superior, POSGAR 2007
u <- vect("universidades/puntos_de_ciencia_y_educacion_020602.shp") |> 
  project("EPSG:5346")

# departamentos, POSGAR 2007
dptos <- vect("extras/dptos_continental.gpkg")

# provincias,POSGAR 2007
pcias <- vect("extras/pcias_continental.gpkg")

# filtro por UTN
d <- u |> 
  mutate(
    es_utn = str_detect(gna, "Universidad Tecnológica Nacional")
  ) |> 
  mutate(
    es_utn = if_else(
      is.na(es_utn),
      FALSE,
      es_utn
    )
  )

# divido los datos
d_utn <- filter(d, es_utn)
d_otra <- filter(d, !es_utn)

# figura ------------------------------------------------------------------

# leyenda
leyenda_tbl <- tibble(
  x = 5.1e6,
  y = c(7.5e6, 7.38e6),
  label = c(
    "Universidad Tecnológica\nNacional (UTN)",
    "Otras instituciones\neducativas")
) |> 
  mutate(x_label = x+.5e5)

# total Universidades
n_u <- format(nrow(u), big.mark = ".", decimal.mark = ",")

# subtítulo
mi_subtitle <- glue(
  "En <b>Argentina</b> hay {n_u} <b style='color:{c1}'>instituciones</b><br>",
  "dedicadas al desarrollo científico y<br>",
  "educativo. Del total, {nrow(d_utn)} son facultades<br>",
  "regionales de la ",
  "<b style='color:{c2}'>Universidad<br>Tecnológica Nacional</b>."
)

# figura
g <- ggplot() +
  # departamentos
  geom_sf(data = dptos, fill = c4, color = c5, linewidth = .1) +
  # provincias
  geom_sf(data = pcias, fill = NA, color = c5, linewidth = .5) +
  
  # otras universidades
  with_blur(
    geom_sf(
      data = d_otra, color = c1, size = 4, shape = 20),
    sigma = 8
  ) +
  geom_sf(
    data = d_otra, color = c1, size = .5, shape = 20) +
  
  # Universidad Tecnológica Nacional
  with_blur(
    geom_sf(
      data = d_utn, color = c2, size = 4, shape = 20),
    sigma = 8
  ) +
  geom_sf(
    data = d_utn, color = c2, size = .5, shape = 20) +
  
  # leyenda
  with_blur(
    geom_point(
      data = leyenda_tbl, aes(x, y), color = c(c2, c1), size = 7),
    sigma = 8
  ) +
  geom_point(
    data = leyenda_tbl, aes(x, y), color = c(c2, c1), size = 2
  ) +
  geom_text(
    data = leyenda_tbl, aes(x_label, y, label = label), family = "ubuntu",
    color = c3, hjust = 0, size = 6, lineheight = unit(.8, "line")
  ) +
  
  # subtítulo
  annotate(
    geom = "richtext", x = 4.7e6, y = 5e6, label = mi_subtitle, size = 8,
    family = "ubuntu", color = c3, fill = NA, label.color = NA, hjust = 0
  ) +
  coord_sf(clip = "off") +
  labs(caption = mi_caption) +
  theme_void() +
  theme(
    plot.margin = margin(r = 160, t = .6, b = .6),
    plot.background = element_rect(fill = c4, color = c3, linewidth = 3),
    plot.caption = element_markdown(
      family = "ubuntu", color = c1, size = 18, 
      margin = margin(b = 10, r = -100))
  )

# guardo
ggsave(
  plot = g,
  filename = "universidades/viz.png",
  width = 30,
  height = 54,
  units = "cm")

# abro
browseURL("universidades/viz.png")
