
# paquetes ---------------------------------------------------------------

library(tidyterra)
library(terra)
library(glue)
library(biscale) # https://chris-prener.github.io/biscale/
library(showtext)
library(ggtext)
library(patchwork)
library(tidyverse)

# https://www.datawim.com/post/creating-professional-bivariate-maps-in-r/

# colores ----------------------------------------------------------------
# paleta de colores bivariada y colores de texto
# https://chris-prener.github.io/biscale/articles/bivariate_palettes.html

paleta <- "PinkGrn"
col <- biscale::bi_pal(paleta, preview = FALSE)

c1 <- col[7]
c2 <- col[3]
c3 <- "grey95"
c4 <- "grey5"
c5 <- col[9]

# fuentes ----------------------------------------------------------------

# Ubuntu
font_add(
  family = "ubuntu", 
  regular = "fuentes/Ubuntu-Regular.ttf",
  bold = "fuentes/Ubuntu-Bold.ttf",
  italic = "fuentes/Ubuntu-Italic.ttf")

# monoespacio & íconos
font_add(
  family = "jet", 
  regular = "fuentes/JetBrainsMonoNLNerdFontMono-Regular.ttf")

# fontawesome
font_add(
  family = "fa", 
  regular = "fuentes/Font Awesome 6 Free-Solid-900.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue(
  "<b>Datos:</b> <span style='color:{c1};'>Terra Climate Data</span>")
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

# datos ------------------------------------------------------------------

# Argentina
arg0 <- vect("extras/arg_continental.gpkg") |> 
  project("EPSG:5346")

# Provincias
arg1 <- vect("extras/pcias_continental.gpkg")

# diviones internas
arg_sf_ext <- sf::st_as_sf(arg1) |> 
  sf::st_union() |> 
  sf::st_cast("MULTILINESTRING")

arg_sf_int <- sf::st_as_sf(arg1) |> 
  sf::st_cast("MULTILINESTRING")

l <- sf::st_difference(arg_sf_int, arg_sf_ext) |> 
  vect()

# verifico límites internos
plot(l, axes = FALSE)

# temperatura ------------------------------------------------------------

# temperatura máxima y mínima, de los últimos 50 años
data_temp = climateR::getTerraClim(AOI = arg0, 
  varname = c("tmax", "tmin"),
  startDate = "1974-01-01",
  endDate  = "2024-01-01")

# calculo la temperatura media a partir del rango
prom_mes_temp <- (data_temp$tmax + data_temp$tmin) / 2

# calculo la media de los 50 años, de todos los promedios mensuales
prom_temp <- mean(mean_temp_monthly, na.rm = TRUE)

# convierto a POSGAR 2007
prom_temp_proj <- project(prom_temp, "EPSG:5346")

# recorto a Argentina
arg_temp <- crop(prom_temp_proj, arg0, mask = TRUE)

# figura
plot(arg_temp)

# precipitación ----------------------------------------------------------

# precipitación, de los últimos 50 años
data_ppt = climateR::getTerraClim(AOI = arg0, 
  varname = "ppt",
  startDate = "1974-01-01",
  endDate  = "2024-01-01")

# calculo la media de los 50 años, de todos los promedios mensuales
prom_ppt <- mean(data_ppt$ppt, na.rm = TRUE)

# convierto a POSGAR 2007
prom_ppt_proj <- project(prom_ppt, "EPSG:5346")

# recorto a Argentina
arg_ppt <- crop(prom_ppt_proj, arg0, mask = TRUE)

# figura
plot(arg_ppt)

# combino datos ----------------------------------------------------------

# combino temperatura y precipitación y cambio de nombre
temp_ppt <- c(arg_temp, arg_ppt)
names(temp_ppt) <- c("temp", "ppt")

# convierto a tibble
temp_ppt_tbl <- temp_ppt |> 
  as.data.frame(xy = TRUE) |> 
  tibble()

# genero los cuartiles de ambas propiedades
data_bi <- bi_class(temp_ppt_tbl,
  x = temp, 
  y = ppt, 
  style = "quantile",
  dim = 3)

# verifico las clases con un histograma para visualizar la frecuencia
data_bi |> 
  count(bi_class) |> 
  ggplot(aes(x = bi_class, y = n)) +
  geom_col() +
  labs(x = "Clases bivariadas", y = "Frecuencia")

write_csv(data_bi, "clima_bivariado/data_bi.csv")
data_bi <- read_csv("clima_bivariado/data_bi.csv")

# mapa -------------------------------------------------------------------

# título y subtítulo
mi_titulo <- glue(
  "Patrones de <b style='color: {c1}'>precipitación</b> y ",
  "<b style='color: {c2}'>temperatura</b>"
)

mi_subtitulo <- glue(
  "Promedios a partir de los últimos <b>50 años</b> en ",
  "<b style='color: {c5}'>Argentina</b>"
)

# ícono de flecha, para el título de eje de la leyenda bivariada
flecha <- glue("<span style='font-family:jet;'>&#xea9c;</span>")

# leyenda bivariada
leyenda <- bi_legend(pal = paleta,
  flip_axes = FALSE,
  rotate_pal = FALSE,
  dim = 3,
  arrows = FALSE
) +
  labs(
    x = glue("Temperatura (°C) {flecha}"),
    y = glue("Precipitación (mm) {flecha}")
  ) +
  # coord_equal(clip = "off") +
  theme_void(base_size = 20) +
  theme(
    plot.background = element_rect(fill = c3, color = NA),
    axis.title.y = element_markdown(angle = 90, hjust = .3),
    axis.title.x = element_markdown(hjust = .3)
  )

# mapa
g <- ggplot() +
  # temperatura y precipitación
  geom_tile(
    data = data_bi, mapping = aes(x = x, y = y, fill = bi_class), color = NA,
    show.legend = FALSE
  ) +
  # límites internos
  geom_spatvector(data = l, fill = NA, color = c3, linewidth = .25) +
  # Argentina
  # geom_spatvector(data = arg0, fill = NA, color = c4, linewidth = 1) +
  bi_scale_fill(pal = paleta, dim = 3, flip_axes = FALSE, rotate_pal = FALSE) +
  labs(
    title = mi_titulo, 
    subtitle = mi_subtitulo,
    caption = mi_caption
  ) +
  theme_void(base_size = 14) +
  theme(
    plot.background = element_rect(fill = c3, color = NA),
    plot.title = element_markdown(
      hjust = 0.5, face = "bold", family = "ubuntu", size = 40
    ),
    plot.subtitle = element_markdown(hjust = 0.5, family = "ubuntu", size = 30),
    plot.caption = element_markdown(
      size = 25, face = "bold", family = "ubuntu", color = c2, hjust = 0
    )
  )


# combino mapa y leyenda
g2 <- g + inset_element(
  leyenda, left = .6, right = .9, bottom = .15, top = .45
) +
plot_annotation(
  theme = theme(
    plot.background = element_rect(fill = NA, color = c5, linewidth = 3),
    plot.margin = margin(b = 10, t = 11, r = 63, l = 63)
  )
)

# guardo
ggsave(
  plot = g2,
  filename = "clima_bivariado/viz.png",
  width = 30,
  height = 60,
  units = "cm"
)

# abro
browseURL(glue("{getwd()}/clima_bivariado/viz.png"))
