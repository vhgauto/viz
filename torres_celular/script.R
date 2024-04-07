
# paquetes ----------------------------------------------------------------

library(glue)
library(showtext)
library(ggtext)
library(sf)
library(magick)
library(tidyverse)

# fuentes -----------------------------------------------------------------

# colores
c1 <- "#76BE72"
c2 <- "#F04C44"
c3 <- "#442224"
c4 <- "grey95"
c5 <- "grey92"

# fuente: Ubuntu
font_add(
  family = "ubuntu", 
  regular = "fuentes/Ubuntu-Regular.ttf",
  bold = "fuentes/Ubuntu-Bold.ttf",
  italic = "fuentes/Ubuntu-Italic.ttf")

# íconos y monoespaciada
font_add(
  family = "jet",
  regular = "fuentes/JetBrainsMonoNLNerdFontMono-Regular.ttf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue(
  "<b>Datos:</b> <span style='color:{c2};'>OpenCelliD</span>")
autor <- glue("<span style='color:{c2};'>Víctor Gauto</span>")
icon_twitter <- glue("<span style='font-family:jet;'>&#xeb72;</span>")
icon_instagram <- glue("<span style='font-family:jet;'>&#xf16d;</span>")
icon_github <- glue("<span style='font-family:jet;'>&#xf09b;</span>")
icon_mastodon <- glue("<span style='font-family:jet;'>&#xf0ad1;</span>")
usuario <- glue("<span style='color:{c2};'>vhgauto</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{fuente}<br>{autor} {sep} <b>{icon_github} {icon_twitter} {icon_instagram} ",
  "{icon_mastodon}</b> {usuario}")

# datos -------------------------------------------------------------------

# referencia de los nombres de las columnas
# browseURL("https://community.opencellid.org/t/documenting-the-columns-in-the-downloadable-cells-database-csv/186")

# Argentina, continental
arg_sf <- st_read("extras/arg_continental.gpkg") |> 
  st_transform(crs = 5346)

# extensión de Argentina, para ampliar el mapa
bb <- st_bbox(arg_sf)
ext <- terra::vect(arg_sf) |> terra::ext()
bb_sf <- terra::vect(ext*1.1, crs = "EPSG:5346") |> 
  st_as_sf() |> 
  st_bbox()

# datos de las torres de radio
d <- read_csv(
  "torres_celular/722.csv",
  col_names = 1:14,
  col_select = c(1, 7, 8, 12, 13)) |> 
  rename(radio = X1, lon = X7, lat = X8, primero = X12, ultimo = X13) |> 
  mutate(primero = as_datetime(primero)) |> 
  mutate(ultimo = as_datetime(ultimo)) |> 
  mutate(primero = as.Date(primero)) |> 
  mutate(ultimo = as.Date(ultimo)) |> 
  mutate(año = year(primero), mes = month(primero)) |> 
  mutate(radio = fct(radio, levels = c("GSM", "UMTS", "LTE"))) |> 
  st_as_sf(coords = c("lon", "lat")) |> 
  st_set_crs(value = 4326) |> 
  st_transform(crs = 5346)

# extraigo todos los meses y años en la base de datos
v_año_mes <- tibble(d) |> 
  distinct(año, mes) |> 
  mutate(dia = 1) |> 
  mutate(fecha = make_date(year = año, month = mes, day = dia)) |> 
  arrange(fecha) |> 
  pull(fecha)

# máxima cantidad de torres de radio
l_max <- count(tibble(d), radio, sort = TRUE) |> 
  arrange(radio) |> 
  pull(n) |> 
  max()

# paleta de colores por tipo de radio
paleta_radio <- c(c1, c2, c3)
names(paleta_radio) <- c("GSM", "UMTS", "LTE")

# posición mínima/máxima horizontal del segmento
eje_x_min <- bb_sf$xmax*.895
eje_x_max <- bb_sf$xmax*.99

# posición vertical, de los segmentos según tipo de radio
eje_y_gsm <- bb_sf$ymin*1.402
eje_y_umts <- eje_y_gsm - 2.15e5
eje_y_lte <- eje_y_gsm - 2*2.15e5

eje_y_radio <- c(eje_y_gsm, eje_y_umts, eje_y_lte)
names(eje_y_radio) <- c("GSM", "UMTS", "LTE")

# función para convertir la cantidad de torres de radio a longitud del segmento
f_escala <- function(x) {
  eje_x_min + x*(eje_x_max - eje_x_min)/l_max
}

# función que genera mapa con los sitios de las torres de radio
f_mapa <- function(año_mes) {
  
  # contador para seguimiento en consola
  l <- length(v_año_mes)
  n <- which(v_año_mes == año_mes)
  
  n <- case_when(
    n <= 9 ~ glue("00{n}"),
    n <= 99 ~ glue("0{n}"),
    .default = glue("{n}")
  )
  
  print(glue("-- {n} de {l} --"))
  
  # etiquetas de año y mes
  año_label <- year(año_mes)
  mes_label <- str_to_upper(format(año_mes, "%B"))
  
  mi_title <- glue(
    "<span style='font-size:20px;'>{año_label}</span><br>",
    "<span style='font-size:10px;'>{mes_label}</span>")
  
  # filtro los datos
  e <- d |> 
    filter(primero <= año_mes)
  
  # cantidad de torres, de acuerdo al tipo
  # n_radio <- count(e, radio) |> 
  #   pull(n)
  
  # color de la leyenda
  label_tbl <- count(e, radio) |> 
    mutate(color = paleta_radio[radio]) |> 
    mutate(label = glue("{radio}<br><span style='color:{color}'>{n}</span>")) |> 
    arrange(radio)
  
  scale_color_names <- pull(label_tbl, radio)
  scale_color_labels <- pull(label_tbl, label)
  scale_color_values <- pull(label_tbl, color)
  
  # extensión
  d_l <- count(tibble(e), radio) |>
    arrange(radio) |>
    mutate(
      x = eje_x_min,
      xend = f_escala(n),
      y = eje_y_radio[radio]
    ) |>
    mutate(yend = y) |>
    mutate(color = paleta_radio[radio])
  
  # figura
  g <- ggplot() +
    # Argentina
    geom_sf(data = arg_sf, fill = c5, color = NA, linewidth = .1) +
    # torres de radio
    geom_sf(data = e, aes(color = radio, size = radio), alpha = .5) +
    # líneas horizontales
    geom_segment(
      data = d_l, aes(x, y, xend = xend, yend = yend),  color = d_l$color,
      linewidth = 1) +
    # año
    annotate(
      geom = "text", x = I(.85), y = I(.95), label = año_label, vjust = 0,
      family = "jet", size = 6, color = c3) +
    # mes
    annotate(
      geom = "text", x = I(.85), y = I(.944), label = mes_label, vjust = 1,
      family = "jet", size = 3, color = c3) +
    scale_color_manual(
      breaks = scale_color_names,
      labels = scale_color_labels,
      values = scale_color_values) +
    scale_size_manual(values = c(.1, .4, .7)) +
    labs(color = NULL, caption = mi_caption) +
    coord_sf(
      xlim = c(bb_sf$xmin, bb_sf$xmax), 
      ylim = c(bb_sf$ymin, bb_sf$ymax),
      expand = FALSE) +
    guides(
      color = guide_legend(
        position = "inside",
        override.aes = list(size = 3, alpha = 1)),
      size = guide_none()
    ) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = c4, color = c3, linewidth = 1),
      plot.margin = margin(r = 1, b = 1),
      plot.caption = element_markdown(
        family = "ubuntu", color = c1, size = 7, lineheight = unit(1.1, "line"),
        margin = margin(t = -15, r = 5)),
      legend.position.inside = c(0.6, 0.4),
      legend.key.spacing.y = unit(.7, "line"),
      legend.justification.inside = c(0, 1),
      legend.text = element_markdown(family = "jet")
    )
  
  # guardo
  ggsave(
    plot = g,
    filename = glue("torres_celular/fig/{n}.png"),
    width = 1000,
    height = 2172,
    units = "px"
  )
  
}

# genero todos los 175 mapas
# LLEVA MUCHO TIEMPO
map(v_año_mes, f_mapa)

# archivos .png de los mapas
pngs <- list.files(path = "torres_celular/fig/", full.names = TRUE)[-c(1, 2)]

# duración del video (s)
dur <- 15
framerate <- length(v_año_mes)/dur

# leo las imágenes
i_pngs <- image_read(pngs)

# guardo .gif
image_write_gif(i_pngs, "torres_celular/i.gif", delay = 1/framerate)

# guardo .mp4
image_write_video(i_pngs, "torres_celular/i.mp4", framerate = framerate)

# abro .gif
browseURL("torres_celular/i.gif")

# abro .mp4
browseURL("torres_celular/i.mp4")
