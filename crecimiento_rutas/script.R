# paquetes ----------------------------------------------------------------

library(terra)
library(tidyterra)
library(magick)
library(showtext)
library(glue)
library(ggtext)
library(ggplot2)
library(purrr)

# colores & fuentes -------------------------------------------------------

c1 <- "dodgerblue"
c2 <- "violetred"
c3 <- "grey95"
c4 <- "grey90"

font_add(family = "ubuntu", regular = "fuentes/Ubuntu-Regular.ttf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
fuente <- glue(
  "Datos: <b style='color: {c1};'>Instituto Geográfico Nacional</b>"
)
autor <- glue("<span style='color:{c1};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:jet;'>&#xf099;</span>")
icon_instagram <- glue("<span style='font-family:jet;'>&#xf16d;</span>")
icon_github <- glue("<span style='font-family:jet;'>&#xf09b;</span>")
icon_mastodon <- glue("<span style='font-family:jet;'>&#xf0ad1;</span>")
icon_bsky <- glue("<span style='font-family:jet;'>&#xe28e;</span>")
usuario <- glue("<span style='color:{c1};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{fuente}<br>{autor} {sep} {icon_github} {icon_twitter} {icon_instagram} ",
  "{icon_mastodon} {icon_bsky} {usuario}"
)

# funciones ---------------------------------------------------------------

# lee vectores y transforma a POSGAR
f_proj <- function(v) {
  project(vect(v), "EPSG:5346")
}

# genera figura y almacena
f_gg <- function(x) {
  
  if (x < 10) {
    i <- paste0("0", x)
  } else {
    i <- x
  }
  
  g <- ggplot() +
    geom_spatvector(data = arg, fill = c4, color = NA) +
    geom_spatvector(
      data = lista_crop_pro[[x]], aes(color = "pro"), linewidth = .1,
      show.legend = TRUE, key_glyph = "path"
    ) +
    geom_spatvector(
      data = lista_crop_nac[[x]], fill = NA, aes(color = "nac"), linewidth = .2, 
      show.legend = TRUE, key_glyph = "path"
    ) +
    scale_color_manual(
      breaks = c("nac", "pro"),
      labels = c("Ruta Nacional", "Ruta Provincial"),
      values = c(c1, c2)
    ) +
    labs(color = NULL, caption = mi_caption) +
    guides(
      color = guide_legend(override.aes = list(linewidth = 1))
    ) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = c3, color = NA),
      plot.caption = element_markdown(
        family = "ubuntu", size = 8, color = c2, lineheight = 1.2,
        margin = margin(b = 5)
      ),
      legend.text = element_text(family = "ubuntu"),
      legend.position = "inside",
      legend.position.inside = c(.7, .3),
      legend.key.width = unit(20, "pt")
    )
  
  ggsave(
    plot = g,
    filename = paste0("crecimiento_rutas/fig/", i, ".png"),
    width = 1000,
    height = 2084,
    units = "px"
  )
  
  print(glue::glue("\n\n--- Figura {i} generada ---\n\n"))
}

# datos -------------------------------------------------------------------

# límites de Argentina, continental
arg <- f_proj("extras/arg_continental.gpkg")

# rutas nacionales, provinciales y terciarias
r_nac <- f_proj("ign_red_vial/LíneaRed vial nacional.json")
r_pro <- f_proj("ign_red_vial/LíneaRed vial provincial.json")

# coordenadas del obelisco, origen de la expansión
o <- vect(
  data.frame(x = -58.38162, y = -34.60376), geom = c("x", "y"),
  crs = "EPSG:4326"
) |> 
  project("EPSG:5346")

# figura ------------------------------------------------------------------

# lista de buffers alrededor del obelisco
# lista de recortes de rutas por cada buffer
lista_buffer_nac <- map(seq(50, 2500, 25)*1e3, ~buffer(o, .x, quadsegs = 250))
lista_crop_nac <- map(lista_buffer_nac, ~terra::crop(r_nac, .x))
lista_crop_pro <- map(lista_buffer_nac, ~terra::crop(r_pro, .x))

# verifico figuras
f_gg(60)
browseURL("crecimiento_rutas/fig/60.png")

# genero todas las figuras
map(1:length(lista_buffer_nac), f_gg)

# animación ---------------------------------------------------------------

# genera la animación (.mp4) a partir de las figuras (.png)
av::av_encode_video(
  input = list.files(
    path = "crecimiento_rutas/fig",
    full.names = TRUE, pattern = ".png"
  ),
  output = "crecimiento_rutas/viz.mp4"
)
