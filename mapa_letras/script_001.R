
# inspiración
browseURL("https://nrennie.rbind.io/blog/creating-typewriter-maps-r/")

# uso de {emojifont}
browseURL("https://cran.r-project.org/web/packages/emojifont/vignettes/emojifont.html")
browseURL("https://guangchuangyu.github.io/2015/12/use-emoji-font-in-r/")

# paquetes ----------------------------------------------------------------

library(showtext)
library(glue)
library(emojifont)
library(ggtext)
library(tidyverse)

# fuentes -----------------------------------------------------------------

c1 <- "#FFCE00"
c2 <- "white"
c3 <- "#122361"

# texto gral
font_add_google(name = "Ubuntu", family = "ubuntu")
# rango de alturas
font_add_google(name = "Victor Mono", family = "victor", db_cache = FALSE)
# título
font_add_google(name = "Tsukimi Rounded", family = "tsukimi", db_cache = FALSE)
# emojis
load.emojifont('OpenSansEmoji.ttf')

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
autor <- glue("<span style='color:{c1};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
icon_mastodon <- glue("<span style='font-family:fa-brands;'>&#xf4f6;</span>")
usuario <- glue("<span style='color:{c1};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{autor} {sep} {icon_github} {icon_twitter} {icon_mastodon} {usuario}")

# datos -------------------------------------------------------------------

# mapa de Argentina, POSGAR 2007
arg_sf <- sf::st_read("mapa_topografico/arg_continental.gpkg") |> 
  sf::st_transform(crs = 5346)

# elevación de Argentina
altura_raster <- elevatr::get_elev_raster(
  locations = arg_sf,
  z = 1, # el mínimo tamaño de zoom, el de menos datos
  clip = "locations")

# convierto el ráster a matriz
altura_matriz <- terra::as.matrix(altura_raster, wide = TRUE)

# cambio los nombres
colnames(altura_matriz) <- 1:ncol(altura_matriz)

# convierto a tabla larga y tibble
altura_tbl <- altura_matriz |> 
  as_tibble() |> 
  mutate(y = row_number()) |> 
  pivot_longer(cols = -y, names_to = "x", values_to = "altura") |> 
  mutate(x = as.numeric(x)) |> 
  drop_na()

# emojis de animales
animales <- emoji(
  c("rabbit", "cow", "bird", "snake", "mouse", "horse", "bear", "cat"))

# tibble con números consecutivos y emojis
animales_tbl <- data.frame(
  rango_numero = seq_len(length(animales)),
  valor_emoji = animales)

# combino las alturas con los emojis
d <- altura_tbl |> 
  mutate(rango = cut_number(altura, n = length(animales))) |> 
  mutate(rango_numero = as.numeric(rango)) |> 
  left_join(animales_tbl, by = join_by(rango_numero))

# figura ------------------------------------------------------------------

# paletas de colores/relleno, como rampas de color
f_color <- colorRampPalette(
  MoMAColors::moma.colors(palette_name = "Palermo") |> rev())

f_fill <- colorRampPalette(
  terrain.colors(8))

# rangos de alturas
d_rangos <- d |> 
  distinct(rango) |> 
  separate_wider_delim(
    cols = rango, delim = ",", names = c("minimo", "maximo")) |> 
  mutate(minimo = parse_number(minimo), maximo = parse_number(maximo)) |> 
  mutate(minimo = if_else(minimo < 0, 0, minimo)) |> 
  arrange(minimo) |> 
  mutate(across(everything(), round)) |> 
  mutate(
    label_rango = case_when(
      minimo == min(minimo) ~ glue("< {min(maximo)}"),
      minimo == max(minimo) ~ glue("> {max(minimo)}"),
      .default = glue("{minimo} - {maximo}"))) |> 
  select(label_rango)

# escala de alturas
d_escala <- tibble(
  x = 30,
  y = seq(50, 65, length.out = length(animales)),
  label = animales,
  valor = 1:8) |> 
  bind_cols(d_rangos)

# figura
g <- ggplot(d, aes(x, y)) +
  # topografía de Argentina
  geom_tile(
    aes(fill = rango_numero), color = c3, linewidth = .7, show.legend = FALSE) +
  geom_text(
    aes(label = valor_emoji, color = rango_numero), family = "OpenSansEmoji", 
    show.legend = FALSE, size = 5) +
  # escala de topografía de Argentina
  geom_tile(
    data = d_escala, aes(x, y, fill = valor), color = c3, linewidth = .3,
    show.legend = FALSE, width = 2, height = 2) +
  geom_text(
    data = d_escala, aes(x, y, label = label, color = valor), size = 9,
    family = "OpenSansEmoji", show.legend = FALSE) +
  # aclaración de la escala
  geom_text(
    data = d_escala, aes(x+1.3, y, label = label_rango), color = c2, hjust = 0,
    family = "victor", show.legend = FALSE, size = 6) +
  annotate(
    geom = "text", x = min(d_escala$x), y = min(d_escala$y), 
    label = "Rangos de\naltura (m)", family = "ubuntu", hjust = 0, vjust = -.6, 
    size = 10, color = c2) +
  scale_y_reverse() +
  scale_colour_gradientn(colors = f_color(8)) +
  scale_fill_gradientn(colors = f_fill(8)) +
  labs(title = "ARGENTINA", caption = mi_caption) +
  coord_fixed() +
  theme_void() +
  theme(
    plot.margin = margin(t = 8, b = 8),
    plot.background = element_rect(
      fill = c3, color = c1, linewidth = 3),
    plot.title.position = "plot",
    plot.title = element_text(
      family = "tsukimi", size = 80, hjust = .5, color = c1,
      margin = margin(t = 10, b = -50)),
    plot.caption = element_markdown(
      family = "ubuntu", color = c2, size = 18, 
      margin = margin(r = 10, b = 10)
      )
    )

# guardo
ggsave(
  plot = g,
  filename = "mapa_letras/viz.png",
  width = 30,
  height = 68,
  units = "cm")

# abro
browseURL("mapa_letras/viz.png")
