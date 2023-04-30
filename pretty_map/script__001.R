
# paquetes ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(MetBrewer)
library(showtext)
library(ggtext)
library(glue)

# fuente ------------------------------------------------------------------

# título
font_add_google(name = "Lobster Two", family = "lobster", db_cache = FALSE)

# caption
font_add_google(name = "Carlito", family = "carlito", db_cache = FALSE)

showtext_auto()
showtext_opts(dpi = 300)

font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")

# colores
col1 <- "#7c4b73" # archambault
col2 <- "#8282aa" # cassatt1
col3 <- "#c9c9dd" # cassatt1
col4 <- "#eaf3ff" # benedictus

# caption
fuente <- glue("Datos: <span style='color:{col1};'>Prettymapp & OpenStreetMap</span>")
autor <- glue("Autor: <span style='color:{col1};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
usuario <- glue("<span style='color:{col1};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue("{fuente} {sep} {autor} {sep} {icon_github} {icon_twitter} {usuario}")

# datos -------------------------------------------------------------------

# link a Prettymapp
browseURL("https://chrieke-prettymapp-streamlit-prettymappapp-1k0qxh.streamlit.app/")

# lista de archivos .geojson
lista_geojson <- list.files("pretty_map/geojson/", full.names = TRUE)

# tabla de archivos .geojson con nombre de ciudad y provincia
geojson_tbl <- tibble(archivo = lista_geojson) |>
  mutate(provincia = str_extract(archivo, "(?<=/)[^,]+")) |>
  mutate(provincia = str_remove(provincia, "geojson/")) |>
  mutate(ciudad = str_extract(archivo, "(?<=,)[^/]+")) |>
  mutate(ciudad = str_remove(ciudad, ".geojson")) |>
  mutate(ciudad = case_when(provincia == "Tierra del Fuego" ~ "Ushuaia",
                            TRUE ~ ciudad))

# función p/leer, generar la figura y guardar
f_prettymap <- function(x) {
  
  # lectura del vector
  pcia_sf <- st_read(geojson_tbl$archivo[x])
  
  # asigno colores a los polígonos usando {MetBrewer}
  # cada clase de cobertura le corresponde una paleta de colores, donde cada
  # polígono es rellenado con un color aleatorio
  pcia_sf2 <- pcia_sf |> 
    mutate(fi = "grey", .before = geometry) |> 
    rowwise() |> 
    mutate(fi = case_when(
      landcover_class == "urban" ~ sample(as.character(met.brewer("Pillement")), size = 1, replace = TRUE),
      landcover_class == "grassland" ~ sample(as.character(met.brewer("VanGogh3")), size = 1, replace = TRUE),
      landcover_class == "streets" ~ sample(as.character(met.brewer("Cassatt1")), size = 1, replace = TRUE),
      landcover_class == "water" ~ sample(as.character(met.brewer("Hokusai2")), size = 1, replace = TRUE),
      TRUE ~ "black")) 
  
  # recuadro
  bb <- st_bbox(pcia_sf2) |> 
    st_as_sfc()
  
  # nombre de ciudad y provincia
  tag_pcia <- geojson_tbl$provincia[x]
  tag_ciudad <- geojson_tbl$ciudad[x]
  
  # si el nombre de la ciudad es igual al de la provincia, se agrega 'Capital'
  if (tag_pcia == tag_ciudad) {
    tag_f <- glue("{tag_pcia} Capital")
  } else {
    tag_f <- glue("{tag_ciudad}, {tag_pcia}")
  }
  
  if (tag_pcia == "Capital Federal") {
    tag_f <- "Ciudad Autónoma de<br>Buenos Aires"
  }
  
  # figura
  g1 <- ggplot() +
    # fondo 
    geom_sf(data = bb, fill = col4, color = NA) +
    geom_sf(
      data = pcia_sf2, 
      aes(fill = I(alpha(fi, .85)), color = highway),
      linewidth = .5, show.legend = FALSE) +
    # recuadro
    geom_sf(data = bb, fill = NA, color = col2, linewidth = 3) +
    coord_sf(expand = TRUE, clip = "off") +
    scale_color_met_d(palette_name = "Signac", direction = -1) +
    labs(
      tag = tag_f,
      caption = mi_caption) +
    theme_void() +
    theme(
      plot.background = element_rect(
        fill = col3, color = col2, linewidth = 6),
      plot.tag = element_textbox_simple(
        family = "lobster", size = 80, color = col2, hjust = 0, 
        margin = margin(50, 0, 0, 0)),
      plot.tag.position = c(.05, .15),
      plot.caption = element_markdown(
        hjust = .5, size = 16, color = col2, family = "carlito",
        margin = margin(214.6, 0, 7, 0)),
      plot.margin = margin(0, 0, 0, 0)
    )

  # guardo la figura
  ggsave(plot = g1,
         filename = glue("pretty_map/map/{str_remove_all(tag_pcia, ' ')}",
                         "_{str_remove_all(tag_ciudad, ' ')}.png"),
         width = 30,
         height = 38.5,
         units = "cm",
         dpi = 300)

}

# map() sobre todas las filas de .geojson, ciudad y provincia
map(.x = 1:nrow(geojson_tbl), ~ f_prettymap(x = .x))
