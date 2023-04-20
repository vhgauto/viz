
# paquetes ----------------------------------------------------------------

library(tidyverse)
library(sf)
library(elevatr)
library(terra)
library(fontawesome)
library(glue)
library(ggtext)
library(showtext)

# fuentes -----------------------------------------------------------------

font_add_google(name = "Cormorant", family = "cormorant") # título
font_add_google(name = "Anuphan", family = "anuphan", db_cache = FALSE) # resto del texto
font_add_google(name = "Abril Fatface", family = "abril") # título

showtext_auto()
showtext_opts(dpi = 300)

# íconos
font_add("fa-reg", "icon/Font Awesome 5 Free-Regular-400.otf")
font_add("fa-brands", "icon/Font Awesome 5 Brands-Regular-400.otf")
font_add("fa-solid", "icon/Font Awesome 5 Free-Solid-900.otf")

# caption
icon_twitter <- glue("<span style='font-family:fa-brands; color:black;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands; color:black;'>&#xf09b;</span>")
datos <- glue("<span style='color:black;'>Datos:</span> <span style='color:#8d1c06;'>IGN, AWS</span>")
autor <- glue("<span style='color:black;'>Autor:</span> <span style='color:#8d1c06;'>**Víctor Gauto**</span>")
sep <- glue("<span style = 'color:black;'>**|**</span>")
usuario <- glue("<span style = 'color:#8d1c06;'>**vhgauto**</span>")

mi_caption <- glue("{datos} {sep} {autor} {sep} {icon_github} {icon_twitter} {usuario}")

# datos -------------------------------------------------------------------

# tutotial
browseURL("https://www.youtube.com/watch?v=zoLChBALc1k")

# script tutorial
browseURL("https://github.com/milos-agathon/create-crisp-topographic-maps-with-r/blob/main/R/main.r")

# leo vector con Argentina
arg <- st_read("mapa_topografico/arg_continental.gpkg")

# sistema de referencia
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

# países limítrofes
chile <- giscoR::gisco_get_countries(
  year = "2020",
  epsg = "4326",
  resolution = "10",
  country = "CL"
) |>
  sf::st_transform(crs = crsLONGLAT) |> 
  st_make_valid()

brasil <- giscoR::gisco_get_countries(
  year = "2020",
  epsg = "4326",
  resolution = "10",
  country = "BR"
) |>
  sf::st_transform(crs = crsLONGLAT) |> 
  st_make_valid()

uruguay <- giscoR::gisco_get_countries(
  year = "2020",
  epsg = "4326",
  resolution = "10",
  country = "UY"
) |>
  sf::st_transform(crs = crsLONGLAT) |> 
  st_make_valid()

paraguay <- giscoR::gisco_get_countries(
  year = "2020",
  epsg = "4326",
  resolution = "10",
  country = "PY"
) |>
  sf::st_transform(crs = crsLONGLAT) |> 
  st_make_valid()

bolivia <- giscoR::gisco_get_countries(
  year = "2020",
  epsg = "4326",
  resolution = "10",
  country = "BO"
) |>
  sf::st_transform(crs = crsLONGLAT) |> 
  st_make_valid()

# evita errores en la unión de los países
sf_use_s2(FALSE)

# vector con todos los países limítrofes unidos
lim <- st_union(chile, brasil) |> 
  st_union(uruguay) |> 
  st_union(paraguay) |> 
  st_union(bolivia)

# límites
df2 <- tibble(lon = c(-76.992188, -51.635742, -51.635742, -76.992188, -76.992188),
              lat = c(-56.170023, -56.170023, -21.289374, -21.289374, -56.170023))
# convierto a 'sf'
df_sf2 <- df2 %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  mutate(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

# recorto el vector de países limítrofes
lim_crop <- st_crop(lim, df_sf2)

# agrego un buffer p/cubrir las diferencias entre el ráster de elevación y el 
# vector de países limítrofes
lim_crop_buf <- st_buffer(lim_crop, .05)

# datos de elevación, crop al bbox
country_elevation <- elevatr::get_elev_raster(
  locations = lim_crop,
  z = 7, # nivel de zoom, [1; 14], mientras más alto, mejor resolución
  clip = "bbox",
  src = "aws"
)

# convierto a raster
country_elevation2 <- country_elevation |>
  terra::rast()

# elevación de Argentina
arg_elev <- terra::mask(country_elevation2, arg)

# sitio web p/obtener bbox de cualquier sitio del planeta, arrastrando el mouse
browseURL("http://bboxfinder.com/")

# convierto de raster a df
country_elevation_df <- country_elevation2 |> 
  as.data.frame(xy = TRUE) |>
  na.omit()

arg_elev_df <- arg_elev |> 
  as.data.frame(xy = TRUE) |>
  na.omit()

# cambio el nombre de la columna de elevación
names(country_elevation_df)[3] <- "elevation"
names(arg_elev_df)[3] <- "elevation"

# figura ------------------------------------------------------------------

# título
tt <- tibble(x = -63.2, y = -45, 
             label = "Mapa físico\nde Argentina")

# caption
tc <- tibble(y = -55, x = -64.1,
             label = mi_caption)

# relación de aspecto del bbox de Argentina, p/el tamaño del .png
st_bbox(lim_crop)
r <- (55.97787 - 21.28937)/(75.69771 - 51.63574)

# dimensiones de la figura .png
ancho <- 30
alto <- ancho*r

# figura
country_map <- ggplot() +
  # ráster elevación bbox
  geom_raster(data = country_elevation_df,
              aes(x = x, y = y, fill = elevation), alpha = 1) +
  # paíces limítrofes
  geom_sf(data = lim_crop_buf, fill = "grey30", color = "grey30") +
  # elevación Argentina
  geom_raster(data = arg_elev_df,
              aes(x = x, y = y, fill = elevation), alpha = 1) +
  # contorno Arg
  geom_sf(data = arg, fill = NA, color = "black", linewidth = .5) +
  # título
  geom_text(data = tt, aes(x =x, y = y, label = label), family = "abril",
            hjust = 0, vjust = 1, size = 17, color = "#8d1c06") +
  # caption
  geom_richtext(data = tc, aes(x = x, y = y, label = label), family = "anuphan",
            hjust = 0, vjust = 1, size = 4, color = "gold", label.color = NA,
            fill = NA) +
  # manual
  marmap::scale_fill_etopo() +
  coord_sf(crs = crsLONGLAT, clip = "on", expand = FALSE) +
  # ejes
  theme_void() +
  theme(
    legend.position = "none",
    panel.ontop = TRUE,
    plot.background = element_rect(fill = NA, color = "#8d1c06", linewidth = 2),
    panel.background = element_rect(fill = NA, color = "#8d1c06", linewidth = 2),
    # plot.margin = margin(25, 96, 10, 96),
    plot.margin = margin(1.1, 0, 1.1, 0)
  )

# guardo
ggsave(plot = country_map,
       filename = "mapa_topografico/viz.png",
       width = 21.9,
       height = 40.4,
       units = "cm",
       dpi = 300)

# abro
browseURL("mapa_topografico/viz.png")
