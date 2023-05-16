
# tutorial ----------------------------------------------------------------

browseURL("https://www.youtube.com/watch?v=2VHuaFqtAsY")

# temperatura de COPERNICUS

# paquetes ----------------------------------------------------------------

library(tidyverse)
library(sf)
library(KrigR)
library(classInt)
library(gganimate)
library(glue)
library(terra)
library(ggtext)
library(showtext)

# fuente ------------------------------------------------------------------

# años
font_add_google(name = "Bebas Neue", family = "bebas")
# meses
font_add_google(name = "Abril Fatface", family = "abril")
# temperaturas
font_add_google(name = "Inconsolata", family = "inconsolata")
# caption, leyenda
font_add_google(name = "Carlito", family = "carlito", db_cache = FALSE)

showtext_auto()
showtext_opts(dpi = 300)

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

c1 <- "#ffd352"

# caption
fuente <- glue("Datos: <span style='color:{c1};'>Copernicus, ERA5-Land</span>")
autor <- glue("Autor: <span style='color:{c1};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
usuario <- glue("<span style='color:{c1};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue("{fuente} {sep} {autor} {sep} {icon_github} {icon_twitter} {usuario}")

# datos -------------------------------------------------------------------

# base de datos
browseURL("https://cds.climate.copernicus.eu/#!/home")
# ERA5 provides hourly estimates of a large number of atmospheric, land and 
# oceanic climate variables

# ERA5-Land monthly averaged data from 1950 to present
browseURL(glue("https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-
               era5-land-monthly-means?tab=overview"))

# en la pestaña: Download data
# Product type -> Monthly averaged reanalysis
# Variable -> 2m temperature (en KELVIN)
# Year -> 2023 (solo para ver qué tal)
# Month -> todos los meses
# Time -> 00.00 (son promedios mensuales, así que la hora no importa)
# Geographical area -> Sub-region extraction -> N:-21.37; S:-55.77; E:-52.64; O:-74.7
# Format -> Zipped NetCDF-3 (experimental)
# SHOW API REQUEST

# lo importante es -> 
# DATASET: 'reanalysis-era5-land-monthly-means'
# VARIABLE: '2m_temperature'

# En mi perfil encuentro mi API key

credenciales <- read_table("clima_temperatura/key.txt", col_names = "key")

api_key <- credenciales$key[2]
api_user <- credenciales$key[1]

# rango de fechas, desde 2010 hasta la actualidad (abril 2023)
start_date <- "2010-01-01"
end_date <- "2023-04-01"

# vector de Argentina, para recortar los datos
arg_sf <- st_read("clima_temperatura/arg_continental.gpkg")

# bbox del país
bb <- st_bbox(arg_sf) |> st_as_sfc()

# factor de los meses
meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", 
           "Oct", "Nov", "Dic")

meses <- factor(meses, levels = meses)

# función para obtener los datos
arg_temp <- KrigR::download_ERA(
  Variable = "2m_temperature",
  DataSet = "era5-land",
  DateStart = start_date,
  DateStop = end_date,
  TResolution = "month",
  TStep = 1,
  Dir = "clima_temperatura/datos",
  FileName = "arg_2m_temperature",
  Extent = as(arg_sf, "Spatial"),
  API_User = api_user,
  API_Key = api_key)

# copio los datos por las dudas
arg_temp2 <- arg_temp

# cambio los nombres de las layers por las fechas
names(arg_temp2) <- seq(ymd(start_date), ymd(end_date), by = "1 month")

# convierto el .nc a tibble
arg_temp_tbl <- as.data.frame(arg_temp2, xy = TRUE, na.rm = TRUE) |> 
  as_tibble()

# arreglo los datos
arg_temp_tbl2 <- arg_temp_tbl |>
  pivot_longer(cols = -c(x, y),
               names_to = "layer",
               values_to = "temp") |> 
  # remuevo 'X' de layer p/crear las fechas
  mutate(fecha = str_remove(layer, "X")) |> 
  mutate(fecha = ymd(fecha)) |> 
  # convierto a Celcius
  mutate(temp = temp - 273.15) |> 
  # obtengo el mes y el año
  mutate(mes = month(fecha)) |> 
  mutate(año = year(fecha)) |> 
  # selecciono columnas de interés
  dplyr::select(-fecha, -layer) |>
  # agrego los meses en texto (fct)
  mutate(mes = meses[mes])

# figura ------------------------------------------------------------------

# breaks
breaks <- classInt::classIntervals(
  arg_temp_tbl2$temp,
  n = 15, # bins
  style = "pretty" # estilo
  )$brks

# paleta de colores
cols <- colorRampPalette(rev(MetBrewer::met.brewer(palette_name = "Tam")))

# figura
arg_mapa <- ggplot(data = arg_temp_tbl2) +
  # mapa
  geom_raster(aes(x = x, y = y, fill = temp)) +
  # contorno, p/mantener la relación de aspecto
  geom_sf(data = bb, color = NA, fill = NA) +
  # grilla
  facet_grid(año ~ mes, switch = "y") +
  # manual
  scale_fill_gradientn(
    name = "Promedios\nmensuales (°C)",
    colors = cols(n = length(breaks)),
    limits = c(min(breaks), max(breaks)),
    breaks = seq(-20, 30, 10)) +
  coord_sf() +
  # caption
  labs(caption = mi_caption) +
  # tema
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    plot.margin = margin(10, 10, 10, 10),
    plot.caption = element_markdown(
      hjust = .4, color = "white", size = 15, margin = margin(20, 0, 0, 0),
      family = "carlito"),
    strip.text.y = element_markdown(
      family = "bebas", size = 30, color = "white"),
    strip.text.x = element_markdown(
      family = "bebas", size = 30, color = "white", margin = margin(0, 0, 10, 0)),
    legend.margin = margin(10, 0, 0, 15),
    legend.key.height = unit(10, "mm"),
    legend.key.width = unit(42, "mm"),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.text = element_text(
      color = "white", size = 30, family = "inconsolata"),
    legend.title = element_text(
      color = "white", size = 25, family = "carlito"),
    legend.box.margin = margin(0, 10, 0, 0),
    legend.box = "horizontal")

# guardo
ggsave(
  plot = arg_mapa,
  filename = "clima_temperatura/viz.png",
  width = 30,
  height = 72,
  units = "cm",
  dpi = 300)

# abro
browseURL("clima_temperatura/viz.png")
