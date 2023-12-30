
# Google Maps
browseURL("https://www.google.com/maps/d/viewer?mid=1sUgXir-dlyHnVkSN5EVSW_qgakk&usp=sharing ")

# Correo Argentino, América UPAEP- Buzones
browseURL("https://www.correoargentino.com.ar/filatelia/planes-de-emisiones/emisiones-2011")

# estampilla, 2011
browseURL("http://upaep.filatelia.free.fr/argentina.html")

# vector de departamentos de Argentina
browseURL("https://www.ign.gob.ar/NuestrasActividades/InformacionGeoespacial/CapasSIG")

# paquetes ----------------------------------------------------------------

library(sf)
library(glue)
library(patchwork)
library(ggtext)
library(showtext)
library(tidyverse)

# fuentes -----------------------------------------------------------------

c1 <- "#FFCE00"
c2 <- "white"
c3 <- "#152663"
c4 <- "#BE5660"

# texto gral
font_add_google(name = "Ubuntu", family = "ubuntu")
# cantidad de buzones
font_add_google(name = "Victor Mono", family = "victor", db_cache = FALSE)
# título
font_add_google(name = "Diphylleia", family = "diphylleia")

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
datos <- glue(
  "Datos: <span style='color:{c1};'>@matiasprofeta</span>, ",
  "<span style='color:{c1};'>@rescatandobuzones</span>")
autor <- glue("<span style='color:{c1};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
icon_mastodon <- glue("<span style='font-family:fa-brands;'>&#xf4f6;</span>")
usuario <- glue("<span style='color:{c1};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{datos}<br>",
  "{autor} {sep} {icon_github} {icon_twitter} {icon_mastodon} {usuario}")

# datos -------------------------------------------------------------------

# todos los vectores en POSGAR 2007, EPSG:5346

# provincias
pcias <- st_read("osm_extras/pcias_continental.gpkg") |> 
  select(provincia = NAM)

# departamentos
dptos <- st_read("buzones_arg/departamento.shp") |> 
  st_transform(crs = 5346) |> 
  st_make_valid() |> 
  st_crop(pcias) |> 
  select(departamento = nam)

# Buenos Aires
bsas <- pcias |> 
  filter(provincia == "Buenos Aires")

# Ciudad Autónoma de Buenos Aires
caba <- pcias |> 
  filter(provincia == "Ciudad Autónoma de Buenos Aires")

# departamentos de Buenos Aires
dptos_bsas <- terra::crop(terra::vect(dptos), terra::vect(bsas)) |> 
  st_as_sf()

# departamentos en Ciudad Autónoma de Buenos Aires
# son los únicos donde se llaman 'Comuna'
dptos_caba <- filter(dptos, str_detect(departamento, "Comuna"))

# buzones -----------------------------------------------------------------

# leo las capas del .kml
layers <- st_layers("buzones_arg/Buzones de Argentina.kml")

# nombres de las capas del .kml
capas <- layers$name

# función para leer las capas del .kml
f_kml <- function(x) {
  l <- st_read(
    dsn = "buzones_arg/Buzones de Argentina.kml",
    layer = x) |> 
    st_transform(crs = 5346)
  
  return(l)
}

# buzones de todo el país
buzones <- map(capas, f_kml) |> 
  list_rbind() |> 
  st_as_sf() |> 
  st_geometry() |> 
  st_sf() |> 
  rename(geometry = 1) |> 
  mutate(nro = row_number())

# buzones en Buenos Aires y Ciudad Autónoma de Buenos Aires
buzones_bsas <- st_intersection(bsas, buzones)
buzones_caba <- st_intersection(caba, buzones)

# figura ------------------------------------------------------------------

# conteo de buzones por pcias y dptos
# para la unión entre pcias y dptos utilizo puntos dentro de los dptos
centros_dptos <- st_point_on_surface(dptos)

centros_pcias <- st_join(centros_dptos, pcias)

polig_dptos <- st_join(dptos, centros_pcias) |> 
  rename(departamento = departamento.x) |> 
  select(-departamento.y)

# buzones por pcia y dpto
buzones_dptos <- st_join(polig_dptos, buzones)

# cantidad de buzones en bsas y caba
n_bsas <- buzones_dptos |> 
  filter(provincia == "Buenos Aires") |> 
  drop_na() |> 
  nrow()

n_caba <- buzones_dptos |> 
  filter(provincia == "Ciudad Autónoma de Buenos Aires") |> 
  drop_na() |> 
  nrow()

n_buzones <- nrow(buzones)

# porcentajes de buzones en bsas y caba
porcentaje_bsas <- gt::vec_fmt_percent(x = n_bsas/n_buzones, decimals = 0)
porcentaje_caba <- gt::vec_fmt_percent(x = n_caba/n_buzones, decimals = 0)

# título y subtítulo
mi_title <- "Buzones Argentinos"

mi_subtitle <- glue(
  "En **Argentina** hay {n_buzones} buzones. En Provincia de Buenos Aires<br>",
  "se encuentra el {porcentaje_bsas}. Capital Federal posee el ",
  "{porcentaje_caba}.")

# descripción de la estampilla
estampilla_label <- glue(
  "Estampilla emitida por<br>",
  "el **Correo Argentino**.<br>",
  "América UPAEP-Buzones,<br>",
  "2011.")

# figura de la estampilla con descripción
estampilla <- ggplot() +
  annotate(
    geom = "richtext", x = 0, y = 0, fill = NA, label.color = NA, hjust = 0,
    label = glue(
      "<img src='buzones_arg/estampilla.jpg' width='140'></img><br>",
      "{estampilla_label}"), family = "ubuntu", size = 5, color = c2) +
  coord_cartesian(xlim = c(-.01, .01), ylim = c(-.01, .01), expand = FALSE) +
  theme_void()

# figura del logo de Correo Argentino
logo <- ggplot() +
  annotate(
    geom = "richtext", x = 0, y = 0, fill = NA, label.color = NA, hjust = 0,
    label = glue(
      "<img src='buzones_arg/logo.png' width='100'></img>")) +
  coord_cartesian(xlim = c(-.01, .01), ylim = c(-.01, .01), expand = FALSE) +
  theme_void()

# mapa Argentina
g1 <- ggplot() +
  geom_sf(data = pcias, fill = c1, color = c2, linewidth = .5) +
  geom_sf(
    data = buzones, color = c3, size = 2, alpha = .6, shape = 4, stroke = 1) +
  coord_sf(clip = "off") +
  theme_void()

# mapa bsas
g2 <- ggplot() +
  geom_sf(data = dptos_bsas, fill = c1, color = c2, linewidth = .4) +
  geom_sf(
    data = buzones_bsas, color = c3, size = 2, alpha = .5, shape = 4, 
    stroke = 1) +
  theme_void()

# mapa caba
g3 <- ggplot() +
  geom_sf(data = dptos_caba, fill = c1, color = c2, linewidth = .4) +
  geom_sf(
    data = buzones_caba, color = c3, size = 3, alpha = .6, shape = 4, 
    stroke = 1) +
  theme_void()

# composición
diseño <- "
AB
AC
"

# combino los mapas con el diseño dado
g <- g1 + g2 + g3 + plot_layout(design = diseño)

# al mapa combinado agrego el resto de elementos
gg <- g + 
  inset_element(estampilla, left = 1.2, bottom = 1.1, top = .3, right = -2.2) +
  inset_element(logo, left = -3.25, bottom = -1, top = .9, right = .9) +
  plot_annotation(
    title = mi_title,
    subtitle = mi_subtitle,
    caption = mi_caption,
    theme = theme(
      aspect.ratio = 1,
      plot.background = element_rect(fill = c4, color = c3, linewidth = 3),
      plot.title.position = "plot",
      plot.title = element_text(
        family = "diphylleia", size = 80, color = c1, hjust = .5,
        margin = margin(t = 5)),
      plot.subtitle = element_markdown(
        color = c2, family = "ubuntu", size = 20, hjust = .5),
      plot.caption = element_markdown(
        color = c2, family = "ubuntu", size = 15, 
        margin = margin(b = 10, r = 10))
    )
  )

# guardo
ggsave(
  plot = gg,
  filename = "buzones_arg/viz.png",
  width = 30,
  height = 40,
  units = "cm")

# abro
browseURL("buzones_arg/viz.png")
