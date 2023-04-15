
# paquetes ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(glue)
library(patchwork)
library(ggspatial)
library(ggtext)
library(showtext)

# fuentes -----------------------------------------------------------------

font_add_google(name = "Abril Fatface", family = "abril") # título
font_add_google(name = "Anuphan", family = "anuphan", 
                db_cache = FALSE) # resto del texto

showtext_auto()
showtext_opts(dpi = 300)

# íconos
font_add("fa-reg", "icon/Font Awesome 5 Free-Regular-400.otf")
font_add("fa-brands", "icon/Font Awesome 5 Brands-Regular-400.otf")
font_add("fa-solid", "icon/Font Awesome 5 Free-Solid-900.otf")

# colores, paleta Homer1 de {MetBrewer}
amarillo <- "#fff178"
celeste <- "#c3f4f6"
rojo <- "#a62f00"

# caption
icon_twitter <- glue("<span style='font-family:fa-brands; color:black;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands; color:black;'>&#xf09b;</span>")
datos <- glue("<span style='color:black;'>*Datos:</span> <span style='color:{rojo};'>Instituto Geográfico Nacional</span>")
proy <- glue("<span style='color:black;'><sup>†</sup>EPSG:</span> <span style='color:{rojo};'>4326</span>")
autor <- glue("<span style='color:black;'>Autor:</span> <span style='color:{rojo};'>**Víctor Gauto**</span>")
sep <- glue("<span style = 'color:black;'>**|**</span>")
usuario <- glue("<span style = 'color:{rojo};'>**vhgauto**</span>")

mi_caption <- glue("{datos} {sep} {proy} {sep} {autor} {sep} {icon_github} {icon_twitter} {usuario}")

# funciones ---------------------------------------------------------------

# función p/generar el rectángulo
make_simple_poly = function(params, area) {
  # Function to return a rectangle as a SpatialPolygons object.
  # params is a 4-element vector.
  
  centroid = c(params[1], params[2])
  angle = params[3]
  aspect = params[4]
  
  lon_side_length = sqrt(area * aspect)
  lat_side_length = area / lon_side_length
  
  vertices = matrix(0, nrow=5, ncol=2)
  vertices[1, ] = c(-lon_side_length/2, -lat_side_length/2)
  vertices[2, ] = c(-lon_side_length/2, +lat_side_length/2)
  vertices[3, ] = c(lon_side_length/2, lat_side_length/2)
  vertices[4, ] = c(lon_side_length/2, -lat_side_length/2)
  vertices[5, ] = vertices[1, ]
  
  # Rotate:
  rot_mat = matrix(c(cos(angle), sin(angle), -sin(angle), cos(angle)),
                   nrow=2, ncol=2)
  
  vertices = t(rot_mat %*% t(vertices))
  
  # Centre:
  vertices[ , 1] = vertices[ , 1] + centroid[1]
  vertices[ , 2] = vertices[ , 2] + centroid[2]
  
  # Convert to SpatialPolygons:
  polygon_object = sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(vertices)), 0)))
  return(polygon_object)
}

# función p/extraer el overlap p/c pcia
f_over <- function(i) {
  overlap <- read_csv(
    file = itereacion[i], 
    col_names = c("step", "overlap", "params1", "params2", "params3", "params4")) |> 
    select(-step) |> 
    mutate(overlap = parse_number(overlap),
           params1 = parse_number(params1)) |> 
    slice_tail(n = 1) |> 
    pull(overlap)
  
  tbl <- tibble(nam = pcias |> filter(nro == i) |> pull(NAM),
                nro = pcias |> filter(nro == i) |> pull(nro),
                overlap = overlap)
  
  return(tbl)
}

# función que extraer los parámetros de la última iteración (.txt)
f_arc <- function(i) {
  archivo <- read_csv(
    file = itereacion[i], 
    col_names = c("step", "overlap", "params1", "params2", "params3", "params4")) |> 
    select(-step) |> 
    mutate(overlap = parse_number(overlap),
           params1 = parse_number(params1)) |> 
    slice_tail(n = 1)
  
  return(archivo)
}

# función que genera las figuras
f_rect <- function(i) {
  
  this_shp = pcias_poly[i, ]
  
  area = rgeos::gArea(this_shp)
  
  archivo2 <- archivo |> slice(i)
  
  lista_pcia <- list(overlap = archivo2$overlap_label,
                     param = c(archivo2$params1, archivo2$params2, archivo2$params3, archivo2$params4),
                     area = area)
  
  cuadro <- make_simple_poly(params = lista_pcia[[2]], area = lista_pcia[[3]])
  
  cuadro_sf <- st_as_sf(cuadro) |> 
    st_set_crs(value = 4326)
  
  pcia_i <- pcias |> 
    filter(nro == i)
  
  int <- st_intersection(cuadro_sf, pcia_i)
  
  pcia_tit <- pcia_i$NAM |> 
    str_wrap(width = 30) |> 
    str_replace_all("\\n", "<br>")
  
  g <- ggplot() +
    geom_sf(data = pcia_i, fill = rojo, color = NA) +
    geom_sf(data = cuadro_sf, fill = rojo, color = NA) +
    geom_sf(data = int, fill = amarillo, color = NA) +
    labs(title = glue("{pcia_tit}<br>**<span style='color:{rojo};'>{lista_pcia[[1]]}**</span>")) +
    coord_sf(clip = "off") +
    theme_void() +
    theme(plot.title.position = "plot",
          plot.title = element_markdown(hjust = .5, family = "anuphan"),
          plot.margin = margin(10, 40, 10, 40))
  
  return(g)
}

# datos -------------------------------------------------------------------

# leo .shp de las provincias
pcias <- st_read("provincia_rectangulo/Provincias.shp") |> 
  mutate(nro = row_number()) |> 
  select(nro, NAM)

# archivos que contienen las iteraciones
itereacion <- list.files("provincia_rectangulo/iterations/",
                         full.names = TRUE)

# leo .shp de las provincias, p/calcular el área
pcias_poly = maptools::readShapePoly("provincia_rectangulo/Provincias.shp")

# ordeno las provincias de acuerdo a su rectangularidad
y <- map(.x = 1:24, .f = ~ f_over(i = .x)) |> 
  list_rbind() |> 
  arrange(desc(overlap))

y |>
  print(n = 30)

# extraigo los parámetros de la última iteración
archivo <- map(.x = 1:24, ~ f_arc(i = .x)) |> 
  list_rbind() |> 
  mutate(overlap_label = format(overlap, nsmall = 3)) |> 
  mutate(overlap_label = str_replace(overlap_label , "\\.", ","))

# evita errores con los vectores de las pcias
sf_use_s2(use_s2 = FALSE)

# figuras -----------------------------------------------------------------

# lista que contiene todas las figuras de las pcias y los rectángulos
g <- map(.x = y$nro, .f = ~ f_rect(i = .x))

# composición
g_w <- wrap_plots(
  g, 
  ncol = 3, 
  widths = c(1, 1, 1), 
  heights = c(1, 1, 1, 1, 1, 1, 1, 1)) &
  plot_annotation(
    title = "Rectangularidad<br>provincial",
    subtitle = "Provincias **argentinas**<span style='color:black;'>*</span>
    ordenadas por su <span style='color:black;'>**rectangularidad**</span>, 
    entendida<br>como la fracción máxima de superposición con un rectángulo de
    la<br>misma área. Una provincia 
    cuadrada<sup><span style='color:black;'>†</span></sup> tendría 
    rectangularidad 1.",
    caption = mi_caption,
    theme = theme(
      plot.title = element_markdown(
        size = 70,
        family = "abril",
        color = rojo,
        hjust = 0
      ),
      plot.title.position = "plot",
      plot.background = element_rect(fill = celeste, color = rojo, linewidth = 2),
      plot.subtitle = element_markdown(
        family = "anuphan",
        size = 17,
        color = rojo,
        margin = margin(15, 5, 15, 0)
      ),
      plot.caption = element_markdown(
        hjust = .5,
        family = "anuphan",
        size = 12,
        margin = margin(10, 0, 5, 0)
      ),
      plot.margin = margin(15, 0, 5, 0)
    )
  )

# guardo
ggsave(plot = g_w,
       filename = "provincia_rectangulo/viz.png",
       width = 22,
       height = 50,
       units = "cm",
       dpi = 300
       )

# abro
browseURL("provincia_rectangulo/viz.png")

