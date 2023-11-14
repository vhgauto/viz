
browseURL("https://www.youtube.com/watch?v=kGadI6_ZIR4")
browseURL("https://www.youtube.com/watch?v=zgFXVhmKNbU")

# paquetes ----------------------------------------------------------------

library(rayvista)
library(rayshader)
library(elevatr)
library(sf)
library(magick)

# datos -------------------------------------------------------------------

# coordenadas del Puente Chaco Corrientes

xmin <- -58.960705
ymin <- -27.488172
xmax <- -58.765182
ymax <- -27.448115

bbox <- st_sfc(
  st_polygon(
    list(
      cbind(c(xmin, xmax, xmax, xmin, xmin), c(ymin, ymin, ymax, ymax, ymin)))), 
  crs = 4326) |> 
  st_as_sf()

# figura ------------------------------------------------------------------

# 3d
puente_3d <- plot_3d_vista(
  # dem = puente_ele,
  req_area = bbox,
  elevation_detail = 14,
  overlay_detail = 15,
  # argumentos de plot_3d()
  solid = FALSE,
  zcale = 1,
  windowsize = c(600, 200))

# posición de la cámara
render_camera(
  zoom = .2, # valores bajos -> más cerca del mapa
  theta = 0, # ángulo de rotación, theta = 0 -> arriba = Norte
  phi = 70, # ángulo de azimut, phi = 90 -> vista superior
  fov = 90)

# tamaño de imagen
rel_asp <- 175/752
ancho <- 6000
alto <- round(rel_asp*ancho)

# guardo imagen
render_snapshot(
  filename = "mapa_relieve/puente.png",
  width = ancho,
  height = alto,
  software_render = TRUE)

# cierro la ventana 3d
rgl::close3d()

# leo la imagen generada
img <- image_read("mapa_relieve/puente.png")

# leo caption
img_autor <- image_read("mapa_relieve/caption.png")

# agrego título y autor
img |> 
  # título
  image_annotate(
    text = "Puente Chaco-Corrientes",
    color = "#6C4D38",
    location = "+100+20",
    size = 120,
    font = "serif",
    gravity = "northwest") |> 
  # autor
  image_composite(
    composite_image = image_scale(img_autor, "1000x"),
    gravity = "southeast", 
    offset = "+10+10") |> 
  # guardo
  image_write(
    path = "mapa_relieve/puente.png")

# abro
browseURL("mapa_relieve/puente.png")



