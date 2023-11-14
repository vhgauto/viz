
browseURL("https://www.youtube.com/watch?v=kGadI6_ZIR4")
browseURL("https://www.youtube.com/watch?v=zgFXVhmKNbU")

# paquetes ----------------------------------------------------------------

library(rayvista)
library(rayshader)
library(elevatr)
library(sf)
library(magick)

# datos -------------------------------------------------------------------

# coordenadas del bbox
xmin <- -64.510174
ymin <- -31.425148
xmax <- -64.424171
ymax <- -31.305542

bbox <- st_sfc(
  st_polygon(
    list(
      cbind(c(xmin, xmax, xmax, xmin, xmin), c(ymin, ymin, ymax, ymax, ymin)))), 
  crs = 4326) |> 
  st_as_sf()

# figura ------------------------------------------------------------------

# 3d
sr <- plot_3d_vista(
  req_area = bbox,
  elevation_detail = 13,
  overlay_detail = 16,
  # argumentos de plot_3d()
  solid = FALSE,
  zcale = 5,
  windowsize = 600)

# posición de la cámara
render_camera(
  zoom = .4, # valores bajos -> más cerca del mapa
  theta = 0, # ángulo de rotación, theta = 0 -> arriba = Norte
  phi = 89, # ángulo de azimut, phi = 90 -> vista superior
  fov = 60)

# tamaño de imagen
rel_asp <- 894/549
ancho <- 5000
alto <- round(rel_asp*ancho)

# guardo
render_snapshot(
  filename = "mapa_relieve/san_roque.png",
  width = ancho,
  height = alto,
  software_render = TRUE)

# cierro la ventana 3d
rgl::close3d()

# leo la imagen generada
img <- image_read("mapa_relieve/san_roque.png")

# leo caption
img_autor <- image_read("mapa_relieve/caption.png")

# agrego título y autor
img |> 
  # título
  image_annotate(
    text = "Embalse San Roque",
    color = "#6D5A49",
    location = "+300+150",
    size = 300,
    font = "serif",
    gravity = "southwest") |> 
  # autor
  image_composite(
    composite_image = image_scale(img_autor, "1500x"),
    gravity = "southeast", 
    offset = "+220+190") |> 
  # guardo
  image_write(
    path = "mapa_relieve/san_roque.png")

# abro
browseURL("mapa_relieve/san_roque.png")
