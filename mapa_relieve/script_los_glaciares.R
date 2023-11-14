
browseURL("https://www.youtube.com/watch?v=kGadI6_ZIR4")

# paquetes ----------------------------------------------------------------

library(rayvista)
library(rayshader)
library(elevatr)
library(sf)
library(magick)

# datos -------------------------------------------------------------------

# vector con los polígonos de los Parques Nacionales
parques <- st_read("ign_parques/area_protegida.json")

# selecciono Los Glaciares
glaciares <- parques |> 
  dplyr::filter(fna == "Parque Nacional Los Glaciares")

# extensión
bb <- st_bbox(glaciares) |> 
  st_as_sfc() |> 
  st_as_sf()

# figura ------------------------------------------------------------------

# 3d
glaciares_3d <- plot_3d_vista(
  req_area = bb,
  elevation_detail = 10,
  overlay_detail = 11,
  # argumentos de plot_3d()
  solid = FALSE,
  zcale = 1,
  windowsize = 600)

# posición de la cámara
render_camera(
  zoom = .6, # valores bajos -> más cerca del mapa
  theta = 45, # ángulo de rotación, theta = 0 -> arriba = Norte
  phi = 85, # ángulo de azimut, phi = 90 -> vista superior
  fov = 0)

# guardo imagen
render_snapshot(
  filename = "mapa_relieve/los_glaciares.png",
  width = 6000,
  height = 6000,
  software_render = TRUE)

# cierro la ventana 3d
rgl::close3d()

# leo la imagen generada
img <- image_read("mapa_relieve/los_glaciares.png")

# leo caption
img_autor <- image_read("mapa_relieve/caption.png")

# agrego título y autor
img |> 
  # título
  image_annotate(
    text = "Parque Nacional Los Glaciares",
    color = "#7AA697",
    location = "+200+100",
    size = 300,
    font = "serif",
    gravity = "northwest") |> 
  # autor
  image_composite(
    composite_image = image_scale(img_autor, "1500x"),
    gravity = "southeast", 
    offset = "+300+200") |> 
  # guardo
  image_write(
    path = "mapa_relieve/los_glaciares.png")

# abro
browseURL("mapa_relieve/los_glaciares.png")
