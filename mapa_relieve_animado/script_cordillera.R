browseURL("https://www.tylermw.com/a-step-by-step-guide-to-making-3d-maps-with-satellite-imagery-in-r/")

# paquetes ----------------------------------------------------------------

library(sf)
library(terra)
library(tidyverse)
library(rayshader)
library(glue)
library(magick)

# datos -------------------------------------------------------------------

# leo ráster S2-MSI y creo el stack
s2_r <- rast("mapa_relieve_animado/S2/T19HDD_20230724T142721_B04_10m.jp2")
s2_g <- rast("mapa_relieve_animado/S2/T19HDD_20230724T142721_B03_10m.jp2")
s2_b <- rast("mapa_relieve_animado/S2/T19HDD_20230724T142721_B02_10m.jp2")

# re-escalo para evitar colores muy opacos (demasiado marrones)
s2_r[s2_r>7000] <- 7000
s2_g[s2_g>7000] <- 7000
s2_b[s2_b>7000] <- 7000

s2_r[s2_r<700] <- 700
s2_g[s2_g<700] <- 700
s2_b[s2_b<700] <- 700


s2_rgb <- c(s2_r, s2_g, s2_b)

# cambio los nombres de las bandas
names(s2_rgb) <- c("r", "g", "b")

# extensión
roi <- st_bbox(s2_rgb) |> 
  st_as_sfc() |> 
  st_as_sf()

# descargo elevación de la región de interés
ele <- elevatr::get_elev_raster(locations = roi, z = 10, clip = "locations") |> 
  rast()

# creo las matrices de las bandas y elevación
r_matriz <- raster_to_matrix(s2_rgb$r)
g_matriz <- raster_to_matrix(s2_rgb$g)
b_matriz <- raster_to_matrix(s2_rgb$b)

ele_matriz <- raster_to_matrix(ele)

# creo array con el contenido de las matrices
rgb_array <- array(0, dim = c(nrow(r_matriz), ncol(r_matriz), 3))

rgb_array[,,1] <- r_matriz/255 # R
rgb_array[,,2] <- g_matriz/255 # G
rgb_array[,,3] <- b_matriz/255 # B

# traspongo el array
rgb_array <- aperm(rgb_array, c(2,1,3))

# rescalo los valores del array
rgb_contrast <- scales::rescale(rgb_array, to = c(0, 1))

# figura 3d ---------------------------------------------------------------

# 3d
plot_3d(
  hillshade = rgb_contrast, 
  heightmap = ele_matriz,
  solid = FALSE,
  zscale = 15, 
  shadowdepth = -50,
  background = "grey80", 
  windowsize = c(600, 500),
  shadowcolor = "grey20")

# vista de la cámara
render_camera(
  theta = 10, # ángulo de rotación, theta = 0 -> arriba = Norte
  phi = 65, # ángulo de azimut, phi = 90 -> vista superior
  zoom = .7, # valores bajos -> más cerca del mapa
  fov = 50)

# guardo la imagen
render_snapshot(
  filename = "mapa_relieve_animado/cordillera.png",
  software_render = TRUE,
  width = 6000,
  height = 5000)

# leo la imagen generada
img <- image_read("mapa_relieve_animado/cordillera.png")

# leo caption
img_autor <- image_read("mapa_relieve_animado/caption.png")

# agrego título y autor
img |> 
  # título
  image_annotate(
    text = "Mendoza",
    color = "#1D1916",
    location = "+200+100",
    size = 300,
    font = "serif",
    gravity = "northwest") |> 
  # autor
  image_composite(
    composite_image = image_scale(img_autor, "1300x"),
    gravity = "southeast", 
    offset = "+100+70") |> 
  # guardo
  image_write(
    path = "mapa_relieve_animado/cordillera.png")

# abro
browseURL("mapa_relieve_animado/cordillera.png")

# aumento el tamaño de la ventana 3d
plot_3d(
  hillshade = rgb_contrast, 
  heightmap = ele_matriz,
  solid = FALSE,
  zscale = 15, 
  shadowdepth = -50,
  background = "grey80", 
  windowsize = c(1200, 1000),
  shadowcolor = "grey20")

# duración del video [s] * fotogramas [fps]
video_duracion <- 15
video_fps <- 60
video_frames <- video_duracion*video_fps

# ángulos de rotación de la imagen
angles <- seq(0,360,length.out = video_duracion*video_fps)[-1]

# función para rotar la imagen y guardar
# la ventana tiene que estar abierta
f_av <- function(angulo, archivo) {
  render_camera(theta = -45+angulo, fov = 0, zoom = .7, phi = 30)
  render_snapshot(
    filename = archivo)
  
  n <- archivo |> 
    str_remove("mapa_relieve_animado/png/") |> 
    str_remove(".png")
  
  print(glue("{n} de {video_frames}"))
}

# nombre de archivo (números consecutivos, de tres cifras)
nro_av <- sprintf("%03d", 1:(video_frames-1))

# corro la función
walk2(
  .x = angles,
  .y = glue("mapa_relieve_animado/png/{nro_av}.png"),
  ~ f_av(angulo = .x, archivo = .y))

# cierro la ventana 3d
rgl::close3d()

# genero un video con los .png
av::av_encode_video(
  input = list.files("mapa_relieve_animado/png/", full.names = TRUE),
  output = "mapa_relieve_animado/cordillera.mp4",
  framerate = video_fps,
  verbose = TRUE)

# abro el video
browseURL("mapa_relieve_animado/cordillera.mp4")
