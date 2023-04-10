
# paquetes ----------------------------------------------------------------

library(tidyverse)
library(gganimate)

# datos -------------------------------------------------------------------

# vector de valores con los que se calcula X & Y
t2 <- seq(-5, 5, .05)

# eje horizontal
x2 <- 16*(sin(t2)^2)

# eje vertical
y2 <- 13*cos(t2) - 5*cos(2*t2) -2*cos(3*t2) - cos(4*t2)

# unión de los vectores de ambos ejes 
cora <- bind_cols(eje_h = x2, eje_v = y2)

# valores límites p/definir el recorrido de avance de la animación
cora %>% 
  filter(eje_h == max(eje_h)) %>% pull(eje_h ) #  15.99754  3.764323

cora %>% 
  filter(eje_h == 0) %>% pull()#  0  5

cora %>% 
  filter(eje_v == max(eje_v)) %>%  .$eje_v #  9.817617  11.92184

# tramos de los vectores 
cora1 <- cora %>% 
  filter(eje_h >= 0 & eje_h <= 9.817617) %>% 
  filter(eje_v >= 5 & eje_v <= 11.92184) %>% 
  arrange(eje_h)

cora2 <- cora %>% 
  filter(eje_h >= 9.817617 & eje_h <= 15.99754) %>% 
  filter(eje_v >= 3.764323 & eje_v <= 11.92184) %>% 
  arrange(eje_h)

cora3 <- cora %>% 
  filter(eje_h >= 0 & eje_h <= 15.99754) %>% 
  filter(eje_v >= -16.999961 & eje_v <= 3.764323) %>% 
  arrange(desc(eje_h))

cora4 <- cora %>% 
  mutate(eje_h = -eje_h) %>% 
  filter(eje_h <= 0 & eje_h >= -15.99754) %>% 
  filter(eje_v >= -16.999961 & eje_v <= 3.764323) %>% 
  arrange(desc(eje_h))

cora5 <- cora %>% 
  mutate(eje_h = -eje_h) %>% 
  filter(eje_h <= -9.817617 & eje_h >= -15.99754) %>% 
  filter(eje_v >= 3.764323 & eje_v <= 11.92184) %>% 
  arrange(eje_h)

cora6 <- cora %>% 
  mutate(eje_h = -eje_h) %>% 
  filter(eje_h <= 0 & eje_h >= -9.817617) %>% 
  filter(eje_v >= 5 & eje_v <= 11.92184) %>% 
  arrange(eje_h)

# combino todos los tramos y agrego un contador p/la animación
cora_plot <- bind_rows(cora1, cora2, cora3, cora4, cora5, cora6) %>% 
  mutate(contador = seq(1, length(.$eje_h)))

# figura ------------------------------------------------------------------

p <- ggplot() +
  geom_path(data = cora_plot, aes(eje_h, eje_v), color = "red", linewidth = 2,
            lineend = "round") +
  theme_void() +
  theme(
    aspect.ratio = 1,
    plot.background = element_rect(fill = "#faafca", color = "#faafca"))

# animación
anim <- p +
  transition_manual(frames = contador, cumulative = TRUE)

# .mp4
animate(
  anim,
  width = 2000, height = 2000, res = 300, 
  fps = 70,
  nframes = nrow(cora_plot),
  renderer = av_renderer("corazon_gif/viz.mp4"))

# .gif
animate(
  anim,
  width = 2000, height = 2000, res = 300, 
  fps = 70,
  nframes = nrow(cora_plot))

anim_save("corazon_gif/viz.gif")
