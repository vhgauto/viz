
# paquetes ----------------------------------------------------------------

library(glue)
library(showtext)
library(gganimate)
library(ggtext)
library(tidyverse)

# fuente ------------------------------------------------------------------

# colores
c1 <- "red"
c2 <- "#faafca"
c3 <- "grey20"

# fuente: Ubuntu
font_add(
  family = "ubuntu", 
  regular = "fuentes/Ubuntu-Regular.ttf",
  bold = "fuentes/Ubuntu-Bold.ttf",
  italic = "fuentes/Ubuntu-Italic.ttf")

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
autor <- glue("<span style='color:{c1};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
icon_mastodon <- glue("<span style='font-family:fa-brands;'>&#xf4f6;</span>")
usuario <- glue("<span style='color:{c1};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue(
  "{autor} {sep} {icon_github} {icon_twitter} 
  {icon_mastodon} {usuario}")

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
cora |> 
  filter(eje_h == max(eje_h)) |> pull(eje_h ) #  15.99754  3.764323

cora |>  
  filter(eje_h == 0) |> pull()#  0  5

cora |>  
  filter(eje_v == max(eje_v)) |> pull(eje_v) #  9.817617  11.92184

# tramos de los vectores 
cora1 <- cora  |>  
  filter(eje_h >= 0 & eje_h <= 9.817617)  |>  
  filter(eje_v >= 5 & eje_v <= 11.92184) |> 
  arrange(eje_h)

cora2 <- cora |> 
  filter(eje_h >= 9.817617 & eje_h <= 15.99754) |> 
  filter(eje_v >= 3.764323 & eje_v <= 11.92184) |> 
  arrange(eje_h)

cora3 <- cora |> 
  filter(eje_h >= 0 & eje_h <= 15.99754) |> 
  filter(eje_v >= -16.999961 & eje_v <= 3.764323) |> 
  arrange(desc(eje_h))

cora4 <- cora |> 
  mutate(eje_h = -eje_h) |> 
  filter(eje_h <= 0 & eje_h >= -15.99754) |> 
  filter(eje_v >= -16.999961 & eje_v <= 3.764323) |> 
  arrange(desc(eje_h))

cora5 <- cora |> 
  mutate(eje_h = -eje_h) |> 
  filter(eje_h <= -9.817617 & eje_h >= -15.99754) |> 
  filter(eje_v >= 3.764323 & eje_v <= 11.92184) |> 
  arrange(eje_h)

cora6 <- cora |> 
  mutate(eje_h = -eje_h) |> 
  filter(eje_h <= 0 & eje_h >= -9.817617) |> 
  filter(eje_v >= 5 & eje_v <= 11.92184) |> 
  arrange(eje_h)

# combino todos los tramos y agrego un contador p/la animación
cora_plot <- bind_rows(cora1, cora2, cora3, cora4, cora5, cora6) |> 
  mutate(contador = row_number())

# figura ------------------------------------------------------------------

p <- ggplot() +
  geom_path(
    data = cora_plot, aes(eje_h, eje_v), color = c1, linewidth = 2,
    lineend = "round") +
  labs(caption = mi_caption) +
  theme_void() +
  theme(
    aspect.ratio = 1,
    plot.background = element_rect(fill = c2, color = c2),
    plot.caption = element_markdown(
      family = "ubuntu", color = c3, size = 6, margin = margin(b = 5, r = 5))
  )

# animación
anim <- p +
  transition_manual(frames = contador, cumulative = TRUE)

# .png
ggsave(
  plot = p,
  filename = "corazon_gif/viz.png",
  width = 1000, height = 1000, units = "px",
)

# abro .png
browseURL("corazon_gif/viz.png")

# .mp4
animate(
  anim,
  width = 1000, height = 1050, res = 300,
  fps = 60,
  nframes = nrow(cora_plot),
  renderer = av_renderer("corazon_gif/viz.mp4"))

# abro .mp4
browseURL("corazon_gif/viz.mp4")

# .gif (MUCHO más lento)
# se abre en la pestaña Viewer en RStudio y luego se guarda
animate(
  anim,
  width = 1000, height = 1050, res = 300,
  fps = 50,
  nframes = nrow(cora_plot))

anim_save("corazon_gif/viz.gif")

# abro .gif
browseURL("corazon_gif/viz.gif")
