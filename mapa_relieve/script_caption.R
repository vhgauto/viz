
# paquetes ----------------------------------------------------------------

library(tidyverse)
library(showtext)
library(ggtext)
library(glue)

# fuentes -----------------------------------------------------------------

font_add_google(name = "Ubuntu", family = "ubuntu")

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# caption
c1 <- "firebrick"
autor <- glue("Autor: <span style='color:{c1};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
usuario <- glue("<span style='color:{c1};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue("{autor} {sep} {icon_github} {icon_twitter} {usuario}")

# figura ------------------------------------------------------------------

g <- ggplot() +
  annotate(
    geom = "richtext", x = 0, y = 0, label = mi_caption, fill = NA, 
    label.color = NA, size = 5, family = "ubuntu") +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    aspect.ratio = 67/1028)

# guardo
ggsave(
  plot = g,
  filename = "mapa_relieve/caption.png",
  width = 1028,
  height = 67,
  units = "px")

# abro
browseURL("mapa_relieve/caption.png")
