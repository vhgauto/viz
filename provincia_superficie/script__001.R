
# paquetes ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(patchwork)
library(ggtext)
library(glue)
library(showtext)
library(fontawesome)

# posgar 2007: 5341

# funciones ---------------------------------------------------------------

# función p/generar graficar las pcias
f_pcia <- function(x) {
  
  # datos de la pcia
  s <- pcias |> 
    filter(FNA == x)
  
  # borde de la pcia
  borde <- s |> 
    st_bbox()
  
  medio_x <- mean(c(borde[1], borde[3]))
  medio_y <- mean(c(borde[2], borde[4]))
  
  limite_y <- c((medio_y-rango_y/xx), (medio_y+rango_y/xx))
  limite_x <- c((medio_x-rango_x/(xx*2.25)), (medio_x+rango_x/(xx*2.25)))
  
  # cantidad de veces que la pcia entra en TDF
  r <- rel_pcia |> 
    filter(FNA == x) |> 
    pull(rela)
  
  # título
  tit <- glue("**{x}**<br>{r}")
  
  # si es TDF, sin título
  if (x == provincias[1]) {
    
    tit <- ""
  }
  
  # si es CABA, acorto el título
  if (x == provincias[24]) {
    tit <- glue("**CABA**<br>{r}")
  }
  
  # título
  h <- ggplot() +
    geom_richtext(aes(x = 0, y = 0, label = tit), vjust = 1,
                  label.color = NA, fill = NA, size = 4, family = "anuphan") +
    coord_cartesian(clip = "off") +
    theme_void() +
    theme(plot.title = element_markdown(size = 10, hjust = .5, vjust = 1,
                                        margin = margin(0, 0, 0, 0)),
          plot.background = element_rect(fill = NA, color = NA),
          panel.background = element_rect(fill = NA, color = NA))
  
  # figura pcia
  g <- s |> 
    ggplot() +
    geom_sf(color = "darkblue", fill = "#f6b40e", linewidth = .1) +
    coord_sf(expand = TRUE, 
             # crs = 4326,
             ylim = limite_y,
             xlim = limite_x,
             clip = "off"
    ) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = NA, color = NA),
      panel.background = element_rect(fill = NA, color = NA)
    )
  
  # composición
  i <- h + g & plot_layout(heights = c(.05, .95))
  
  return(i)
}

# función para generar el título sobre TDF
f_tit <- function(x) {
  s <- pcias |> 
    filter(FNA == x)
  
  borde <- s |> 
    st_bbox()
  
  medio_x <- mean(c(borde[1], borde[3]))
  medio_y <- mean(c(borde[2], borde[4]))
  
  limite_y <- c((medio_y-rango_y/xx), (medio_y+rango_y/xx))
  limite_x <- c((medio_x-rango_x/(xx*2.25)), (medio_x+rango_x/(xx*2.25)))
  
  r <- rel_pcia |> 
    filter(FNA == x) |> 
    pull(rela)
  
  tit <- glue("**{str_wrap(x, 30) |> str_replace('\\n', '<br>')}**<br>{r}")
  
  h <- ggplot() +
    geom_richtext(aes(x = 0, y = 0, label = tit), vjust = 1,
                  label.color = NA, fill = NA, size = 4, family = "anuphan") +
    coord_cartesian(clip = "off") +
    theme_void() +
    theme(plot.title = element_markdown(size = 10, hjust = .5, vjust = 1,
                                        margin = margin(0, 0, 0, 0)),
          plot.background = element_rect(fill = NA, color = NA),
          panel.background = element_rect(fill = NA, color = NA))
  
  g <- s |> 
    ggplot() +
    geom_sf(color = NA, fill = NA, linewidth = .1) +
    coord_sf(expand = TRUE, 
             # crs = 4326,
             ylim = limite_y,
             xlim = limite_x,
             clip = "off"
    ) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = NA, color = NA),
      panel.background = element_rect(fill = NA, color = NA)
    )
  
  i <- h + g & plot_layout(heights = c(.05, .95))
  
  return(i)
}

# fuentes -----------------------------------------------------------------

co <- c("#00008b", "#74acdf", "#f6b40e")

font_add_google(name = "Abril Fatface", family = "abril") # título
font_add_google(name = "Anuphan", family = "anuphan", 
                db_cache = FALSE) # resto del texto

showtext_auto()
showtext_opts(dpi = 300)

# íconos
font_add("fa-reg", "icon/Font Awesome 5 Free-Regular-400.otf")
font_add("fa-brands", "icon/Font Awesome 5 Brands-Regular-400.otf")
font_add("fa-solid", "icon/Font Awesome 5 Free-Solid-900.otf")

# caption
icon_twitter <- glue("<span style='font-family:fa-brands; color:black;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands; color:black;'>&#xf09b;</span>")
datos <- glue("<span style='color:black;'>*Datos:</span> <span style='color:white;'>Instituto Geográfico Nacional</span>")
proy <- glue("<span style='color:black;'><sup>†</sup>EPSG:</span> <span style='color:white;'>5346</span>")
autor <- glue("<span style='color:black;'>Autor:</span> <span style='color:white;'>**Víctor Gauto**</span>")
sep <- glue("<span style = 'color:black;'>**|**</span>")
usuario <- glue("<span style = 'color:white;'>**vhgauto**</span>")

mi_caption <- glue("{datos} {sep} {proy} {sep} {autor} {sep} {icon_github} {icon_twitter} {usuario}")

# provincias .shp ---------------------------------------------------------

# EPSG de las fajas, POSGAR 2007
browseURL("http://cafegeodesico.blogspot.com/2011/12/que-son-los-codigos-epsg.html")

# leo .gpkg que contiene los vectores de las provincias argentinas
pcias <- read_sf("provincia_superficie/pcias.gpkg") |> 
  # modifico el nombre de las provincias
  mutate(FNA = str_remove(FNA, "Provincia de |Provincia del ")) |> 
  # transformo a coordenadas locales
  st_transform(crs = 5346)

# TDF
20698.3+15907.7+873718.4

# agrego las superficies (km), de acuerdo al IGN, 2020
browseURL("https://www.ign.gob.ar/descargas/geoespacial/Informe_supercies_de_Argentina.pdf")

# leo archivo con las superficies de c/pcia
super_tbl <- read_tsv("provincia_superficie/super.tsv")

# combino los datos de las pcias con sus superficies
pcias_sup <- pcias |> 
  left_join(super_tbl, by = c("FNA" = "pcia")) |> 
  # ordeno las pcias de acuerdo a su superficie
  mutate(FNA = fct_reorder(FNA, desc(sup)))

# vector de pcias, en orden creciente de superficie
provincias <- sort(pcias_sup$FNA)

# borde de TDF, para mantener la escala uniforme
borde_TDF <- pcias |> 
  filter(FNA == provincias[1]) |> 
  st_bbox()

# límites de TDF
rango_x <- abs(borde_TDF[1] - borde_TDF[3])
rango_y <- abs(borde_TDF[2] - borde_TDF[4])

# factor de escala para las pcias
# aumentar p/hacer más zoom
xx <- 4.7

factor_inf <- 1+xx
factor_sup <- 1-xx

# cantidad de veces que una provincia entra en TDF
rel_pcia <- pcias_sup |> 
  mutate(rela = 1/(sup/max(sup))) |> 
  mutate(rela = if_else(rela > 1000,
                        format(round(rela, 0), nsmall = 0),
                        format(round(rela, 1), nsmall = 1))) |> 
  mutate(rela = str_replace(rela, "\\.", ","))

# superficie de TDF
sup_tdf <- rel_pcia |> 
  filter(FNA == provincias[1]) |> 
  mutate(sup = round(sup, 0)) |> 
  pull(sup) |> 
  # formato de número
  prettyNum(x = _, big.mark = ".", decimal.mark = ",")

# lista con las figuras de las pcias
lista_pcias <- map(.x = provincias, ~ f_pcia(x = .x))

# subtítulo, como figura
sub_tbl <- tibble(
  x = 1, 
  y = 1,
  label = glue(
    "**{provincias[1]}** es la provincia más grande de **Argentina**, con 
    {sup_tdf}* km<sup>2</sup> de superficie. Se muestran todas las provincias, 
    en orden decreciente de extensión, manteniendo la escala<sup>†</sup>. El número indica 
    la cantidad de veces que esa provincia cabe dentro de Tierra del Fuego.")) |> 
  mutate(label = str_wrap(label, width = 45)) |> 
  mutate(label = str_replace_all(label, "\\n", "<br>"))

g_sub <- ggplot(data = sub_tbl, aes(x, y, label = label)) +
  geom_richtext(size = 6, fill = NA, label.color = NA, hjust = 0, vjust = 1,
                color = "white", family = "anuphan") +
  coord_cartesian(clip = "off", xlim = c(.9, 1.5), ylim = c(.5, 1.5),
                  expand = FALSE) +
  theme_void() 

# diseño p/las figuras en {patchwork}
diseño <- "
1#A
##B
###
CDE
FGH
IJK
LMN
OPQ
RST
UVW
XYZ
"

# figura ------------------------------------------------------------------

gg <- wrap_plots(c(list(g_sub,
                        f_tit(provincias[1])),
                   lista_pcias),
                 design = diseño) &
  plot_annotation(
    title = "Superficies provinciales",
    caption = mi_caption,
    theme = theme(
      plot.title = element_text(
        size = 53,
        family = "abril",
        color = "white",
        hjust = .5
      ),
      plot.title.position = "plot",
      plot.subtitle = element_textbox_simple(
        family = "anuphan",
        size = 17,
        color = "white",
        margin = margin(10, 5, 25, 5)
      ),
      plot.caption = element_markdown(
        hjust = .5,
        family = "anuphan",
        size = 12,
        margin = margin(0, 0, 15, 0)
      ),
      plot.background = element_rect(
        color = "white",
        fill = co[2],
        linewidth = 2
      ),
      plot.margin = margin(15, 0, 0, 5)
    )
  )

# guardo
ggsave(
  plot = gg,
  filename = "provincia_superficie/viz.png",
  width = 22,
  height = 50,
  units = "cm",
  dpi = 300
)

# abro
browseURL("provincia_superficie/viz.png")
