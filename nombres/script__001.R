# inspiración
# sitio web con gráficos interactivos, buscador de nombres, popularidad
browseURL("https://nombres.datos.gob.ar/")

# figura estática, popularidad en el tiempo
browseURL("https://www.chartr.co/newsletters/2023-05-15")


# paquetes ----------------------------------------------------------------

library(tidyverse)
library(ggtext)
# library(ggh4x)
library(glue)
library(showtext)

# fuente ------------------------------------------------------------------

# eje horizontal, años
font_add_google(name = "Bebas Neue", family = "bebas")
# eje vertical, porcentajes
font_add_google(name = "Inconsolata", family = "inconsolata")
# nombres
font_add_google(name = "Alice", family = "alice")
# título
font_add_google(name = "Libre Bodoni", family = "libre", db_cache = FALSE)
# texto
font_add_google(name = "Heebo", family = "heebo", db_cache = FALSE)

showtext_auto()
showtext_opts(dpi = 300)

# íconos
font_add("fa-brands", "icon/Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

# MetBrewer: Monet
c1 <- "grey80"
c2 <- "white"
c3 <- "grey20"
c4 <- "grey30"

# caption
fuente <- glue("Datos: <span style='color:{c1};'>RENAPER, Our World in Data</span>")
autor <- glue("Autor: <span style='color:{c1};'>**Víctor Gauto**</span>")
icon_twitter <- glue("<span style='font-family:fa-brands;'>&#xf099;</span>")
icon_github <- glue("<span style='font-family:fa-brands;'>&#xf09b;</span>")
usuario <- glue("<span style='color:{c1};'>**vhgauto**</span>")
sep <- glue("**|**")

mi_caption <- glue("{fuente} {sep} {autor} {sep} {icon_github} {icon_twitter} {usuario}")

# datos -------------------------------------------------------------------

# POBLACIÓN
# OUR WORLD IN DATA
browseURL("https://ourworldindata.org/grapher/population?time=1922..latest&country=~ARG")

# leo base de datos, población Argentina
pop <- read_csv("nombres/population.csv") |> 
  filter(Entity == "Argentina") |> 
  select(año = Year, pob = `Population (historical estimates)`) |> 
  filter(año >= 1922)

# NOMBRES
# RENAPER
browseURL("https://datos.gob.ar/dataset/otros-nombres-personas-fisicas/archivo/otros_2.1")

# leo la base de datos
# 9.761.609 de filas!!!!!!!!!
# datos <- read_csv("nombres/historico-nombres.csv") |> 
#   rename(año = anio) |> 
#   mutate(nombre = toupper(nombre))

# selecciono únicamente el primer nombre
# datos2 <- datos |> 
#   # creo una nueva columnas por c/nombre individual
#   separate(col = nombre, into = c("nombre", NA), sep = " ")

datos2 <- read_tsv("primer_nombre.tsv")

# remplazo todas las vocales con acento, para simplificar
# muchos nombres aparecen varias veces para un mismo año, con diferentes 
# cantidades; agrupo y sumo
# combino datos de nombres con poblaciones, acorde al año
datos3 <- datos2 |> 
  mutate(nombre = str_replace_all(nombre, pattern = "Á", replacement = "A")) |> 
  mutate(nombre = str_replace_all(nombre, pattern = "É", replacement = "E")) |> 
  mutate(nombre = str_replace_all(nombre, pattern = "Í", replacement = "I")) |> 
  mutate(nombre = str_replace_all(nombre, pattern = "Ó", replacement = "O")) |> 
  mutate(nombre = str_replace_all(nombre, pattern = "Ú", replacement = "U")) |> 
  group_by(nombre, año) |> 
  summarise(cantidad = sum(cantidad), .groups = "drop") |> 
  inner_join(pop, by = "año") |> 
  mutate(rel = 100*(cantidad/pob)) |> 
  mutate(decada = año - (año %% 10))

# voy década por década, obteniendo los nombres (femenino/masculino),
# sin repeticiones

# datos3 |> 
# group_by(decada) |>
# slice_max(order_by = rel, n = 150) |>
# ungroup() |>
# group_by(decada, nombre) |>
# slice_max(order_by = rel, n = 150) |>
# arrange(decada, desc(cantidad)) |>
# filter(decada == 2010) |>
# distinct(nombre, .keep_all = TRUE) |>
# filter(!nombre %in% n_ambos)

# top 10 nombres femeninos
n_mujer <- c("MARIA", "ROSA", "ANA", "SILVIA", "CLAUDIA", "NATALIA", "ROMINA",
             "MICAELA", "CAMILA", "MIA")

# top 10 nombres masculinos
n_varon <- c("JUAN", "JOSE", "CARLOS", "JORGE", "MARCELO", "DIEGO", "CRISTIAN",
             "LUCAS", "SANTIAGO", "SANTINO")

# top 20
n_ambos <- c(n_mujer, n_varon)

# filtro los datos, femenino
top_mujer <- datos3 |> 
  filter(nombre %in% n_mujer) |> 
  mutate(sexo = "M")

# filtro los datos, masculino
top_varon <- datos3 |> 
  filter(nombre %in% n_varon) |> 
  mutate(sexo = "V")

# combino ambos datasets
top_ambos <- bind_rows(top_mujer, top_varon) |> 
  mutate(sexo = factor(sexo, levels = c("M", "V")))

# acomodo los datos según sexo y popularidad (rel)
pop_max_ambos <- top_ambos |> 
  group_by(nombre) |> 
  slice_max(order_by = rel, n = 1) |> 
  ungroup() |> 
  arrange(sexo, desc(rel)) |>
  # arrange(sexo, decada) |>
  mutate(nombre = fct_inorder(nombre))

# orden de los nombres, según sexo y popularidad (rel)
orden_nombre_ambos <- unique(pop_max_ambos$nombre)

# convierto a factor los nombres
top_ambos <- top_ambos |>
  mutate(nombre = factor(nombre, levels = orden_nombre_ambos))

# obtengo el valor de popularidad máxima
punto_max <- top_ambos |> 
  group_by(nombre, decada) |> 
  slice_max(order_by = rel, n = 1) |> 
  ungroup()

# década en la que hacer la iteación
n_decada <- seq(1920, 2010, 10)

# función que obtiene toda la serie histórica de datos, por nombre
f_decada <- function(n_vector, x) {
  top_ambos |> 
    filter(nombre == n_vector[x] & decada == n_decada[x]) |> 
    slice_max(order_by = rel, n = 1)
}

# caso excepcional de MARIA & JUAN, para una mejor ubiación en la figura
mariajuan <- datos3 |> 
  filter(nombre %in% c("MARIA", "JUAN") & año == 1945)

# combino las series históricas de los 20 nombres más populares
punto_max2 <- bind_rows(
  map(.x = 1:10, ~ f_decada(n_vector = n_mujer, x = .x)) |> list_rbind(),
  map(.x = 1:10, ~ f_decada(n_vector = n_varon, x = .x)) |> list_rbind()) |> 
  mutate(nombre = fct_inorder(nombre)) |> 
  # cambio manualmente MARIA y JUAN
  mutate(año = case_match(
    nombre,
    "MARIA" ~ 1945, 
    "JUAN" ~ 1945,
    .default = año)) |> 
  mutate(rel = case_match(
    nombre, 
    "MARIA" ~ mariajuan$rel[2], 
    "JUAN" ~ mariajuan$rel[1], 
    .default = rel)) |> 
  # CARLOS
  mutate(rel = if_else(nombre == "CARLOS", 0.08, rel))

# modifico por el factor de los nombres
top_ambos <- top_ambos |>
  mutate(nombre = factor(nombre, levels = punto_max2$nombre))

# figura ------------------------------------------------------------------

# aclaración primer nombre
acl <- tibble(x = Inf, y = Inf, label = "*Se consideró únicamente el primer nombre")

# eje X
breaks_x <- seq(1920, 2020, 10) + 5
label_x <- glue("'{str_sub(breaks_x-5, 3, 4)}")

# eje Y
eje_y <- list(
  scale_y_continuous(
    breaks = seq(0, .25, .05), 
    expand = c(0, 0), 
    limits = c(0, .2502),
    labels = scales::label_number(
      big.mark = ".", decimal.mark = ",", suffix = "%")),
  scale_y_continuous(
    breaks = seq(0, .12, .02), 
    expand = c(0, 0), 
    limits = c(0, .1201),
    labels = scales::label_number(
      big.mark = ".", decimal.mark = ",", suffix = "%"))
)

# líneas horizontales
int <- bind_rows(
  tibble(yintercept = seq(0, .25, .05), sexo = "M"),
  tibble(yintercept = seq(0, .12, .02), sexo = "V"))

# tamaño de los nombres en la figura
tamaño_nombre <- 5

# paleta de colores
colores2 <- RColorBrewer::brewer.pal(n = 10, name = "Paired")

# ARGENTINA, estilizado
arg <- "<span style='color:#74acdf'>ARG</span>E<span style='color:#f6b40e'>N</span>T<span style='color:#74acdf'>INA</span>"

# función que genera el gráfico de popularidad, por sexo
f_g <- function(sexo_i) {
  
  # datos serie histórica
  d <- top_ambos |> 
    filter(sexo == sexo_i) |> 
    mutate(alfa = as.numeric(nombre))
  
  # nombres
  n <- punto_max2 |> 
    filter(sexo == sexo_i) |> 
    mutate(alfa = as.numeric(nombre))
  
  # eje Y
  if (sexo_i == "M") {
    breaks_y <- seq(0, .25, .05)
    limits_y <- c(0, .2502)
  } else {
    breaks_y <- seq(0, .12, .02)
    limits_y <- c(0, .1201)
  }
  
  # título
  if (sexo_i == "M") {
    tit <- glue("Los nombres femeninos<br>más populares en {arg}")
  } else {
    tit <- glue("Los nombres masculinos<br>más populares en {arg}")
  }
  
  # nombre del archivo, para exportar
  fn <- glue("nombres/viz_{sexo_i}.png")
  
  # líneas horizontales
  int_i <- int |> 
    filter(sexo == sexo_i)
  
  # figura
  g <- ggplot(data = d, aes(x = año, y = rel, fill = nombre)) +
    # verticales
    geom_vline(xintercept = breaks_x-5, color = c4, linewidth = .2) +
    # horizontales
    geom_hline(
      data = int_i, aes(yintercept = yintercept), 
      color = c4, linewidth = .2) +
    # área
    geom_ribbon(
      aes(ymin = 0, ymax = rel), show.legend = FALSE,
      alpha = 1) +
    # fondo de etiqueta
    geom_label(
      data = n, aes(label = nombre, color = nombre), 
      nudge_x = 1, 
      fill = "white",
      label.size = 1,
      size = tamaño_nombre,
      family = "alice", 
      # alpha = 1,
      hjust = .5, vjust = 0, show.legend = FALSE) +
    # línea blanca
    geom_line(
      color = "white",
      show.legend = FALSE, alpha = 1, linewidth = .15, linetype = 1,
      lineend = "round") +
    # línea color
    geom_line(
      aes(color = nombre),
      show.legend = FALSE, alpha = 1, linewidth = .25, 
      # linetype = 1,
      linetype = "f3",
      lineend = "round") +
    # nombres
    geom_label(
      data = n, aes(label = nombre), 
      nudge_x = 1, 
      color = "black",
      size = tamaño_nombre,
      fill = NA, family = "alice",
      hjust = .5, vjust = 0, label.size = unit(0, "mm"),
      fontface = "bold",
      show.legend = FALSE) +
    scale_y_continuous(
      breaks = breaks_y,
      limits = limits_y,
      expand = c(0, 0),
      labels = scales::label_number(
        big.mark = ".", decimal.mark = ",", suffix = "%")) +
    # aclaración
    geom_text(
      data = acl, aes(x = x, y = y, label = label), color = "white", 
      family = "heebo", size = 5, inherit.aes = FALSE, hjust = 1) +
    # ejes
    scale_x_continuous(
      breaks = breaks_x,
      labels = label_x,
      limits = c(1920, 2020),
      expand = c(0, 0)) +
    scale_fill_manual(values = c(colores2, colores2)) +
    scale_color_manual(values = c(colores2, colores2)) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = "Popularidad",
         title = tit,
         subtitle = 
           "Evolución de la popularidad de los 10 nombres<b style='color:#ffffff;'>*</b> 
         más frecuentes de 
       <b style='color:#ffffff;'>Argentina</b>. La **popularidad** se entiende 
       como la fracción entre la **cantidad de personas** nacidas que comparten el 
       mismo nombre, respecto de la **población total**, para un año en particular, 
       expresada en porcentaje. Por cada década se seleccionó el 
       nombre más popular, sin repeticiones. Datos de 1922-2015.",
       caption = mi_caption) +
    # temas
    theme_minimal() +
    theme(
      text = element_text(color = "grey80"),
      aspect.ratio = 1,
      # plot
      plot.margin = margin(5, 9, 5, 9),
      plot.background = element_rect(
        fill = c3, color = "black", linewidth = 3),
      plot.title.position = "plot",
      plot.title = element_markdown(
        size = 55, family = "libre", color = "white", hjust = .5, lineheight = 1.25),
      plot.subtitle = element_textbox_simple(
        size = 20, family = "heebo", color = "gold", margin = margin(5, 0, 25, 0)),
      plot.caption = element_markdown(
        color = "gold", hjust = .38, size = 12, margin = margin(10, 0, 2, 0)),
      # panel
      panel.background = element_rect(
        fill = c3, color = NA),
      panel.grid = element_blank(),
      panel.spacing.y = unit(2, "line"),
      # strip
      strip.background = element_rect(fill = NA, color = NA),
      strip.text = element_text(
        color = "#74acdf", size = 40, family = "heebo", hjust = 1),
      # axis
      axis.line = element_line(color = NA),
      axis.line.x = element_line(color = NA),
      axis.text = element_text(color = c1),
      axis.text.x = element_text(
        family = "bebas", size = 40, margin = margin(10, 0, 0, 0)),
      axis.text.y = element_text(family = "inconsolata", size = 20, vjust = 0),
      axis.title.y = element_text(
        family = "heebo", size = 25, color = c1, margin = margin(0, 10, 0, 0))
    )
  
  # guardo
  ggsave(
    plot = g,
    filename = fn,
    width = 30,
    height = 38,
    units = "cm",
    dpi = 300)
  
  # abro
  browseURL(fn)
}

# ejecuto la función
walk(.x = c("M", "V"), f_g)

# función que genera las figuras de popularidad en el tiempo, un nombre por panel
f_g2 <- function(sexo_i) {

  # datos serie histórica
  d <- top_ambos |> 
    filter(sexo == sexo_i) |> 
    mutate(alfa = as.numeric(nombre))
  
  # punto máximo de popularidad
  d_max <- d |> 
    group_by(nombre) |> 
    slice_max(order_by = rel, n = 1)
  
  # nombres
  n <- punto_max2 |> 
    filter(sexo == sexo_i) |> 
    mutate(alfa = as.numeric(nombre))
  
  # nombre en el strip
  strip_nombre_vec <- tibble(n = 1:10) |> 
    mutate(col = colores2[n]) |> 
    mutate(nombre = sort(unique(d$nombre))) |> 
    mutate(strip = glue("<span style='color:{col};'>{nombre}</span>")) |> 
    mutate(strip = as.character(strip)) |> 
    pull(strip)
  
  # genero named vector
  names(strip_nombre_vec) <- sort(unique(d$nombre))
  
  # eje Y
  if (sexo_i == "M") {
    breaks_y <- seq(0, .25, .05)
    limits_y <- c(0, .2502)
  } else {
    breaks_y <- seq(0, .12, .02)
    limits_y <- c(0, .1201)
  }
  
  # título
  if (sexo_i == "M") {
    tit <- glue("Los nombres femeninos<br>más populares en {arg}")
  } else {
    tit <- glue("Los nombres masculinos<br>más populares en {arg}")
  }
  
  # nombre del archivo, para exportar
  fn <- glue("nombres/viz_facet_{sexo_i}.png")
  
  # líneas horizontales
  int_i <- int |> 
    filter(sexo == sexo_i)
  
  # figura
  g <- ggplot(data = d, aes(x = año, y = rel, fill = nombre)) +
    # verticales
    geom_vline(xintercept = breaks_x-5, color = c4, linewidth = .2) +
    # horizontales
    geom_hline(
      data = int_i, aes(yintercept = yintercept),
      color = c4, linewidth = .2) +
    # área
    geom_ribbon(
      aes(ymin = 0, ymax = rel), show.legend = FALSE,
      alpha = 1) +
    # punto máximo
    geom_point(data = d_max, color = "white", size = 2, show.legend = FALSE) +
    geom_label(data = d_max, aes(label  = año),
              vjust = 0, fill = NA, family = "heebo",
              label.size = 0,
              color = "white", size = 6, show.legend = FALSE) +
    scale_y_continuous(
      breaks = breaks_y,
      limits = limits_y,
      expand = c(0, 0),
      labels = scales::label_number(
        big.mark = ".", decimal.mark = ",", suffix = "%")) +
    facet_wrap(~ nombre, nrow = 2, ncol = 5, scales = "free", 
               labeller = as_labeller(strip_nombre_vec)) +
    # ejes
    scale_x_continuous(
      breaks = breaks_x,
      labels = label_x,
      limits = c(1920, 2020),
      expand = c(0, 0)) +
    scale_fill_manual(values = c(colores2, colores2)) +
    scale_color_manual(values = c(colores2, colores2)) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = "Popularidad",
         title = tit,
         subtitle = 
           "Evolución de la popularidad de los 10 nombres más frecuentes de 
       <b style='color:#ffffff;'>Argentina</b>. La **popularidad** se entiende 
       como la fracción entre la **cantidad de personas** nacidas que comparten el 
       mismo nombre, respecto de la **población total**, para un año en particular, 
       expresada en porcentaje. Por cada década se seleccionó el 
       nombre más popular, sin repeticiones. Cada panel 
       indica el año en que el nombre alcanzó <b style='color:#ffffff;'>máxima</b> 
       popularidad. La escala de los ejes se mantiene para una correcta 
       comparación. Se consideró únicamente el primer nombre. Datos de 1922-2015.",
       caption = mi_caption) +
    # temas
    theme_minimal() +
    theme(
      text = element_text(color = "grey80"),
      aspect.ratio = 1,
      # plot
      plot.margin = margin(7, 9, 7, 9),
      plot.background = element_rect(
        fill = c3, color = "black", linewidth = 3),
      plot.title.position = "plot",
      plot.title = element_markdown(
        size = 55, family = "libre", color = "white", hjust = .5, 
        lineheight = 1.25, margin = margin(5, 0, 15, 0)),
      plot.subtitle = element_textbox_simple(
        size = 20, family = "heebo", color = "gold", margin = margin(5, 0, 25, 0)),
      plot.caption = element_markdown(
        color = "gold", hjust = .5, size = 12, margin = margin(10, 0, 2, 0)),
      # panel
      panel.background = element_rect(
        fill = NA, color = NA),
      panel.grid = element_blank(),
      panel.spacing.x = unit(2.25, "line"),
      panel.spacing.y = unit(2, "line"),
      # strip
      strip.background = element_rect(fill = NA, color = NA),
      strip.text = element_markdown(
        # color = "#74acdf",
        size = 30, family = "heebo", hjust = 1),
      # axis
      axis.line = element_line(color = NA),
      axis.line.x = element_line(color = NA),
      axis.text = element_text(color = c1),
      axis.text.x = element_text(
        family = "bebas", size = 20, margin = margin(10, 0, 0, 0)),
      axis.text.y = element_text(family = "inconsolata", size = 15, vjust = 0),
      axis.title.y = element_text(
        family = "heebo", size = 25, color = c1, margin = margin(0, 10, 0, 0))
    )
  
  # guardo
  ggsave(
    plot = g,
    filename = fn,
    width = 60,
    height = 34,
    units = "cm",
    dpi = 300)
  
  # abro
  browseURL(fn)
}

# ejecuto la función
walk(.x = c("M", "V"), f_g2)
