# Mis proyectos de visualización

Figuras creadas usando **R**, de datos que me parezcan interesantes. Mi repositorio de Tidytuesday lo encuentran [acá](https://github.com/vhgauto/tidytuesday).

## Chaco-Corrientes (relieve)

Imagen de Chaco, Corrientes y el puente sobre el Río Paraná.

[Scripts](mapa_relieve/script_puente.R)

![](mapa_relieve/puente.png)

## Ríos de Argetina

Datos de OpenStreetMap, descargados de [Geofabrik](https://download.geofabrik.de/south-america.html). A cada observación le asigné un color distinto, para generar un mapa pintorezco.

[Scripts](osm/script__001.R)

![](osm/viz.png)

## Red Eléctrica de Argentina

Datos del [Instituto Geográfico Nacional](https://www.ign.gob.ar/NuestrasActividades/InformacionGeoespacial/CapasSIG) de líneas de energía y plantas transformadoras.

[Scripts](ign_electricidad/script__001.R)

![](ign_electricidad/viz.png)

## Parques Nacionales de Argentina

Datos del [Instituto Geográfico Nacional](https://www.ign.gob.ar/NuestrasActividades/InformacionGeoespacial/CapasSIG) de áreas protegidas de Argentina.

[Scripts](ign_parques/script__001.R)

![](ign_parques/viz.png)

## Red vial Argentina

Datos del [Instituto Geográfico Nacional](https://www.ign.gob.ar/NuestrasActividades/InformacionGeoespacial/CapasSIG) de rutas a diferentes jurisdicciones.

[Scripts](ign_red_vial/script__001.R)

![](ign_red_vial/viz.png)

## Popularidad de nombres en Argentina

Tomando como inspiración esta [figura](https://nombres.datos.gob.ar/), grafiqué la evolución en popularidad de los 10 nombres más frecuentes (femeninos y masculinos). El portal de datos de Argentina tiene un [gráfico interactivo](https://nombres.datos.gob.ar/).

[Scripts](nombres/script__001.R)

![](nombres/viz_M.png)

2023-05-22

## Mapa de temperaturas mensuales en Argentina

Datos de temperatura mensual, desde enero 2020 a abril 2023, provenientes de [ERA5](https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land-monthly-means?tab=overview). Basado en [este](https://www.youtube.com/watch?v=2VHuaFqtAsY) tutorial.

[Scripts](clima_temperatura/script__001.R)

![](clima_temperatura/viz.png)

2023-05-15

## Prettymaps de las ciudades capitales de Argentina

Tomando de base los mapas generados por [Prettymapp](https://chrieke-prettymapp-streamlit-prettymappapp-1k0qxh.streamlit.app/), hice mi propia versión con las capitales argentinas.

[Scripts](pretty_map/script__001.R)

![](pretty_map/map/CapitalFederal_Obelisco.png)

2023-04-29

## Mapa topográfico de Argentina

Relieve continental y del lecho marino de Argentina, siguiendo este [tutorial](https://www.youtube.com/watch?v=zoLChBALc1k).

[Scripts](mapa_topografico/script__001.R)

![](mapa_topografico/viz.png)

2023-04-19

## Rectangularidad de las provincias argentinas

Qué tan rectangulares son nuestras provincias. A partir de este post, ["The rectangularness of countries"](https://pappubahry.com/misc/rectangles/), y con modificaciones menores, obtuve los mismos resultados pero aplicado a nuestra región.

[Scripts](provincia_rectangulo/script__001.R)

![](provincia_rectangulo/viz.png)

2023-04-15

## Animación de un corazón

Archivo .gif de un corazón.

[Script](corazon_gif/script__001.R)

![](https://raw.githubusercontent.com/vhgauto/viz/main/corazon_gif/viz.gif)

2023-04-10

## Provincias ordenadas por su superficie

Se ordenan las provincias argentinas de acuerdo a su superficie, y se grafican manteniendo la escala, para comparar los tamaños de las provincias.

[Script](provincia_superficie/script__001.R)

![](provincia_superficie/viz.png)

2023-04-09
