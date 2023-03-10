#### Configuraciones Iniciales ####

# Limpiar memoria
rm(list = ls())
library(tidyverse)
library(sf)
library(ggrepel)
library(RColorBrewer)
library(readxl)

#### Rutas del proyecto ####

# Definamos una estructura de directorios, tanto para inputs como para outputs

wd <- list()

wd$root <- "D:/Clase4_pdeBigData/Clase4_pdeBigDataTarea/"
wd$inputs <- paste0(wd$root, "01_inputs/")
wd$shapef <- paste0(wd$inputs, "shapefiles/")
wd$datasets <- paste0(wd$inputs, "datasets/")
wd$outputs <- paste0(wd$root, "02_outputs/")

#### Informacion espacial ####
peru_sf <- st_read(paste0(wd$shapef, "INEI_LIMITE_DEPARTAMENTAL.shp"))

peru_sf <- peru_sf %>% mutate(centroid = map(geometry, st_centroid),
                              coords = map(centroid, st_coordinates),
                              coords_x = map_dbl(coords, 1),
                              coords_y = map_dbl(coords, 2))

Departamentos <- unique(peru_sf$NOMBDEP)

#### Dataset ####
poblacion <- read_excel(paste0(wd$datasets, "poblacion.xlsx"))

sinadef <- read.csv(paste0(wd$datasets, "fallecidos_sinadef.csv"), sep=';')

data <- sinadef %>% filter(PAIS == "PERU")
data <- data %>% filter(NOMBDEP != "     ")
data <- data %>% filter(NOMBDEP != "SIN REGISTRO")
data$EDAD <- as.integer(data$EDAD)

unique(data$NOMBDEP)

#### Separar datos por Departamento ####

# Conteo de mujeres por departamento
cant_muertes_departamento <- aggregate(SEXO ~ NOMBDEP, data = data, FUN = length)
colnames(cant_muertes_departamento)[2] <- "cant"

cant_mujeres <- aggregate(SEXO ~ NOMBDEP, data = data, FUN = function(x) sum(x == "FEMENINO"))
colnames(cant_mujeres)[2] <- "cant_mujeres"

# Conteo de hombres por departamento
cant_hombres <- aggregate(SEXO ~ NOMBDEP, data = data, FUN = function(x) sum(x == "MASCULINO"))
colnames(cant_hombres)[2] <- "cant_hombres"

cant_muertes_departamento <- cant_muertes_departamento %>%
  left_join(poblacion) %>%
  left_join(cant_mujeres) %>%
  left_join(cant_hombres)

cant_muertes_departamento <- cant_muertes_departamento %>% mutate(tasa_mujeres = cant_mujeres/(cant_mujeres+cant_hombres),
                                                                  muertes_pob = cant/(Poblacion*1e-5))

# Promedio de edad de defuncion
promedio_edad_por_departamento <- aggregate(EDAD ~ NOMBDEP, data = data %>% filter(UNIDADES_EDAD == "ANIOS"), FUN = mean)


#### Separar los datos por departamento y año de defuncion ####

cant_muertes_departamento_y_anio <- aggregate(SEXO ~ NOMBDEP + ANIO, data = data, FUN = length)
colnames(cant_muertes_departamento_y_anio)[3] <- "cant"

promedio_edad_departamento_y_anio <- aggregate(EDAD ~ NOMBDEP + ANIO, data = data %>% filter(UNIDADES_EDAD == "ANIOS"), FUN = mean)
colnames(promedio_edad_departamento_y_anio)[3] <- "prom_edad"

# Separamos los datos por genero: FEMENINO
datos_mujeres <- data[data$SEXO == "FEMENINO", ]

# Contar cuántas mujeres hay por departamento y por año de defuncion
mujeres_por_departamento_y_anio <- aggregate(SEXO ~ NOMBDEP + ANIO, data = datos_mujeres, FUN = length)
colnames(mujeres_por_departamento_y_anio)[3] <- "cant_mujeres"

# Separamos los datos por genero: MASCULINO
datos_hombres <- data[data$SEXO == "MASCULINO", ]

# Contar cuántas mujeres hay por departamento y por año de defuncion
hombres_por_departamento_y_anio <- aggregate(SEXO ~ NOMBDEP + ANIO, data = datos_hombres, FUN = length)

cant_muertes_departamento_y_anio <- cant_muertes_departamento_y_anio %>%
  left_join(poblacion) %>%
  left_join(mujeres_por_departamento_y_anio)

cant_muertes_departamento_y_anio <- cant_muertes_departamento_y_anio %>% mutate(tasa_mujeres = cant_mujeres/cant,
                                                                                muertes_pob = cant/(Poblacion*1e-5))
#### Grafica muertes por año ####

cant_muertes_departamento_y_anio %>%   
  group_by(ANIO, NOMBDEP) %>% 
  ggplot(aes(x = ANIO, y = cant/1000)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Muertes por año", 
       x = "Año", 
       y = "Miles de muertes") +
  facet_wrap(~NOMBDEP,scales = "free_y")
ggsave(filename = paste0(wd$outputs, "Muertes_anio.png"),
       width = 12, height = 12)

#### Grafica edad promedio por año ####

promedio_edad_departamento_y_anio %>%   
  group_by(ANIO, NOMBDEP) %>% 
  ggplot(aes(x = ANIO, y = prom_edad)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Esperanza de vida promedio por año", 
       x = "Año", 
       y = "Edad (Años)") +
  facet_wrap(~NOMBDEP,scales = "free_y")
ggsave(filename = paste0(wd$outputs, "Esperanza_vida_anio.png"),
       width = 12, height = 12)

#### Grafica de datos espaciales por departamento ####

peru_datos <- peru_sf %>%
  left_join(cant_muertes_departamento) %>%
  left_join(promedio_edad_por_departamento)

# Grafica de muertes por cantidad de habitantes por departamento
ggplot(data=peru_datos) + 
  geom_sf(aes(fill = muertes_pob)) + theme_void() +
  scale_fill_distiller(palette = "Spectral") +  
  labs(title = "Muertes por cada 100,000 habitantes",
       caption = "Fuente de datos: SINADEF",
       x = "Longitud",
       y = "Latitud",
       fill = "Muertes sobre poblacion") + 
  geom_text_repel(mapping = aes(coords_x, coords_y, label = NOMBDEP), size = 2)
ggsave(filename = paste0(wd$outputs, "MapaMuertesPorPoblacion.png"),
       width = 8.5, height = 11)

# Grafica de esperanza de vida por departamento
ggplot(data=peru_datos) + 
  geom_sf(aes(fill = EDAD)) + theme_void() +
  scale_fill_distiller(palette = "Spectral") +  
  labs(title = "Esperanza de vida promedio",
       caption = "Fuente de datos: SINADEF",
       x = "Longitud",
       y = "Latitud",
       fill = "Edad") + 
  geom_text_repel(mapping = aes(coords_x, coords_y, label = NOMBDEP), size = 2)
ggsave(filename = paste0(wd$outputs, "MapaEsperanzaVida.png"),
       width = 8.5, height = 11)

# Grafica de tasa de mortalidad sexo femenino
ggplot(data=peru_datos) + 
  geom_sf(aes(fill = tasa_mujeres)) + theme_void() +
  scale_fill_distiller(palette = "Spectral") +  
  labs(title = "Tasa de fallecidos de sexo femenino",
       caption = "Fuente de datos: SINADEF",
       x = "Longitud",
       y = "Latitud",
       fill = "Tasa mortalidad sexo femenino") + 
  geom_text_repel(mapping = aes(coords_x, coords_y, label = NOMBDEP), size = 2)
ggsave(filename = paste0(wd$outputs, "MapaTasaMujeres.png"),
       width = 8.5, height = 11)


#### Grafica de datos espaciales por departamento ####

peru_anio_datos <- peru_sf %>%
  left_join(cant_muertes_departamento_y_anio) %>%
  left_join(promedio_edad_departamento_y_anio)

# Grafica de muertes por cantidad de habitantes por departamento y año
ggplot(data=peru_anio_datos) + 
  geom_sf(aes(fill = muertes_pob)) + theme_void() +
  scale_fill_distiller(palette = "Spectral") +  
  labs(title = "Muertes por cada 100,000 habitantes",
       caption = "Fuente de datos: SINADEF",
       x = "Longitud",
       y = "Latitud",
       fill = "Muertes sobre poblacion") +
  facet_wrap(~ANIO) + 
  geom_text_repel(mapping = aes(coords_x, coords_y, label = NOMBDEP), size = 2,
                  max.overlaps = 100)
ggsave(filename = paste0(wd$outputs, "MapaMuertesPorPoblacionAnio.png"),
       width = 12, height = 12)

# Grafica de esperanza de vida por departamento y año
ggplot(data=peru_anio_datos %>% filter(ANIO > 2019)) + 
  geom_sf(aes(fill = prom_edad)) + theme_void() +
  scale_fill_distiller(palette = "Spectral") +  
  labs(title = "Esperanza de vida promedio",
       caption = "Fuente de datos: SINADEF",
       x = "Longitud",
       y = "Latitud",
       fill = "Edad") +
  facet_wrap(~ANIO) + 
  geom_text_repel(mapping = aes(coords_x, coords_y, label = NOMBDEP), size = 2,
                  max.overlaps = 100)
ggsave(filename = paste0(wd$outputs, "MapaEsperanzaVidaAnio.png"),
       width = 12, height = 12)

# Grafica de muertes por cantidad de habitantes por departamento y año
ggplot(data=peru_anio_datos) + 
  geom_sf(aes(fill = tasa_mujeres)) + theme_void() +
  scale_fill_distiller(palette = "Spectral") +  
  labs(title = "Tasa de fallecidos de sexo femenino",
       caption = "Fuente de datos: SINADEF",
       x = "Longitud",
       y = "Latitud",
       fill = "Tasa mortalidad sexo femenino") +
  facet_wrap(~ANIO) + 
  geom_text_repel(mapping = aes(coords_x, coords_y, label = NOMBDEP), size = 2,
                  max.overlaps = 100)
ggsave(filename = paste0(wd$outputs, "MapaTasaMortalidadSexoFemeninoAnio.png"),
       width = 12, height = 12)