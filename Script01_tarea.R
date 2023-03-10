#### Configuraciones Iniciales ####

# Limpiar memoria
rm(list = ls())
library(tidyverse)
library(sf)
library(ggrepel)
library(RColorBrewer)

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

Departamentos <- unique(peru_sf$NOMBDEP)

#### Dataset ####
sinadef <- read.csv(paste0(wd$datasets, "fallecidos_sinadef.csv"), sep=';')

data <- sinadef %>% filter(PAIS == "PERU")
data <- data %>% filter(NOMBDEP != "     ")
data <- data %>% filter(NOMBDEP != "SIN REGISTRO")
data$EDAD <- as.integer(data$EDAD)

unique(data$NOMBDEP)

#### Separar datos por Departamento ####

# Conteo de mujeres por departamento
cant_mujeres <- aggregate(SEXO ~ NOMBDEP, data = data, FUN = function(x) sum(x == "FEMENINO"))
colnames(cant_mujeres)[2] <- "cant_mujeres"

# Conteo de hombres por departamento
cant_hombres <- aggregate(SEXO ~ NOMBDEP, data = data, FUN = function(x) sum(x == "MASCULINO"))
colnames(cant_hombres)[3] <- "cant_hombres"


# Promedio de edad de defuncion
promedio_edad_por_departamento <- aggregate(EDAD ~ NOMBDEP, data = data %>% filter(UNIDADES_EDAD == "ANIOS"), FUN = mean)


#### Separar los datos por departamento y año de defuncion ####

cant_muertes_departamento_y_anio <- aggregate(INDEX ~ NOMBDEP + ANIO, data = data %>% filter(UNIDADES_EDAD == "ANIOS"), FUN = length)
promedio_edad_departamento_y_anio <- aggregate(EDAD ~ NOMBDEP + ANIO, data = data %>% filter(UNIDADES_EDAD == "ANIOS"), FUN = mean)


# Separamos los datos por genero: FEMENINO
datos_mujeres <- data[data$SEXO == "FEMENINO", ]

# Contar cuántas mujeres hay por departamento y por año de defuncion
mujeres_por_departamento_y_anio <- aggregate(SEXO ~ NOMBDEP + ANIO, data = datos_mujeres, FUN = length)

# Separamos los datos por genero: MASCULINO
datos_hombres <- data[data$SEXO == "MASCULINO", ]

# Contar cuántas mujeres hay por departamento y por año de defuncion
hombres_por_departamento_y_anio <- aggregate(SEXO ~ NOMBDEP + ANIO, data = datos_hombres, FUN = length)
