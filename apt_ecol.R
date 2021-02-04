#####################################################################
#####################################################################
#### Script para la elaboración de mapas de aptitud ecológica   #####
#### Elaborado por MC Luis Daniel Olivares Martínez             #####
#### Bucerías, Bahía de Banderas, Nayarit, México               #####
#### 04 de febrero del 2021                                     #####
#####################################################################
#####################################################################

#Librerías requeridas
library(raster)
library(rgdal)
library(XML)
library(rgdal)
library(gdalUtils)
library(httr)

#### PASO 1: Ingresar la información de entrada y cargar las funciones y objetos de trabajo ####
# Funciones de entrada
aptitud <- function(capa){
  dum <- ifelse(capa$apti == 'Alta', 3,
                ifelse(capa$apti == 'Media' ,2,
                       ifelse(capa$apti == 'Baja', 1, -100)))
  return(dum)
} # Función para pasar de apti a aptitud (clases a números)

mapa_apt <- function(n, sector){
  apti <- raster("_apt_sum.tif")
  apt <- apti/n
  
  apt[apt <= 0] <- 0
  apt[apt <= 1 & apt > 0] <- 1
  apt[apt <= 2 & apt > 1] <- 2
  apt[apt <= 3 & apt > 2] <- 3
  
  nombre <- paste0("_apt_",sector,".tif")
  writeRaster(apt, nombre, format= "GTiff")
}

# Es necesario tener la proyección (CRS) de todos los raters y vectores iguales, eso se puede hacer desde R o desde el software SIG con la herramienta de reproyección
# A continuación ejemplos de CRS (preferentemente uno que tenga unidades métricas como UTM o LCC para poder definir fácilmente el tamaño de la celda raster)
# WGS 84 / UTM zone 13N - EPSG:32613
# Mexico ITRF92 / UTM zone 13N - EPSG:4486

# Objeto raster de entrada, para ingresarlo será importante consultar en QGIS o cualquier software SIG el área de trabajo
# los márgenes se consulta desde la barra de coordenadas de QGIS, viendo las coordenadas del área de visualización para saber los valores de x y y mínimos y máximos
# una vez conocidos estos valores se ajustan en un software de hojas de cálculo o en R para conocer el siguiente valor múltiplo del tamaño de lado deseado para cada celda del raster

r <- raster(res = 30, xmn=431310, xmx=509490, ymn=2281230,ymx=2325300, crs="+init=EPSG:32613") # Plantilla raster con 30 m de lado para una rejilla

# extent(r) # función para conocer los márgenes del área del raster (la cual debe ser mayor al área de estudio


#### PASO 2: Cargar las capas de entrada y asociar criterios de aptitud ####
# setwd("directorio_de_trabajo_con_las_capas")
{ # Edafología Agrícola
#  setwd("directorio_de_trabajo_con_las_capas")
  
  soil <- readOGR("Edafología_UTM13N.shp", encoding = "system") # Capa vectorial con los datos de unidades de suelo
  
  soil$apti <- ifelse(soil$GRUPO1 == 'AR' | soil$CALIFS_G1 == 'sk'|
                        soil$GRUPO1 == 'LP' | soil$GRUPO1 == 'PH' & soil$CALIFP_G1 == 'so', 'Baja',
                      ifelse(soil$GRUPO1 == 'CM' & soil$CALIFP_G1 == 'lep' &  soil$CALIFS_G1 == 'eu'|
                               soil$GRUPO1 == 'LV', 'Media', ifelse(soil$GRUPO1 == 'SC', 0, 'Alta')))
  soil$apti[soil$CLAVE_WRB == 'NA'] <-  0  # Criterios de aptitud para la capa de entrada, imporante tener los strings 'Alta', 'Media', 'Baja' y 0 (para un uso sin aptitud)
  
  soil$aptitud <- aptitud(soil) # Función para cambiar las clases de aptitud a notación ordinal
  writeOGR(soil, dsn = ".", layer = "Edafología_UTM13N_atrib", driver = "ESRI Shapefile", overwrite_layer = TRUE) # Guardado del archivo vectorial con las modificaciones hechas
  
  # setwd("directorio_de_guardado_de_la_capa")
  S_uso <- rasterize(soil,r, "aptitud") # Rasterización según la plantilla de entrada
  writeRaster(S_uso, "_class_soil.tif", format= "GTiff", overwrite = T) # Guardado del raster con los criterios de aptitud
} # Edafología Agrícola

{ # Pendiente Agrícola
# setwd("directorio_de_trabajo_con_las_capas")
  pend <- raster("Pendientes_por.tif") # Capa raster con los valores de pendientes procesados a partir de un MDE
  
# setwd("directorio_de_guardado_de_la_capa")
  m <- matrix(c(0, 10, 3,  10, 20, 2,  20, 35, 1,  35, 250, -100), ncol=3, byrow=TRUE) # Criterios de re-asignación para el raster
  pend <- reclassify(pend, m, filename = "_class_pend.tif", include.lowest=T, right = T, overwrite = T) # Esta función guarda al mismo tiempo el objeto en la ruta de trabjo con el nombre especificado
  # You can also supply a vector that can be coerced into a n*3 matrix (with byrow=TRUE)
  
} # Pendiente Agrícola

{ # Precipitación Agrícola
# setwd("directorio_de_trabajo_con_las_capas")
  prec <- raster("w001001.adf") #si el objeto fue guardado en arcGIS como una carpeta de archivos raster se puede acceder al archivo de interés con el que tenga este nombre
  
# setwd("directorio_de_guardado_de_la_capa")
  m <- matrix(c(0, 500, 1,  500, 1000, 2,  1000, 5000, 3), ncol=3, byrow=TRUE) # Criterios de re-asignación para el raster
  preci <- reclassify(prec, m, filename = "_class_prec.tif", include.lowest=T, right = F, overwrite = T) # You can also supply a vector that can be coerced into a n*3 matrix (with byrow=TRUE)
  
} # Precipitación Agrícola

{ # Temperatura Agrícola
# setwd("directorio_de_trabajo_con_las_capas")
  tempe <- raster("temp_krig_arc1.tif") # Capa raster con los valores de precipitación interpolados mediante interpolación de los puntos extraidos de las isoyetas
  
  m <- matrix(c(-5, 18, 2,  18, 25, 3,  25, 28, 2,  28, 40, 1), ncol=3, byrow=TRUE) # Criterios de re-asignación para el raster
  Temp <- reclassify(tempe, m, filename = "_class_temp.tif", include.lowest=T, right = F, overwrite = T) # You can also supply a vector that can be coerced into a n*3 matrix (with byrow=TRUE)
  
} # Temperatura Agrícola

#### PASO 3: Generar mapas de suma de todos los rasters de criterios de aptitud ####
# 

#### PASO 4: Generar el mapa de aptitud ponderada ####
