##
##
#
#                          Trabajo final del curso de Demografía
#                                        2026-1
#                             Facultad de Ciencias UNAM
#                         Tabla de Mortalidad Sinaloa 2010-2020
#                                     Defunciones
#
#         Creado por:               Pimentel Becerril Dulce Sofía
#                                   Javier Magaña
#         Fecha de creación:        04/11/2025
#         Actualizado por:          Pimentel Becerril Dulce Sofía
#                                   Javier Magaña
#         Fecha de actualización:   06/11/2025
#         Contacto:                 
#
##
##

# Preámbulo ----

## Limpieza de gráficas ----
graphics.off()

## Limpieza de memoria ----
rm(list = ls())

## Carga de paquetes y funciones----
source("script/funciones.R")
library(readxl)
library(reshape2)
library(lubridate)
library(ggplot2)
library(data.table)
library(dplyr)

## Carga de tablas de datos ----
def_pro <- fread("Data/def_pro.csv") %>% 
  .[year %in% c(2009, 2010, 2011, 2018, 2019, 2021)]


## calculo del promedio para el años de referencia
def_pro[ , year_new := ifelse( year %in% 2009:2011, 
                               2010,
                               ifelse( year %in% 2018:2019,
                                       2019,
                                       year ) ) ]

# datos preparados de defunciones
def <- 
  def_pro[ , 
           .( deaths = mean( deaths ) ),
           .( year = year_new, sex, age ) ] 

# Grafica defunciones

def_pro <- fread("Data/def_pro.csv")
def <- read.csv("Data/def")


# 1. Defunciones por edad (ambos sexos)
ggplot(def, aes(x = age, y = deaths, color = factor(year))) +
  geom_line(size = 1) +
  labs(title = "Defunciones por edad en Sinaloa",
       subtitle = "Años de referencia 2010 y 2019",
       x = "Edad",
       y = "Número promedio de defunciones",
       color = "Año") +
  theme_minimal()

## 2. Defunciones por edad y sexo
ggplot(def, aes(x = age, y = deaths, color = sex)) +
  geom_line(size = 1) +
  facet_wrap(~ year) +
  labs(title = "Defunciones por edad y sexo en Sinaloa",
       x = "Edad",
       y = "Número promedio de defunciones",
       color = "Sexo") +
  theme_minimal()

# Guardar tabla de DEF ----
write.csv(def, "Data/def", row.names = F)