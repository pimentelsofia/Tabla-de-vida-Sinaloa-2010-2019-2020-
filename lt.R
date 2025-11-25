##
##
#
#                          Trabajo final del curso de Demografía
#                                        2026-1
#                             Facultad de Ciencias UNAM
#                         Tabla de Mortalidad Sinaloa 2010-2020
#                                         lt
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
source("Script/Funciones.R")
library(readxl)
library(reshape2)
library(lubridate)
library(ggplot2)
library(data.table)
library(dplyr)

# Carga de tablas de datos ----
def <- fread("data/def")
apv <- fread("data/apv.csv") 

# Unión de tablas de Años Persona Vividos y Defunciones ----
lt_input <- setDT(left_join(apv, def, by = c("year", "sex", "age")))

# Cálculo de mx ----
lt_input[ , mx := deaths/N]
lt_input[ , sex := if_else(sex=="male", "m", "f")]

## Gráfica - mx por año y sexo ----
ggplot(lt_input, aes(x = age, y = log(mx), color = sex, group = sex)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.5) +
  facet_wrap(~ year, ncol = 2) +
  scale_color_manual(
    values = c("m" = "steelblue", "f" = "lightcoral"),
    labels = c("m" = "Hombres", "f" = "Mujeres")
  ) +
  labs(
    title = "Tasa de mortalidad de México por año y sexo Sinaloa",
    x = "Edad",
    y = "log(mx)",
    color = "Sexo"
  ) +
  theme_minimal() 

## Gráfica - mx por sexo y año ----
ggplot(lt_input, aes(x = age, y = log(mx), color = factor(year), group = year)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.2) +
  facet_grid(. ~ sex, labeller = as_labeller(c("m" = "Hombres", "f" = "Mujeres"))) +
  scale_color_manual(
    values = c("2010" = "steelblue", "2019" = "brown", "2021" = "green"),
    name = "Año"
  ) +
  labs(
    title = "Evolución de la tasa de mortalidad de Sinaloa",
    #    subtitle = "Sinaloa",
    x = "Edad",
    y = "log(mx)"
  ) +
  theme_minimal()



# Tablas de mortalidad nacional - eevv + censales 2010, 2019 ----

lt_output <- data.table()

for( s in c( 'm', 'f' ) ){
  for( y in unique( lt_input$year ) ){
    
    temp_dt <- lt_input[ sex == s & year == y ]
    
    temp_lt <-
      lt_abr(x = temp_dt$age, 
             mx = temp_dt$mx, 
             sex = s) %>%
      setDT %>%
      .[ , year := y ] %>%
      .[ , sex := s ]
    
    lt_output <- 
      rbind(
        lt_output,
        temp_lt[ , .( lt_desc = 'LT VR/Census, MEX',
                      year = y, 
                      sex,
                      age = x, 
                      mx = round( mx, 6 ), 
                      qx = round( qx, 6 ),
                      ax = round( ax, 2 ), 
                      lx = round( lx, 0 ), 
                      dx = round( dx, 0 ), 
                      Lx = round( Lx, 0 ), 
                      Tx = round( Tx, 0 ), 
                      ex = round( ex, 2 )) ]
      )
    
  }
  
}

## Esperanzas de vida al nacer ----
lt_output[ age == 0 ] %>% dcast( year ~ sex, value.var = 'ex' )

## Mortalidad infantil ----
lt_output[ age == 0 ] %>% dcast( year ~ sex, value.var = 'qx' )

## Gráfica - qx por sexo y año ---- 
lt_output %>%
  ggplot( ) +
  geom_line( aes( x = age, y = qx, color = factor( year ), group = factor(year ) ), size = 1 ) +
  scale_y_log10() +
  scale_color_manual(
    values = c(
      "2010" = "#C75DAA",  # Pinkish-purple
      "2019" = "#40E0D0",  # Turquoise
      "Other Years"      = "gray70"    # Light gray for other years
    ),
    name = "Años"
  ) +
  facet_wrap( ~ sex, ) +
  labs(color='año') +   
  # theme_bw()
  theme_classic() +
  ylab("Probabilidad de muerte (qx)") +
  xlab("Edad") +
  labs(colour = "Años")

write.csv(lt_output, "Data/tabla_mortalidad.csv", row.names = F)
# -------- FIN ----------*