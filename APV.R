##
##
#
#                          Trabajo final del curso de Demografía
#                                        2026-1
#                             Facultad de Ciencias UNAM
#                         Tabla de Mortalidad Sinaloa 2010-2020
#                                 Años-Persona vividos 
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

# Prámbulo ----

## Limpieza de gráficas
graphics.off()

## LImpieza de memoria ----
rm(list = ls())

## Carga de paquetes y funciones ----

source("Script/funciones.R")
library(readxl)
library(reshape2)
library(lubridate)
library(ggplot2)
library(data.table)
library(dplyr)

## Carga de tabla de datos ----
censos_pro <- fread("Data/censos_pro.csv")

# Cálculo de APV 2010 ----
N <- expo(censos_pro[year==2010] %>% .$pop, 
          censos_pro[year==2020] %>% .$pop, 
          t_0 = "2010-06-25", t_T = "2020-03-15", t = 2010.5)

apv2010 <- censos_pro[year==2010, .(age, sex, N)]
apv2010[ , year := 2010]

ggplot(apv2010, aes(x = factor(age), y = ifelse(sex == "male", -N/1e6, N/1e6), fill = sex)) +
  geom_col(width = 0.7, alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) paste0(abs(x), "M"),
    breaks = scales::pretty_breaks(n = 8)
  ) +
  scale_fill_manual(
    values = c("male" = "#1f77b4", "female" = "#d62728"),
    labels = c("male" = "Hombres", "female" = "Mujeres")
  ) +
  labs(
    title = "Pirámide Poblacional 2010 Sinaloa",
    subtitle = "Distribución por edad y sexo",
    x = "Grupo de edad",
    y = "Población mitad de año (millones)",
    fill = "Sexo",
    caption = "Fuente: INEGI"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank()
  )

# Calculo APV 2019 ----
N <- expo(censos_pro[year==2010] %>% .$pop, 
          censos_pro[year==2020] %>% .$pop, 
          t_0 = "2010-06-25", t_T = "2020-03-15", t = 2019.5)

apv2019 <- censos_pro[year==2020, .(age, sex, N)]
apv2019[, year := 2019]

ggplot(apv2019, aes(x = factor(age), y = ifelse(sex == "male", -N/1e6, N/1e6), fill = sex)) +
  geom_col(width = 0.7, alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) paste0(abs(x), "M"),
    breaks = scales::pretty_breaks(n = 8)
  ) +
  scale_fill_manual(
    values = c("male" = "#1f77b4", "female" = "#d62728"),
    labels = c("male" = "Hombres", "female" = "Mujeres")
  ) +
  labs(
    title = "Pirámide Poblacional 2019 Sinaloa",
    subtitle = "Distribución por edad y sexo",
    x = "Grupo de edad",
    y = "Población mitad de año (millones)",
    fill = "Sexo",
    caption = "Fuente: INEGI"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank()
  )

# Calculo APV 2021 ----
N <- expo(censos_pro[year==2010] %>% .$pop, 
          censos_pro[year==2020] %>% .$pop, 
          t_0 = "2010-06-25", t_T = "2020-03-15", t = 2021.5)

apv2021 <- censos_pro[year==2020, .(age, sex, N)]
apv2021[, year := 2021]

ggplot(apv2021, aes(x = factor(age), y = ifelse(sex == "male", -N/1e6, N/1e6), fill = sex)) +
  geom_col(width = 0.7, alpha = 0.8) +
  coord_flip() +
  scale_y_continuous(
    labels = function(x) paste0(abs(x), "M"),
    breaks = scales::pretty_breaks(n = 8)
  ) +
  scale_fill_manual(
    values = c("male" = "#1f77b4", "female" = "#d62728"),
    labels = c("male" = "Hombres", "female" = "Mujeres")
  ) +
  labs(
    title = "Pirámide Poblacional 2021 Sinaloa",
    subtitle = "Distribución por edad y sexo",
    x = "Grupo de edad",
    y = "Población mitad de año (millones)",
    fill = "Sexo",
    caption = "Fuente: INEGI"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank()
  )


# Consolidar tablas 2010, 2019 y 2021
apv <- rbind(apv2010, apv2019, apv2021)


# Load required libraries
library(ggplot2)
library(dplyr)

# Your data
df <- data.frame(
  age = c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,
          0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,
          0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,
          0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85),
  sex = c(rep("male", 18), rep("female", 18), rep("male", 18), rep("female", 18)),
  N = c(129154.433,135228.271,138113.700,139742.627,122128.648,102509.031,102054.975,102585.497,86874.030,72721.354,63377.930,51264.130,41276.109,31991.680,
        25405.171,15536.508,9072.756,7372.574,124241.701,130315.151,133478.418,136846.005,121758.530,106193.718,105775.046,105844.882,90324.622,77860.091,
        68683.747,53149.670,42714.502,32344.591,25852.844,16275.847,10485.565,9660.442,
        120765.19,128422.19,133000.20,133150.38,127631.32,112713.53,105115.49,100964.16,99033.76,92685.17,81699.05,67252.35,57477.53,44081.13,
        33410.58,23186.06,13981.88,10207.35,116834.75,123305.30,128160.06,129668.07,128561.63,117964.27,109958.16,104923.96,103318.93,96632.35,
        87243.41,72971.51,62818.09,48384.01,35687.66,24384.03,15983.59,13356.13),
  year = c(rep(2010, 36), rep(2020, 36))
)

# Convert male values to negative for pyramid effect
df <- df %>%
  mutate(N = ifelse(sex == "male", -N, N))

# Versión 1: Pirámides sobrepuestas con barras más gruesas
ggplot(df, aes(x = age, y = N, fill = interaction(sex, factor(year)))) +
  geom_bar(stat = "identity", position = "identity", 
           width = 0.8,  # Controla el grosor de las barras
           alpha = 0.8) +
  scale_fill_manual(values = c("male.2010" = "#1f77b4", "female.2010" = "#d62728",
                               "male.2020" = "#aec7e8", "female.2020" = "#ff9896"),
                    labels = c("Male 2010", "Female 2010", "Male 2020", "Female 2020"),
                    name = "Gender & Year") +
  scale_y_continuous(labels = function(x) format(abs(x), big.mark = ",")) +
  coord_flip() +
  labs(title = "Población comparada: 2010 vs 2020",
       x = "Grupo de edad",
       y = "Población") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))

# Versión 2: Pirámides facetadas con barras más gruesas
ggplot(df, aes(x = age, y = N, fill = sex)) +
  geom_bar(stat = "identity", position = "identity", 
           width = 0.8,  # Controla el grosor de las barras
           alpha = 0.8) +
  facet_wrap(~year, ncol = 2) +
  scale_fill_manual(values = c("male" = "#1f77b4", "female" = "#d62728")) +
  scale_y_continuous(labels = function(x) format(abs(x), big.mark = ",")) +
  coord_flip() +
  labs(title = "Piramide de población: 2010 vs 2020",
       x = "Grupo de edad",
       y = "Población",
       fill = "Gender") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))

# Versión 3: Con barras EXTRA gruesas (width = 1)
ggplot(df, aes(x = age, y = N, fill = interaction(sex, factor(year)))) +
  geom_bar(stat = "identity", position = "identity", 
           width = 1,  # Barras extra gruesas
           alpha = 0.8) +
  scale_fill_manual(values = c("male.2010" = "#1f77b4", "female.2010" = "#d62728",
                               "male.2020" = "#aec7e8", "female.2020" = "#ff9896"),
                    labels = c("Male 2010", "Female 2010", "Male 2020", "Female 2020"),
                    name = "Gender & Year") +
  scale_y_continuous(labels = function(x) format(abs(x), big.mark = ",")) +
  coord_flip() +
  labs(title = "Piramide comparada de población: 2010 vs 2020 (Barras Gruesas)",
       x = "Grupo de edad",
       y = "Población") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 14))


## Guardar tabla de APV ----
write.csv(apv, "Data/apv.csv")