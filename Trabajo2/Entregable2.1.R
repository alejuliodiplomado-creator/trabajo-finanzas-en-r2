install.packages("usethis")
library(usethis)
use_git()
use_github()


#Librerias usadas

library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)

#Carga de la data

clientes <- read_excel("data/clientes.xlsx")
facturas <- read_excel("data/facturas.xlsx")

#para saber que contienen las filas y columnas
glimpse(clientes)
glimpse(facturas)

#para conocer los encabezados de cada columna 
names(clientes)
names(facturas)

