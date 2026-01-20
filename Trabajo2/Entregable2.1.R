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

glimpse(clientes)
glimpse(facturas)
names(clientes)
names(facturas)

setwd("D:/R/Entregable2/trabajo-finanzas-en-r2/Trabajo2")
getwd()
git pull()