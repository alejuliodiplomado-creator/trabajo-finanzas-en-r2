#1. Librerias usadas

library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)

#2. Carga de la data

clientes <- read_excel("data/clientes.xlsx")
facturas <- read_excel("data/facturas.xlsx")

#2.1.para saber que contienen las filas y columnas
glimpse(clientes)
glimpse(facturas)

#2.2.para conocer los encabezados de cada columna 
names(clientes)
names(facturas)

#3. limpieza y preparacion de la data

# 3.1. Si el archivo viene con Rut o RUT con espacios se debe normalizar
names(clientes) <- trimws(names(clientes))
names(facturas) <- trimws(names(facturas))

# 3.2. estandarizacion de las fechas para que sean en formato DD/MM/AAAA

facturas <- facturas %>%
  mutate(
    Emisión     = dmy(Emisión),
    Vencimiento = dmy(Vencimiento),
    FechaPago   = dmy(Pago)
  )

#3.3 base para calcular el estado de cada factura y los dias de mora

hoy <- Sys.Date()

facturas <- facturas %>%
  mutate(
    Estado = case_when(
      !is.na(FechaPago) ~ "Pagado",
      Vencimiento < hoy ~ "Vencido",
      TRUE ~ "Por vencer"
    ),
    Dias_Mora = if_else(Estado == "Vencido",
                        as.numeric(hoy - Vencimiento),
                        0)
  )

#4.