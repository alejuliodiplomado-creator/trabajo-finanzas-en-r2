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

#4.integracion de los datos de clientes con la facturaciona travesd de la llave RUT

#me di cuenta quue en la tabla clientes la columna RUT se llama "RUT Plataforma Sheriff"
#aqui lo cambio para no tener error

clientes <- clientes %>%
  rename(Rut = `RUT Plataforma Sheriff`)

#genera una tabla df_cobranza que une ambas tablas por Rut

df_cobranza <- facturas %>%
  left_join(clientes, by = "Rut")

#quiero confirmar que no existan datos faltantes en la columna clientes (razon social)

sum(is.na(df_cobranza$Cliente.x))

#5. resumen de riesgo por cliente (output minimo esperado)

resumen <- df_cobranza %>%
  group_by(Rut, Cliente.x) %>%
  summarise(
    NumFacturas = n(),
    NumVencidas = sum(Estado == "Vencido"),
    MontoVencido = sum(Total[Estado == "Vencido"], na.rm = TRUE),
    DiasMoraProm = mean(Dias_Mora[Estado == "Vencido"], na.rm = TRUE),
    .groups = "drop"
  )

#6 dado lo anterior el objetivo es visualizar el top 10 de los clientes con mas
#monto vencido. se debe graficar

top10 <- resumen %>%
  arrange(desc(MontoVencido)) %>%
  slice(1:10)

ggplot(top10, aes(x = reorder(Cliente.x, MontoVencido), y = MontoVencido)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 10 clientes por monto vencido",
       x = "Cliente", y = "Monto vencido")

#7 como agregado estos resultados deben ser verificados manualemnte por lo que
#se debe exportar

dir.create("output", showWarnings = FALSE)

write.csv(resumen, "output/resumen_cobranza.csv", row.names = FALSE)
write.csv(df_cobranza, "output/base_cobranza.csv", row.names = FALSE)

ggsave("output/top10_monto_vencido.png", width = 9, height = 4, dpi = 150)