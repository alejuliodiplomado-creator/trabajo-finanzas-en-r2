library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(scales)

# -------------------------
# Carga de datos
# -------------------------
datos <- read_csv("resumen_cobranza.csv")

# -------------------------
# UI
# -------------------------
ui <- fluidPage(
  
  titlePanel("Dashboard de Cobranza"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "riesgo",
        "Nivel de Riesgo:",
        choices = c("Todos", unique(datos$Riesgo)),
        selected = "Todos"
      ),
      
      sliderInput(
        "dias",
        "Días de Mora Promedio:",
        min = floor(min(datos$DiasMoraProm, na.rm = TRUE)),
        max = ceiling(max(datos$DiasMoraProm, na.rm = TRUE)),
        value = c(
          floor(min(datos$DiasMoraProm, na.rm = TRUE)),
          ceiling(max(datos$DiasMoraProm, na.rm = TRUE))
        )
      )
    ),
    
    mainPanel(
      
      h4("Indicadores Generales"),
      fluidRow(
        column(4, strong("Clientes:"), textOutput("n_clientes")),
        column(4, strong("Monto Vencido Total:"), textOutput("monto_total")),
        column(4, strong("Mora Promedio:"), textOutput("mora_prom"))
      ),
      
      hr(),
      
      h4("Relación Mora vs Monto Vencido"),
      plotOutput("grafico"),
      
      hr(),
      
      h4("Detalle por Cliente"),
      tableOutput("tabla"),

    )
  )
)

# -------------------------
# Server
# -------------------------
server <- function(input, output) {
  
  datos_filtrados <- reactive({
    df <- datos
    
    if (input$riesgo != "Todos") {
      df <- df %>% filter(Riesgo == input$riesgo)
    }
    
    df %>%
      filter(
        DiasMoraProm >= input$dias[1],
        DiasMoraProm <= input$dias[2]
      )
  })
  
  output$n_clientes <- renderText({
    nrow(datos_filtrados())
  })
  
  output$monto_total <- renderText({
    comma(sum(datos_filtrados()$MontoVencido, na.rm = TRUE))
  })
  
  output$mora_prom <- renderText({
    round(mean(datos_filtrados()$DiasMoraProm, na.rm = TRUE), 1)
  })
  
  output$grafico <- renderPlot({
    ggplot(datos_filtrados(),
           aes(x = DiasMoraProm, y = MontoVencido, color = Riesgo)) +
      geom_point(size = 3, alpha = 0.7) +
      scale_y_continuous(
        labels = scales::comma_format(big.mark = ".", decimal.mark = ",")
      ) +
      labs(
        x = "Días de Mora Promedio",
        y = "Monto Vencido",
        title = "Mora promedio vs Monto vencido"
      ) +
      theme_minimal()
  })
  
  output$tabla <- renderTable({
    datos_filtrados() %>%
      select(Cliente, Riesgo, DiasMoraProm, MontoVencido)
  })
  

}

# -------------------------
# Run App
# -------------------------
shinyApp(ui, server)