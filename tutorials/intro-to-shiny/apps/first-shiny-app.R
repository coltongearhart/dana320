### ----- Load packages ----

library(shiny)

### ---- Define UI ----

ui <- fluidPage(
  "Hello, world!"
)

### ---- Define server ----

server <- function(input, output) {
}

### ---- Run app ----

shinyApp(ui, server)

