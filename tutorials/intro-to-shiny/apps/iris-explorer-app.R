### ----- Load packages ----

library(shiny)
library(tidyverse)
library(plotly)

### ---- Define UI ----

# create named vector for mapping of labels to variable names
vars <- c("Sepal Length (cm)" = "Sepal.Length",
          "Sepal Width (cm)" = "Sepal.Width",
          "Petal Length (cm)" = "Petal.Length",
          "Petal Width (cm)" = "Petal.Width")

ui <- fluidPage(
  
  titlePanel(title = "Shiny Iris Explorer App!"),
  
  sidebarPanel(
  
    # create dropdowns of variable names to plot
    selectInput(inputId = "yvar",
                label = "Choose Y Variable",
                choices = vars),
    selectInput(inputId = "xvar",
                label = "Choose X Variable",
                choices = vars,
                selected = vars[[2]]),

    # create checkbox to optionally color by a categorical variable
    checkboxInput(inputId = "species",
                  label = "Color by Species",
                  value = TRUE)
  ),
  
  # create tabs with spots for plots
  mainPanel(
    tabsetPanel(
      tabPanel(title = "ggplot",
               plotOutput(outputId = "plot1")),
      tabPanel(title = "plotly",
               plotlyOutput(outputId = "plot2"))
    )
  )
)

### ---- Define server ----

server <- function(input, output) {
  
  # update dropdown
  observeEvent(input$yvar, {
    updateSelectInput(inputId = "xvar", choices = vars[vars != input$yvar])
  })
  
  output$plot1 <- renderPlot({  

    # build points layer
    # -> conditionally colored variable -> selected via user input
    if(input$species == TRUE){
      p1 <- ggplot() +
        geom_point(aes(x = .data[[input$xvar]],
                       y = .data[[input$yvar]],
                       color = Species),
                   data = iris)
    }else{
      p1 <- ggplot() +
        geom_point(aes(x = .data[[input$xvar]],
                       y = .data[[input$yvar]]),
                   data = iris)
    }
    
    # add nice labels and theme
    p1 <- p1 + 
      labs(x = names(vars)[vars == input$xvar],
           y = names(vars)[vars == input$yvar]) + 
      theme_bw()
    
    print(p1)
    
  })
  
  output$plot2 <- renderPlotly({

    # build base plot
    p2 <- iris %>%
      plot_ly()

    # build points layer with plotly
    # -> conditionally colored variable -> selected via user input
    if(input$species == TRUE){
      p2 <- p2 %>%
        add_markers(x = paste0("~", input$xvar) %>% as.formula,
                    y = paste0("~", input$yvar) %>% as.formula,
                    color = ~Species)
    }else{
      p2 <- p2 %>%
        add_markers(x = paste0("~", input$xvar) %>% as.formula,
                    y = paste0("~", input$yvar) %>% as.formula,
                    color = I("black")) %>% 
        hide_legend()
    }

    print(p2)

  })
}

### ---- Run app ----

shinyApp(ui, server)

