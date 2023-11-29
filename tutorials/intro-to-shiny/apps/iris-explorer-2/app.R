library(shiny)
library(plotly)
library(gridlayout)
library(bslib)

# create named vector for mapping of labels to variable names
vars <- c("Sepal Length (cm)" = "Sepal.Length",
          "Sepal Width (cm)" = "Sepal.Width",
          "Petal Length (cm)" = "Petal.Length",
          "Petal Width (cm)" = "Petal.Width")

ui <- grid_page(
  layout = c(
    "header  header",
    "sidebar plot  "
  ),
  row_sizes = c(
    "100px",
    "1fr"
  ),
  col_sizes = c(
    "205px",
    "1fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "sidebar",
    card_body(
      selectInput(
        inputId = "yvar",
        label = "Choose Y Variable",
        choices = vars,
      ),
      selectInput(
        inputId = "xvar",
        label = "Choose X Variable",
        choices = vars,
        selected = vars[[2]]
      ),
      checkboxInput(
        inputId = "species",
        label = "Color by Species",
        value = TRUE
      )
    )
  ),
  grid_card_text(
    area = "header",
    content = "Shiny Iris Explorer App!",
    alignment = "start",
    is_title = FALSE
  ),
  grid_card(
    area = "plot",
    card_body(
      tabsetPanel(
        nav_panel(
          title = "ggplot",
          plotOutput(outputId = "plot1")
        ),
        nav_panel(
          title = "plotly",
          plotlyOutput(outputId = "plot2")
        )
      )
    )
  )
)


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

shinyApp(ui, server)
  

