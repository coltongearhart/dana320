# create input list
input <- list()
input$xvar <- "Petal.Width"
input$yvar <- "Sepal.Length"
input$species <- TRUE

# create base plot
p1 <- ggplot() + 
  theme_bw()

# add points layer
# -> conditionally colored variable -> selected via user input
if(input$species == TRUE){
  p1 <- p1 +
    geom_point(aes(x = .data[[input$xvar]],
                   y = .data[[input$yvar]],
                   color = Species),
               data = iris)
}else{
  p1 <- p1 +
    geom_point(aes(x = .data[[input$xvar]],
                   y = .data[[input$yvar]]),
               data = iris)
}

print(p1)
