#setwd("~/Documents/R/gre_color")
library(tidyverse)
library(npreg)
library(truncnorm)
library(pals)
library(oompaBase)
library(shiny)
#library(rsconnect)

# Define the user interface for the Shiny app
ui <- fluidPage(
  # Add a text input for the GRE score
  textInput(inputId = "gre_score", label = "GRE Score:", value = "150"),
  # Add a checkbox to toggle the use of the noised score
  checkboxInput(inputId = "use_noise", label = "Add Noise", value = TRUE),
  # Add radio buttons to select the color palette
  radioButtons(inputId = "palette", label = "Color Palette:", 
               choices = c("Red to Blue" = "warmcool", "Dark Green to Light Green" = "greenscale", "Blue to Yellow" = "parula"),
               selected = "warmcool"),
  # Add a plot output to display the gre_color plot
  plotOutput(outputId = "gre_color_plot")
)

# Define the server function for the Shiny app
server <- function(input, output) {
  # Reactive function to generate the gre_color plot
  gre_color_plot <- reactive({
    # Calculate the noised score if the "use_noise" checkbox is checked, otherwise use the input GRE score
    if (input$use_noise) {
      score <- as.numeric(input$gre_score) + rtruncnorm(mean=0, sd=1, n = 1, a = -3, b = 3)
    } else {
      score <- as.numeric(input$gre_score)
    }
    
    # Generate the color palette and map the score to a color
    palette_function <- get(input$palette)
    palette <- palette_function(50)
    col_num <- number2color(score, colors = palette, ncol = 50, equidistant = TRUE, xmin = min(127), xmax = max(173))
    
    # Create the data frame with x, y, and z variables set to 0
    x <- 0
    y <- 0
    z <- 1
    df <- tibble(data.frame(cbind(x, y, z)))
    
    # Create the gre_color plot using ggplot2
    gre_color <- ggplot(df, aes(colour = palette)) +
      geom_point(aes(x, y), color = col_num, size = 200, shape = "square") +
      scale_x_continuous(limits = c(-1, 1)) +
      scale_y_continuous(limits = c(-1, 1)) + 
      theme_void()
    gre_color
  })
  
  # Output the gre_color plot to the plot output
  output$gre_color_plot <- renderPlot({
    gre_color_plot()
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
