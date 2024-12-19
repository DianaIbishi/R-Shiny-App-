# Install Packages -------------------------------------------------------------

library(shiny)
library(ggplot2)
library(leaflet)
library(dplyr)
library(plotly)


# Create the app ---------------------------------------------------------------


# Define UI for application
ui <- fluidPage(
  titlePanel("Daily Weather Trends of Kosovo"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown to select the variable to plot
      selectInput("variable", 
                  "Select Weather Metric:", 
                  choices = c("Rainfall (mm)" = "rain_sum_flat", 
                              "Snowfall (cm)" = "snowfall_sum_flat", 
                              "Max Temperature (°C)" = "temperature_2m_max_flat")),
      
      # Date range input for filtering
      dateRangeInput("date_range", 
                     "Select Date Range:",
                     start = min(prepared_data$dates),
                     end = max(prepared_data$dates)),
      
      textOutput("clicked_point")
    ),
    
    mainPanel(
      # Display the plot
      plotlyOutput("linePlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Reactive subset of data based on input
  filtered_data <- reactive({
    prepared_data %>%
      filter(dates >= input$date_range[1], dates <= input$date_range[2])
  })
  
  # Define custom labels and colors for each metric
  metric_labels <- c(
    rain_sum_flat = "Rainfall (mm)",
    snowfall_sum_flat = "Snowfall (cm)",
    temperature_2m_max_flat = "Max Temperature (°C)"
  )
  
  metric_colors <- c(
    rain_sum_flat = "blue",
    snowfall_sum_flat = "purple",
    temperature_2m_max_flat = "orange"
  )
  
  # Generate the interactive plot using plotly
  output$linePlot <- renderPlotly({
    plot_data <- filtered_data()
    
    metric_colors <- metric_colors[input$variable]
    
    plot_ly(data = plot_data, x = ~dates, y = ~.data[[input$variable]], 
            type = 'scatter', mode = 'markers',
            marker = list(color = metric_colors)) %>%
      layout(
        title = paste("Daily Trend of", metric_labels[[input$variable]]),
        xaxis = list(title = "Date"),
        yaxis = list(title = metric_labels[[input$variable]])
      )
  })
  
  # Show value of clicked point
  output$clicked_point <- renderText({
    # Get the click event information
    event_data <- event_data("plotly_click")
    
    # If there is a click event, display the value
    if (!is.null(event_data)) {
      clicked_date <- event_data$x
      clicked_value <- event_data$y
      
      paste("You clicked on:", clicked_date, "with a value of", 
            clicked_value, metric_labels[[input$variable]])
    } else {
      "Click on a point to see the value."
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

