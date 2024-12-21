# For this R-Project I am building an interactive shiny app with the shiny package.
# Through a free weather API I will get the data from Kosovo 2023. My goal for this 
# project is to create a shiny app where someone can choose a specific category like:
# rain, snowfall and max temperature. The shiny app will be accompanied by a 
# visualization. 



# Load Packages ---------------------------------------------------------------

library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(ggplot2)


# Prepare the data ------------------------------------------------------------


url <- "https://archive-api.open-meteo.com/v1/archive"

latitude <- c(42.6629, #Prishtina
              42.6530, #Istog
              42.4632, #Gjilan
              42.8833, #Mitrovica
              42.6744, #Peja
              42.3800, #Gjakove
              42.2111, #Prizren
              42.2141 ) #Suhareka

longitude <- c(21.1655,
               20.2883,
               21.4699,
               20.8681,
               20.2900,
               20.4294,
               20.7394,
               20.4943)

start_date <- "2023-01-01"  # I actually wanted the data from 2000 until 2023.
end_date <- "2023-12-31"    # I exceeded the API calls that's why it did not work
# I have shortened the timeframe
daily <- c("snowfall_sum",
           "rain_sum",
           "temperature_2m_max")

# Now I create a tibble of my values

params_tibble <- tibble(
  latitude = latitude,
  longitude = longitude,
  start_date = start_date,
  end_date = end_date,
  daily = list(daily)  # Store daily parameters as a list-column
)


# API request 

api_responses <- params_tibble %>%
  rowwise() %>%
  mutate(
    response = list(
      GET(
        url,
        query = list(
          latitude = latitude,
          longitude = longitude,
          start_date = start_date,
          end_date = end_date,
          daily = paste(daily, collapse = ",")
        )
      )
    )
  ) %>%
  ungroup()

# With this code I could check the first values of the API response
# it seemed that is worked
content(api_responses$response[[1]])


# Here I got response and daily_data in my params_tibble
# daily_data represents every date from January 2023 until December 2023 for 
# every longitude and latitude

params_tibble <- api_responses %>%
  mutate(
    daily_data = map(response, ~ {
      if (inherits(.x, "response") && status_code(.x) == 200) {
        content(.x)$daily  # Extract the "daily" section
      } else {
        NULL  # Handle failed requests
      }
    })
  )


# Mutate daily_data in to own columns: snowfall, rain and temperature and date
prepared_data <- params_tibble %>%
  rowwise() %>%  # Handle row-wise list processing
  mutate(
    flat_data = list(
      tibble(
        dates = unlist(daily_data$time),
        snowfall_sum_flat = unlist(daily_data$snowfall_sum),
        rain_sum_flat = unlist(daily_data$rain_sum),
        temperature_2m_max_flat = unlist(daily_data$temperature_2m_max)
      )
    )
  ) %>%
  unnest(flat_data) %>%
  ungroup()

# Mutate longitude and latitude into one column and naming the value "Prishtina"


prepared_data <- prepared_data %>%
  mutate(
    city = case_when(
      latitude == 42.6530 & longitude == 20.2883 ~ "Istog",
      latitude == 42.6629 & longitude == 21.1655 ~ "Prishtina",
      latitude == 42.4632 & longitude == 21.4699 ~ "Gjilan",
      latitude == 42.8833 & longitude == 20.8681 ~ "Mitrovica",
      latitude == 42.6744 & longitude == 20.2900 ~ "Peja",
      latitude == 42.3800 & longitude == 20.4294 ~ "Gjakove",
      latitude == 42.2111 & longitude == 20.7394 ~ "Prizren",
      latitude == 42.2141 & longitude == 20.4943 ~ "Suhareka",
      TRUE ~ "Unknown" # Optional: Handle cases where coordinates don't match
    )
  ) %>%
  select(-latitude, -longitude)

# As these upper values are in own columns unnecessary columns can be removed:

prepared_data <- prepared_data %>% 
  select(-start_date, 
         -end_date, 
         -response,
         -daily,
         -daily_data)
# this was successful

# When checking the structure of my data frame I saw that dates is still character
str(prepared_data)

# So I mutate it to date

prepared_data <- prepared_data %>%
  mutate(dates = as.Date(dates, format = "%Y-%m-%d"))


# For having a better overview on my data frame I'll add °N, °E, °C, mm and cm
coordinates <- paste0(prepared_data$latitude, "°N ", prepared_data$longitude, "°E")

grad_celsius <- paste0(prepared_data$temperature_2m_max_flat, "°C")

mm <- paste0(prepared_data$rain_sum_flat, "mm")

cm <- paste0(prepared_data$snowfall_sum_flat, "cm")



# Install Packages -------------------------------------------------------------

library(shiny)
library(shinyjs)
library(plotly)


# Create the app ---------------------------------------------------------------

# UI ---------------------------------------------------------------------------

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
      
      # Dropdown to select the city
      selectInput("city", 
                  "Select City:", 
                  choices = unique(prepared_data$city)),
      
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


# Server -----------------------------------------------------------------------
# Define server logic
server <- function(input, output) {
 
  # Reactive subset of data based on input
  filtered_data <- reactive({
    prepared_data %>%
      filter(dates >= input$date_range[1], 
             dates <= input$date_range[2],
             city == input$city)
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

