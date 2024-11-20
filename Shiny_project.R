#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
# For this R-Project I aim building an interactive map with the shiny package.



# Examples for shiny ------------------------------------------------------



library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)




# Load Data ---------------------------------------------------------------

library(httr)
library(jsonlite)
library(lubridate)
library(dplyr)
library(purrr)

# Setup Open-Meteo API request details
url <- "https://archive-api.open-meteo.com/v1/archive"
params <- list(
  latitude =  42.6629,
              42.2159,
              42.6530,
              42.8853,
              42.4632,
              42.3703,
              42.3828,
              42.6667,
              42.5607,
              42.3800,
              42.5861,
              42.4461,
              42.4328,
              42.5631,
              42.6244,
              42.7736,
              42.5958,
  
  longitude = 21.1655,
              20.7420,
              20.2883,
              20.8675,
              21.4699,
              21.1553,
              20.4283,
              20.5000,
              20.4377,
              20.6167,
              20.6261,
              20.2989,
              21.1653,
              21.1408,
              21.4672,
              20.7561,
              21.4011, 

  start_date = "2023-01-01",  # I actually wanted the data from 2000 until 2023.
  end_date = "2023-12-31",    # I exceeded the API calls that's why it did not work
                              # I have shortened the timeframe
  daily = "snowfall_sum",
          "rain_sum",
          "temperature_2m_max"
)




# Function to make the API request with retries
make_request <- function(url, params, retries = 5, backoff_factor = 0.2) {
  attempt <- 1
  repeat {
    response <- tryCatch({
      httr::GET(url, query = params)
    }, error = function(e) {
      NULL
    })
    
    if (!is.null(response) && status_code(response) == 200) {
      return(content(response, as = "parsed", simplifyDataFrame = TRUE))
    }
    
    if (attempt >= retries) {
      stop("API request failed after ", retries, " attempts.")
    }
    
    Sys.sleep(backoff_factor * attempt)
    attempt <- attempt + 1
  }
}

# Fetch weather data
response <- make_request(url, params)


# Extract relevant metadata
coordinates <- paste0(response$latitude, "°N ", response$longitude, "°E")
elevation <- response$elevation
timezone <- response$timezone
timezone_abbreviation <- response$timezone_abbreviation
utc_offset_seconds <- response$utc_offset_seconds

cat("Coordinates:", coordinates, "\n")
cat("Elevation:", elevation, "m asl\n")
cat("Timezone:", timezone, timezone_abbreviation, "\n")
cat("Timezone difference to GMT+0:", utc_offset_seconds, "s\n")

# Process hourly data
hourly <- response$hourly
time <- as.POSIXct(hourly$time, origin = "1970-01-01", tz = "UTC")
temperature_2m <- hourly$temperature_2m

# Create hourly data frame
hourly_dataframe <- data.frame(
  date = seq(
    from = min(time),
    to = max(time) - lubridate::seconds(diff(time)[1]),
    by = paste0(diff(time)[1], " secs")
  ),
  temperature_2m = temperature_2m
)

# Print the resulting data frame
print(hourly_dataframe)








