

# For this R-Project I am building an interactive map with the shiny package.
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


# leaflet für interaktive maps
# ggmap

# Prepare the data ------------------------------------------------------------


url <- "https://archive-api.open-meteo.com/v1/archive"

latitude <- c(42.6629,
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
              42.5958)
  
longitude <- c(21.1655,
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
              21.4011)

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


# As these upper values are in own columns unnecessary columns can be removed:

prepared_data <- prepared_data %>% 
  select(#-start_date, 
         #-end_date, 
         #-response,
         -daily)
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












