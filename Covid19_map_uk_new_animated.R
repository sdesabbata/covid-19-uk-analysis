##########
# Covid19 map animated
# Author: Stefano De Sabbata
# Date: 30 April 2020
# Licensed under the GNU General Public License v3.0 https://www.gnu.org/licenses/gpl-3.0.html
# See also:
#   Coronavirus (COVID-19) in the UK
#   Developers guide
#   Documentations for the API — v.1
#   https://coronavirus.data.gov.uk/developers-guide
##########


rm(list = ls())


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(magrittr)

library(httr)
library(jsonlite)

library(rgdal)
library(tmap)
library(sf)


# Government API ----------------------------------------------------------

# Coronavirus (COVID-19) in the UK
# API endpoint
endpoint <- "https://api.coronavirus.data.gov.uk/v1/data"

# Data structure to request
structure <- list(
  date       = "date", 
  name       = "areaName", 
  code       = "areaCode", 
  cumCasesBySpecimenDate = "cumCasesBySpecimenDate",
  cumCasesBySpecimenDateRate = "cumCasesBySpecimenDateRate",
  newCasesBySpecimenDate = "newCasesBySpecimenDate"
)



# Load spatial data -------------------------------------------------------

# Load borders 
lower_tier_la <- readOGR("Data/Shapefiles/Local_Authority_Districts__December_2019__Boundaries_UK_BGC-shp/Local_Authority_Districts__December_2019__Boundaries_UK_BGC.shp")



# Main --------------------------------------------------------------------

start_date <- "2020-03-01"
end_date <- "2020-10-18"

start_date <- start_date %>% lubridate::ymd() %>% lubridate::floor_date(unit = "week")
end_date <- end_date %>% lubridate::ymd() %>% lubridate::floor_date(unit = "week") - lubridate::days(1)
this_date <- start_date

this_interval_data <- NA
this_interval_srt <- 
  paste(
    start_date %>% format('%Y%m%d'),
    end_date %>% format('%Y%m%d'),
    sep = "_"
  )

# Get covid data ----------------------------------------------------------

while (this_date <= end_date) {
  
  # date to string format
  date_to_request_str <- this_date %>% 
    format('%Y-%m-%d')
  cat("Requesting:", date_to_request_str, "\n")
  
  # Set filters
  #   - type of area
  #   - date
  filters <- c(
    sprintf("areaType=%s", "ltla"),
    sprintf("date=%s", date_to_request_str)
  )
  
  # Request
  httr::GET(
    url   = endpoint,
    query = list(
      filters   = paste(filters, collapse = ";"),
      structure = jsonlite::toJSON(structure, auto_unbox = TRUE),
      latestBy  = "newCasesByPublishDate",
      format    = "json"
    ),
    timeout(10)
  ) -> response
  
  # Check response status
  if (response$status_code >= 400) {
    err_msg = httr::http_status(response)
    stop(err_msg)
  }
  
  # Extract response from json
  # and append to csv
  this_date_data <-
    response %>%
    httr::content("text", encoding="UTF-8") %>%
    jsonlite::fromJSON() %$%
    data
  
  # Instantiate the dataset if it doesn't exist
  # or append the data
  if (is.na(this_interval_data)) {
    this_interval_data <- this_date_data
  } else {
    this_interval_data <- 
      rbind(
        this_interval_data,
        this_date_data
      )
  }
  
  # Next date
  this_date <- this_date + lubridate::days(1)

}



# Seven–day rolling rate of new cases by specimen -------------------------

coronavirus_cases <- 
  this_interval_data  %>%
  rename(
    covid19_date = date,
    covid19_area_name = name,
    covid19_area_code = code
  ) %>%
  mutate(
    covid19_week_ending = covid19_date %>% lubridate::ymd() %>% lubridate::ceiling_date(unit = "week") %>% format('%Y-%m-%d')
  ) %>%
  group_by(covid19_area_name, covid19_area_code, covid19_week_ending) %>%
  summarise(
    max_cumCasesBySpecimenDate = max(cumCasesBySpecimenDate),
    max_cumCasesBySpecimenDateRate = max(cumCasesBySpecimenDateRate),
    sum_newCasesBySpecimenDate = sum(newCasesBySpecimenDate)
  ) %>%
  mutate(
    area_population = (max_cumCasesBySpecimenDate / max_cumCasesBySpecimenDateRate) * 100000
  ) %>%
  mutate(
    seven_day_rate_newCasesBySpecimenDate = (sum_newCasesBySpecimenDate / area_population) * 100000
  )


# Merge data --------------------------------------------------------------

# Generate complete dataset per LAD and date
# Start from all LAD from spatial file
uk_covid19 <- st_as_sf(lower_tier_la) %>%
  # Drop geometries
  st_drop_geometry() %>%
  # Join with covid19 cases rate
  full_join(
    coronavirus_cases,
    by = c("lad19cd" = "covid19_area_code")
  ) %>%
  select(lad19cd, covid19_week_ending, seven_day_rate_newCasesBySpecimenDate) %>%
  # Set missing values to zero
  complete(lad19cd, covid19_week_ending, fill = list(seven_day_rate_newCasesBySpecimenDate = 0)) %>%
  # Drop rows created with NA date
  filter(!is.na(covid19_week_ending))

# Re-join with geometries
uk_covid19_shp <- st_as_sf(lower_tier_la) %>%
  right_join(
    uk_covid19,
    by = c("lad19cd" = "lad19cd")
  ) %>%
  select(lad19cd, covid19_week_ending, seven_day_rate_newCasesBySpecimenDate)


  
# Mapping -----------------------------------------------------------------

# Maps
covid19_cases_facets <- tm_layout(
    frame = FALSE,
    legend.position = c("left","top"),
    legend.title.size = 0.8
  ) + 
  tm_shape(uk_covid19_shp) +
  tm_polygons(
    "seven_day_rate_newCasesBySpecimenDate",
    title = "Seven–day rolling rate of\nnew cases by specimen",
    n = 7,
    style = "fixed",
    breaks = c(
      0, 11, 51, 101, 201, 401,
      uk_covid19_shp %>% 
        pull(seven_day_rate_newCasesBySpecimenDate) %>%
        max() %>%
        ceiling()
    ),
    as.count = TRUE,
    palette = "viridis",
    #border.col = "#cccccc",
    lwd = 0.1
    ) +
  #tm_shape(regioni_shp) +
  tm_borders(
    col = "#FFFFFF",
    lwd = 0.5
  ) +
  tm_credits(
    "Stefano De Sabbata - @maps4thought
https://github.com/sdesabbata/covid-19-uk-analysis
Contains data from the UK Government and the Office 
for National Statistics.",
    position = c ("left", "bottom"),
    size = 0.4
  ) +
  #tm_scale_bar(position=c("right", "bottom")) +
  tm_facets(along = "covid19_week_ending", free.coords = FALSE, drop.NA.facets = TRUE)

# Generate gif
tmap_animation(
  covid19_cases_facets, 
  filename = paste0("Maps/covid19_cases_uk_7DayRateNew_anim_", this_interval_srt, ".gif"),
  width = 1600, 
  delay = 50,
  restart.delay = 200
)