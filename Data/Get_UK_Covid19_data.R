##########
# Get data
# Author: Stefano De Sabbata
# Date: 19 October 2020
# Licensed under the GNU General Public License v3.0 https://www.gnu.org/licenses/gpl-3.0.html
# See also:
#   Coronavirus (COVID-19) in the UK
#   Developers guide
#   Documentations for the API — v.1
#   https://coronavirus.data.gov.uk/developers-guide
##########

library(tidyverse)
library(magrittr)
library(httr)
library(jsonlite)

# Coronavirus (COVID-19) in the UK
# API endpoint
endpoint <- "https://api.coronavirus.data.gov.uk/v1/data"

# Data structure to request
structure <- list(
  date       = "date", 
  name       = "areaName", 
  code       = "areaCode", 
  cumCasesBySpecimenDateRate = "cumCasesBySpecimenDateRate"
)

# First date to request
date_to_request <- lubridate::ymd(20201001)
is_this_first_date <- TRUE

# Request all dates before today
while(date_to_request < lubridate::today()){
  
  # date to string format
  date_to_request_str <- date_to_request %>% 
    format('%Y-%m-%d')
  cat(date_to_request_str, "\n")
  
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
  response %>%
    httr::content("text", encoding="UTF-8") %>%
    jsonlite::fromJSON() %$%
    data %>%
    write_csv(
      "Data/Gov/coronavirus-cases_latest_20201001.csv",
      append = !is_this_first_date
    )
  
  date_to_request <- date_to_request + lubridate::days(1)
  is_this_first_date <- FALSE

}