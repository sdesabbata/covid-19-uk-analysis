##########
# Covid19 map animated
# Author: Stefano De Sabbata
# Date: 30 April 2020
# Licensed under the GNU General Public License v3.0 https://www.gnu.org/licenses/gpl-3.0.html
##########


rm(list = ls())


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(rgdal)
library(tmap)
library(sf)


# Load data ---------------------------------------------------------------

# Load borders 
lower_tier_la <- readOGR("Data/Shapefiles/Local_Authority_Districts__December_2019__Boundaries_UK_BGC-shp/Local_Authority_Districts__December_2019__Boundaries_UK_BGC.shp")


data_dates <- c(
  "20200301",
  "20200401",
  "20200501",
  "20200601",
  "20200701",
  "20200801",
  "20200901",
  "20201001"
)

for (data_date in data_dates){
  
  # Load Covid data
  coronavirus_cases <- 
    read_csv(
      paste0("Data/Gov/coronavirus-cases_latest_", data_date, ".csv"),
      col_types = "cccd"
    ) %>%
    rename(
      covid19_date = date,
      covid19_area_name = name,
      covid19_area_code = code
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
    select(lad19cd, covid19_date, cumCasesBySpecimenDateRate) %>%
    # Set missing values to zero
    complete(lad19cd, covid19_date, fill = list(cumCasesBySpecimenDateRate = 0)) %>%
    # Drop rows created with NA date
    filter(!is.na(covid19_date))
  
  # Re-join with geometries
  uk_covid19_shp <- st_as_sf(lower_tier_la) %>%
    right_join(
      uk_covid19,
      by = c("lad19cd" = "lad19cd")
    ) %>%
    select(lad19cd, covid19_date, cumCasesBySpecimenDateRate)
  
  
  # Mapping -----------------------------------------------------------------
  
  # Maps
  covid19_cases_facets <- tm_layout(
      frame = FALSE,
      legend.position = c("left","top"),
      legend.title.size = 0.8
    ) + 
    tm_shape(uk_covid19_shp) +
    tm_polygons(
      "cumCasesBySpecimenDateRate",
      title = "Total covid-19 cases\nper 100,000 inhabitants",
      n = 8,
      style = "fixed",
      breaks = c(0, 1, 5, 10, 50, 100, 500, 1000, 5000),
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
    tm_facets(along = "covid19_date", free.coords = FALSE, drop.NA.facets = TRUE)
  
  # Generate gif
  tmap_animation(
    covid19_cases_facets, 
    filename = paste0("Maps/covid19_cases_uk_fixed_anim_", data_date, ".gif"),
    width = 1600, 
    delay = 20,
    restart.delay = 100
  )

}