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

# Load population data
ukmidyearestimates20182019ladcodes <- read_csv("Data/Population/ukmidyearestimates20182019ladcodes.csv") %>%
  rename(
    population = AllAges
  )


# Load borders 
upper_tier_la <- readOGR("Data/Shapefiles/Counties_and_Unitary_Authorities_(December_2017)_Boundaries_UK-shp/c12469f3-669b-42a3-b7e0-3eb2c7662239202044-1-vfyrcg.6vemg.shp")


# Load Covid data
coronavirus_cases <- read_csv("Data/Gov/coronavirus-cases_latest.csv") %>%
  select(`Area code`, `Specimen date`, `Cumulative lab-confirmed cases`) %>%
  rename(
    code = `Area code`,
    covid19_date = `Specimen date`,
    covid19_cases = `Cumulative lab-confirmed cases`
  ) %>%
  pivot_wider(
    names_from = covid19_date,
    values_from = covid19_cases
  ) %>%
  pivot_longer(
    -code,
    names_to = "covid19_date",
    values_to = "covid19_cases",
  ) %>%
  arrange(
    code, covid19_date
  ) %>%
  group_by(
    code
  ) %>%
  fill(
    covid19_cases
  ) %>%
  mutate(
    covid19_cases = ifelse(is.na(covid19_cases), 0, covid19_cases)
  )



# Merge data --------------------------------------------------------------

# Merge geometries to ...
eng_covid19_shp <- st_as_sf(upper_tier_la) %>%
  # Covid19 cases
  right_join(
    coronavirus_cases,
    by = c("ctyua17cd" = "code")
  ) %>%
  # Population
  left_join(
    ukmidyearestimates20182019ladcodes,
    by = c("ctyua17cd" = "Code")
  ) %>%
  select(
    ctyua17nm, ctyua17cd, covid19_date, covid19_cases, population
  ) %>%
  mutate(
    covid19_cases_incidence100k = covid19_cases / (population / 100000)
  )

# Filter out pre Feb 24 for comparison with Italy data
eng_covid19_shp <- eng_covid19_shp %>%
  filter(as.Date(covid19_date) >= as.Date("2020-02-24"))


# Mapping -----------------------------------------------------------------

# Maps
covid19_cases_facets <- tm_layout(
    frame = FALSE,
  ) + 
  tm_shape(eng_covid19_shp) +
  tm_polygons(
    "covid19_cases_incidence100k",
    title = "Total covid-19 cases\nper 100,000 inhabitants",
    n = 9,
    style = "fixed",
    breaks = c(0, 1, 10, 25, 50, 100, 250, 500, 1000, 2500),
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
for National Statistics.
Note: Population estimates mid-year 2018. The 
categories in the legend include the minimum value
and exclude the maximum value of the bracket.",
    position = c ("left", "center"),
    size = 0.4
  ) +
  tm_facets(along = "covid19_date", free.coords = FALSE)

# Generate gif
tmap_animation(
  covid19_cases_facets, 
  filename = "Maps/covid19_cases_eng_fixed_anim.gif",
  width = 1600, 
  delay = 20,
  restart.delay = 100
)
