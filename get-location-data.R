library(tidyverse)
library(tidygeocoder)
library(readr)
library(stringr)
library(lubridate)
library(Imap)

# test for mapbox api. 
#address_single <- tibble(singlelineaddress = c(
#  "11 Wall St, NY, NY",
#  "600 Peachtree Street NE, Atlanta, Georgia"
#))
#
#sample_lat_long <- address_single %>% geocode(address = singlelineaddress, method = "mapbox", limit = 1, lat = latitude, long = longitude)


setwd("/Users/evanlsolomon/environment/RProjects/biostatistics/final_project/")
old_runner_data <- read_csv("./20211129_163050_clean-anonymized-runner-data.csv", col_types = cols(
  ID = col_integer(),
  race_year = col_integer(),
  race_type = col_factor(),
  sex = col_factor(),
  age = col_integer(),
  Time = col_character(),
  imputed_time = col_time(format = ""),
  Pace = col_time(format = ""),
  `PiS/TiS` = col_character(),
  Division = col_character(),
  `PiD/TiD` = col_character(),
  Hometown = col_character(),
  city = col_character(),
  state = col_character(),
  country_1 = col_character(),
  country_2 = col_character(),
  races_run = col_integer()
))


# get lat long for locations where you have (the country AND the (city or state))
# 369,731 records in this subset
city_or_state_and_country <- old_runner_data %>% filter((!is.na(city) | !is.na(state)) & !is.na(country_1))

# get lat long for locations where you have (the country AND the (city or state))
# 639 records in this subset
just_country_not_US <- old_runner_data %>% filter(!is.na(country_1) & is.na(city) & is.na(state) & !(country_1 %in% "United States"))

# this group gets excluded
# 35 records in this subset
old_runner_data %>% filter(is.na(city) & is.na(state) & (country_1 %in% "United States"))

# this group gets excluded
# 42 records in this subset
old_runner_data %>% filter((!is.na(city) | !is.na(state)) & is.na(country_1))

# this group gets excluded
# 3,877 records in this subset
old_runner_data %>% filter(is.na(city) & is.na(state) & is.na(country_1))

# combine them
records_for_which_to_get_locations <- rbind(city_or_state_and_country, just_country_not_US)

records_without_locations <- setdiff(old_runner_data, records_for_which_to_get_locations)

# create the addr column to send to the query
records_for_which_to_get_locations <- records_for_which_to_get_locations %>% 
  unite(addr, c(city,state,country_1), sep= ", ", remove = FALSE, na.rm = TRUE)

records_for_which_to_get_locations

lat_longs<- records_for_which_to_get_locations %>% 
  geocode(addr, method = 'mapbox',limit = 1, lat = latitude , long = longitude)

lat_longs$miles_from_dc <- mapply(FUN = gdist, lon.1 = lat_longs$longitude, lat.1 =  lat_longs$latitude, lon.2 = -77.0366, lat.2  = 38.895)
lat_longs 


records_without_locations <- records_without_locations %>% mutate(addr = NA, latitude = NA, longitude = NA, miles_from_dc = NA)

lat_longs <- rbind(lat_longs, records_without_locations)

#save mostly cleaned dataset
fname <- paste0(format(lubridate::now(), "%Y%m%d_%H%M%S_"), "runner-data-mapbox-lat-longs.csv")
write_csv(lat_longs, fname)
