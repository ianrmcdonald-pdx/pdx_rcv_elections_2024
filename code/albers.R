library(sf)
library(tidyverse)
library(USAboundaries)

usa <- us_states() %>% 
  filter(!state_abbr %in% c("PR", "AK", "HI"))  # lower 48 only

wgs84 <- usa %>% 
  st_transform(4326) # WGS84 - good default

albers <- usa %>% 
  st_transform(5070) # Albers - a popular choice for the lower 48

ggplot() +
  geom_sf(data = wgs84, fill = NA)

ggplot() +
  geom_sf(data = albers, fill = "darkgreen", color = "white")

usa |> 
  #filter(name == "Washington") |> 
  st_transform(5070)  |> 
  ggplot() +
    geom_sf(fill = "dodgerblue")
