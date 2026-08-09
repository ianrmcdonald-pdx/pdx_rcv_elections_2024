library(sf)
library(tidyverse)
library(vote)


#map graphics:  a few examples

pdx_gen <- st_read(
  "data/shapefiles/Multnomah_Elections_Precinct_Split_2024/Multnomah_Elections_Precinct_Split_2024.shp")

pdx_gen <- st_transform(pdx_gen, crs=4269)


or_house <- st_read("data/geojson/House_Districts.geojson") |> 
  st_transform(crs=4269) 


pcc_dist <- st_read("data/shapefiles/Portland_City_Council_Districts/Portland_City_Council_Districts.shp")
pcc_dist <- st_transform(pcc_dist, crs=4269)
st_crs(pcc_dist)


#home_values <- st_transform(home_values, crs=4269)
voter_precincts <- st_read("data/shapefiles/Voter_Precincts/Voter_Precincts.shp") |> 
  st_transform(crs=4269)

#delete <- st_join(home_values, pdx_gen_d4)
#delete <- st_intersection(pdx_gen_d4, home_values)
#plot(delete["estimate"])

voter_precincts <- st_make_valid(voter_precincts)
pcc_dist <- st_make_valid(pcc_dist)
x <- st_intersection(voter_precincts, pcc_dist)   

x |> filter(PRECINCTID == "M2804") |> ggplot() + geom_sf()  +
  geom_sf_text(aes(label = PRECINCTID)) 

pdx_gen_d4 <- pdx_gen %>% 
  filter(CoP_Dist == 4) 

sf_use_s2(FALSE)

delete <- st_intersection(pdx_gen, pdx_gen_d4)
delete %>% 
  ggplot() + geom_sf()

plot(voter_precincts["COUNTY"])
plot(x["COUNTY"])


pdx_gen_d4 %>% 
  ggplot() + geom_sf()

voter_precincts %>% 
  ggplot() + geom_sf()

or_house %>% 
  ggplot() + geom_sf()

or_house <- st_make_valid(or_house)
voter_precincts <- st_make_valid(voter_precincts)
pcc_dist <- st_make_valid(pcc_dist)

delete <- st_intersection(or_house, pcc_dist)

or_house %>% 
  ggplot() + geom_sf(fill=NA, color="blue")

delete %>% 
  ggplot() + geom_sf(fill=NA, color="blue") +
  geom_sf_text(aes(label = DISTRICT)) 

delete <- st_intersection(or_house, pcc_dist)
delete_1 <- st_intersection(voter_precincts, or_house)

delete %>% 
  ggplot() + geom_sf(fill=NA, color="blue") 

ggplot(pcc_dist) + geom_sf(fill=NA, color = "red")

library(ggnewscale)

ggplot() +
  # First Layer (e.g., fill color)
  geom_sf(data = delete, aes(fill = DISTRICT)) +
  scale_fill_viridis_c() +
  
  # Reset scale so a second legend can be added
  new_scale_fill() + 
  
  # Second Layer (e.g., overlapping fill)
  geom_sf(data = delete, aes(fill = DISTRICT.1), alpha = 0.5) +
  geom_sf_text(data = delete, aes(label = DISTRICT)) 


