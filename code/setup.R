library(sf)
library(tidyverse)
library(vote)


#maps graphics
pdx_gen <- st_read(
  "data/shapefiles/Multnomah_Elections_Precinct_Split_2024/Multnomah_Elections_Precinct_Split_2024.shp")

pdx_gen <- st_transform(pdx_gen, crs=4269)

pcc_dist <- st_read("data/shapefiles/Portland_City_Council_Districts/Portland_City_Council_Districts.shp")
pcc_dist <- st_transform(pcc_dist, crs=4269)

pcc_dist_4 <- pcc_dist |> filter(DISTRICT == 4)
pcc_dist_4 <- st_transform(pcc_dist_4, crs=4269)


pcc_dist_4 %>% 
  ggplot() + geom_sf()

voter_precincts <- st_read("data/shapefiles/Voter_Precincts/Voter_Precincts.shp")
voter_precincts <- st_transform(voter_precincts, crs=4269)

voter_precincts |> ggplot() + geom_sf()



pdx_gen_d4 <- pdx_gen %>% 
  filter(CoP_Dist == 4) 


sf_use_s2(FALSE)

exp_1 <- st_join(pcc_dist_4, voter_precincts)
exp_1 %>% 
  ggplot() + geom_sf(fill = NA)

plot(voter_precincts["COUNTY"])

exp_1|>  
  ggplot() + geom_sf()




