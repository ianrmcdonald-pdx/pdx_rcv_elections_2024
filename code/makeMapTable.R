
# code to make cleanMap data
# using data files from Ian and shapefiles of zipcodes and neighborhood bounds.

# load library and data
library(sf)        # spatial vector data handling
library(dplyr)  
library(ggplot2)
library(plotly)


# raw
voter_precincts_raw <- st_read("data/shapefiles/Voter_Precincts/Voter_Precincts.shp")       
pcc_dist_raw        <- st_read("data/shapefiles/Portland_City_Council_Districts/Portland_City_Council_Districts.shp")  
zip_raw             <- st_read("data/zcta/tl_2022_us_zcta520.shp")  
neighborhoods_raw   <- st_read("data/portlandBounds.geojson")


# clean geom and transform to match America instead of ft
voter_precincts <- voter_precincts_raw |> st_make_valid() |> st_transform(26910)  
pcc_dist        <- pcc_dist_raw        |> st_make_valid() |> st_transform(26910)
zip             <- zip_raw             |> st_make_valid() |> st_transform(26910) 
neighborhoods   <- neighborhoods_raw   |> st_make_valid() |> st_transform(26910)

voter_precincts <- voter_precincts |> st_make_valid()  # making sure geoms are valid 


# remove any holes
voter_precincts <- voter_precincts |> st_cast("MULTIPOLYGON", warn = FALSE)  # cast precinct geom, dropping interior holes hopefully



# only keep portland zips and neighborhoods that intersect voter precincts
zip_portland          <- zip |> st_filter(voter_precincts)   
neighborhoods_portland <- neighborhoods |> st_filter(voter_precincts) 


# join and assign a district to each precinct
precinct_base <- voter_precincts |> st_join(pcc_dist)



# intersection over precincts and zips
precinct_zip_all <- st_intersection(voter_precincts, zip_portland) 

# add area for selecting main ZIP
precinct_zip_all <- precinct_zip_all |>
  mutate(area_overlap = st_area(geometry))  # measure overlap size


# get the main zip per precinct
main_zip <- precinct_zip_all |>
  st_drop_geometry() |>      # work with attribute table only
  group_by(PRECINCTID) |> 
  slice_max(area_overlap, n = 1) |>  # keep zip with largest overlap area per precinct
  ungroup() |>   
  select(PRECINCTID, main_zip = ZCTA5CE20)      # keep precinct ID and rename col to main_zip



## zipcode overlap flags
zip_flags <- precinct_zip_all |>
  st_drop_geometry() |>   #  work with attributes only
  group_by(PRECINCTID) |>
  summarise(                         # zip overlap info per precinct
    n_zip = n_distinct(ZCTA5CE20),    # count each zipcodes intersecting each precinct
    overlap_zip = ifelse(n_zip > 1, 1, 0)      # flag precincts with more than one as overlapping
  )


# store all zipcodes per precinct
alt_zip <- precinct_zip_all |>
  st_drop_geometry() |>     # attributes only
  group_by(PRECINCTID) |>
  summarise(               # zips per precinct
    alt_zip = paste(unique(ZCTA5CE20), collapse = ", ")  # add unique zipcodes into string
  )



#### make usable data table :  precinct, district,zip
master <- precinct_base |>
  left_join(main_zip,   by = "PRECINCTID") |>
  left_join(zip_flags,  by = "PRECINCTID") |>
  left_join(alt_zip,    by = "PRECINCTID")  



# clean na vals
master <- master |>
  mutate( 
    overlap_zip = ifelse(is.na(overlap_zip), 0, overlap_zip),  # set overlap flag to 0
    n_zip       = ifelse(is.na(n_zip), 1, n_zip),              # set count to 1 
    main_zip    = ifelse(is.na(main_zip), "UNKNOWN", main_zip) # set to "UNKNOWN"
  )

master <- master |>
  group_by(PRECINCTID) |> 
  slice(1) |>       # only the first row per precinct
  ungroup() 

master <- master |> 
  filter(!is.na(DISTRICT))   # drop missing districts



# intersect precincts with neighborhoods to get overlap 
precinct_nbhd_all <- st_intersection(voter_precincts, neighborhoods_portland) |> 
  mutate(area = st_area(geometry))  

main_nbhd <- precinct_nbhd_all |>
  st_drop_geometry() |>            # attributes only
  group_by(PRECINCTID) |> 
  slice_max(area, n = 1) |>   # keep the neighborhood with the largest overlap area per precinct
  ungroup() |>  
  select(PRECINCTID, neighborhood = NAME)       # keep precinct ID and rename col to neighborhood

master2 <- master |>
  left_join(main_nbhd, by = "PRECINCTID")       # attach to the table



# make it match other data tables, from Precinct_ID_Reference, correct corresponding precinct

# get rid of chars and convert to numeric
Precinct_ID_Reference <- Precinct_ID_Reference |>
  mutate( 
    precinct_num = as.numeric(gsub("[^0-9]", "", precinct))
  )

# standardize precinct with the id_number
master3 <- master2 |>
  left_join(Precinct_ID_Reference, by = c("PRECINCT" = "precinct_num")) |>  
  mutate(PRECINCT = id_number) |>  
  select(-id_number)  # drop



# drop where they did not match
master3 <- master3 |>
  filter(!is.na(PRECINCT)) 




# manually fixing district issues
cleanMap <- master3 |>
  mutate(
    DISTRICT = case_when(
      PRECINCTID == "M4406" ~ "2",
      PRECINCTID == "M4205" ~ "3",
      PRECINCTID == "M4502" ~ "2",
      PRECINCTID == "M4503" ~ "3",
      PRECINCTID == "M4603" ~ "3",
      PRECINCTID == "M3304" ~ "4",
      PRECINCTID == "C115" ~ "4",
      PRECINCTID == "M4103"~ "4",
      PRECINCTID == "M4101"~ "4",
      PRECINCTID == "M4105" ~ "4",
      PRECINCTID == "M3306" ~ "4",
      PRECINCTID == "M2801" ~ "4",
      PRECINCTID == "M2803" ~ "4",
      # PRECINCTID == "W366"  ~ NA_character_,
      # PRECINCTID == "M3301" ~ NA_character_,
      # PRECINCTID == "W375"  ~ NA_character_,
      # PRECINCTID == "M3401"  ~ NA_character_,
      TRUE ~ DISTRICT
    )
  ) |>
  filter(!is.na(DISTRICT))


##### plot maps

# ggplot(cleanMap) +
#   geom_sf(aes(fill = as.factor(DISTRICT)),  #color by district
#           color = "black",linewidth = 0.2) +     # black borders for precinct outlines
#   scale_fill_brewer(palette = "Set1") +  
#   coord_sf(expand = FALSE) + 
#   # theme_minimal()


# precincts to exclude from visual maps because their geometries overlap
# but belong to dist 4
precincts_exclude_visual <- c("W366", "M3301", "W375", "M3401")  # vector of precinct IDs to hide in some visualizations


ggplot(cleanMap |> filter(!PRECINCTID %in% precincts_exclude_visual)) +
  geom_sf(aes(fill = as.factor(DISTRICT)),
          color = "black",  linewidth = 0.2) +
  scale_fill_brewer(palette = "Set1") +  
  coord_sf(expand = FALSE)   
  # +theme_minimal()   


ggplot() +
  geom_sf(data = cleanMap,aes(fill = as.factor(DISTRICT)),  
          color = "black", linewidth = 0.2,alpha = 0.6) +     #  kinda transparent fill for base
  geom_sf(data = cleanMap,
          fill = NA,    
          color = "white",  
          linewidth = 0.1) +    # thin padding
  coord_sf(expand = FALSE) +  
  theme_minimal()   




# did these when I was manually fixing the districts

ggplot(cleanMap |> filter(DISTRICT == "1")) +  # only district 1
  geom_sf(fill = "lightblue", color = "black", linewidth = 0.2) +
  geom_sf_text(aes(label = PRECINCTID), size = 3) +   # label each with its ID
  coord_sf(expand = FALSE) +
  theme_minimal()

ggplot(cleanMap |> filter(DISTRICT == "2")) +
  geom_sf(fill = "lightblue", color = "black", linewidth = 0.2) +
  geom_sf_text(aes(label = PRECINCTID), size = 3) +
  coord_sf(expand = FALSE) +
  theme_minimal()

ggplot(cleanMap |> filter(DISTRICT == "3")) +
  geom_sf(fill = "lightblue", color = "black", linewidth = 0.2) +
  geom_sf_text(aes(label = PRECINCTID), size = 3) +
  coord_sf(expand = FALSE) +
  theme_minimal()


ggplot(cleanMap |> filter(DISTRICT == "4")) +
  geom_sf(fill = "lightblue", color = "black", linewidth = 0.2) +
  geom_sf_text(aes(label = PRECINCTID), size = 3) +
  coord_sf(expand = FALSE) +
  theme_minimal()





# ggplotly()

p <- ggplot(cleanMap) +    
  geom_sf(aes(fill = as.factor(DISTRICT), text = PRECINCTID),      # colored by district and store the ID to hover view
          color = "black",  
          linewidth = 0.2) +
  scale_fill_brewer(palette = "Set1") + 
  coord_sf(expand = FALSE) +
  theme_minimal()

ggplotly(p, tooltip = "text")   # make interactive
