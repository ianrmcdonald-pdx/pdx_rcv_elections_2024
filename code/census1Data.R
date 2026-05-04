

# code to make census dataset matching precincts and distrcits, checking how well it did.


library(sf)
library(dplyr)
library(ggplot2)

# raw
census_raw <- st_read("data/Vulnerability.geojson")

# reproject census to match precinct CRS
census_raw <- census_raw |> 
  st_transform(st_crs(cleanMap))

# ensure valid geoms for both layers
census_raw <- census_raw |> st_make_valid()
cleanMap <- cleanMap |> st_make_valid()


# intersect census tracts with precincts to get overlap areas
precinct_census_all <- st_intersection(census_raw, cleanMap) |>
  mutate(area = st_area(geometry))


# pick the precinct with the largest overlap per tract
mainAssign <- precinct_census_all |>
  st_drop_geometry() |>     # remove geometry before summarizing
  group_by(GEOID) |>
  slice_max(area, n = 1) |>  # keep precinct with largest overlap
  ungroup() |>
  select(GEOID, PRECINCT, PRECINCTID, DISTRICT)   # only these cols adding


# make dataset
census_info <- census_raw |>
  left_join(mainAssign, by = "GEOID")



##### check how well it worked

# count how many census tracts assigned to each district
table(census_info$DISTRICT)

# 1  2  3  4 
# 36 41 37 51 


# % of tract area represented by chosen precinct
check_overlap <- precinct_census_all |>
  group_by(GEOID) |>
  mutate(total_area = sum(area)) |>  # total area of all overlaps for tract
  ungroup() |>
  group_by(GEOID) |>
  slice_max(area, n = 1) |>
  mutate(pct_used = as.numeric(area / total_area)) |>   # percent of tract used
  ungroup()

summary(check_overlap$pct_used)
# 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.3484  0.6098  0.8288  0.7987  0.9951  1.0000 


length(unique(census_info$PRECINCTID)) #67


# which precincts were not assigned to any census tract
setdiff(
  unique(cleanMap$PRECINCTID),
  unique(census_info$PRECINCTID)
)
# 
# [1] "C113"  "C114"  "C115"  "C116"  "M3305" "M3401" "M3806" "M4507" "M4509" "M4710" "M4801"
# [12] "M4805" "M4809" "M4910" "M5001" "M5003" "W366"  "W375"  "W391"  "W392"  "W441" 
# 
# or
# [1] "88 " "89 " "85 " "86 " "14 " "64 " "75 " "19 " "41 " "16 " "60 " "27 " "62 " "36 " "52 "
# [16] "5 "  "80 " "81 " "82 " "83 " "84 "



# see precinct assignment across census tracts
ggplot() +
  geom_sf(data = cleanMap, fill = NA, color = "black") +
  geom_sf(data = census_info, aes(fill = as.factor(PRECINCTID)),
          alpha = 0.6) +
  coord_sf(expand = FALSE) +
  theme_minimal()

# see vulnerability score across census tracts
ggplot(census_info) +
  geom_sf(aes(fill = Vulnerability_Score),
          color = "black",
          linewidth = 0.1) +
  coord_sf(expand = FALSE) +
  theme_minimal()
