library(tidycensus)
library(tidyverse)

# Set your API key

# Fetch block group median home value for a specific county
home_values <- get_acs(
  geography = "block group",
  variables = "B25077_001",
  state = "OR",
  county = "Multnomah",
  year = 2024,
  geometry = TRUE
)
