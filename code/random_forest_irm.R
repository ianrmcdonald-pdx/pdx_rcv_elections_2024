# load libraries 
library(tidyverse)
library(randomForest)
library(vip)

set.seed(100)
df_final <- read_csv("data/df_final.csv", col_types = cols())
# df_final.csv is on this GitHub


df_final <- df_final |> 
  rename(willamette_week = millamette_week, twelve_for_pdx = X12_for_pdx) |> 
  select(-precinct) |> 
  mutate(candidate_name = ifelse(candidate_name == "MIchelle DePass", "Michelle DePass", candidate_name)) |> 
  select(1:36, 63) |> 
  distinct() |> 
  drop_na()

rf_data <- df_final |>
  filter(district %in% c(1, 2, 3, 4)) |>
  select(
    candidate_name,
    elected,
    nw_oregon_labor_council,
    dsa_endorsed, dsa_greenlit, dsa_redlit,
    ibew_local_48, ironworkers_local_29, apano_action_fund,
    future_portland, next_up_action_fund, portland_firefighters_any,
    the_oregonian, united_for_portland, willamette_week,
    working_families_party, afscme_local_189, portland_mercury,
    portland_police_any, progresso_latino, sierra_club,
    twelve_for_pdx, berniepdx, home_pac, street_trust_action_fund,
    portland_for_all, friends_of_portland_street_response,
    portland_neighbors_welcome, protec17, sunrise_movement_pdx,
    portland_association_of_teachers,
    american_federation_of_teachers_oregon, multifamily_nw,
    lgbtq_victory_fund, teamsters_joint_council_37,
    incum_or_former) |>
  mutate(
    elected = as.factor(elected),
    across(where(is.character), as.factor)) |>
  drop_na()

# randomly select training candidates 
train_candidates <- df_final |> 
  distinct(candidate_name) |>
  sample_frac(0.8) |>
  pull(candidate_name)

# split testing and training data 
train <- rf_data %>% filter(candidate_name %in% train_candidates) %>% select(-candidate_name)
test  <- rf_data %>% filter(!candidate_name %in% train_candidates) %>% select(-candidate_name)


# create random forest model 
rf_model <- randomForest(
  elected ~ .,
  data      = train,
  ntree     = 500,
  importance = TRUE)


vip(rf_model, num_features = 20, geom = "point", aesthetics = list(color = "darkgreen", size = 3)) +
  theme_bw() +
  labs(title = "Mean Decrease in Accuracy: All Districts", 
       subtitle = "Training Set",
       y = "Mean Decrease in Accuracy (when feature is shuffled)", 
       x = "Feature")

print(dists)

