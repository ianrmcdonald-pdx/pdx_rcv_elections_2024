# random forest 

# load libraries 
library(tidyverse)
library(randomForest)
library(vip)


# df_final.csv is on this GitHub
rf_data <- df_final |>
  select(
    candidate_name,
    elected,
    precinct, 
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
    incum_or_former,
    homeowner_pct, renter_pct, percentage_turnout,
    caucasian_white_percent, edu_bach_total, edu_grad_total,
    income_75_149k, income_150k_plus, pop_in_povery_18_plus,
    people_with_disability, avg_household_size) |>
  mutate(
    elected = as.factor(elected),
    across(where(is.character), as.factor)) |>
  drop_na()


rf_data2 <- df_final |>
  filter(district == c(3, 4)) |>
  select(
    candidate_name,
    elected,
    precinct, 
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
    incum_or_former,
    homeowner_pct, renter_pct, percentage_turnout,
    caucasian_white_percent, edu_bach_total, edu_grad_total,
    income_75_149k, income_150k_plus, pop_in_povery_18_plus,
    people_with_disability, avg_household_size) |>
  mutate(
    elected = as.factor(elected),
    across(where(is.character), as.factor)) |>
  drop_na()


# set seed for reproducibility 
set.seed(24)

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


# rf d3 and d4
train_candidates2 <- df_final |> 
  filter(district == c(3, 4)) |>
  distinct(candidate_name) |>
  sample_frac(0.8) |>
  pull(candidate_name)

# split testing and training data 
train2 <- rf_data2 %>% filter(candidate_name %in% train_candidates2) %>% select(-candidate_name)
test2  <- rf_data2 %>% filter(!candidate_name %in% train_candidates2) %>% select(-candidate_name)


# create random forest model 
rf_model2 <- randomForest(
  elected ~ .,
  data      = train2,
  ntree     = 500,
  importance = TRUE)


# mean decrease in accuracy plot for all districts 
vip(rf_model, num_features = 40, geom = "point", aesthetics = list(color = "darkgreen", size = 3)) +
  theme_bw() +
  labs(title = "Mean Decrease in Accuracy (All Districts)", 
       subtitle = "average decrease in the model ability to predict election outcome 
if a given feature was removed",
       y = "Mean Decrease in Accuracy", 
       x = "Feature")


# mean decrease in accuracy plot for model with only districts 3 and 4 
vip(rf_model2, num_features = 40, geom = "point", aesthetics = list(color = "darkgreen", size = 3)) +
  theme_bw() + 
  labs(title = "Mean Decrease in Accuracy (Districts 3 and 4)", 
       subtitle = "average decrease in the model ability to predict election outcome 
if a given feature was removed",
       y = "Mean Decrease in Accuracy", 
       x = "Feature")
