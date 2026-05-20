# RANDOM FOREST MODEL 

# filter relevant data 
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


# use testing data for prediction
test_preds <- predict(rf_model, newdata = test)

# confusion matrix to check model accuracy 
confusionMatrix(test_preds, test$elected, positive = "yes")

# variable importance plot (accuracy and gini)
varImpPlot(rf_model, sort = TRUE)

# alternate vip plot (accuracy only). includes longer variable list and is a bit more aesthetically pleasing
ggvip(rf_model, sqrt = FALSE, type = 1) 


# another option for vip plot, also more aesthetically pleasing than default from randomForest package
vip(rf_model, num_features = 20, geom = "point", aesthetics = list(color = "darkgreen", size = 3)) +
  theme_bw()


# CREATING ENDORSEMENT PLOTS 

# filter data, only keeping first instance 
df_clean_2 <- df_clean |>
  distinct(candidate_name, .keep_all = TRUE) |>
  select(dsa_endorsed : teamsters_joint_council_37, elected)

# summary of endorsement info
endorsement_summary <- df_clean_2 |>
  pivot_longer(
    cols = dsa_endorsed:teamsters_joint_council_37,  
    names_to = "org",
    values_to = "endorsed") |>
  filter(endorsed == "yes") |>
  group_by(org) |>
  summarise(
    total_endorsements = n(),
    successful         = sum(elected == "yes"),
    unsuccessful       = sum(elected == "no")) |>
  arrange(desc(total_endorsements))

# pivot endorsement summary to format that works for plot 
endorsement_long <- endorsement_summary |>
  pivot_longer(
    cols = c(successful, unsuccessful),
    names_to = "outcome",
    values_to = "count") |>
  mutate(org = fct_reorder(org, -count, sum))


# create interactive endorsement plot 
endorsement_plot <- ggplot(endorsement_long, aes(x = org, y = count, fill = outcome)) +
  geom_col() +
  scale_fill_manual(values = c("successful" = "darkgreen", "unsuccessful" = "#D85A30")) +
  labs(x = "Organization", y = "Candidates endorsed", fill = "Outcome") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplotly(endorsement_plot)


