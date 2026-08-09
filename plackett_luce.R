library(PlackettLuce)


# R <- as.rankings(x_cvr_2, input = "orderings")
# mod <- PlackettLuce(R, npseudo = 0)
# coef(mod, log = FALSE)
# summary(mod)
# 
# 
# A <- adjacency(R)
# library(igraph)
# net <- graph_from_adjacency_matrix(A)

#plot(net, edge.arrow.size = 0.5, vertex.size = 30)

#plot(net)
mayor_tally <- compute_pdx_stv(mayor_2024, eliminated = "")
mayor_tally |> 
  count(rank_1, sort = TRUE)


xt <- c("Rene Gonzalez", "Carmen Rubio")

x_cvr <- mayor_2024 |> 
  mutate(rank_1 = ifelse(rank_1 %in% xt, rank_1, "undervote")) |> 
  mutate(rank_2 = ifelse(rank_2 %in% xt, rank_2, "undervote")) |> 
  mutate(rank_3 = ifelse(rank_3 %in% xt, rank_3, "undervote")) |> 
  mutate(rank_4 = ifelse(rank_4 %in% xt, rank_4, "undervote")) |> 
  mutate(rank_5 = ifelse(rank_5 %in% xt, rank_5, "undervote")) |> 
  mutate(rank_6 = ifelse(rank_6 %in% xt, rank_6, "undervote"))

x_cvr_1 <- compute_pdx_stv(x_cvr, eliminated = "") 


  
#x_cvr_2 <- x_cvr_1 |> select(-c("recordID", "precinct"))


  
#connect precincts to council districts

council_all_2024_list <- list(d1 = compute_pdx_stv(council_d1_2024), d2 = compute_pdx_stv(council_d2_2024), d3 = compute_pdx_stv(council_d3_2024), d4 = compute_pdx_stv(council_d4_2024))

council_all_2024 <- bind_rows(council_all_2024_list, .id = "district") 

mayor_and_dall <- x_cvr_1 |> 
  #select(recordID, precinct, rank_1) |> 
  inner_join(council_all_2024, by=(c("recordID", "precinct"))) |> 
  filter(district == "d4")

mayor_and_dall_count <- mayor_and_dall |> 
  count(rank_1.x, sort = TRUE)

vote_counts <- mayor_and_dall |> 
  count(rank_1.x, rank_1.y) |> 
  pivot_wider(id_cols=rank_1.y, names_from = rank_1.x, values_from = n) |> 
  mutate(all_votes = `Rene Gonzalez` + `Carmen Rubio`) |> 
  mutate(rene_pct = `Rene Gonzalez` / all_votes) |> 
  mutate(carmen_pct = `Carmen Rubio` / all_votes) |> 
  mutate(peacock = ifelse(carmen_pct >= .7, TRUE, FALSE)) 
  
vote_counts_all <- vote_counts |>  
  group_by(peacock) |> 
  summarize(carmen = sum(`Carmen Rubio`), rene = sum(`Rene Gonzalez`), all = sum(all_votes))
    
rene_voters_peacock_candidates <- mayor_and_dall |> 
  filter(rank_1.x == "Rene Gonzalez") |> 
  #select(-c("rank_3.x", "rank_4.x", "rank_5.x", "rank_6.x")) |>
  inner_join(vote_counts, join_by(rank_1.y)) |> 
  filter(peacock) |> 
  count(rank_1.y, sort = TRUE) |> 
  slice_head(n=10) |> 
  mutate(pct = n/sum(n))

rene_voters_normal_candidates <- mayor_and_dall |> 
  filter(rank_1.x == "Rene Gonzalez") |> 
  #select(-c("rank_3.x", "rank_4.x", "rank_5.x", "rank_6.x")) |>
  inner_join(vote_counts, join_by(rank_1.y)) |> 
  filter(!peacock) |> 
  count(rank_1.y, sort = TRUE) |> 
  slice_head(n=10) |> 
  mutate(pct = n/sum(n))

carmen_voters_peacock_candidates <- mayor_and_dall |> 
  filter(rank_1.x == "Carmen Rubio") |> 
  #select(-c("rank_3.x", "rank_4.x", "rank_5.x", "rank_6.x"))  |> 
  inner_join(vote_counts, join_by(rank_1.y)) |> 
  filter(peacock) |> 
  count(rank_1.y, sort = TRUE) |> 
  slice_head(n=10) |> 
  mutate(pct = n/sum(n))

carmen_voters_normal_candidates <- mayor_and_dall |> 
  filter(rank_1.x == "Carmen Rubio") |> 
  #select(-c("rank_3.x", "rank_4.x", "rank_5.x", "rank_6.x"))  |> 
  inner_join(vote_counts, join_by(rank_1.y)) |> 
  filter(!peacock) |> 
  count(rank_1.y, sort = TRUE) |> 
  slice_head(n=10) |> 
  mutate(pct = n/sum(n))

sum(rene_voters_peacock_candidates$n)
sum(rene_voters_normal_candidates$n)
sum(carmen_voters_peacock_candidates$n)
sum(carmen_voters_normal_candidates$n)
