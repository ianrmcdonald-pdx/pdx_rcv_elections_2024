

library(tidyverse)

load("~/Library/Mobile Documents/com~apple~CloudDocs/PSU Data Science Spring 2026/260511/process_cvr_objects.RData")

source("new_cvr_functions.R")

tally_number <- 10

xt <- council_d4_2024 |> 
  count(rank_1, sort = TRUE) |> 
  slice_head(n=tally_number) |> 
  select(rank_1) 
  
xt <- xt$rank_1

x_cvr <- council_d4_2024 |> 
  mutate(rank_1 = ifelse(rank_1 %in% xt, rank_1, "undervote")) |> 
  mutate(rank_2 = ifelse(rank_2 %in% xt, rank_2, "undervote")) |> 
  mutate(rank_3 = ifelse(rank_3 %in% xt, rank_3, "undervote")) |> 
  mutate(rank_4 = ifelse(rank_4 %in% xt, rank_4, "undervote")) |> 
  mutate(rank_5 = ifelse(rank_5 %in% xt, rank_5, "undervote")) |> 
  mutate(rank_6 = ifelse(rank_6 %in% xt, rank_6, "undervote"))

x_cvr_1 <- compute_pdx_stv(x_cvr, eliminated = "")

x_cvr_2 <- x_cvr_1 |> select(-c(recordID, precinct))

