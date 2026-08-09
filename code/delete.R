#the trick of doing this correctly is to regenerate the ballot list 
#that gets created by pivot_longer

#count ballots and find a loser or winner
#eliminate that name and then reallocate that person's results into rank_1
#repeat

library(tidyverse)

compute_stv <- function(office_inp, eliminated = "") {
  
  office <- office_inp |> 
    pivot_longer(cols = !c(recordID, precinct), 
                 names_to = "choice_num", values_to = "choice") |> 
    mutate(choice_num = as.numeric(str_sub(choice_num, -1, -1))) |> 
    filter(choice != "undervote" & 
             !str_detect(choice, "\n") & 
              choice != eliminated) |> 
    
    group_by(recordID, precinct, choice) |> 
      slice_min(choice_num, with_ties = FALSE) |> 
    ungroup() |> 
    
    group_by(recordID, precinct) |> 
      mutate(choice_num = rank(choice_num)) |> 
    ungroup()
  
  
  office |> 
    pivot_wider(id_cols = c("recordID", "precinct"),
                names_from = choice_num,
                values_from = choice) |> 
    relocate(recordID, precinct, rank_1 = `1`, rank_2 = `2`, rank_3 = `3`,
           rank_4 = `4`, rank_5 = `5`, rank_6 = `6`) 

}

#Figure out how to build a baseline list

d0_init <- compute_stv(council_d3_2024)

tally_init <- d0_init |> 
  group_by(rank_1) |> 
    count(sort=TRUE) |> 
  ungroup()

eliminated_init <- tally_init[[nrow(tally_init), 1]]

txfer_init <- d0_init |> 
  filter(rank_1 == eliminated_init) |> 
  group_by(rank_2) |> 
  count(sort=TRUE) |> 
  ungroup() |> 
  rename(rank_1 = rank_2) |> 
  filter(!is.na(rank_1))


dlist00 <- list(cvr = d0_init, tally = tally_init, transfers = txfer_init, eliminated = eliminated_init)

do_the_d <- function(dlist_base) {

  tally <- dlist_base$tally |> 
    slice_head(n = -1) |> 
    left_join(dlist_base$transfers, join_by(rank_1)) |> 
    mutate(n.y = replace_na(n.y, 0)) |> 
    mutate(n = n.x + n.y) |> 
    select(rank_1, n) |> 
    arrange(desc(n))  

  d0 <- compute_stv(dlist_base$cvr, eliminated = dlist_base$eliminated)
  
  new_eliminated <- tally[[nrow(tally),1]]
  
  txfer <- d0 |> 
    filter(rank_1 == new_eliminated) |> 
    group_by(rank_2) |> 
      count(sort=TRUE) |> 
    ungroup() |> 
    rename(rank_1 = rank_2) |> 
    filter(!is.na(rank_1))
  
  list(cvr = d0, tally = tally, transfers = txfer, eliminated = new_eliminated)
}

dlist_base <- dlist00

dlist01 <- do_the_d(dlist00)


dlist02 <- do_the_d(dlist01)
dlist03 <- do_the_d(dlist02)
dlist04 <- do_the_d(dlist03)
dlist05 <- do_the_d(dlist04)
dlist06 <- do_the_d(dlist05)
dlist07 <- do_the_d(dlist06)
dlist08 <- do_the_d(dlist07)
dlist09 <- do_the_d(dlist08)

dlist10 <- do_the_d(dlist09)
dlist11 <- do_the_d(dlist10)
dlist12 <- do_the_d(dlist11)
dlist13 <- do_the_d(dlist12)
dlist14 <- do_the_d(dlist13)
dlist15 <- do_the_d(dlist14)
dlist16 <- do_the_d(dlist15)
dlist17 <- do_the_d(dlist16)

dlist18 <- do_the_d(dlist17)
dlist19 <- do_the_d(dlist18)
dlist20 <- do_the_d(dlist19)
dlist21 <- do_the_d(dlist20)
dlist22 <- do_the_d(dlist21)

#we now have a winner

#1. adjust tally for surplus votes (12 in this case)
#2. eliminate Novick from getting more votes
#3.  Find the next candidate to eliminate
#https://www.portland.gov/code/2/08/030


  quota <- 21129
  novick_surplus <- dlist22$tally[[1,2]] - quota
  surplus_fraction <- (dlist22$tally[[1,2]] - quota) / dlist22$tally[[1,2]]
  
  
  
tally <- dlist21$tally |> 
  slice_head(n = -1) |> 
  left_join(dlist21$transfers, join_by(rank_1)) |> 
  mutate(n.y = replace_na(n.y, 0)) |> 
  mutate(n = n.x + n.y) |> 
  select(rank_1, n) |> 
  arrange(desc(n))  

tally_check <- dlist22$cvr |> 
  group_by(rank_1) |> 
    count(sort=TRUE) |> 
  ungroup()
  
d0 <- compute_stv(dlist21$cvr, eliminated = dlist21$eliminated)

d1 <- d0 |> 
  filter(rank_1 == "Steve Novick") |> 
  group_by(rank_2) |> 
    count(sort = TRUE) |> 
  ungroup() |> 
  mutate(adjust = surplus_fraction * n)

d2 <- compute_stv(dlist22$cvr, eliminated = "Steve Novick")

new_eliminated <- tally[[nrow(tally),1]]

txfer <- d0 |> 
  filter(rank_1 == new_eliminated) |> 
  mutate(rank_2 = ifelse(rank_2 == "Steve Novick", rank_3, rank_2)) |> 

  group_by(rank_2) |> 
  count(sort=TRUE) |> 
  ungroup() |> 
  rename(rank_1 = rank_2) |> 
  filter(!is.na(rank_1))

#need to analyze these numbers



find_threshold <- function(cvr_input) {
  floor(nrow(cvr_input$cvr) / 4 + 1)
}

dlist00 <- dothed(council_d3_2024)
#create a the baseline object








