#the trick of doing this correctly is to regenerate the ballot list 
#that gets created by pivot_longer

#count ballots and find a loser or winner
#eliminate that name and then reallocate that person's results into rank_1
#repeat

load("~/Library/Mobile Documents/com~apple~CloudDocs/PSU Data Science Spring 2026/260511/process_cvr_objects.RData")

rm(list=setdiff(ls(), "council_d4_2024"))

library(tidyverse)

compute_stv <- function(office_inp, eliminated = "", elected = "") {
  
  office_inp |> 
    filter(rank_1 != elected) |> 
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
    ungroup() |> 
    pivot_wider(id_cols = c("recordID", "precinct"),
                names_from = choice_num,
                values_from = choice) |> 
    relocate(recordID, precinct, rank_1 = `1`, rank_2 = `2`, rank_3 = `3`,
           rank_4 = `4`, rank_5 = `5`, rank_6 = `6`) 

}



make_iteration <- function(cvr = cvr_table) {

  tally_1 <- cvr_table |>
      count(rank_1, sort=TRUE)
    
  quota <- (nrow(d0_init) / 4) + 1
  max_ballots <- max(tally_1$n)
  
  elim_or_elected <- ifelse(quota <= max_ballots, "elected", "eliminated")
  
  if(elim_or_elected == "eliminated") {
    
    eliminated_1 <- tally_1[[nrow(tally_1), 1]]
    elected_1 <- NA
    
    
    txfer_1 <- cvr_table |> 
      filter(rank_1 == eliminated_1) |> 
      count(rank_2, sort=TRUE) |> 
      rename(rank_1 = rank_2) |> 
      filter(!is.na(rank_1))
  }
  
  if(elim_or_elected == "elected") {
    elected_1 <- tally_1[[1, 1]]
    eliminated_1 <- NA
    
    txfer_1 <- cvr_table |> 
      filter(rank_1 == elected_1) |> 
      count(rank_2, sort=TRUE) |> 
      mutate(surplus_value = (max_ballots - quota) / max_ballots * n) |> 
      select(rank_2, n = surplus_value) |> 
      rename(rank_1 = rank_2) |> 
      filter(!is.na(rank_1)) 
    
  }
  list(cvr = cvr_table, tally = tally_1, transfers = txfer_1, eliminated = eliminated_1,
       elected = elected_1, elim_or_elected)
  
}

dlist00 <- make_iteration(processed_cvr)
dlist01 <- make_iteration(processed_cvr_1)

























### take one object and create a new one assuming elimination

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


# generate objects based on elimination

dlist01 <- do_the_d(dlist00)
dlist02 <- do_the_d(dlist01)
dlist03 <- do_the_d(dlist02)
dlist04 <- do_the_d(dlist03)
dlist05 <- do_the_d(dlist04)
dlist06 <- do_the_d(dlist05)
dlist07 <- do_the_d(dlist06)
dlist08 <- do_the_d(dlist0)










#we now have a winner

#1. adjust tally for surplus votes (12 in this case)
#2. eliminate Novick from getting more votes
#3.  Find the next candidate to eliminate
#https://www.portland.gov/code/2/08/030
#https://fairvote.org/archives/multi_winner_rcv_example/
#As candidates are eliminated, their fractional share of votes
#given to them by the surplus of a winning candidate continue to get spread
#at that fraction

#we have triggered a semaphore that tells us we have a winner

number_of_winners <- 1
winner_1_voters <- dlist22$cvr |> 
  filter(rank_1 == "Steve Novick")
no_winner_1_voters <- compute_stv(dlist22$cvr, eliminated = "Steve Novick")

quota <- 21129
winner_1_surplus <- dlist22$tally[[1,2]] - quota
surplus_fraction_1 <- (dlist22$tally[[1,2]] - quota) / dlist22$tally[[1,2]]

xfer_winner_1 <- dlist22$cvr |> 
  filter(rank_1 == "Steve Novick") |> 
  group_by(rank_2) |> 
    count(sort = TRUE) |> 
  ungroup() |> 
  mutate(adjust = surplus_fraction_1 * n) |> 
  filter(!is.na(rank_2)) |> 
  add_row(rank_2 = "Steve Novick", n = quota, adjust = winner_1_surplus * -1) |> 
  rename(rank_1 = rank_2)

new_tally <- dlist22$tally |> 
  left_join(xfer_winner_1, join_by(rank_1)) |> 
  mutate(n = n.x + adjust) |> 
  select(rank_1, n)

new_eliminated <- new_tally[[nrow(new_tally),1]]

new_cvr <- compute_stv(dlist22$cvr, eliminated = new_eliminated)
  
txfer <- dlist22$cvr |> 
  filter(rank_1 == new_eliminated) |> 
  mutate(rank_2 = ifelse(rank_2 == "Steve Novick", rank_3, rank_2)) |> 
  group_by(rank_2) |> 
    count(sort=TRUE) |> 
  ungroup() |> 
  rename(rank_1 = rank_2) |> 
  filter(!is.na(rank_1))

new_tally_1 <- new_tally |> 
  inner_join(txfer, join_by(rank_1)) |>
  mutate(n = n.x + n.y) |> 
  select(rank_1, n) |> 
  add_row(rank_1 = "Steve Novick", n = quota)

dlist24 <- list(cvr = new_cvr, tally = new_tally_1, transfers = txfer, eliminated = new_eliminated)



## Need routine to add back the surplus of the eliminated candidate

#in this case, we consider the sshare of votes that went to 
#Jon Walker.  How many Novick votes did he get?
#My tally shows 0.19980133
#he gets eliminated....that number is passed forward to Novick voters' next
#choice



#need to analyze these numbers



find_threshold <- function(cvr_input) {
  floor(nrow(cvr_input$cvr) / 4 + 1)
}

dlist00 <- dothed(council_d3_2024)
#create a the baseline object








