

load("~/Library/Mobile Documents/com~apple~CloudDocs/PSU Data Science Spring 2026/260511/process_cvr_objects.RData")


rm(list=setdiff(ls(), "council_d4_2024"))

library(tidyverse)

FIXED_QUOTA <- 19180
SOURCE_DATA <- "council_d4_2024"

elec_status <- function(votes, elec_quota) {
  if(votes >= elec_quota) {
    "elected"
  } else {
    "eliminated"
  }
}

compute_stv <- function(office_inp, eliminated = "", elected = "") {
  
  o <- office_inp |> 
    filter(rank_1 != elected) |> 
    pivot_longer(cols = !c(recordID, precinct), 
                 names_to = "choice_num", values_to = "choice") |> 
    mutate(choice_num = as.numeric(str_sub(choice_num, -1, -1))) |> 
    filter(choice != "undervote" & 
             !str_detect(choice, "\n") & 
             choice != eliminated &
             choice != elected) |> 
    
    group_by(recordID, precinct, choice) |> 
    slice_min(choice_num, with_ties = FALSE) |> 
    ungroup() |> 
    
    group_by(recordID, precinct) |> 
    mutate(choice_num = rank(choice_num)) |> 
    ungroup() |> 
    pivot_wider(id_cols = c("recordID", "precinct"),
                names_from = choice_num,
                values_from = choice)
  
  if (!("6" %in% names(o))) {
    o$`6` <- NA
  }
  if (!("5" %in% names(o))) {
    o$`5` <- NA
  }
  if (!("4" %in% names(o))) {
    o$`4` <- NA
  }
  if (!("3" %in% names(o))) {
    o$`3` <- NA
  }
  if (!("2" %in% names(o))) {
    o$`2` <- NA
  }
  if (!("1" %in% names(o))) {
    o$`1` <- NA
  }
  
  o <- o |> 
    relocate(recordID, precinct, rank_1 = `1`, rank_2 = `2`, rank_3 = `3`,
             rank_4 = `4`, rank_5 = `5`, rank_6 = `6`) 
  
}

cvr_initial <- compute_stv(get(SOURCE_DATA))
tally_initial <- cvr_initial |> 
  count(rank_1, sort=TRUE)
quota <- nrow(cvr_initial)/4 + 1
quota <- FIXED_QUOTA
to_be_elected <- 3
top_vote_total <- tally_initial[[1,2]]
print(elec_status(top_vote_total, quota))

if(elec_status(top_vote_total, quota) == "eliminated" ) {
  
  eliminated_candidate <- tally_initial[[nrow(tally_initial), 1]]
  elected_candidate <- ""
  elec_status_semaphore <- "eliminated"
  
} else if(elec_status(top_vote_total, quota) == "elected" ) {
  
  eliminated_candidate <- tally_initial[[nrow(tally_initial), 1]]
  elected_candidate <- tally_initial[[1, 1]]
  elec_status_semaphore <- "elected"
}

dlist00 <- list(cvr = cvr_initial, tally = tally_initial,
                quota = quota, to_be_elected = to_be_elected, 
                top_vote_total = top_vote_total, elec_status_semaphore = elec_status_semaphore,
                elected_candidate = elected_candidate, 
                eliminated_candidate = eliminated_candidate)

eliminated_status_list_gen <- function(dlist_input) {

  cvr <- compute_stv(dlist_input$cvr, eliminated = dlist_input$eliminated_candidate)
  tally <- cvr |> 
    count(rank_1, sort=TRUE)
  quota <- nrow(cvr_initial)/4 + 1
  quota <- FIXED_QUOTA
  to_be_elected <- dlist_input$to_be_elected
  
  
  top_vote_total <- tally[[1,2]]


  if(elec_status(top_vote_total, quota) == "eliminated" ) {
  
    eliminated_candidate <- tally[[nrow(tally), 1]]
    elected_candidate <- ""
    elec_status_semaphore <- "eliminated"
  
  } else if(elec_status(top_vote_total, quota) == "elected" ) {
  
    eliminated_candidate <- ""
    elected_candidate <- tally[[1, 1]]
    elec_status_semaphore <- "elected"
}

  list(cvr = cvr, tally = tally,
              quota = quota, to_be_elected = to_be_elected, 
                top_vote_total = top_vote_total, elec_status_semaphore = elec_status_semaphore,
                elected_candidate = elected_candidate, 
                eliminated_candidate = eliminated_candidate)

}

elected_status_list_gen <- function(dlist_input) {
  
  surplus_factor <- (dlist_input$top_vote_total - 
                       dlist_input$quota) / 
    dlist_input$top_vote_total
  
  winner_ballots <- dlist_input$cvr |> 
    filter(rank_1 == dlist_input$elected_candidate) |> 
    compute_stv(eliminated= dlist_input$elected_candidate) 
  
  tally_surplus <- dlist_input$cvr |> 
    filter(rank_1 == dlist_input$elected_candidate) |> 
    count(rank_2, sort=TRUE) |> 
    mutate(adjust = surplus_factor * n) |> 
    select(rank_2, adjust) |> 
    rename(rank_1 = rank_2, n = adjust)
  
  cvr <- compute_stv(dlist_input$cvr, elected = dlist_input$elected_candidate)
  #generate surplus factor and base the tally on this calculation
  tally <- cvr |> 
    count(rank_1, sort=TRUE) |> 
    left_join(tally_surplus, join_by(rank_1)) |> 
    mutate(n = n.x + n.y) |> 
    select(rank_1, n)
  
  quota <- nrow(cvr_initial)/4 + 1
  quota <- FIXED_QUOTA
  to_be_elected <- dlist_input$to_be_elected - 1
  top_vote_total <- tally[[1,2]]
  
  
  if(elec_status(top_vote_total, quota) == "eliminated" ) {
    
    eliminated_candidate <- tally[[nrow(tally), 1]]
    elected_candidate <- ""
    elec_status_semaphore <- "eliminated"
    
  } else if(elec_status(top_vote_total, quota) == "elected" ) {
    
    eliminated_candidate <- ""
    elected_candidate <- tally[[1, 1]]
    elec_status_semaphore <- "elected"
  }
  
  list(cvr = cvr, tally = tally,
       quota = quota, to_be_elected = to_be_elected, 
       top_vote_total = top_vote_total, elec_status_semaphore = elec_status_semaphore,
       elected_candidate = elected_candidate, 
       eliminated_candidate = eliminated_candidate,
       tally_surplus = tally_surplus, 
       winner_ballots = winner_ballots,
       surplus_factor = surplus_factor)
  
}


gen_status_list <- function(basis_list) {
  if(basis_list$elec_status_semaphore == "eliminated") {
    new_list <- eliminated_status_list_gen(basis_list)
  }
  
  if(basis_list$elec_status_semaphore == "elected") {
    new_list <- elected_status_list_gen(basis_list)
  }
  return(new_list)
}


vec <- sprintf("dlist%02d", 0:34)

dlist <- vector("list", length = 34)
dlist[[1]] <- dlist00

for (i in 1:34) {
  print(paste(dlist[[i]]$eliminated_candidate, " ", dlist[[i]]$to_be_elected))
  dlist[[i+1]] <- gen_status_list(dlist[[i]])
}


#fix elected routine
# y <- x$cvr |> 
#   filter(rank_1 == "Mitch Green") |> 
#   count(rank_2)
#the problem is we need to keep transferring the surplus votes from round to round
#even when other candidates are eliminated.  When Green won he produced 89.2 surplus votes
#for Arnold and 117 for Zimmerman (206 total).  When Clark won the number was infinteimally
#smaller but still greater than zero, as eliminated candidates gave up their votes as surplus 

#the way we fix this:

#retain the ballots after a round with a winner
#determine the new first place winner of each ballot
#adjust each ballot value by the surplus factor.  THIS VALUE WONT CHANGE AS NA's ARE DROPPED
#add the ballot values to each candidate

#repeat after each elimination but retain the ballot values

#repeat the process after the second winner


