library(dplyr)

generate_all_candidates_vector <- function() {
  bind_rows(council_d1_2024,
                            council_d2_2024,
                            council_d3_2024,
                            council_d4_2024,
                            mayor_2024) |>
  pivot_longer(cols = !c(recordID, precinct), 
               names_to = "choice_num", values_to = "choice") |> 
  filter(choice != "undervote" & 
           !str_detect(choice, "\n")) |>
  select(choice) |> 
  arrange(choice) |> 
  distinct()
}

eliminate_zero_vote_rank_1_candidates <- function(office_input) {
  
  #the scenario:  a candidate has zero first place votes but, if there are some 2-6 votes
  #because of the surplus value routine, those candidates will end up some 
  #surplus votes (potentially) and not get eliminated from the last ballots
  
  #This routine eliminates any candidate without a first place ranking completely from all rankings for all ballots at the start of balloting.  It just needs to be run once before the rounds are computed.
  
  office_output <- office_input |> 
    pivot_longer(cols = !c(recordID, precinct), 
                 names_to = "choice_num", values_to = "choice") |> 
    mutate(choice_num = as.numeric(str_sub(choice_num, -1, -1))) |> 
    filter(choice != "undervote" & 
             !str_detect(choice, "\n")) 
  
  tally <- office_output |> 
    filter(choice_num == 1) |> 
    count(choice, sort=TRUE)
  
  elim_candidates <- all_candidates |> 
    anti_join(tally, by=c("choice"))
  
  office_output <- office_output |> 
    
    filter(!(choice %in% elim_candidates$choice)) |> 
    group_by(recordID, precinct, choice) |> 
      slice_min(choice_num, with_ties = FALSE) |> 
    ungroup() |> 
    
    group_by(recordID, precinct) |> 
     mutate(choice_num = rank(choice_num)) |> 
    ungroup() |> 
  
    pivot_wider(id_cols = c("recordID", "precinct"),
                names_from = choice_num,
                values_from = choice)
  
  if (!("6" %in% names(office_output))) {
    office_output$`6` <- NA
  }
  if (!("5" %in% names(office_output))) {
    office_output$`5` <- NA
  }
  if (!("4" %in% names(office_output))) {
    office_output$`4` <- NA
  }
  if (!("3" %in% names(office_output))) {
    office_output$`3` <- NA
  }
  if (!("2" %in% names(office_output))) {
    office_output$`2` <- NA
  }
  if (!("1" %in% names(office_output))) {
    office_output$`1` <- NA
  }
  
  office_output <- office_output |> 
    relocate(recordID, precinct, rank_1 = `1`, rank_2 = `2`, rank_3 = `3`,
             rank_4 = `4`, rank_5 = `5`, rank_6 = `6`) 
  
}

compute_pdx_stv <- function(office_input, eliminated = "", elected = "") {
  
  office_output <- office_input |> 
    pivot_longer(cols = !c(recordID, precinct), 
                 names_to = "choice_num", values_to = "choice") |> 
    mutate(choice_num = as.numeric(str_sub(choice_num, -1, -1))) |> 
    #FIX THIS above: will not work if rank positions is > 9
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
  
  if (!("6" %in% names(office_output))) {
    office_output$`6` <- NA
  }
  if (!("5" %in% names(office_output))) {
    office_output$`5` <- NA
  }
  if (!("4" %in% names(office_output))) {
    office_output$`4` <- NA
  }
  if (!("3" %in% names(office_output))) {
    office_output$`3` <- NA
  }
  if (!("2" %in% names(office_output))) {
    office_output$`2` <- NA
  }
  if (!("1" %in% names(office_output))) {
    office_output$`1` <- NA
  } 
  
  office_output|> 
    relocate(recordID, precinct, rank_1 = `1`, rank_2 = `2`, rank_3 = `3`,
             rank_4 = `4`, rank_5 = `5`, rank_6 = `6`) 
  
}

generate_rank_1_tally <- function(office_input) {
  
  office_input |> 
    select(rank_1) |> 
    filter(rank_1 != "undervote" & 
             !str_detect(rank_1, "\n")) |> 
    count(rank_1, sort = TRUE)
  
}

elec_status <- function(votes = 0, elec_quota) {
  if(votes >= elec_quota) {
    "elected"
  } else {
    "eliminated"
  }
}

eliminated_status_list_gen <- function(dlist_input, 
                                       seats = 3, 
                                       overall_quota_status = "FIXED",
                                       absolute_quota = FALSE) {

  to_be_elected <- dlist_input$to_be_elected
  
  if(to_be_elected == 0) {
    return(list(cvr = NULL, tally = NULL,
                quota = NULL, to_be_elected = 0, 
                surplus_factor = NULL,
                top_vote_total = NULL, elec_status_semaphore = NULL,
                elected_candidate = NULL, 
                elimiNULLted_candidate = NULL,
                transfer_values = NULL))
  }
  
  cvr <- compute_pdx_stv(dlist_input$cvr, eliminated = dlist_input$eliminated_candidate)
  
  surplus_factor <- dlist_input$surplus_factor
  transfer_values <- dlist_input$transfer_values
  
  cvr <- cvr |> 
    left_join(dlist_input$transfer_value, join_by(recordID, precinct))
  
  tally <- cvr |> 
    group_by(rank_1) |> 
    summarize(n = sum(transfer_value)) |> 
    arrange(desc(n)) |> 
    ungroup()
  
  transfer_values <- cvr |> 
    select(recordID, precinct, transfer_value)
  
  cvr <- cvr |> 
    select(!transfer_value)
  
  if(overall_quota_status == "FIXED") {
    quota <- nrow(dlist[[1]]$cvr) / (seats + 1) + 1 
  } else {
    quota <- nrow(cvr) / (seats + 1) + 1 
  }
  
  if(absolute_quota) quota = ABSOLUTE_QUOTA
  
  

  if(nrow(tally) > 0) {
    top_vote_total <- tally[[1,2]]
  }
  
  if(nrow(tally) <= dlist_input$to_be_elected |
     (elec_status(top_vote_total, quota) == "elected" & nrow(tally) > 0)) {
    
    eliminated_candidate <- "" #tally_initial[[nrow(tally_initial), 1]]
    tally[[1,2]] <- quota
    elected_candidate <- tally[[1, 1]]
    elec_status_semaphore <- "elected"
  } 
  else if(elec_status(top_vote_total, quota) == "eliminated" & nrow(tally) > 0) {
    
    eliminated_candidate <- tally[[nrow(tally), 1]]
    elected_candidate <- ""
    elec_status_semaphore <- "eliminated"
    
  } else if(nrow(tally) == 1 |
            (elec_status(top_vote_total, quota) == "elected" & nrow(tally) > 0)) {
    
    eliminated_candidate <- "" #tally_initial[[nrow(tally_initial), 1]]
    tally[[1,2]] <- quota
    elected_candidate <- tally[[1, 1]]
    elec_status_semaphore <- "elected"
  }
  
  
  list(cvr = cvr, tally = tally,
       quota = quota, to_be_elected = to_be_elected, 
       top_vote_total = top_vote_total, elec_status_semaphore = elec_status_semaphore,
       elected_candidate = elected_candidate, 
       eliminated_candidate = eliminated_candidate,
       transfer_values = transfer_values,
       surplus_factor = surplus_factor)
  
}

elected_status_list_gen <- function(dlist_input, 
                                    seats = 3, 
                                    overall_quota_status = "FIXED",
                                    absolute_quota = FALSE) {
  #https://www.portland.gov/code/2/08/030
  
  to_be_elected <- dlist_input$to_be_elected - 1
  #need to return a list that allows us to break the loop
  if(to_be_elected == 0) {
    return(list(cvr = 0, tally = 0,
                quota = 0, to_be_elected = 0, 
                surplus_factor = 0,
                top_vote_total = 0, elec_status_semaphore = 0,
                elected_candidate = 0, 
                elimiNULLted_candidate = 0,
                transfer_values = 0))
  }
  
  new_surplus_factor <- (
    dlist_input$top_vote_total - dlist_input$quota) / 
    dlist_input$top_vote_total
  
  winner_cvr <- dlist_input$cvr |> 
    filter(rank_1 == dlist_input$elected_candidate) |>
    compute_pdx_stv(elected = dlist_input$elected_candidate) |> 
    left_join(dlist_input$transfer_values, join_by(recordID, precinct)) |> 
    mutate(transfer_value = new_surplus_factor * transfer_value)
  
  non_winner_cvr <- dlist_input$cvr |> 
    filter(rank_1 != dlist_input$elected_candidate)
  
  if(nrow(non_winner_cvr) > 0) {
    non_winner_cvr <- non_winner_cvr |> 
      compute_pdx_stv(elected = dlist_input$elected_candidate)  |> 
      left_join(dlist_input$transfer_values, join_by(recordID, precinct))
  }
  
  
  if(nrow(non_winner_cvr) > 0) {
    cvr <- bind_rows(winner_cvr, non_winner_cvr)
  } else {
    cvr <- winner_cvr
  }
  
  tally <- cvr |> 
    group_by(rank_1) |> 
    summarize(n = sum(transfer_value)) |>
    arrange(desc(n)) |> 
    ungroup()
  
  top_vote_total <- tally[[1,2]]
  
  
  transfer_values <- cvr |> 
    select(recordID, precinct, transfer_value)
  
  cvr <- cvr |> 
    select(!transfer_value)
  
  if(overall_quota_status == "FIXED") {
    quota <- nrow(dlist[[1]]$cvr) / (seats + 1) + 1 
  } else {
    quota <- nrow(cvr) / (seats + 1) + 1 
  }
  
  if(absolute_quota) quota = ABSOLUTE_QUOTA
  
  if(elec_status(top_vote_total, quota) == "eliminated" & nrow(tally) > 1) {
    
    eliminated_candidate <- tally[[nrow(tally), 1]]
    elected_candidate <- ""
    elec_status_semaphore <- "eliminated"
    
  } else if(elec_status(top_vote_total, quota) == "elected" | nrow(tally) == 1) {
    
    eliminated_candidate <- ""
    elected_candidate <- tally[[1, 1]]
    elec_status_semaphore <- "elected"
  }
  
  list(cvr = cvr, tally = tally,
       quota = quota, to_be_elected = to_be_elected, 
       surplus_factor = new_surplus_factor,
       top_vote_total = top_vote_total, elec_status_semaphore = elec_status_semaphore,
       elected_candidate = elected_candidate, 
       eliminated_candidate = eliminated_candidate,
       transfer_values = transfer_values)
}

gen_status_list <- function(basis_list, ...) {
  
  if(basis_list$to_be_elected == 0) return("all done")
  
  if(basis_list$elec_status_semaphore == "eliminated") {
    new_list <- eliminated_status_list_gen(basis_list, ...)
  }
  
  if(basis_list$elec_status_semaphore == "elected") {
    new_list <- elected_status_list_gen(basis_list, ...)
  }
  return(new_list)
}

candidate_number <- function(office_input) {
  office_input |> 
    pivot_longer(cols = !c(recordID, precinct), 
                names_to = "choice_num", values_to = "choice") |> 
    select(choice) |> 
    distinct() |> 
    na.omit() |> 
  nrow()
}

combine_mayor_and_district <- function(district, mayor_cand = "ALL") {
  district_data <- str_c("council_d", as.character(district), "_2024")
  
  mayor_2024 |> 
    select(recordID, precinct, rank_1) |> 
    inner_join(get(district_data), by=(c("recordID", "precinct"))) |> 
    rename(rank_m = rank_1.x, rank_1 = rank_1.y) |> 
    filter(if (mayor_cand != "ALL") rank_m == mayor_cand else TRUE) |> 
    select(-rank_m)
}

gen_initial_list <- function(SOURCE_DATA_NAME, 
                             seats = 3, 
                             absolute_quota = FALSE) {
  
  cvr_initial <- compute_pdx_stv(get(SOURCE_DATA_NAME)) 
  
  transfer_value_initial <- cvr_initial |> 
    select(recordID, precinct) |>
    mutate(transfer_value = 1)
  
  tally_initial <- generate_rank_1_tally(cvr_initial)
  
  quota <- nrow(cvr_initial) / (seats + 1) + 1
  if(absolute_quota) quota <- ABSOLUTE_QUOTA

  to_be_elected <- seats

  top_vote_total <- tally_initial[[1,2]]
  
  if(elec_status(top_vote_total, quota) == "eliminated" ) {
    
    eliminated_candidate <- tally_initial[[nrow(tally_initial), 1]]
    elected_candidate <- ""
    elec_status_semaphore <- "eliminated"
    
  } else if(elec_status(top_vote_total, quota) == "elected" ) {
    
    eliminated_candidate <- "" #tally_initial[[nrow(tally_initial), 1]]
    tally_initial[[1,2]] <- round(quota)
    elected_candidate <- tally_initial[[1, 1]]
    elec_status_semaphore <- "elected"
  }
  
  list(cvr = cvr_initial, tally = tally_initial,
       quota = quota, to_be_elected = to_be_elected, 
       top_vote_total = top_vote_total, elec_status_semaphore = elec_status_semaphore,
       elected_candidate = elected_candidate, 
       eliminated_candidate = eliminated_candidate,
       transfer_values = transfer_value_initial,
       surplus_factor = 1)
}


elim_write_in <- function(office_input, eliminated = "", elected = "") {
  
  office_output <- office_input |> 
    pivot_longer(cols = !c(recordID, precinct), 
                 names_to = "choice_num", values_to = "choice") |> 
    mutate(choice_num = as.numeric(str_sub(choice_num, -1, -1))) |> 
    #FIX THIS above: will not work if rank positions is > 9
    filter(choice != "undervote" & 
             !str_detect(choice, "\n") & 
             choice != eliminated &
             choice != elected &
             !str_detect(choice, "Write-in-"))|> 
    
    group_by(recordID, precinct, choice) |> 
    slice_min(choice_num, with_ties = FALSE) |> 
    ungroup() |> 
    
    group_by(recordID, precinct) |> 
    mutate(choice_num = rank(choice_num)) |> 
    ungroup() |> 
    
    pivot_wider(id_cols = c("recordID", "precinct"),
                names_from = choice_num,
                values_from = choice)
  
  if (!("6" %in% names(office_output))) {
    office_output$`6` <- NA
  }
  if (!("5" %in% names(office_output))) {
    office_output$`5` <- NA
  }
  if (!("4" %in% names(office_output))) {
    office_output$`4` <- NA
  }
  if (!("3" %in% names(office_output))) {
    office_output$`3` <- NA
  }
  if (!("2" %in% names(office_output))) {
    office_output$`2` <- NA
  }
  if (!("1" %in% names(office_output))) {
    office_output$`1` <- NA
  } 
  
  office_output|> 
    relocate(recordID, precinct, rank_1 = `1`, rank_2 = `2`, rank_3 = `3`,
             rank_4 = `4`, rank_5 = `5`, rank_6 = `6`) 
  
}



