
#need to make this work well for mayor
#reinsert fixed quota back into the functions

#Still have a problem when no one gets over quota on last ballot if quota is fixed

library(tidyverse)

load("~/Library/Mobile Documents/com~apple~CloudDocs/PSU Data Science Spring 2026/260511/process_cvr_objects.RData")

source("new_cvr_functions.R")
mayor_cand_example <- "Rene Gonzalez"
d3_mayor <- combine_mayor_and_district(3, mayor_cand = mayor_cand_example) 
d2_mayor <- combine_mayor_and_district(2, mayor_cand = mayor_cand_example) 
d1_mayor <- combine_mayor_and_district(1, mayor_cand = mayor_cand_example) 
d4_mayor <- combine_mayor_and_district(4, mayor_cand = mayor_cand_example) 


precinct_reference <- precinct_reference |> 
  mutate(house_district = str_sub(precinct, 1, 2))
d2_mayor <- eliminate_zero_vote_rank_1_candidates(d2_mayor)

council_d4_2024_M4101 <- council_d4_2024 |> 
  filter(precinct == "6")
SOURCE_DATA_NAME <- "council_d4_2024"
#ABSOLUTE_QUOTA <- 19180
MY_SAMPLE <- read_csv("logic_fix.csv")

####################### 
# temporarily eliminate ballots with "write-in"
#######################
council_d4_2024 <- elim_write_in(council_d4_2024)
#SOURCE_DATA_NAME <- "council_d4_2024"
#########################


precinct_reference <- precinct_reference |> 
  mutate(id_number = as.integer(id_number))

# p_choices <- get(SOURCE_DATA_NAME) |> 
#   left_join(precinct_reference, join_by(precinct==id_number)) |> 
#   filter(house_district == 42) |> 
#   select(-c(precinct.y, house_district))

#SOURCE_DATA_NAME <- "p_choices"


n_seats <- 3
dlist <- vector("list", length = 1)
i <- 1

start <- Sys.time()
dlist[[1]] <- gen_initial_list(SOURCE_DATA_NAME, seats = n_seats, absolute_quota = FALSE)

#need to convert this to a WHILE loop and figure out what I'm actually counting

while (TRUE) {  #!(dlist[[i]]$to_be_elected == 1 &
                  # dlist[[i]]$elec_status_semaphore != "elected")) {

      dlist[[i+1]] <- 
        gen_status_list(dlist[[i]], seats = n_seats, overall_quota_status = "FIXED",
                        absolute_quota = FALSE)
        
      
      print(paste("round:",i,"  ", 
                  dlist[[i]]$eliminated_candidate, " ", 
                  dlist[[i]]$to_be_elected,
                  dlist[[i]]$elected_candidate))
      i <- i + 1
      if(dlist[[i]]$to_be_elected == 0) break
     
  }
  
dlist <- dlist[-length(dlist)]

tally_master <- dlist[1:length(dlist)]  |> 
  map("tally") |> 
  list_rbind(names_to = "round")

dlist[1:length(dlist)] |>  
  map_dbl("top_vote_total") 

dlist |> 
  map_dbl("quota") 

end <- Sys.time()

start
end

